/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Labratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

/*! 
 * \file power_plant_capture_component.cpp
 * \ingroup Objects
 * \brief PowerPlantCaptureComponent source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "util/base/include/xml_helper.h"
#include "technologies/include/power_plant_capture_component.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"
#include "containers/include/scenario.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/dependency_finder.h"
#include "containers/include/scenario.h"

using namespace std;

extern Scenario* scenario;

/*!
 * \brief Constructor.
 * \details Protected constructor which prevents the capture component from
 *          being created without using the CaptureComponentFactory.
 */
PowerPlantCaptureComponent::PowerPlantCaptureComponent():
mSequesteredAmount( scenario->getModeltime()->getmaxper() ),
mRemoveFraction( 0 ),
mCaptureEnergy( 0 ),
mNonEnergyCostPenalty( 0 )
{
}

PowerPlantCaptureComponent* PowerPlantCaptureComponent::clone() const {
    return new PowerPlantCaptureComponent( *this );
}

bool PowerPlantCaptureComponent::isSameType( const std::string& aType ) const {
    return aType == getXMLNameStatic();
}

/*!
 * \brief Get the XML node name in static form for comparison when parsing XML.
 * \details This public function accesses the private constant string, XML_NAME.
 *          This way the tag is always consistent for both read-in and output
 *          and can be easily changed. The "==" operator that is used when
 *          parsing, required this second function to return static.
 * \note A function cannot be static and virtual.
 * \author Josh Lurz, James Blackwood
 * \return The constant XML_NAME as a static.
 */
const string& PowerPlantCaptureComponent::getXMLNameStatic() {
    const static string XML_NAME = "power-plant-capture-component";
    return XML_NAME;
}

const string& PowerPlantCaptureComponent::getName() const {
    return getXMLNameStatic();
}

// Documentation inherits.
bool PowerPlantCaptureComponent::XMLParse( const xercesc::DOMNode* node ){
    /*! \pre Assume we are passed a valid node. */
    assert( node );

    const xercesc::DOMNodeList* nodeList = node->getChildNodes();
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ) {
        const xercesc::DOMNode* curr = nodeList->item( i );
        if( curr->getNodeType() != xercesc::DOMNode::ELEMENT_NODE ){
            continue;
        }
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
        if( nodeName == "storage-market" ){
            mStorageMarket = XMLHelper<string>::getValue( curr );
        }
        else if( nodeName == "remove-fraction" ){
            mRemoveFraction = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "capture-energy" ){
            mCaptureEnergy = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "non-energy-penalty" ){
            mNonEnergyCostPenalty = XMLHelper<double>::getValue( curr );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Unknown tag " << nodeName << " encountered while processing "
                    << getXMLNameStatic() << endl;
        }
    }

    // TODO: Handle success and failure better.
    return true;
}

void PowerPlantCaptureComponent::toInputXML( ostream& aOut,
                                             Tabs* aTabs ) const
{
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElementCheckDefault( mStorageMarket, "storage-market", aOut, aTabs, string( "" ) );
    XMLWriteElementCheckDefault( mRemoveFraction, "remove-fraction", aOut, aTabs, 0.0 );
    XMLWriteElementCheckDefault( mCaptureEnergy, "capture-energy", aOut, aTabs, 0.0 );
    XMLWriteElementCheckDefault( mNonEnergyCostPenalty, "non-energy-penalty", aOut, aTabs, 0.0 );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void PowerPlantCaptureComponent::toDebugXML( const int aPeriod,
                                             ostream& aOut,
                                             Tabs* aTabs ) const
{
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElement( mStorageMarket, "storage-market", aOut, aTabs );
    XMLWriteElement( mRemoveFraction, "remove-fraction", aOut, aTabs );
    XMLWriteElement( mCaptureEnergy, "capture-energy", aOut, aTabs );
    XMLWriteElement( mNonEnergyCostPenalty, "non-energy-penalty", aOut, aTabs );
    XMLWriteElement( mSequesteredAmount[ aPeriod ], "sequestered-amount", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void PowerPlantCaptureComponent::completeInit( const string& aRegionName,
                                               const string& aSectorName,
                                               DependencyFinder* aDependencyFinder )
{
    // Add the storage market as a dependency of the sector. This is because
    // this sector will have to be ordered first so that the total demand and
    // price for storage are known.
    if( aDependencyFinder ){
        aDependencyFinder->addDependency( aSectorName, mStorageMarket );
    }
    
    // Check that the remove fraction is valid.
    if( mRemoveFraction < 0 || mRemoveFraction > 1 ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Invalid removal fraction of " << mRemoveFraction << "." << endl;
        mRemoveFraction = 0;
    }
}

void PowerPlantCaptureComponent::initCalc( const string& aRegionName,
                                           const string& aSectorName,
                                           const string& aFuelName,
                                           const int aPeriod )
{
    // Calculate the emissions coefficient of the fuel.
    const IInfo* fuelInfo = scenario->getMarketplace()->getMarketInfo( aFuelName, aRegionName, aPeriod, true );
    mCachedFuelCoef = fuelInfo ? fuelInfo->getDouble( "CO2Coef", true ) : 0;
}

double PowerPlantCaptureComponent::getStorageCost( const string& aRegionName,
                                                   const int aPeriod ) const
{
    // Check if there is a market for storage.
    double storageMarketPrice = scenario->getMarketplace()->getPrice( mStorageMarket,
                                                                      aRegionName,
                                                                      aPeriod, true );

    // If there is no carbon market so use a large number to disable the
    // technology. Otherwise use the storage market price.
    return ( storageMarketPrice == Marketplace::NO_MARKET_PRICE ) ? util::getLargeNumber()
                                                                  : storageMarketPrice;
}

double PowerPlantCaptureComponent::getRemoveFraction() const {
    return mRemoveFraction;
}

void PowerPlantCaptureComponent::calcSequesteredAmount( const string& aRegionName,
                                                        const string& aGHGName,
                                                        const double aInput,
                                                        const double aOutput,
                                                        const double aInputCoef,
                                                        const double aOutputCoef,
                                                        const int aPeriod )
{
    // Currently sequestration objects should only be used for CO2.
    assert( aGHGName == "CO2" );

    // Calculate the amount.
    mSequesteredAmount[ aPeriod ] = mRemoveFraction 
                                    * ( aInput * aInputCoef - aOutput * aOutputCoef );
    
    // Add the demand to the marketplace.
    if( mSequesteredAmount[ aPeriod ] > 0 ){
        // Set sequestered amount as demand side of carbon storage market.
        Marketplace* marketplace = scenario->getMarketplace();
        marketplace->addToDemand( mStorageMarket, aRegionName, mSequesteredAmount[ aPeriod ], aPeriod,
                                  false );
    }
}

double PowerPlantCaptureComponent::getSequesteredAmount( const string& aGHGName,
                                                         const bool aGetGeologic,
                                                         const int aPeriod ) const 
{
    // Only return emissions if the type of the sequestration equals is geologic.
    // TODO: Determine how to handle other GHGs.
    if( aGetGeologic && aGHGName == "CO2" ){
        return mSequesteredAmount[ aPeriod ];
    }
    return 0;
}

double PowerPlantCaptureComponent::getEffectiveEfficiency( const double aTechnologyEfficiency,
                                                           const int aPeriod ) const
{
    // Check for a valid efficiency.
    assert( aTechnologyEfficiency > 0 );

    // Check that the coefficient has been initialized.
    assert( mCachedFuelCoef.isInited() );

    // Calculate effective efficiency, reduces the efficiency by a penalty.
    return aTechnologyEfficiency - mCaptureEnergy * mCachedFuelCoef * mRemoveFraction;
}

double PowerPlantCaptureComponent::getTotalNonEnergyCost( const double aTechnologyEfficiency,
                                                          const double aTechnologyNonEnergyCost,
                                                          const int aPeriod ) const
{
    // Check for valid efficiencies and non-energy costs.
    assert( aTechnologyEfficiency > 0 );
    assert( aTechnologyNonEnergyCost >= 0 );

    // Check that the coefficient has been initialized.
    assert( mCachedFuelCoef.isInited() );
    
    // Calculate the "a" term.
    const double a = 1 / aTechnologyEfficiency * mCachedFuelCoef * mRemoveFraction;
    
    // A must be positive.
    assert( a >= 0 );   
    
    // Calculate the total non-energy cost.
    const double totalNonEnergyCost = ( aTechnologyNonEnergyCost + a * mNonEnergyCostPenalty )
                                      * aTechnologyEfficiency / getEffectiveEfficiency( aTechnologyEfficiency,
                                                                                        aPeriod );

    // Total non-energy cost is greater or equal to zero.
    assert( totalNonEnergyCost >= 0 );
    return totalNonEnergyCost;
}
