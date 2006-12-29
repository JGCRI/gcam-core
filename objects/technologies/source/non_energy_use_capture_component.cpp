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
 * \file non_energy_use_capture_component.cpp
 * \ingroup Objects
 * \brief NonEnergyUseCaptureComponent source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "util/base/include/xml_helper.h"
#include "technologies/include/non_energy_use_capture_component.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/model_time.h"
#include "containers/include/scenario.h" // for modeltime.

using namespace std;

extern Scenario* scenario; // For modeltime.

/*!
 * \brief Constructor.
 * \details Protected constructor which prevents the capture component from
 *          being created without using the CaptureComponentFactory.
 */
NonEnergyUseCaptureComponent::NonEnergyUseCaptureComponent()
:mSequesteredAmount( scenario->getModeltime()->getmaxper() ),
mRemoveFraction( 0 )
{
}

// Documentation inherits.
NonEnergyUseCaptureComponent* NonEnergyUseCaptureComponent::clone() const {
    return new NonEnergyUseCaptureComponent( *this );
}

// Documentation inherits.
bool NonEnergyUseCaptureComponent::isSameType( const string& aType ) const {
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
const string& NonEnergyUseCaptureComponent::getXMLNameStatic() {
    const static string XML_NAME = "non-energy-use-capture-component";
    return XML_NAME;
}

const string& NonEnergyUseCaptureComponent::getName() const {
    // Capture components are not named as only one can exist per Technology.
    return getXMLNameStatic();
}

bool NonEnergyUseCaptureComponent::XMLParse( const xercesc::DOMNode* node ){
    /*! \pre Assume we are passed a valid node. */
    assert( node );

    const xercesc::DOMNodeList* nodeList = node->getChildNodes();
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ) {
        const xercesc::DOMNode* curr = nodeList->item( i );
        if( curr->getNodeType() != xercesc::DOMNode::ELEMENT_NODE ){
            continue;
        }
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
        // TODO: Fix this on commit.
        if( nodeName == "remove-fraction" || nodeName == "removefrac" ){
            mRemoveFraction = XMLHelper<double>::getValue( curr );
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

void NonEnergyUseCaptureComponent::toInputXML( ostream& aOut,
                                               Tabs* aTabs ) const 
{
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElementCheckDefault( mRemoveFraction, "remove-fraction", aOut, aTabs, 0.0 );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void NonEnergyUseCaptureComponent::toDebugXML( const int aPeriod,
                                               ostream& aOut,
                                               Tabs* aTabs ) const
{
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElement( mRemoveFraction, "remove-fraction", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void NonEnergyUseCaptureComponent::completeInit( const string& aRegionName,
                                                 const string& aSectorName,
                                                 DependencyFinder* aDependencyFinder )
{
    // Does not need to add any dependencies. Check that the remove fraction is
    // valid.
    if( mRemoveFraction < 0 || mRemoveFraction > 1 ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Invalid removal fraction of " << mRemoveFraction << "." << endl;
        mRemoveFraction = 0;
    }
}

void NonEnergyUseCaptureComponent::initCalc( const string& aRegionName,
                                             const string& aSectorName,
                                             const string& aFuelName,
                                             const int aPeriod )
{
}

double NonEnergyUseCaptureComponent::getStorageCost( const string& aRegionName,
                                                     const int aPeriod ) const
{
    // Emissions stored in the product so there is zero cost.
    return 0;
}

double NonEnergyUseCaptureComponent::getRemoveFraction() const {
    return mRemoveFraction;
}

void NonEnergyUseCaptureComponent::calcSequesteredAmount( const string& aRegionName,
                                                          const string& aGHGName,
                                                          const double aInput,
                                                          const double aOutput,
                                                          const double aInputCoef,
                                                          const double aOutputCoef,
                                                          const int aPeriod )
{
    // Currently sequestration objects should only be used for CO2.
    assert( aGHGName == "CO2" );

    // Calculate the amount as the removal fraction multiplied by the difference
    // between input and output carbon.
    mSequesteredAmount[ aPeriod ] = mRemoveFraction 
                                    * ( aInput * aInputCoef - aOutput * aOutputCoef );
}

double NonEnergyUseCaptureComponent::getSequesteredAmount( const string& aGHGName,
                                                           const bool aGetGeologic,
                                                           const int aPeriod ) const 
{
    // Only return emissions if non-geologic emissions are requested.
    // TODO: Determine how to handle other GHGs.
    if( !aGetGeologic && aGHGName == "CO2" ){
        return mSequesteredAmount[ aPeriod ];
    }
    return 0;
}

double NonEnergyUseCaptureComponent::getEffectiveEfficiency( const double aTechnologyEfficiency,
                                                             const int aPeriod ) const
{
    assert( aTechnologyEfficiency > 0 );

    // No penalty on efficiency
    return aTechnologyEfficiency;
}

double NonEnergyUseCaptureComponent::getTotalNonEnergyCost( const double aTechnologyEfficiency,
                                                            const double aTechnologyNonEnergyCost,
                                                            const int aPeriod ) const
{
    assert( aTechnologyEfficiency > 0 );
    assert( aTechnologyNonEnergyCost >= 0 );

    // No penalty on non-energy cost.
    return aTechnologyNonEnergyCost;
}
