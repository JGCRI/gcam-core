/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


/*! 
 * \file standard_capture_component.cpp
 * \ingroup Objects
 * \brief StandardCaptureComponent source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <string>
#include "util/base/include/xml_helper.h"
#include "technologies/include/standard_capture_component.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"
#include "containers/include/scenario.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/market_dependency_finder.h"
#include "functions/include/iinput.h"

using namespace std;

extern Scenario* scenario;

//! Constructor
StandardCaptureComponent::StandardCaptureComponent()
{
    mRemoveFraction = 0;
    mStorageCost = 0;
    mIntensityPenalty = 0;
    mNonEnergyCostPenalty = 0;
}

StandardCaptureComponent::~StandardCaptureComponent() {
}

StandardCaptureComponent* StandardCaptureComponent::clone() const {
    StandardCaptureComponent* clone = new StandardCaptureComponent();
    clone->copy( * this );
    return clone;
}

void StandardCaptureComponent::copy( const StandardCaptureComponent& aOther ) {
    mStorageMarket = aOther.mStorageMarket;
    mTargetGas = aOther.mTargetGas;
    mRemoveFraction = aOther.mRemoveFraction;
    mStorageCost = aOther.mStorageCost;
    mIntensityPenalty = aOther.mIntensityPenalty;
    mNonEnergyCostPenalty = aOther.mNonEnergyCostPenalty;
}

bool StandardCaptureComponent::isSameType( const string& aType ) const {
    return aType == getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
* \details This public function accesses the private constant string, XML_NAME.
*          This way the tag is always consistent for both read-in and output and
*          can be easily changed. The "==" operator that is used when parsing,
*          required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& StandardCaptureComponent::getXMLNameStatic() {
    const static string XML_NAME = "standard-capture-component";
    return XML_NAME;
}

const string& StandardCaptureComponent::getXMLName() const {
    return getXMLNameStatic();
}

const string& StandardCaptureComponent::getName() const {
    return getXMLNameStatic();
}

void StandardCaptureComponent::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElement( mStorageMarket, "storage-market", aOut, aTabs );
    XMLWriteElement( mRemoveFraction, "remove-fraction", aOut, aTabs );
    XMLWriteElement( mStorageCost, "storage-cost", aOut, aTabs );
    XMLWriteElement( mIntensityPenalty, "intensity-penalty", aOut, aTabs );
    XMLWriteElement( mNonEnergyCostPenalty, "non-energy-penalty", aOut, aTabs );
    XMLWriteElement( mSequesteredAmount[ aPeriod ], "sequestered-amount", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void StandardCaptureComponent::completeInit( const string& aRegionName,
                                             const string& aSectorName )
{
    // Add the storage market as a dependency of the sector. This is because
    // this sector will have to be ordered first so that the total demand and
    // price for storage are known.
    scenario->getMarketplace()->getDependencyFinder()->addDependency( aSectorName,
                                                                      aRegionName,
                                                                      mStorageMarket,
                                                                      aRegionName );

    // Default the target gas to CO2.
    if( mTargetGas.empty() ){
        mTargetGas = "CO2";
    }
}

void StandardCaptureComponent::initCalc( const string& aRegionName,
                                           const string& aSectorName,
                                           const string& aFuelName,
                                           const int aPeriod )
{
}

/**
 * \details Storage cost is only valid for mTargetGas ("CO2")
 * \param aRegionName 
 * \param aGHGName 
 * \param aPeriod 
 * \return storage cost
 */
double StandardCaptureComponent::getStorageCost( const string& aRegionName,
                                                 const string& aGHGName,
                                                 const int aPeriod ) const
{
    // First check if this component can capture the gas.
    // Storage cost is only valid for mTargetGas (currently CO2).
    if( aGHGName != mTargetGas ){
        return 0;
    }

    // Check if there is a market for storage.
    double storageMarketPrice = scenario->getMarketplace()->getPrice( mStorageMarket,
                                                                      aRegionName,
                                                                      aPeriod, false );
    
    // Check if there is a carbon market.
    double carbonMarketPrice = scenario->getMarketplace()->getPrice( mTargetGas,
                                                                     aRegionName,
                                                                     aPeriod, false );

    // If there is no carbon market, return a large number to disable the
    // capture technology.
    if( carbonMarketPrice == Marketplace::NO_MARKET_PRICE || carbonMarketPrice == 0.0 ){
        return util::getLargeNumber();
    }

    double storageCost;
    if( storageMarketPrice == Marketplace::NO_MARKET_PRICE ){
        // There is no carbon market. Use the read-in cost.
        storageCost = mStorageCost;
    }
    else {
        // Use the market cost.
        storageCost = storageMarketPrice;
    }
    return storageCost;
}

/**
 * \details Has a valid remove fraction for the target gas only (currently CO2).
 * \param aGHGName 
 * \return remove fraction
 */
double StandardCaptureComponent::getRemoveFraction( const string& aGHGName ) const {
    return aGHGName == mTargetGas ? mRemoveFraction : 0;
}

/**
 * \details Calculate sequestered amount for all gases, but do not add
 *  to market demand if gas is not mTargetGas.
 * \param aRegionName 
 * \param aGHGName 
 * \param aTotalEmissions 
 * \param aPeriod 
 * \return emissions sequestered
 */
double StandardCaptureComponent::calcSequesteredAmount( const string& aRegionName,
                                                        const string& aGHGName,
                                                        const double aTotalEmissions,
                                                        const int aPeriod )
{
    // Calculate the amount of sequestration.
    // Note the remove fraction is only greater than zero if the current GHG matches
    // the target gas of this capture component.
    double removeFrac = getRemoveFraction( aGHGName );
    double sequestered =  0.0;

    // Add the demand to the marketplace.
    if( removeFrac > 0 ){
        sequestered = removeFrac * aTotalEmissions;
        mSequesteredAmount[ aPeriod ] = sequestered;
        // set sequestered amount as demand side of carbon storage market
        Marketplace* marketplace = scenario->getMarketplace();
        marketplace->addToDemand( mStorageMarket, aRegionName, mSequesteredAmount[ aPeriod ], aPeriod,
                                  false );
    }
    return sequestered;
}

/**
 * \param aGHGName 
 * \param aGetGeologic 
 * \param aPeriod 
 * \return sequestered amount
 */
double StandardCaptureComponent::getSequesteredAmount( const string& aGHGName,
                                                       const bool aGetGeologic,
                                                       const int aPeriod ) const 
{
    // Only return emissions if the type of the sequestration equals is geologic.
    if( aGetGeologic && aGHGName == mTargetGas ){
        return mSequesteredAmount[ aPeriod ];
    }
    return 0;
}

void StandardCaptureComponent::adjustInputs( const string& aRegionName,
                                             std::vector<IInput*>& aInputs,
                                             const int aPeriod ) const
{
    // Loop through the inputs and search for energy and non-energy inputs.
    for( unsigned int i = 0; i < aInputs.size(); ++i ){
        // Check if the input is an energy input.
        if( aInputs[ i ]->hasTypeFlag( IInput::ENERGY ) ){
            /*! \pre Energy input intensity must be greater than zero. */
            double currIntensity = aInputs[ i ]->getCoefficient( aPeriod );
            assert( currIntensity > 0 );
            
            // Calculate effective intensity, reduces the intensity by a penalty.
            aInputs[ i ]->setCoefficient( currIntensity * ( 1 + mIntensityPenalty ), aPeriod );
        }
        // Check for capital inputs 
        else if( aInputs[ i ]->hasTypeFlag( IInput::CAPITAL ) ){
            double currCost = aInputs[ i ]->getPrice( aRegionName, aPeriod);
            /*! \pre Non-energy cost must be positive for a penalty to be
            *        applied. 
            */
            assert( currCost > 0 );
            aInputs[ i ]->setPrice( aRegionName, currCost * ( 1 + mNonEnergyCostPenalty ), aPeriod );
        }
    }
}
