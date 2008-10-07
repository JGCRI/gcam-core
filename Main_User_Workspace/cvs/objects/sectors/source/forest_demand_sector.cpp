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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*! 
* \file forest_demand_sector.cpp
* \ingroup Objects
* \brief ForestDemandSector class source file.
* \author James Blackwood
*/

#include "util/base/include/xml_helper.h"
#include "containers/include/iinfo.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/scenario.h"
#include "containers/include/gdp.h"
#include "demographics/include/demographic.h"
#include "sectors/include/energy_final_demand.h"
#include "sectors/include/forest_demand_sector.h"
#include "sectors/include/sector_utils.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief Default constructor.
* \author James Blackwood
*/
ForestDemandSector::ForestDemandSector(){
	mRotationPeriod = 0;
    mDemandFunction.reset( new PerCapitaNotAdjGDPDemandFunction );
}

/*! \brief Parses any attributes specific to derived classes
* \author James Blackwood
* \param nodeName The name of the curr node. 
* \param curr pointer to the current node in the XML input tree
*/
bool ForestDemandSector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
// Nothing to read in, but need to reset demand function.
    if( nodeName == "perCapitaBased" ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "perCapitaBased not implimented for forest sector " << endl;
    }

    // Reset demand function to one based on GDP that is never adjusted for price feedbacks.
    mDemandFunction.reset( new PerCapitaNotAdjGDPDemandFunction );

    return false; 
}

//! XML output for viewing.
void ForestDemandSector::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
}

//! Write object to debugging xml output stream.
void ForestDemandSector::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    XMLWriteElement( mRotationPeriod, "rotation-period", out, tabs );
    XMLWriteElement( mFutureForestDemand, "forestFutureDemand", out, tabs );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& ForestDemandSector::getXMLName() const {
	return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& ForestDemandSector::getXMLNameStatic() {
	const static string XML_NAME = "ForestDemandSector";
	return XML_NAME;
}

/*! \brief Perform any initializations needed for each period.
*
* Any initializations or calculations that only need to be done once per period
* (instead of every iteration) should be placed in this function.
*
* \author James Blackwood
* \param period Model period
*/
void ForestDemandSector::initCalc( const string& aRegionName,
                                   const GDP* aGDP,
                                   const Demographic* aDemographics,
                                   const int aPeriod )
{
    EnergyFinalDemand::initCalc( aRegionName, aGDP, aDemographics, aPeriod );
    // TODO: Calibration checking needs to be implemented.
}

/*! \brief Complete the initialization of a forest demand sector.
*/
void ForestDemandSector::completeInit( const string& aRegionName,
                                       const IInfo* aRegionInfo )
{
	mRotationPeriod = aRegionInfo->getInteger( "rotationPeriod", true );
    EnergyFinalDemand::completeInit( aRegionName, aRegionInfo );
}

/*!\brief Set baseScaler for all future periods to current value.
 *
 * This is needed so that future forest demand will be calculated consistently 
 * with current demand.
 *
 * \param aFuelName 
 * \param aScaleValue 
 * \param aPeriod 
 */
void ForestDemandSector::scaleCalibratedValues( const string& aFuelName,
                                               const double aScaleValue,
                                               const int aPeriod )
{
    EnergyFinalDemand::scaleCalibratedValues( aFuelName, aScaleValue, aPeriod );

    if ( aPeriod > 0 ) {
        const Modeltime* modeltime = scenario->getModeltime();
        for( int aFuturePeriod = aPeriod + 1; aFuturePeriod < modeltime->getmaxper(); ++aFuturePeriod ){
                // Set to previous value, which is what was used to calc demand
                // Need to do this for all future periods since this same scaler needs to be used to
                // calculate future forest as well as current forest.
   //             mBaseScaler[ aFuturePeriod ] = mBaseScaler[ aFuturePeriod - 1 ];
       }
    }
}

/*! \brief Aggregate sector forest service demand function
*
* Function calculates the aggregate demand for forest services and passes that down to the subsectors. 
* Demand is proportional to GDP (to a power) times population.
*
* \author Sonny Kim, Steve Smith, James Blackwood
* \param aGDP GDP (relative or absolute?)
* \param period Model period
* \todo Steve: need to find way to get the name of the corresponding supply sector here
* \pre Sector price attribute must have been previously calculated and set (via calcPrice)
*/
void ForestDemandSector::setFinalDemand( const string& aRegionName,
                                     const Demographic* aDemographics,
                                     const GDP* aGDP,
                                     const int aPeriod )
{
    // Demand in current period
    const double annualServiceDemand = calcFinalDemand( aRegionName, aDemographics, aGDP, aPeriod );
    // Set the service demand into the marketplace.
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->addToDemand( mName, aRegionName, annualServiceDemand, aPeriod );

    // Demand for future market period
    const Modeltime* modeltime = scenario->getModeltime();
    const int futureMarketIncrement = mRotationPeriod / modeltime->gettimestep( aPeriod );
    // Need to calc demand for every intermediate period since demand depends on previous period's demand
    // Could create new demand function that doesn't work like this since demand currently has a fixed pathway for forests
    
    string forwardForestMarketName = futureMarketPrefix() + "Forest";
    for ( int tempPeriod = aPeriod + 1; tempPeriod <= aPeriod + futureMarketIncrement; tempPeriod++ ) {
        mFutureForestDemand = calcFutureForestDemand( aRegionName, aDemographics, aGDP, 
            forwardForestMarketName, tempPeriod, mName, aPeriod );
    }
    
    // Need to put the demand for future forests into the marketplace
    marketplace->addToDemand( forwardForestMarketName, aRegionName, mFutureForestDemand, aPeriod );    
}

//TODO add docs
/*
TODO check if this formulation gives what want for forest demand after end period. 
Not sure how this is interacting with basescaor. Might want to re-initalize these to larger sizes for this class. 
*/
double ForestDemandSector::calcFutureForestDemand( const string& aRegionName,
                                           const Demographic* aDemographics,
                                           const GDP* aGDP,
                                           string& aMarketName,
                                           const int aPeriod, 
                                           string& aBaseMarketName, 
                                           const int aBasePeriod )
{
    // Freeze demand after last modeled period
    int thisPeriodForParameters = aPeriod;
    const Modeltime* modeltime = scenario->getModeltime();
    if( thisPeriodForParameters >= modeltime->getmaxper() ){
        thisPeriodForParameters = modeltime->getmaxper() - 1;
    }
        
    int previousPeriod = 0;
    if( thisPeriodForParameters > 0 ){
        previousPeriod = thisPeriodForParameters - 1;
    }

    // Key difference here is that the marketname for forward forest is used instead
    // of the name of this sector.
    // We also do not know the price of any period after the current period
    // So, we cannot use the priceRatio calculated for energyFinalDemand sectors
    // Instead, we use the ratio of the futureForest price in the current period
    // to the Forest price in the current period.
    const Marketplace* marketplace = scenario->getMarketplace(); 
    const double basePrice = marketplace->getPrice( aBaseMarketName, aRegionName, aBasePeriod );
    const double futurePrice = marketplace->getPrice( aMarketName, aRegionName, aBasePeriod );
    double priceRatio = 1.0;
    if ( basePrice > util::getSmallNumber() ){
        priceRatio = futurePrice / basePrice;
    }
 
    // This price ratio is for the entire rotation period
    // We want to scale it to represent only the difference in prices in one timestep
    const int numPeriods = mRotationPeriod / modeltime->gettimestep( aBasePeriod );
    priceRatio = pow( priceRatio, 1.0 / (double) numPeriods );

    const double demandScaler = mDemandFunction->calcDemand( aDemographics,
                                   aGDP, mPriceElasticity[ thisPeriodForParameters ],
                                   mIncomeElasticity[ thisPeriodForParameters ], priceRatio,
                                   thisPeriodForParameters );

    // For periods after the end of the model time horizon we are overwriting the final period's
    // service demand. We want the service demand in one period to be based on the
    // preTechChangeServiceDemand in the previous period which is always stored in the
    // final element of the vector.
    if ( aPeriod > thisPeriodForParameters ){
        mServiceDemands[ thisPeriodForParameters ] = mBaseScaler[ thisPeriodForParameters ] 
                               * mPreTechChangeServiceDemand[ thisPeriodForParameters ]
                               * demandScaler;
    }
    // Prior to the end of the model horizon, we use the previous period's preTechChangeServiceDemand
    // to compute the service demand in the current period. This preTechChangeServiceDemand is 
    // stored in the vector as the previous period's element.
    else {
        mServiceDemands[ thisPeriodForParameters ] = mBaseScaler[ thisPeriodForParameters ] 
                               * mPreTechChangeServiceDemand[ thisPeriodForParameters - 1]
                               * demandScaler;
    }

    assert( mServiceDemands[ thisPeriodForParameters ] >= 0 );
    mPreTechChangeServiceDemand[ thisPeriodForParameters ] = mServiceDemands[ thisPeriodForParameters ];

    // Final demand for service adjusted using cummulative technical change.
    // Generally used for forests, but code is here just in case someone puts that in as this is in
    // there automatically for the present-day forest market demand called through EnergyFinalDemand.
    if( mFinalEnergyConsumer.get() ){
        mServiceDemands[ thisPeriodForParameters ] /= mFinalEnergyConsumer->calcTechChange( thisPeriodForParameters );
    }

    return mServiceDemands[ thisPeriodForParameters ];
}
/*! \brief Return the prefix used to differentiate the future market from the
*          present market.
* \return The future market prefix.
*/
const string& ForestDemandSector::futureMarketPrefix() {
    const static string FUTURE_MARKET_PREFIX = "Future";
    return FUTURE_MARKET_PREFIX;
}

// Same as per-capita GDP demand function except that the not adjusted GDP is used.
// This is useful for demand functions that require a known demand into the future.
double ForestDemandSector::PerCapitaNotAdjGDPDemandFunction::calcDemand(
                                                           const Demographic* aDemographics,
                                                           const GDP* aGDP,
                                                           const double aPriceElasticity,
                                                           const double aIncomeElasticity,
                                                           const double aPriceRatio,
                                                           const int aPeriod ) const
{
    // If perCapitaBased, service_demand = B * P^r * GDPperCap^r * Population.
    // All ratios are based on previous period values.
    const Modeltime* modeltime = scenario->getModeltime();
    if( aPeriod == 0 ){
        // No changes in price, income and population scales.
        return 1;
    }
    double GDPperCapRatio = aGDP->getGDPPerCapitaNotAdjusted( aPeriod )
                          / aGDP->getGDPPerCapitaNotAdjusted( aPeriod - 1);

    double populationRatio = aDemographics->getTotal( aPeriod )
                           / aDemographics->getTotal( aPeriod - 1);

    double macroEconomicScaler = pow( aPriceRatio, aPriceElasticity )
                         * pow( GDPperCapRatio, aIncomeElasticity )
                         * populationRatio;
    return macroEconomicScaler;
}

