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
 * \file energy_final_demand.cpp
 * \ingroup Objects
 * \brief EnergyEnergyFinalDemand class source file.
 * \author Josh Lurz
 */

#include <string>
#include <algorithm>

#include "util/base/include/definitions.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/configuration.h"
#include "util/base/include/model_time.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/scenario.h"
#include "containers/include/gdp.h"
#include "containers/include/iinfo.h"
#include "marketplace/include/marketplace.h"
#include "demographics/include/demographic.h"
#include "sectors/include/energy_final_demand.h"
#include "sectors/include/sector_utils.h"

using namespace std;

extern Scenario* scenario;

/*! \brief Constructor.
* \author Sonny Kim, Steve Smith, Josh Lurz
*/
EnergyFinalDemand::EnergyFinalDemand():
mFinalEnergyConsumer(0),
mIsPerCapBased(false)
{
}

/*! \brief Destructor.
*/
EnergyFinalDemand::~EnergyFinalDemand(){
    delete mFinalEnergyConsumer;
}

const string& EnergyFinalDemand::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. The "==" operator that is used when parsing, required this second
* function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& EnergyFinalDemand::getXMLNameStatic() {
    const static string XML_NAME = "energy-final-demand";
    return XML_NAME;
}

const string& EnergyFinalDemand::getName() const {
    return mName;
}

void EnergyFinalDemand::toDebugXML( const int aPeriod,
                                    ostream& aOut,
                                    Tabs* aTabs ) const
{
    XMLWriteOpeningTag ( getXMLName(), aOut, aTabs, mName );

    // write the xml for the class members.
    XMLWriteElement( mDemandFunction->isPerCapitaBased(),
                     "perCapitaBased", aOut, aTabs );

    if( mFinalEnergyConsumer ){
        mFinalEnergyConsumer->toDebugXML( aPeriod, aOut, aTabs );
    }

    XMLWriteElement( mBaseService[ aPeriod ], "base-service", aOut, aTabs );
    XMLWriteElement( mServiceDemands[ aPeriod ], "service", aOut, aTabs );
    XMLWriteElement( mPreTechChangeServiceDemand[ aPeriod ], "service-pre-tech-change", aOut, aTabs );
    XMLWriteElement( mIncomeElasticity[ aPeriod ], "income-elasticity", aOut, aTabs );
    XMLWriteElement( mPriceElasticity[ aPeriod ], "price-elasticity", aOut, aTabs );

    toDebugXMLDerived( aPeriod, aOut, aTabs );
    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
}

void EnergyFinalDemand::toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const {

}

void EnergyFinalDemand::completeInit( const string& aRegionName,
                                      const IInfo* aRegionInfo )
{
    if(mIsPerCapBased) {
        mDemandFunction.reset( new PerCapitaGDPDemandFunction );
    }
    else {
        mDemandFunction.reset( new TotalGDPDemandFunction );
    }

    if( mBaseService[ 0 ] < 0.0 ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::DEBUG );
        mainLog << "Zero base service for demand sector " << mName
                << " in region " << aRegionName << "." << endl;
        mBaseService[ 0 ] = 1;
        mPreTechChangeServiceDemand[ 0 ] = 1;
    }
    // initialize base year mPreTechChangeServiceDemand.
    else{
        mPreTechChangeServiceDemand[ 0 ] = mBaseService[ 0 ];
    }
    
    // Make sure that we have income and price elasticities for each model period.
    // If not we should interpolate between model periods that we do have.
    const Modeltime* modeltime = scenario->getModeltime();
    for( int per = modeltime->getFinalCalibrationPeriod()+1; per < modeltime->getmaxper(); ++per ) {
        if( !mIncomeElasticity[ per ].isInited() ) {
            int iFound = per + 1; // start with next per
            // Look for index of existing elasticity
            while( iFound < modeltime->getmaxper() && 
                   !mIncomeElasticity[ iFound ].isInited()) {
                ++iFound;
            }
            // If not found, then use previous existing elasticity.
            if( iFound == modeltime->getmaxper() ) {
                iFound = per - 1;
            }
            mIncomeElasticity[ per ].set( mIncomeElasticity[ iFound ] );
        }
        if( !mPriceElasticity[ per ].isInited() ) {
            int iFound = per + 1; // start with next per
            // Look for index of existing elasticity
            while( iFound < modeltime->getmaxper() && 
                   !mPriceElasticity[ iFound ].isInited() ) {
                ++iFound;
            }
            // If not found, then use previous existing elasticity.
            if( iFound == modeltime->getmaxper() ) {
                iFound = per - 1;
            }
            mPriceElasticity[ per ].set( mPriceElasticity[ iFound ] );
        }
    }

    if( mFinalEnergyConsumer ){
        mFinalEnergyConsumer->completeInit( aRegionName, mName );
    }
}

void EnergyFinalDemand::initCalc( const string& aRegionName,
                                  const GDP* aGDP,
                                  const Demographic* aDemographics,
                                  const int aPeriod )
{
}

/*! \brief Set the final demand for service into the marketplace after 
* calling the aggregate demand function.
*
* \detail Adding the demand for final services into the marketplace
*  is separted from the actual calculation of final service so that services can
*  be calculated and used without being adding to marketplace.
* \author Sonny Kim, Josh Lurz
* \param string& aRegionName region name.
* \param GDP* aGDP object.
* \param Demographic* aDemographicss.
* \param aPeriod Model aPeriod
*/
void EnergyFinalDemand::setFinalDemand( const string& aRegionName,
                                        const Demographic* aDemographics,
                                        const GDP* aGDP,
                                        const int aPeriod )
{
    calcFinalDemand( aRegionName, aDemographics, aGDP, aPeriod );
    // Set the service demand into the marketplace.
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->addToDemand( mName, aRegionName, mServiceDemands[ aPeriod ], aPeriod );
}

/*! \brief Set the final demand for service using the aggrgate sector energy service 
*    demand function.
*
* Function calculates the aggregate demand for energy services and passes that
* down to the sub-sectors. Demand is proportional to either GDP (to a power) or
* GDP per capita (to a power) times population.
*
* \author Sonny Kim, Josh Lurz
* \param string& aRegionName region name.
* \param GDP* aGDP object.
* \param Demographic* aDemographicss.
* \param aPeriod Model aPeriod
* \return The calculated service demand.
*/
double EnergyFinalDemand::calcFinalDemand( const string& aRegionName,
                                           const Demographic* aDemographics,
                                           const GDP* aGDP,
                                           const int aPeriod )
{
    if( aPeriod <= scenario->getModeltime()->getFinalCalibrationPeriod() ){
        // read-in initial demand 
        mServiceDemands[ aPeriod ] = mBaseService[ aPeriod ];
        mPreTechChangeServiceDemand[ aPeriod ] = mBaseService[ aPeriod ];
    }
    // Do for non-calibration periods
    else{
        // Update AEEI.
        if( mFinalEnergyConsumer ){
            mFinalEnergyConsumer->updateAEEI( aRegionName, aPeriod );
        }

        // Note the use of previous period service demand without technical change 
        // applied.
        // TODO: preferable to use actual previous service with technical change for
        // current period applied.
        mServiceDemands[ aPeriod ] = mPreTechChangeServiceDemand[ aPeriod - 1 ] > 0 ? 
            mPreTechChangeServiceDemand[ aPeriod - 1] * calcMacroScaler( aRegionName, aDemographics, aGDP, aPeriod) :
            0;

        assert( mServiceDemands[ aPeriod ] >= 0 );
        mPreTechChangeServiceDemand[ aPeriod ] = mServiceDemands[ aPeriod ];
 
        // Final demand for service adjusted using cummulative technical change.
        if( mFinalEnergyConsumer ){
            mServiceDemands[ aPeriod ] /= mFinalEnergyConsumer->calcTechChange( aPeriod );
        }
    }
    return mServiceDemands[ aPeriod ];
}

/*!
* \brief Calculate the macro-economic scaler for the service demand.
* \details Using the aggregate demand function, this method calculates
* the growth in the demand for services from changes in prices, incomes
* and population if demand function is per capita based.
*
* \param aRegionName 
* \param aDemographics 
* \param aGDP 
* \param aPeriod 
* \return The macro-economic scaler.
*/
double EnergyFinalDemand::calcMacroScaler( const string& aRegionName,
                                           const Demographic* aDemographics,
                                           const GDP* aGDP,
                                           const int aPeriod ) const
{
    int previousPeriod = 0;
    if( aPeriod > 0 ){
        previousPeriod = aPeriod - 1;
    }

    const double priceRatio = SectorUtils::calcPriceRatio( aRegionName, mName,
                                                     previousPeriod, aPeriod );
    const double cappedPriceRatio = max( priceRatio, SectorUtils::getDemandPriceThreshold() );

    double macroScaler = mDemandFunction->calcDemand( aDemographics,
                                                      aGDP, mPriceElasticity[ aPeriod ],
                                                      mIncomeElasticity[ aPeriod ], cappedPriceRatio,
                                                      aPeriod );
    // May need to make an adjustment in case of negative prices.
    if( priceRatio < cappedPriceRatio && mPriceElasticity[ aPeriod ] != 0 ) {
        macroScaler = SectorUtils::adjustDemandForNegativePrice( macroScaler, priceRatio );
    }

    return macroScaler;
}

double EnergyFinalDemand::getWeightedEnergyPrice( const string& aRegionName,
                                                  const int aPeriod ) const
{
    // If this is not a final energy demand, it has no impact on the energy
    // price.
    if( !mFinalEnergyConsumer ){
        return 0;
    }

    const Marketplace* marketplace = scenario->getMarketplace();

    // Make sure the market exists. Note that this currently uses the energy
    // supplies from the previous period to avoid ordering issues.
    assert( marketplace->getPrice( mName, aRegionName, aPeriod, true ) !=
        Marketplace::NO_MARKET_PRICE );

    // TODO: Should this use the previous period, current period, etc?
    return marketplace->getPrice( mName, aRegionName, aPeriod, true ) *
        mServiceDemands[ 0 ];
}

// Documentation is inherited.
void EnergyFinalDemand::accept( IVisitor* aVisitor,
                                const int aPeriod ) const
{
    aVisitor->startVisitFinalDemand( this, aPeriod );
    // TODO: Because iVisitor takes AFinalDemand for the visitFinalDemand
    // method, data members of EnergyFinalDemand can not be accessed.
    // AFinalDemand is pure interface, change to an abstract base class.
    acceptDerived( aVisitor, aPeriod );
    aVisitor->endVisitFinalDemand( this, aPeriod );
}

// Work around to get access to data members.
void EnergyFinalDemand::acceptDerived( IVisitor* aVisitor,
                                const int aPeriod ) const
{
    aVisitor->startVisitEnergyFinalDemand( this, aPeriod );
    aVisitor->endVisitEnergyFinalDemand( this, aPeriod );
}

EnergyFinalDemand::FinalEnergyConsumer::FinalEnergyConsumer() {
}

double EnergyFinalDemand::PerCapitaGDPDemandFunction::calcDemand(
                                                           const Demographic* aDemographics,
                                                           const GDP* aGDP,
                                                           const double aPriceElasticity,
                                                           const double aIncomeElasticity,
                                                           const double aPriceRatio,
                                                           const int aPeriod ) const
{
    // If perCapitaBased, service_demand = B * P^r * GDPperCap^r * Population.
    // All ratios are based on previous period values.
    if( aPeriod == 0 ){
        // No changes in price, income and population scales.
        return 1;
    }
    double GDPperCapRatio = aGDP->getGDPperCap( aPeriod )
                          / aGDP->getGDPperCap( aPeriod - 1);

    double populationRatio = aDemographics->getTotal( aPeriod )
                           / aDemographics->getTotal( aPeriod - 1);

    //! \pre aPriceRatio > 0.  Prices calculated by
    //! calcMacroScaler will automatically meet this
    //! condition.
    double macroEconomicScaler = pow( aPriceRatio, aPriceElasticity )
                         * pow( GDPperCapRatio, aIncomeElasticity )
                         * populationRatio;

    return macroEconomicScaler;
}

double EnergyFinalDemand::TotalGDPDemandFunction::calcDemand( const Demographic* aDemographics,
                                                       const GDP* aGDP,
                                                       const double aPriceElasticity,
                                                       const double aIncomeElasticity,
                                                       const double aPriceRatio,
                                                       const int aPeriod ) const
{
    // If not perCapitaBased, service_demand = B * P^r * GDP^r
    // Demand based on price changes and scale of GDP 
    if( aPeriod == 0 ){
        // No changes in price and income.
        return 1;
    }

    // All ratios are based on previous period values.
    double GDPRatio = aGDP->getGDP( aPeriod )
                    / aGDP->getGDP( aPeriod - 1 );


    //! \pre aPriceRatio > 0.  Prices calculated by
    //! calcMacroScaler will automatically meet this
    //! condition.
    double macroEconomicScaler = pow( aPriceRatio , aPriceElasticity ) 
                               * pow( GDPRatio, aIncomeElasticity );

    return macroEconomicScaler;
}

const string& EnergyFinalDemand::FinalEnergyConsumer::getXMLNameStatic() {
    static const string XML_NAME = "final-energy-consumer";
    return XML_NAME;
}

double EnergyFinalDemand::FinalEnergyConsumer::noCalibrationValue() {
    return -1;
}

void EnergyFinalDemand::FinalEnergyConsumer::completeInit( const string& aRegionName,
                                                           const string& aFinalDemandName )
{
    mTFEMarketName = SectorUtils::createTFEMarketName( aFinalDemandName );
    // Set up demand sector calibration market.
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->createMarket( aRegionName, aRegionName, mTFEMarketName,
                               IMarketType::INVERSE_CALIBRATION );
    // Set price and output units for period 0 market info
    IInfo* marketInfo = marketplace->getMarketInfo( mTFEMarketName, aRegionName, 0, true );
    marketInfo->setString( "price-unit", "EJ" );
    marketInfo->setString( "output-unit", "EJ" );

    SectorUtils::setFinalEnergyFlag( aRegionName, aFinalDemandName );
    const Modeltime* modeltime = scenario->getModeltime();
    
    // Make sure that we have an aeei for all model periods.  Use the one from the
    // next period where one exists.  If none exists then use the one from the
    // previous period.
    for( int per = modeltime->getFinalCalibrationPeriod()+1; per < modeltime->getmaxper(); ++per ) {
        if( !mAEEI[ per ].isInited() ) {
            int iFound = per + 1; // start with next per
            // Look for index of existing aeei
            while( iFound < modeltime->getmaxper() && 
                   !mAEEI[ iFound ].isInited() ) {
                ++iFound;
            }
            // If not found, then use previous existing aeei.
            if( iFound == modeltime->getmaxper() ) {
                iFound = per - 1;
            }
            mAEEI[ per ].set( mAEEI[ iFound ] );
        }
    }

    for( unsigned int i = ( modeltime->getFinalCalibrationPeriod() + 1 );
        i < mCalFinalEnergy.size(); ++i ){
        if( mCalFinalEnergy[ i ].isInited() ){
            // Solve all initialized periods.
            marketplace->setMarketToSolve( mTFEMarketName, aRegionName, i );

            // Setup the constraint.
            marketplace->addToSupply( mTFEMarketName, aRegionName,
                                      mCalFinalEnergy[ i ], i, true );

            // Set the initial price.
            double totalAEEI = pow( 1 + mAEEI[ i ], modeltime->gettimestep( i ) );
            marketplace->setPrice( mTFEMarketName, aRegionName, totalAEEI, i );
        }
    }
}

double EnergyFinalDemand::FinalEnergyConsumer::getCalibratedFinalEnergy( const int aPeriod ) const {
    return mCalFinalEnergy[ aPeriod ].isInited() ?
        mCalFinalEnergy[ aPeriod ].get() : noCalibrationValue();
}

void EnergyFinalDemand::FinalEnergyConsumer::updateAEEI( const string& aRegionName,
                                                         const int aPeriod )
{
    // Do only if mCalFinalEnergy object exists.
    if( mCalFinalEnergy[ aPeriod ].get() ){
        const Modeltime* modeltime = scenario->getModeltime();
        Marketplace* marketplace = scenario->getMarketplace();

        // Get the technical change parameter from the calibration market.
        if( aPeriod > modeltime->getFinalCalibrationPeriod() && mCalFinalEnergy[ aPeriod ].isInited() ){
            double totalAEEI = marketplace->getPrice( mTFEMarketName, aRegionName,
                aPeriod, true );
            if( totalAEEI > 0 ){
                mAEEI[ aPeriod ] = pow( totalAEEI, 1.0 /
                    static_cast<double>( modeltime->gettimestep( aPeriod ) ) ) - 1;
            }
        }
    }
}

/*!
 * \brief Calculate cummulative technical change up to the current period.
 * \details Calculates the cummulative technical change up to the current
 *          period.
 * \note This method uses recursion to build cumulative technical change.
 * \todo This could be optimized to store the technical change at the end of the
 *       iteration(once AEEI is known) if it is determined that this is taking a
 *       large amount of time.
 */
double EnergyFinalDemand::FinalEnergyConsumer::calcTechChange( const int aPeriod ) const {
        // There is no tech change in the base period.
        double cummTechChange = 1;
        
        // Loop starting in period 1 which is the first period with technical
        // change.
        const Modeltime* modeltime = scenario->getModeltime();

        if( aPeriod == 0 ){
            return cummTechChange;
        }
        // Builds cumulative technical change recursively to include total change
        // from base period.  AEEI for each period is applied for one time step only.
        cummTechChange = calcTechChange( aPeriod - 1 ) 
                       * pow( 1 + mAEEI[ aPeriod ], modeltime->gettimestep( aPeriod ) );
        return cummTechChange;
}

void EnergyFinalDemand::FinalEnergyConsumer::toDebugXML( const int aPeriod,
                                                         ostream& aOut,
                                                         Tabs* aTabs ) const
{
    XMLWriteOpeningTag ( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElement( mAEEI[ aPeriod ], "aeei", aOut, aTabs );
    XMLWriteElement( mCalFinalEnergy[ aPeriod ].isInited() ?
                     mCalFinalEnergy[ aPeriod ].get() : -1,
                     "cal-final-energy", aOut, aTabs );
    XMLWriteElement( calcTechChange( aPeriod ), "cumm-tech-change", aOut,
                     aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}
