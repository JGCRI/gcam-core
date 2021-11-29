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
 * \file market_container.cpp
 * \ingroup Objects
 * \brief MarketContainer class source file.
 * \author Pralit Patel
 */

#include "util/base/include/definitions.h"
#include <cassert>

#include "marketplace/include/market_container.h"
#include "util/base/include/model_time.h"
#include "util/base/include/util.h"
#include "containers/include/scenario.h"
#include "marketplace/include/imarket_type.h"
#include "marketplace/include/market.h"
#include "marketplace/include/price_market.h"
#include "marketplace/include/demand_market.h"
#include "marketplace/include/calibration_market.h"
#include "marketplace/include/inverse_calibration_market.h"
#include "marketplace/include/market_tax.h"
#include "marketplace/include/market_subsidy.h"
#include "marketplace/include/market_RES.h"
#include "marketplace/include/normal_market.h"
#include "marketplace/include/trial_value_market.h"
#include "marketplace/include/linked_market.h"
#include "util/base/include/atom_registry.h"
#include "util/base/include/atom.h"
#include "containers/include/iinfo.h"
#include "util/logger/include/ilogger.h"

using namespace std;
using namespace objects;

extern Scenario* scenario;


/*! \brief Constructor
 * \details This is the constructor for the MarketContainer class. No default constructor
 *          exists to prevent the creation of empty markets.
 * \warning The arguments are required to define the good name, region name and
 *          model period. These values are invariants.
 * \param aType Type of market to create.
 * \param aGoodName The good or fuel name for the item in the market.
 * \param aRegionName The region which this market covers. It may include
 *        several model regions.
 */
MarketContainer::MarketContainer( const IMarketType::Type aMarketType,
                                  const string& aGoodName,
                                  const string& aRegionName )
{
    // Store the market name so that it can be returned without any allocations.
    mName = aRegionName + aGoodName;
    mGood = aGoodName;
    mRegion = aRegionName;

    // Assign a serial number that is guaranteed to be invalid.  This will help
    // us catch any failure to assign a serial number to the market.
    mSerialNumber = -1;
    
    // create each market object of the same type
    const Modeltime* modeltime = scenario->getModeltime();
    for( int period = 0; period < size(); ++period ) {
        mMarkets[ period ] = createMarket( aMarketType );
        mMarkets[ period ]->setYear( modeltime->getper_to_yr( period ) );
    }
}

/*!
 * \brief Create a MarketContainer that holds LinkedMarkets which are linked to the
 *        given market.
 * \param aMarketToLink The market that LinkedMarket should link to.
 * \param aGoodName The good or fuel name for the item in the market.
 * \param aRegionName The region which this market covers. It may include
 *        several model regions.
 * \param aStartPeriod If a non-negative value is the period in which a linked market may
 *                     override a previously set market to effectively switch markets over time.
 */
MarketContainer::MarketContainer( MarketContainer* aMarketToLink,
                                  const string& aGoodName,
                                  const string& aRegionName,
                                  const int aStartPeriod )
{
    // Store the market name so that it can be returned without any allocations.
    mName = aRegionName + aGoodName;
    mGood = aGoodName;
    mRegion = aRegionName;
    
    // Assign a serial number that is guaranteed to be invalid.  This will help
    // us catch any failure to assign a serial number to the market.
    mSerialNumber = -1;
    
    // create the linked market object for each period.
    const Modeltime* modeltime = scenario->getModeltime();
    // ensure model periods prior to aStartPeriod atleast get a market not linked to anything
    // instead of a crash
    for( int period = 0; period < max( aStartPeriod, 0 ); ++period ) {
        mMarkets[ period ] = new LinkedMarket( 0, this );
        mMarkets[ period ]->setYear( modeltime->getper_to_yr( period ) );
    }
    for( int period = max( aStartPeriod, 0 ); period < size(); ++period ) {
        mMarkets[ period ] = new LinkedMarket( aMarketToLink ? aMarketToLink->mMarkets[ period ] : 0, this );
        mMarkets[ period ]->setYear( modeltime->getper_to_yr( period ) );
    }
}

//! Destructor.
MarketContainer::~MarketContainer(){
    for( auto currMarket : mMarkets ) {
        delete currMarket;
    }
}

/*! \brief Create a market based on its type.
 * \details
 * \param aType Type of market to create.
 * \return A pointer to the newly allocated market, null if the type did not
 *         exist.
 */
Market* MarketContainer::createMarket( const IMarketType::Type aType )
{
    assert( aType < IMarketType::END );
    Market* rNewMarket = 0;
    if ( aType == IMarketType::NORMAL ){
        rNewMarket = new NormalMarket( this );
    }
    else if ( aType == IMarketType::TAX ) {
        rNewMarket = new MarketTax( this );
    }
    else if ( aType == IMarketType::RES ) {
        rNewMarket = new MarketRES( this );
    }
    else if ( aType == IMarketType::SUBSIDY ) {
        rNewMarket = new MarketSubsidy( this );
    }
    else if ( aType == IMarketType::CALIBRATION ) {
        rNewMarket = new CalibrationMarket( this );
    }
    else if ( aType == IMarketType::INVERSE_CALIBRATION ) {
        rNewMarket = new InverseCalibrationMarket( this );
    }
    else if ( aType == IMarketType::DEMAND ) {
        rNewMarket = new DemandMarket( this );
    }
    else if ( aType == IMarketType::TRIAL_VALUE ) {
        rNewMarket = new TrialValueMarket( this );
    }
    else if ( aType == IMarketType::PRICE ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Price markets are only created internally in the marketplace." << endl;
    }
    else if ( aType == IMarketType::LINKED ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Linked markets are only created via Marketplace::createLinkedMarket." << endl;
    }
    else {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Invalid market type: " << aType << endl;
    }
    return rNewMarket;
}

/*!
 * \brief Change the market which is linked to starting at the given period.
 * \details If the aStartPeriod parameter is set (non negative value) then
 *          this call may potentially override a previously set market with
 *          one that links elsewhere.
 * \param aMarketToLink The market that LinkedMarket should link to.
 * \param aStartPeriod If a non-negative value is the period in which a linked market may
 *                     ioverride a previously set market to effectively switch markets over time.
 */
void MarketContainer::changeLinkedMarket( MarketContainer* aMarketToLink, const int aStartPeriod ) {
    if( aStartPeriod > 0 ) {
        for( unsigned int period = aStartPeriod; period < size(); period++ ){
            mMarkets[ period ]->resetLinkedMarket( aMarketToLink ? aMarketToLink->mMarkets[ period ] : 0 );
        }
    }
}

/*!
 * \brief Reset the currently contained markets to a price market.
 * \details Such a request may occur to resolve simultaneities in model activities.
 *          In this case each NormalMarket will be replaced with a PriceMarket that
 *          interacts with a DemandMarket which should be contained in the given
 *          MarketContainer.
 * \param aDemandMarkets A MarketContainer which contains DemandMarkets that this
 *                       market will interact with to provide the abstraction of
 *                       a NormalMarket while using trial values.
 */
void MarketContainer::resetToPriceMarket( MarketContainer* aDemandMarkets ) {
    // loop through time periods
    // TODO: why does this loop start at period 1?  It seems to fail to solve otherwise.
    for( int period = 1; period < size(); ++period ) {
     
        // Get the pointer of the new demand market.
        Market* newDemandMarket = aDemandMarkets->mMarkets[ period ];
        assert( newDemandMarket );
        assert( newDemandMarket->getType() == IMarketType::DEMAND );

        // Check if the old market had set an initial trial demand which we
        // could use as the initial price.
        Market* oldMarket = mMarkets[ period ];
        assert( oldMarket->getType() == IMarketType::NORMAL );
        IInfo* marketInfoFrom = oldMarket->getMarketInfo();
        double initialDemand = marketInfoFrom->getDouble( "initial-trial-demand", false );
        newDemandMarket->setPrice( initialDemand );

        // Create a new price market from the old market.
        Market* newPriceMarket = new PriceMarket( *oldMarket, newDemandMarket );

        // Delete the old market.
        delete oldMarket;

        // Insert the new price market.
        mMarkets[ period ] = newPriceMarket;

        // Set both markets to solve.
        // Note that we hard code not solving period zero.
        mMarkets[ period ]->setSolveMarket( true );
        aDemandMarkets->mMarkets[ period ]->setSolveMarket( true );
        // this assumes that all markets have the same number of periods
    }
}

/*! \brief Add a region to the list of contained regions.
 * \details This function is used to add a region to the list of model regions
 *          which are contained in the market. If the region already exists in
 *          the list it is not added and a warning is printed.
 * \param aRegion The name of the region to add.
 */
void MarketContainer::addRegion( const string& aRegion ) {
    // Convert the string to an atom.
    const Atom* regionID = AtomRegistry::getInstance()->findAtom( aRegion );
    
    // Could be the first request for this name.  Note the atom registry will
    // manage this memory.
    if( !regionID ) {
        regionID = new Atom( aRegion );
    }
    
    /*! \invariant The ID of the found atom is the same as the name of the
     *              region, this ensures the lookup was correct.
     */
    assert( regionID->getID() == aRegion );
    
    // Check if the region ID does not already exist in the list.
    if( find( mContainedRegions.begin(), mContainedRegions.end(), regionID ) == mContainedRegions.end() ) {
        // Add the region ID.
        mContainedRegions.push_back( regionID );
    }
}

/*! \brief Get the IDs of all regions contained by this market.
 * \details Return the list of contained regions implemented as a vector of
 *          constant Atoms. This vector consists of the IDs of all regions within
 *          this market.
 * \return The IDs of all regions contained by this market.
 */
const vector<const Atom*>& MarketContainer::getContainedRegions() const {
    /*! \pre There is at least one contained region in the market. */
    assert( !mContainedRegions.empty() );
    return mContainedRegions;
}

/*! \brief Return the market name.
 * \details This function returns the name of the market, as defined by region
 *          name plus good name.
 * \return The market name
 */
const string& MarketContainer::getName() const {
    return mName;
}

/*! \brief Return the market region.
 * \details This method returns the region of the market. This may not be one of
 *          the miniCAM regions, as a market region can contain several regions.
 * \return The market region.
 */
const string& MarketContainer::getRegionName() const {
    return mRegion;
}

/*! \brief Return the market good name.
 * \details This function returns the good that the market represents.
 * \return The market good.
 */
const string& MarketContainer::getGoodName() const {
    return mGood;
}

/*!
 * \brief Get the number of contained markets.
 * \return The number of contained markets.
 */
int MarketContainer::size() const {
    return mMarkets.size();
}

/*!
 * \brief Use the market price history to forecast a price in the upcoming period.
 * \details Rather than setting the initial guess for a period to the
 *          last period value (which is almost certainly wrong for
 *          some markets), we extrapolate using the price history.
 *          Right now we use a crude extrapolation, but in the future
 *          we might adopt something more sophisticated.  To support
 *          that possibility, we record the forecast for the period,
 *          as future forecasting methods might try to estimate
 *          forecast bias or otherwise use the forecast history in
 *          addition to the price history.
 * \param aPeriod period for which to forecast
 */
double MarketContainer::forecastPrice( const int aPeriod )
{
    
    double forecastedPrice = extrapolate( aPeriod, &Market::getRawPrice );
    mMarkets[ aPeriod ]->setForecastPrice( forecastedPrice );
    
    return forecastedPrice;
}

/*!
 * \brief Use the market demand history to forecast a demand in the upcoming period.
 * \details An estimate for a reasonable range of demand values is necessary so
 *          that newton raphson algorithms can rescale supply/demand values from
 *          all markets to be in a similar range.
 * \param aPeriod period for which to forecast
 */
double MarketContainer::forecastDemand( const int aPeriod )
{
    double forecastedDemand = 0.0;
    if( mMarkets[ aPeriod ]->getType() == IMarketType::TAX && mMarkets[ aPeriod ]->isSolvable() ) {
        forecastedDemand = abs( mMarkets[ aPeriod ]->getSupply() );
    }
    else if( mMarkets[ aPeriod ]->getType() == IMarketType::SUBSIDY && mMarkets[ aPeriod ]->isSolvable() ) {
        forecastedDemand = abs( mMarkets[ aPeriod ]->getDemand() );
    }
    else {
        forecastedDemand = extrapolate( aPeriod, &Market::getSolverDemand );
        // set some reasonable limits on what kinds of forecast you get
        // this is going to use as a scale factor, so lose the sign
        forecastedDemand = abs( forecastedDemand );
        // we will rely on the preconditioner to correct unreasonable forecast
        // values as the requisite heuristics are implemented there
    }
    
    mMarkets[ aPeriod ]->setForecastDemand( forecastedDemand );
    return forecastedDemand;
}

/*!
 * \brief extrapolate some arbitrary value  using the last three values from the
 *        previous model periods.
 * \param aPeriod The current model period to extrapolate to.
 * \param aDataFn A function pointer which will be used to look up the actual data
 *                value that we are extrapolating.
 * \return The extrapolated data point for aPeriod.
 */
double MarketContainer::extrapolate( const int aPeriod, getpsd_t aDataFn )
{
    // for now, just do a simple extrapolation using the last 3 points
    double x[ 3 ],y[ 3 ];
    const Modeltime* modeltime = Modeltime::getInstance();
    
    
    /*!
     * \pre Period must be greater than zero.
     */
    assert( aPeriod > 0 );
    
    for( int i = 2; i >= 0; --i ) {
        int currPeriod = aPeriod + i - 3;
        if( currPeriod < 0 ) {
            // not enough history.  Note that if aPeriod>0, then this
            // can't happen for i==2, so referring to x[i+1] and y[i+1] is safe
            x[ i ] = x[ i + 1 ] - 1.0;
            y[ i ] = y[ i + 1 ];
        }
        else {
            x[ i ] = modeltime->getper_to_yr( currPeriod );
            // retrieve the data of the requested type.
            y[ i ] = (mMarkets[ currPeriod ]->*aDataFn)();
            if( i < 2 && y[ i ] < util::getTinyNumber() ) {
                // We have a few cases where sectors don't come into
                // use until some future period.  When that happens
                // the price, demand, etc. is zero until the sector
                // turns on, then it abruptly becomes nonzero.  Don't
                // try to extrapolate in those cases.  Just use the
                // last period value.
                y[ i ] = y[ i + 1 ];
            }
        }
    }
    
    // second order extrapolation
    double m, m1, m2;
    m1 = ( y[ 1 ] - y[ 0 ] ) / ( x[ 1 ] - x[ 0 ] );
    m2 = ( y[ 2 ] - y[ 1 ] ) / ( x[ 2 ] - x[ 1 ] );
    m = m2 + 2.0 * ( m2 - m1 ) / ( x[ 2 ] - x[ 0 ] );
    
    double currYear = modeltime->getper_to_yr( aPeriod );
    double extrapValue = y[ 2 ] + m * ( currYear - x[ 2 ] );
    
    // If we extrapolated a negative value but none of the actual values
    // were negative this won't be a viable result.  In this case we should
    // just return the last value.
    if( extrapValue < 0.0 && !(y[0] < 0.0 || y[1] < 0.0 || y[2] < 0.0)) {
        extrapValue = y[2];
    }
    return extrapValue;
}
