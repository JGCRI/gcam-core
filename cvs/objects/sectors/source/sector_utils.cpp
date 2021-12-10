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
 * \file sector_utils.cpp
 * \ingroup Objects
 * \brief The SectorUtils class source file.
 * \author Josh Lurz, Sonny Kim
 */

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <algorithm>
#include <numeric>
#include <cfloat>

#include "sectors/include/sector_utils.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/model_time.h"
#include "containers/include/iinfo.h"
#include "util/base/include/util.h"

using namespace std;

extern Scenario* scenario; // for marketplace and modeltime.

HashMap<std::string, std::string> SectorUtils::sTrialMarketNames;

typedef HashMap<string, string>::const_iterator NameIterator;

/*!
 * \brief Create a trial value market
 * \details This is a backward-compatibility wrapper around a
 *          version that does not require the IInfo structure,
 *          which was being used only to convey a unit string.  New
 *          calls to this function should use the version that accepts
 *          the unit string directly.
 * \note Deprecated
 */
bool SectorUtils::createTrialSupplyMarket( const string& aRegionName,
                                           const string& aSectorName,
                                           const IInfo *aTechnologyInfo,
                                           const string& aMarketName )
{
    string unitname = aTechnologyInfo->getString( "output-unit", true );
    return createTrialSupplyMarket(aRegionName, aSectorName, unitname, aMarketName);
}


/*!
 * \brief Create a trial market for the supply of a given good.
 * \details Sets up a trial value market for the given good in the given region.
 *          The trial market will be solved for all periods after the base
 *          period.
 * \author Josh Lurz, Sonny Kim
 * \param aRegionName Name of the region in which to create the market.
 * \param aSectorName Name of the sector for which to create the market.
 * \param aTechnologyInfo Technology Info object for passing information from
 *        technology to SectorUtil and market.
 * \param aMarketName The market name which may differ from aRegionName when the
 *                    trial value is shared amongst different regions.
 * \return Whether the market was created and did not already exist.
 */
bool SectorUtils::createTrialSupplyMarket( const string& aRegionName,
                                           const string& aSectorName,
                                           const string& aUnitStr,
                                           const string& aMarketName )
{
    const string& marketName = aMarketName.empty() ? aRegionName : aMarketName;
    // Add the trial market name to the cached list of trial names.
    const string trialName = getTrialMarketName( aSectorName );
    sTrialMarketNames.insert( make_pair( aSectorName, trialName ) );

    // Create the additional market.
    Marketplace* marketplace = scenario->getMarketplace();
    bool isNewMarket = marketplace->createMarket( aRegionName,
                                                  marketName,
                                                  trialName,
                                                  IMarketType::TRIAL_VALUE );

    // Set price and output units for period 0 market info
    // no operating trial market for base period, although vector always contains base period
    IInfo* marketInfoTrialSupplySector = marketplace->getMarketInfo( trialName, aRegionName, 0, true );
    marketInfoTrialSupplySector->setString( "price-unit", aUnitStr );
    marketInfoTrialSupplySector->setString( "output-unit", aUnitStr );

    // Set the market to solve.
    for( int per = 1; per < scenario->getModeltime()->getmaxper(); ++per ){
        marketplace->setMarketToSolve( trialName, aRegionName, per );
    }
    return isNewMarket;
}

/*!
 * \brief Set the trial value of supply for a given sector.
 * \details Sets the known value of the trial market such that at equilibrium,
 *          the set value is equal to the trial value.
 * \author Josh Lurz
 * \param aRegion Region of the market.
 * \param aSector Name of the sector.
 * \param aSupply Known value of supply for the iteration.
 * \param aPeriod Model period.
 */
void SectorUtils::addToTrialDemand( const string& aRegionName,
                                    const string& aSectorName,
                                    const Value& aSupply,
                                    const int aPeriod )
{
    // Market is not created until period 1.
    if( aPeriod == 0 ){
        return;
    }

    // Locate the trial market name.
    NameIterator trialName = sTrialMarketNames.find( aSectorName );
    
    // Check if the market existed.
    assert( trialName != sTrialMarketNames.end() );

    // Demand is the known value of the trial market. Trial markets do not solve
    // well when the value of one side is zero.
    scenario->getMarketplace()->addToDemand( trialName->second, aRegionName, aSupply,
                                             aPeriod, true );
}

/*!
 * \brief Get the trial value of supply for a given sector.
 * \details Gets the trial value of the trial market such that at equilibrium,
 *          the known value is equal to the trial value.
 * \author Josh Lurz
 * \param aRegion Region of the market.
 * \param aSector Name of the sector.
 * \param aPeriod Model period.
 * \return Trial value of supply, -1 if the market does not exist.
 */
double SectorUtils::getTrialSupply( const string& aRegionName,
                                    const string& aSectorName,
                                    const int aPeriod )
{
    // Market is not created yet in period 0.
    if( aPeriod == 0 ){
        return -1;
    }

    // Locate the trial market name.
    NameIterator trialName = sTrialMarketNames.find( aSectorName );
    
    // Check if the market existed.
    if( trialName == sTrialMarketNames.end() ){
        return -1;
    }

    // Get the trial value of supply from the marketplace, which is stored as
    // the price.
    double trialPrice = scenario->getMarketplace()->getPrice( trialName->second,
                                                              aRegionName, aPeriod );
    
    // The market should have existed if the trial market name search succeeded.
    assert( trialPrice != Marketplace::NO_MARKET_PRICE );
    return trialPrice;
}

/*!
 * \brief Calculate the scale factor used to reduce fixed output.
 * \details Calculates the scaling factor applied to fixed output in the sector
 *          when fixed output exceeds the market demand. The scale factor is one
 *          when this condition does not occur.
 * \param aMarketDemand Market demand for the good produced by the sector.
 * \param aFixedOutput Fixed output of the sector.
 * \return Fixed output scaling factor.
 */
double SectorUtils::calcFixedOutputScaleFactor( const double aMarketDemand,
                                                const double aFixedOutput )
{
    double scaleFactor = 1;

    // it's actually possible, for cases far from equilibrium, for
    // aMarketDemand < 0.  Ignore it for now.  As long as we don't
    // crash the model the solver will eventually move us to a
    // reasonable set of prices.
    if( aFixedOutput > aMarketDemand && aFixedOutput > 0 ){
      scaleFactor = aMarketDemand / aFixedOutput;
    }
    /*! \post Scale factor must be less than or equal to 1. */
    assert( scaleFactor <= 1 && scaleFactor >= 0 );
    return scaleFactor;
}

/*!
 * \brief Normalize a set of shares.
 * \details Attempts to normalize a vector of shares such that each share is
 *          equal to the initial share divided by the sum of the shares. If the
 *          sum of the shares is less than a very small number, the function
 *          will set the shares to 1/n, where n is the number of non-zero
 *          shares. If all shares are zero, there are no children which can
 *          produce output and the function returns 0 without adjusting the
 *          shares.
 * \param alogShares A vector of logs of unnormalized shares on input, normalized shares
 *                   (not logs) on output
 * \return The unnormalized sum of the shares and a log(adjustment factor) that
 *         has been factored out of the sum.  Having both can allow users to make
 *         calculations using these values in a numerically stable way.
 */
pair<double, double> SectorUtils::normalizeLogShares( vector<double>& alogShares ){
    // find the log of the largest unnormalized share
    double lfac = *max_element(alogShares.begin(), alogShares.end());
    double sum = 0.0;
    
    // check for all zero prices
    if( lfac == -numeric_limits<double>::infinity() ) {
        // In this case, set all shares to zero and return.
        // This is arguably wrong, but the rest of the code seems to expect it.
        for( size_t i = 0; i < alogShares.size(); ++i ) {
            alogShares[ i ] = 0.0;
        }
        return make_pair( 0.0, 0.0 );
    }

    // in theory we could check for lfac == +Inf here, but in light of how the log
    // shares are calculated, it would seem like that can't happen.

    // rescale and get normalization sum
    for( size_t i = 0; i < alogShares.size(); ++i ) {
        alogShares[ i ] -= lfac;
        sum += exp( alogShares[ i ] );
    }
    double unnormAdjustedSum = sum;
    double norm = log( sum );
    sum = 0.0;                               // double check the normalization
    for( size_t i = 0; i < alogShares.size(); ++i ) {
        alogShares[ i ] = exp( alogShares[ i ] - norm );   // divide by norm constant and unlog
        sum += alogShares[ i ];                      // accumulate sum of normalized shares 
                                                     //   (should be 1.0 when we're done.)
    }
    
    // In actuality, this rescaling scheme should eliminate the problem of
    // failed normalizations, but we'll allow for the possibility anyhow.
    assert( sum < numeric_limits<double>::min() || util::isEqual( sum, 1.0 ) );

    return make_pair( unnormAdjustedSum, lfac );
}

double SectorUtils::normalizeShares( vector<double>& aShares ){
    // Calculate the total of the shares so they can be normalized.
    const double sum = accumulate( aShares.begin(), aShares.end(), 0.0 );

    typedef vector<double>::iterator VecIterator;
    
    // Check for an unnormalizable vector. Unnormalized shares may be very
    // small.
    if( sum > DBL_MIN ){
        // Adjust all the shares.
        for( VecIterator i = aShares.begin(); i != aShares.end(); ++i ){
            *i /= sum;
        }
        // Check that the normalization was performed correctly.
        assert( util::isEqual( accumulate( aShares.begin(),
                                           aShares.end(), 0.0 ), 1.0 ) );
        
        // If the shares could be normalized assume they were correct.
        return 1;
    }

    // Shares cannot be normalized.
    return 0;
}

/*!
 * \brief Get the base period to use for calculation of price ratios used in demand calculations
 * \details Price ratio calculations need to be normalized to the last calibation period. 
 * \param aPeriod Model period.
 * \warning This approach will result in price effects of zero for all demands in a calibration year
 *          even if that particular sector has no calibration values.
 * \return The base period for price ratio calculations
 */
int SectorUtils::getDemandNormPeriod( const int aPeriod ){
    const Modeltime* modeltime = scenario->getModeltime();

    int normPeriod = 0;
    // Normalization period is the current period for calibration.
    // Price ratio has no impact for calibration years.
    if( aPeriod < modeltime->getFinalCalibrationPeriod() ){
        normPeriod = aPeriod;
    }
    // Normalization period is the last calibration period for future periods
    else {
        normPeriod = modeltime->getFinalCalibrationPeriod();
    }
    
    return normPeriod;
}

/*!
 * \brief Get the name of the trial supply market for a given sector.
 * \param aSector Sector name.
 * \return The name of the trial supply market for the sector.
 */
const string SectorUtils::getTrialMarketName( const string& aSectorName ){
    return aSectorName + "-trial-supply";
}

/*!
 * \brief Converts energy to capacity using a given capacity factor.
 * \param aCapacityFactor Capacity factor to use in the conversion.
 * \param aEnergy The energy quantity to convert.
 * \return Capacity equivalent of the energy.
 */
double SectorUtils::convertEnergyToCapacity( const double aCapacityFactor,
                                             const double aEnergy )
{
    /*! \pre Capacity factor must be positive and non-zero. */
    assert( aCapacityFactor > util::getSmallNumber() );

    // Conversion: 1 gigaWattHour of electricity = 3.6E-6 ExaJoules
    const double EJ_PER_GWH = 3.6E-6;
    
    // Number of hours in a year.
    const unsigned int HOURS_PER_YEAR = 8760;

    return aEnergy / ( EJ_PER_GWH * HOURS_PER_YEAR * aCapacityFactor );
}

/*!
 * \brief Get the ratio of an sector's price in the current period to the base
 *        period.
 * \details Returns the ratio of the sector's price in the given region in the
 *          current period to the base period. If the base period is greater than
 *          or equal to the current period this will return 1.
 * \param aRegionName Name of the region in which to find the ratio.
 * \param aSectorName Sector for which to find the price ratio.
 * \param aBasePeriod Base period for the ratio.
 * \param aCurrentPeriod Current period for the ratio.
 * \return Price ratio for the sector.
 * \warning No checks are made for negative prices and users of this method should
 *          ensure proper behavior is such cases.
 * \sa adjustDemandForNegativePrice
 * \author Josh Lurz
 */
double SectorUtils::calcPriceRatio( const string& aRegionName,
                                    const string& aSectorName,
                                    const int aBasePeriod,
                                    const int aCurrentPeriod )
{
    // The price ratio is always 1 in the base period.
    double priceRatio = 1;
    double internalBasePeriod = aBasePeriod;
    
    // Prices before 1990 are not valid.
    if ( aBasePeriod == 0 ) {
        internalBasePeriod = 1; 
    }
    if( aCurrentPeriod > internalBasePeriod ) {
        const Marketplace* marketplace = scenario->getMarketplace();
        double basePrice = marketplace->getPrice( aSectorName, aRegionName, internalBasePeriod );
        double currentPrice = marketplace->getPrice( aSectorName, aRegionName, aCurrentPeriod );

        priceRatio = currentPrice / basePrice;
        
    }

    return priceRatio;
}

/*!
 * \brief Defines an arbitrarily small price threshold which will be used to cap
 *        negative prices which escape to the demand calculations.
 * \return A small price threshold value to use.
 * \sa adjustDemandForNegativePrice
 */
double SectorUtils::getDemandPriceThreshold() {
    return util::getVerySmallNumber();
}

/*!
 * \brief Increases some demand scalar the more negative the price driver becomes.
 * \details Many demand formulations do not properly handle negative prices.  We
 *          do not expect to have negative service prices in equilibrium however
 *          during solution we may land in such scenarios so we will add some
 *          behavior to the demand scalar that was calculated at a price value of
 *          getDemandPriceThreshold() to give the solver some continuous (there is
 *          still a discontinuity at the threshold value) behavior to get the prices
 *          back into a reasonable range.
 * \param aDemandScalar The demand scalar calculated at the price threshold.
 * \param aPrice The negative (or really less than the threshold) price driver.
 * \return An adjusted demand driver inflated the more negative aPrice is.
 * \todo We do not expect that this function is called when we finally reach
 *       equilibrium, maybe devise some signaling to raise an error if it was.
 */
double SectorUtils::adjustDemandForNegativePrice( const double aDemandScalar, const double aPrice ) {
    /*!
     * \pre aPrice is below the threshold value.
     */
    assert( aPrice <= getDemandPriceThreshold() );

    return aDemandScalar * ( 1.0 + ( getDemandPriceThreshold() - aPrice ) );
}

/*!
 * \brief Create the name for the TFE market associated with a sector.
 * \param aSectorName Name of the sector.
 * \return The name of the TFE market.
 * \warning Due to the string concatenation, this function is slow.
 */
const string SectorUtils::createTFEMarketName( const string& aSectorName ){
    return aSectorName + "-tfe";
}

/*!
 * \brief Sets a flag into the marketplace indicating that a sector is a
 *        supplier of final energy.
 * \param aRegionName Region name.
 * \param aSectorName Sector name.
 */
void SectorUtils::setFinalEnergyFlag( const string& aRegionName,
                                      const string& aSectorName )
{
    IInfo* sectorInfo =
        scenario->getMarketplace()->getMarketInfo( aSectorName, aRegionName, 0,
                                                   true );

    // The marketplace will print a warning if the market does not already
    // exist.
    if( !sectorInfo ){
        return;
    }

    sectorInfo->setBoolean( "is-final-energy", true );
}

/*! 
 * \brief Return whether the given sector is a supplier of final energy.
 * \param aRegionName Region name.
 * \param aSectorName Sector name.
 * \return Whether the given sector is a supplier of final energy.
 */
bool SectorUtils::isFinalEnergySector( const string& aRegionName,
                                       const string& aSectorName )
{
    const IInfo* sectorInfo =
        scenario->getMarketplace()->getMarketInfo( aSectorName, aRegionName, 0, true );

    assert( sectorInfo );

    // Check the final energy flag.
    return sectorInfo->getBoolean( "is-final-energy", false );
}

/*!
 * \brief Convert the capacity of a sector into the energy produced using the
 *        capacity factor.
 * \param aCapacityFactor Capacity factor to use in the conversion.
 * \param aCapacity The capacity quantity to convert.
 * \return The energy equivalent of the capacity.
 */
double SectorUtils::convertCapacityToEnergy( const double aCapacityFactor,
                                             const double aCapacity )
{
    /*! \pre Capacity factor must be positive and non-zero. */
    assert( aCapacityFactor > util::getSmallNumber() );

    // Conversion: 1 gigaWattHour of electricity = 3.6E-6 ExaJoules
    const double EJ_PER_GWH = 3.6E-6;
    // Number of hours in a year.
    const unsigned int HOURS_PER_YEAR = 8760;
    
	//converts capacity in GW to energy in EJ
    return aCapacity * ( aCapacityFactor * EJ_PER_GWH * HOURS_PER_YEAR );
}

/*!
 * \brief Make available to the solution mechanism some best guess bounds for which
 *        we expect there to be supply behavior.
 * \details Since this meta-data is only calculated at the start of the period and
 *          performance is not critical it is passed via market-info.  Only valid
 *          values that are smaller / greater than the currently set lower / upper
 *          bounds are set.
 * \param aGoodName The name of the good for which to set the bounds information.
 * \param aRegionName The name of the region in which to set the bounds information.
 * \param aLowerPriceBound The price below which zero supply is likely.
 * \param aUpperPriceBound The price above which no additional supply is likely.
 *\ param aPeriod The current model period.
 */
void SectorUtils::setSupplyBehaviorBounds( const string& aGoodName, const string& aRegionName,
                                           const double aLowerPriceBound, const double aUpperPriceBound,
                                           const int aPeriod )
{
    const string LOWER_BOUND_KEY = "lower-bound-supply-price";
    const string UPPER_BOUND_KEY = "upper-bound-supply-price";

    IInfo* sectorInfo = scenario->getMarketplace()->getMarketInfo( aGoodName, aRegionName, aPeriod, true );

    /*!
     * \pre the info object must exist.
     */
    assert( sectorInfo );

    // Set the lower price bound.
    double lowerPriceBound = sectorInfo->hasValue( LOWER_BOUND_KEY ) ? sectorInfo->getDouble( LOWER_BOUND_KEY, false ) :
        util::getLargeNumber();
    if( util::isValidNumber( aLowerPriceBound ) ) {
        lowerPriceBound = min( lowerPriceBound, aLowerPriceBound );
    }
    sectorInfo->setDouble( LOWER_BOUND_KEY, lowerPriceBound );

    // Set the upper price bound.
    double upperPriceBound = sectorInfo->hasValue( UPPER_BOUND_KEY ) ? sectorInfo->getDouble( UPPER_BOUND_KEY, false ) :
        -util::getLargeNumber();
    if( util::isValidNumber( aUpperPriceBound ) ) {
        upperPriceBound = max( upperPriceBound, aUpperPriceBound );
    }
    sectorInfo->setDouble( UPPER_BOUND_KEY, upperPriceBound );
}


/*! \brief Fills missing period elements in a Value vector with values linearly
*        interpolated from initialized or read-in values.
* \detail This method is intended for enabling variable time-step capability
*         and filling in values that have not been read-in or initialized
*         in a PeriodVector.
* \param aValueVector a period vector of Values.
* \author Sonny Kim
*/
void SectorUtils::fillMissingPeriodVectorInterpolated( objects::PeriodVector<Value>& aPeriodVector ){
    const Modeltime* modeltime = scenario->getModeltime();
    
    // the periodVector for the final calibration period should be initialized
    assert( aPeriodVector[ modeltime->getFinalCalibrationPeriod() ].isInited() );

    for( int per = modeltime->getFinalCalibrationPeriod() + 1; per < modeltime->getmaxper(); ++per ) {
        int currYear = modeltime->getper_to_yr( per );
        // search for the bounded read-in values
        // There is always an initialized value for the calibration period.
        if( !aPeriodVector[ per ].isInited() ) { // not read in
            int prevPer = per - 1;
            // find the previous read-in value
            while( prevPer > modeltime->getBasePeriod() &&
                !aPeriodVector[ prevPer ].isInited() )
            {
                --prevPer;
            }
            int prevYear = modeltime->getper_to_yr( prevPer );
            Value prevValue = aPeriodVector[ prevPer ];

            int nextPer = per + 1;
            // find the next or following read-in value
            while( nextPer < modeltime->getmaxper() &&
                !aPeriodVector[ nextPer ].isInited() )
            {
                ++nextPer;
            }
            if( nextPer == modeltime->getmaxper() ){
                // got to end of model time without finding initialized value
                // set nextPer to previous period with initialized value
                nextPer = prevPer;
            }
            int nextYear = modeltime->getper_to_yr( nextPer );
            Value nextValue = aPeriodVector[ nextPer ];
            // Initialize period vector with interpolated values.
            // Note we are allowing interpolation from uninitialized end points (which
            // will have a value of 0) but not if both are uninitialized.  The subtle
            // difference is if the isInited() flag on the interpolated value is set
            // or not.
            if( prevValue.isInited() || nextValue.isInited() ) {
                aPeriodVector[ per ].set( prevYear != nextYear ? util::linearInterpolateY( 
                    currYear, prevYear, nextYear, prevValue, nextValue ) : prevValue.get() );
            }
        }
    }
}

/*! \brief Fills missing period elements in a Value vector with values 
*         available from next initialized or read-in values.
* \detail This method is intended for enabling variable time-step capability
*         and filling in values that have not been read-in or initialized
*         in a PeriodVector.
* \param aValueVector a period vector of Values.
* \author Sonny Kim
*/
void SectorUtils::fillMissingPeriodVectorNextAvailable( objects::PeriodVector<Value>& aPeriodVector ){
    const Modeltime* modeltime = scenario->getModeltime();
    
    // the periodVector for the final calibration period should be initialized
    assert( aPeriodVector[ modeltime->getFinalCalibrationPeriod() ].isInited() );

    for( int per = modeltime->getFinalCalibrationPeriod() + 1; per < modeltime->getmaxper(); ++per ) {
        // search for the bounded read-in values
        // There is always an initialized value for the calibration period.
        if( !aPeriodVector[ per ].isInited() ) { // not read in
            int prevPer = per - 1;
            // find the previous read-in value
            while( prevPer > modeltime->getBasePeriod() &&
                !aPeriodVector[ prevPer ].isInited() )
            {
                --prevPer;
            }

            int nextPer = per + 1;
            // find the next or following read-in value
            while( nextPer < modeltime->getmaxper() &&
                !aPeriodVector[ nextPer ].isInited() )
            {
                ++nextPer;
            }
            if( nextPer == modeltime->getmaxper() ){
                // got to end of model time without finding initialized value
                // set nextPer to previous period with initialized value
                nextPer = prevPer;
            }

            // Set period vector with value from next available initialized period.
            aPeriodVector[ per ] = aPeriodVector[ nextPer ];
        }
    }
}
