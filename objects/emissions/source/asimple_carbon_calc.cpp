/*! 
 * \file asimple_carbon_calc.cpp
 * \ingroup Objects
 * \brief ASimpleCarbonCalc class source file.
 * \author James Blackwood
 */

#include "util/base/include/definitions.h"
#include <cassert>
#include <cfloat>

#include "emissions/include/asimple_carbon_calc.h"
#include "util/base/include/ivisitor.h"
#include "util/base/include/util.h"
#include "land_allocator/include/land_use_history.h"

using namespace std;
using namespace xercesc;

/*!
 * \brief Constructor.
 * \author James Blackwood
 */
ASimpleCarbonCalc::ASimpleCarbonCalc() : mCurrentEmissions( getStartYear(), getEndYear() ),
                                         mCalculated( getStartYear(), getEndYear() ),
                                         mTotalEmissions( getStartYear(), getEndYear() ),
                                         mIsFirstTime( true ),
                                         mHistoricalShare( 0 ),
                                         mLandUseHistory( 0 )
{
}

//! Default destructor
ASimpleCarbonCalc::~ASimpleCarbonCalc() {
}

void ASimpleCarbonCalc::initLandUseHistory( const LandUseHistory* aHistory,
                                            const double aShare )
{
    mLandUseHistory = aHistory;
    mHistoricalShare = aShare;
}

void ASimpleCarbonCalc::calc( const int aPeriod ) {
    const Modeltime* modeltime = scenario->getModeltime();
    const int timeStep = modeltime->gettimestep( aPeriod );
    const int calcYear = modeltime->getper_to_yr( aPeriod );

    // Find any years up to the current period that have not been calculated.
    for( int i = getStartYear(); i <= calcYear - timeStep; ++i ){
        if( !mCalculated[ i ] ){
            // Calculate above and below ground land use change emissions.
            calcAboveGroundCarbonEmission( i, false );
            calcBelowGroundCarbonEmission( i, false );
            mCalculated[ i ] = true;
        }
    }

    // Clear the current emissions as this is a new year.
    if( mIsFirstTime[ aPeriod ] ){
        mIsFirstTime[ aPeriod ] = false;
        mCurrentEmissions.assign( mCurrentEmissions.size(), 0.0 );
    }
    // If the period was already calculated, remove the previously added
    // emissions or uptake from the totals.
    else {
        for( unsigned int i = getStartYear(); i <= getEndYear(); ++i ){
            mTotalEmissions[ i ] -= mCurrentEmissions[ i ];

            // Clear the current emissions for the year.
            mCurrentEmissions[ i ] = 0;
        }
    }

    // Calculate the present period.
    for( int year = calcYear - timeStep + 1; year <= calcYear; ++year ){
        calcAboveGroundCarbonEmission( year, true );
        calcBelowGroundCarbonEmission( year, true );
        mCalculated[ year ] = true;
    }
}

double ASimpleCarbonCalc::getNetLandUseChangeEmission( const int aYear ) const {
    assert( mCalculated[ aYear ] );
    return mTotalEmissions[ aYear ];
}

double ASimpleCarbonCalc::getNetTerrestrial( const int aYear ) const {
    // The simple carbon calculator does not implement this function as it is
    // not detailed enough to include a full carbon cycle.
    return DBL_MAX;
}

void ASimpleCarbonCalc::setTotalLandUse( const double aLandUse, const int aPeriod ) {
    mLandUse[ aPeriod ] = aLandUse;
}

void ASimpleCarbonCalc::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitCarbonCalc( this, aPeriod );
    aVisitor->endVisitCarbonCalc( this, aPeriod );
}

/*!
 * \brief Get the land usage for a year.
 * \details Returns the land usage for a year. An appropriate historical or
 *          calculated value will be returned depending on the year.
 * \return Land usage for the year.
 */
double ASimpleCarbonCalc::getLandUse( const unsigned int aYear ) const {
    // TODO: Make this dynamic.
    const unsigned int basePeriod = 1;

    // Store the first calculated year to save time.
    static const unsigned int baseYear
        = static_cast<unsigned int>( scenario->getModeltime()->getper_to_yr( basePeriod ) );

    // TODO: Determine what to do if the history overlaps with calculated
    // values.

    double landUse;
    // If the year is within the range of the history use the historical
    // allocation. The land use history may be null if none was read-in.
    unsigned int maxHistoryYear = 0;
    if( mLandUseHistory ){
        maxHistoryYear = mLandUseHistory->getMaxYear();
    }

    if( aYear < maxHistoryYear ){
        landUse = mHistoricalShare * mLandUseHistory->getAllocation( aYear );
    }
    // If the year is between the last historical year and the first
    // calculated year interpolate between the two.
    else if( aYear < baseYear && maxHistoryYear != 0 ){
        landUse = util::linearInterpolateY( aYear, maxHistoryYear, baseYear,
                                            mHistoricalShare * mLandUseHistory->getAllocation( maxHistoryYear ),
                                            mLandUse[ basePeriod ] );
    }
    // Otherwise use data interpolated from the current data.
    else {
        landUse = interpYearHelper( mLandUse, aYear );
    }
    return landUse;
}

/*!
 * \brief Calculate the emission from above ground carbon for a given year.
 * \details Above ground carbon is emitted as a pulse.
 * \param aYear Year.
 * \param aIsCurrentYear Whether the year being calculated is the current year.
 */
void ASimpleCarbonCalc::calcAboveGroundCarbonEmission( const unsigned int aYear,
                                                       const bool aIsCurrentYear )
{
    // Above ground carbon currently always contains the maximum potential
    // amount of carbon.
    double prevCarbon = getLandUse( aYear - 1 )
                        * getPotentialAboveGroundCarbon( aYear - 1 );
    
    double currCarbon = getLandUse( aYear )
                        * getPotentialAboveGroundCarbon( aYear );
    
    // Add a positive emission if the previous year contained more carbon
    // than the current year. If the previous year contains less carbon than the
    // current year than a negative emission representing uptake has occurred.
    mTotalEmissions[ aYear ] += ( prevCarbon - currCarbon );

    // If this is the current year being calculated store the emission
    // separately so it can be removed in future iterations.
    if( aIsCurrentYear ){
        mCurrentEmissions[ aYear ] += ( prevCarbon - currCarbon );
    }
}

/*!
* \brief Calculate the emission from below ground carbon for the given year.
* \details Below ground, or soil carbon, is not emitted as a pulse but at a
*          rate defined by an exponential decay function.
* \param aYear Year.
* \param aIsCurrentYear Whether the year being calculated is the current year.
*/
void ASimpleCarbonCalc::calcBelowGroundCarbonEmission( const unsigned int aYear,
                                                       const bool aIsCurrentYear )
{
    // Calculate the total emission which will be spread across the full
    // emission time.
    
    double soilCarbonPrev = getLandUse( aYear - 1 )
                            * getPotentialBelowGroundCarbon( aYear - 1 );

    double soilCarbonCurr = getLandUse( aYear )
                            * getPotentialBelowGroundCarbon( aYear );

    // Calculate the difference in carbon between the previous period and the
    // current period. If this is negative, an uptake has occurred. If this is
    // positive an emissions has occurred.
    double carbonDifference = soilCarbonPrev - soilCarbonCurr;

    // If the carbon content is equivalent than there are no emissions to
    // distribute.
    if( util::isEqual( carbonDifference, 0.0 ) ){
        return;
    }

    double tempSum = 0;

    // Set emissions from now until the end of the model.
    bool aboveMin = true;
    for( unsigned int year = aYear; year <= getEndYear() && aboveMin; ++year ){
        // Calculate the future emissions for the year defined by the function:
        // E = deltaC/Tau * e^(-t/Tau)
        double futureAnnualEmiss = carbonDifference / getSoilTimeScale()
                                   * ( exp( -1 * double( year - aYear ) / getSoilTimeScale() ) );

        // Check if the future emissions are below a minimum. This is an
        // optimization to avoid iterating over years with negligible emissions.
        if( futureAnnualEmiss < util::getSmallNumber() ){
            // Integrate the remaining emissions tail and add the emission in the
            // current year. The integral from t to infinity of the above
            // function is deltaC * e^(-t/Tau) so multiple the future annual
            // emission by Tau to determine the remaining emissions.
            futureAnnualEmiss *= getSoilTimeScale();
            aboveMin = false;
        }

        // Only store annual emissions values if this is not a historical
        // emissions calculation. Historical emissions calculations only occur
        // once, unlike current emissions calculations which need to remove the
        // effect of the previous iteration.
        if( aIsCurrentYear ){
            mCurrentEmissions[ year ] += futureAnnualEmiss;
        }

        // Add to the total carbon emission for the year. This will be the sum
        // of the effects of all carbon emissions for the previous years.
        mTotalEmissions[ year ] += futureAnnualEmiss;
    }
}

/*!
 * \brief A static function to return the starting year to index the arrays.
 * \todo Use a read-in value for the start year.
 * \author James Blackwood
 * \return The startYear.
*/
const unsigned int ASimpleCarbonCalc::getStartYear() const {
    // First model period ends in 1975 and starts in 1961.
    return 1961;
}

/*!
 * \brief Return the last year of the climate calculation.
 * \todo Make this value dynamic.
 * \author James Blackwood
 * \return The last year of the climate calculation.
 */
const unsigned int ASimpleCarbonCalc::getEndYear() const {
    return 2095;
}

/*
 * \brief Returns a parameter which defines the time scale for the soil
 *        emissions decay function.
 * \return Soil decay function time scale parameter.
 * \todo This should be dynamic by land type.
 */
double ASimpleCarbonCalc::getSoilTimeScale() const {
    return 40;
}

/*!
 * \brief Helper function to interpolate a value for a year from a PeriodVector.
 * \details Calculates a linearly interpolated value for the year. If the year
 *          is before the first period of the vector, the first value is
 *          returned. If the year is after the last period of the vector, the
 *          last value is used. Otherwise a value is linearly interpolated
 *          between the nearest two periods.
 * \param aPeriodVector Vector from which to interpolate the value.
 * \param aYear Year for which to interpolate a value.
 * \return Interpolated value for the year.
 */
double ASimpleCarbonCalc::interpYearHelper( const objects::PeriodVector<double>& aPeriodVector,
                                            const unsigned int aYear )
{
    // If the year is before the first period of the model use the value
    // in the base period.
    const Modeltime* modeltime = scenario->getModeltime();
    if( aYear <= static_cast<unsigned int>( modeltime->getStartYear() ) ){
        return *aPeriodVector.begin();
    }

    // If the year is after the end period use the value in the last period.
    if( aYear > static_cast<unsigned int>( modeltime->getEndYear() ) ){
        return *aPeriodVector.last();
    }
    
    // Find the period containing aYear. This cannot be zero because the year
    // was already checked against the start year.
    int currPeriod = modeltime->getyr_to_per( aYear );

    // Find the last year of the current period.
    int lastYear = modeltime->getper_to_yr( currPeriod );

    // Find the first year of the current period.
    int firstYear = modeltime->getper_to_yr( currPeriod - 1 );

    // Interpolate the result.
    return util::linearInterpolateY( aYear, firstYear, lastYear,
                                     aPeriodVector[ currPeriod - 1 ],
                                     aPeriodVector[ currPeriod ] );
}

/*!
 * \brief Helper function to interpolate a value for a year from a YearVector.
 * \details Determines a value for a given year from a YearVector. If the year
 *          is within the range of the year vector the value for the year will
 *          be returned, otherwise a value will be extrapolated. The
 *          extrapolation considers all values before the first year equal to
 *          the first year, and all values after the end year to be equal to the
 *          end year.
 * \param aYearVector Vector from which to interpolate the value.
 * \param aStartYear First year of the climate model.
 * \param aEndYear Last year of the climate model.
 * \param aYear Year for which to interpolate a value.
 * \return Interpolated value for the year.
 */
double ASimpleCarbonCalc::interpYearHelper( const objects::YearVector<double>& aYearVector,
                                            const unsigned int aStartYear,
                                            const unsigned int aEndYear,
                                            const unsigned int aYear )
{
    // If the year is before the first year of the carbon cycle use the value in the
    // first year.
    if( aYear < aStartYear ){
        return *aYearVector.begin();
    }

    // If the year is after the last year of the carbon cycle use the value in the last year.
    if( aYear > aEndYear ){
        return *aYearVector.last();
    }

    // Return the value from inside the range of the vector.
    return aYearVector[ aYear ];
}
