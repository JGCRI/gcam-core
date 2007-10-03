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
#include "ccarbon_model/include/carbon_model_utils.h"
#include "util/base/include/ivisitor.h"
#include "util/base/include/util.h"
#include "land_allocator/include/land_use_history.h"

using namespace std;
using namespace xercesc;

/*!
 * \brief Constructor.
 * \author James Blackwood
 */
ASimpleCarbonCalc::ASimpleCarbonCalc()
: mCurrentEmissions( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() ),
  mCalculated( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() ),
  mTotalEmissions( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() ),
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

void ASimpleCarbonCalc::calc( const int aYear ) {
    const Modeltime* modeltime = scenario->getModeltime();
    // If this is a land-use history year...
    if( aYear < modeltime->getStartYear() ) {
        calcAboveGroundCarbonEmission( aYear, false );
        calcBelowGroundCarbonEmission( aYear, false );
    }
    // Otherwise...
    else {
        int aPeriod = modeltime->getyr_to_per( aYear );
        const int timeStep = modeltime->gettimestep( aPeriod );
        const int calcYear = aYear;

        // Clear the current emissions as this is a new year.
        if( mIsFirstTime[ aPeriod ] ){
            mIsFirstTime[ aPeriod ] = false;
            mCurrentEmissions.assign( mCurrentEmissions.size(), 0.0 );
        }
        // If the period was already calculated, remove the previously added
        // emissions or uptake from the totals.
        else {
            for( int i = CarbonModelUtils::getStartYear(); i <= CarbonModelUtils::getEndYear(); ++i ){
                mTotalEmissions[ i ] -= mCurrentEmissions[ i ];

                // Clear the current emissions for the year.
                mCurrentEmissions[ i ] = 0;
            }
        }
        // Calculate the present year
        calcAboveGroundCarbonEmission( aYear, true );
        calcBelowGroundCarbonEmission( aYear, true );
    }
}

void ASimpleCarbonCalc::calcLandUseChange( const int aYear, FlowType aFlowType ) {
    // Do nothing for now, this method was added for the complex carbon model...
}

double ASimpleCarbonCalc::getNetLandUseChangeEmission( const int aYear ) const {
    //assert( mCalculated[ aYear ] );
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
    double prevCarbon = CarbonModelUtils::getLandUse( aYear - 1, mLandUseHistory,
                                                  mHistoricalShare, mLandUse )
                        * getPotentialAboveGroundCarbon( aYear - 1 );
    
    double currCarbon = CarbonModelUtils::getLandUse( aYear, mLandUseHistory,
                                                  mHistoricalShare, mLandUse )
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
    
    double soilCarbonPrev = CarbonModelUtils::getLandUse( aYear - 1, mLandUseHistory,
                                                      mHistoricalShare, mLandUse )
                        * getPotentialAboveGroundCarbon( aYear - 1 );
    
    double soilCarbonCurr = CarbonModelUtils::getLandUse( aYear, mLandUseHistory,
                                                      mHistoricalShare, mLandUse )
                        * getPotentialAboveGroundCarbon( aYear );

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
    for( int year = aYear; year <= CarbonModelUtils::getEndYear() && aboveMin; ++year ){
        // Calculate the future emissions for the year defined by the function:
        // E = deltaC/Tau * e^(-t/Tau)
        double futureAnnualEmiss = carbonDifference / CarbonModelUtils::getSoilTimeScale()
                                   * ( exp( -1 * double( year - aYear )
                                   / CarbonModelUtils::getSoilTimeScale() ) );

        // Check if the future emissions are below a minimum. This is an
        // optimization to avoid iterating over years with negligible emissions.
        if( futureAnnualEmiss < util::getSmallNumber() ){
            // Integrate the remaining emissions tail and add the emission in the
            // current year. The integral from t to infinity of the above
            // function is deltaC * e^(-t/Tau) so multiple the future annual
            // emission by Tau to determine the remaining emissions.
            futureAnnualEmiss *= CarbonModelUtils::getSoilTimeScale();
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

