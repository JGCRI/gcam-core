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
 * \file asimple_carbon_calc.cpp
 * \ingroup Objects
 * \brief ASimpleCarbonCalc class source file.
 * \author James Blackwood
 */

#include "util/base/include/definitions.h"
#include <cassert>
#include <cfloat>
#include <stdio.h>

#include "emissions/include/asimple_carbon_calc.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include "util/base/include/ivisitor.h"
#include "util/base/include/util.h"
#include "land_allocator/include/land_use_history.h"
#include "util/logger/include/ilogger.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
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
  mLandUseHistory( 0 ),
  mMatureAge( 1 ),
  mSoilTimeScale ( CarbonModelUtils::getSoilTimeScale() ),
  precalc_sigmoid( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() ),
  precalc_expdecay( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() )
{
    // The exp() function used to calculate decay and growth is computationally expensive,
    // so precalculate basic curves that will be scaled as necessary later.
    int startYear = CarbonModelUtils::getStartYear();
    int endYear = CarbonModelUtils::getEndYear();
    int soilTimeScale = CarbonModelUtils::getSoilTimeScale();
    
    for ( int i = startYear; i <= endYear; i++ ) {

        // Original: double futureCarbon = carbonDifference * pow( 1 - exp( -10.0/static_cast<double>( mMatureAge ) * ( year-aYear+1 ) ), mMatureAge );
        // Here we fill an array, 1975-2095, with a sigmoid curve; it will be scaled in x (time) and y (carbonDifference)
        precalc_sigmoid[ i ] = 1.0 * pow( 1 - exp( -3.0 - 10.0/(endYear-startYear+1) * ( i-startYear+1 ) ), endYear-startYear );

        // Original: double futureAnnualEmiss = ( carbonDifference / CarbonModelUtils::getSoilTimeScale() )
        //      * ( exp( -1 * double( year - aYear )
        //      / CarbonModelUtils::getSoilTimeScale() ) );  
        // Here we fill an array, 1975-2095, with a exp decay curve; it will be scaled in y (carbonDifference) but NOT in x (time)
        precalc_expdecay[ i ] = ( 1.0/soilTimeScale ) * ( exp( -1.0 * static_cast<double>( i-startYear ) / soilTimeScale ) );
        
    }

    mStoredEmissions.resize( scenario->getModeltime()->getmaxper() );
    for( unsigned int i = 0; i < mStoredEmissions.size(); ++i ){
        mStoredEmissions[ i ].resize( CarbonModelUtils::getEndYear() - CarbonModelUtils::getStartYear() + 1 );
        mStoredEmissions[ i ].assign( mStoredEmissions[ i ].size(), 0.0 );
    }

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
    const int startYear = CarbonModelUtils::getStartYear();
    const int endYear = CarbonModelUtils::getEndYear();

    // If this is a land-use history year...
    if( aYear < modeltime->getStartYear() ) {
        // This code requires our land use history to be accurate.
        // AboveGroundCarbon is overwritten in these years
        // BelowGroundCarbon affects future model periods that are not overwritten
        calcAboveGroundCarbonEmission( aYear, false );
        calcBelowGroundCarbonEmission( aYear, false );

    }
    // Otherwise...
    else {
        int aPeriod = modeltime->getyr_to_per( aYear );

        // Clear the current emissions as this is a new year.
        if( mIsFirstTime[ aPeriod ] ){
            mIsFirstTime[ aPeriod ] = false;
            mCurrentEmissions.assign( mCurrentEmissions.size(), 0.0 );
            mStoredEmissions[ aPeriod ].assign( mStoredEmissions[ aPeriod ].size(), 0.0 );
        }
        // If the period was already calculated, remove the previously added
        // emissions or uptake from the totals.
        else {
            for( int i = startYear; i <= endYear; ++i ){
                mTotalEmissions[ i ] -= mStoredEmissions[ aPeriod ][ i - startYear ];

                // Clear the current emissions for the year.
                mCurrentEmissions[ i ] = 0;
                mStoredEmissions[ aPeriod ][ i - startYear ] = 0.0;
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
//    assert( mCalculated[ aYear ] ); 
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

    acceptDerived( aVisitor, aPeriod );

    aVisitor->endVisitCarbonCalc( this, aPeriod );
}

void ASimpleCarbonCalc::acceptDerived( IVisitor* aVisitor, const int aPeriod ) const {
    // do nothing currently
}

/*!
 * \brief    Calculate the sigmoidal sequestration curve.
 * \details  Called by calcAboveGroundCarbonEmission and calcBelowGroundCarbonEmission, below.
 * \param    carbonDifference Total area under sequestration curve 
 * \param    aYear Year.
 * \param    aIsCurrentYear Whether the year being calculated is the current year.
 */
void ASimpleCarbonCalc::calcSigmoidCurve( double carbonDifference, const unsigned int aYear, const bool aIsCurrentYear )
{
    assert( mMatureAge > 0 );
    
    const int endYear = CarbonModelUtils::getEndYear();
    const int startYear = CarbonModelUtils::getStartYear();
    const double timeScale = static_cast<double>( endYear - startYear ) / mMatureAge;

    double futureCarbon = 0.0;
    double lastCarbonValue = 0.0;
    for( int currYear = aYear; currYear <= endYear; ++currYear ){
        // Calculate the future emissions for the year defined by the sigmoid function:
        // E(t) = a(1-e^(-10t/b))^b    where a is carbonDifference and b is mMatureAge.
        // The sigmoid curve has been precomputed; here we scale by time and carbonDifference
        if ( currYear-aYear < mMatureAge) {
            futureCarbon = carbonDifference * precalc_sigmoid[ static_cast<double>( currYear-aYear+1 ) * timeScale + startYear ];
        }
        else {
            futureCarbon = carbonDifference;
        }
        double futureAnnualEmiss = futureCarbon - lastCarbonValue;
        lastCarbonValue = futureCarbon;
        
        // Only store annual emissions values if this is not a historical
        // emissions calculation. Historical emissions calculations only occur
        // once, unlike current emissions calculations which need to remove the
        // effect of the previous iteration.
        if( aIsCurrentYear ){
            int period = scenario->getModeltime()->getyr_to_per( aYear );
            mCurrentEmissions[ currYear ] += futureAnnualEmiss;
            mStoredEmissions[ period ][ currYear - startYear ] += futureAnnualEmiss;
        }
        
        // Add to the total carbon emission for the year. This will be the sum
        // of the effects of all carbon emissions for the previous years.
        mTotalEmissions[ currYear ] += futureAnnualEmiss;
    }
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
    // Land use emissions are based on actual carbon which varies with yield/land size
    // Actual carbon is calculated in each period. The getActual method linearly
    // interpolates between periods which can cause unexpected land use emissions.
    // (e.g., positive emissions when land mass grows ). To get around this problem,
    // we calculate the change in carbon over a full period and assume that the carbon
    // is released/sequestered evenly throughout that period.
    int currYear = aYear;
    int prevYear = aYear - 1;
    double prevCarbonDensity = mLandUseHistory->getHistoricAboveGroundCarbonDensity();
    double currCarbonDensity = mLandUseHistory->getHistoricAboveGroundCarbonDensity();
    const Modeltime* modeltime = scenario->getModeltime();
    if ( aYear >= modeltime->getStartYear() ) {
        const int currPeriod = modeltime->getyr_to_per( aYear );
        currYear = modeltime->getper_to_yr( currPeriod );
        currCarbonDensity = getActualAboveGroundCarbonDensity( currYear );
    
        if ( aYear >= modeltime->getStartYear() + modeltime->gettimestep( currPeriod ) ) {
            prevYear = currYear - modeltime->gettimestep( currPeriod );
            prevCarbonDensity = getActualAboveGroundCarbonDensity( prevYear );
        }
    }

    double prevCarbon = CarbonModelUtils::getLandUse( prevYear, mLandUseHistory,
                                                        mHistoricalShare, mLandUse )
                        * prevCarbonDensity;

    
    double currCarbon = CarbonModelUtils::getLandUse( currYear, mLandUseHistory,   
                                                        mHistoricalShare, mLandUse )
                        * currCarbonDensity;
    
    // double carbonDifference = ( prevCarbon - currCarbon ) / modeltime->gettimestep( currPeriod );
    double carbonDifference = ( prevCarbon - currCarbon ) / ( currYear - prevYear );

    // If the carbon content is equivalent than there are no emissions to
    // distribute.
    if( util::isEqual( carbonDifference, 0.0 ) ){
        return;
    }
    
    // Carbon sequestration is stretched out in time, based on mMatureAge, because some
    // land cover types (notably forests) don't mature instantly.    
    const int startYear = CarbonModelUtils::getStartYear();
    if ( carbonDifference < 0.0 ){        // sequestration
        calcSigmoidCurve(carbonDifference, aYear, aIsCurrentYear);
    }
    else {        // emission
        mTotalEmissions[ aYear ] += carbonDifference;

        // If this is the current year being calculated store the emission
        // separately so it can be removed in future iterations.
        if( aIsCurrentYear ){
            int period = scenario->getModeltime()->getyr_to_per( aYear );
            mCurrentEmissions[ aYear ] += carbonDifference;
            mStoredEmissions[ period ][ aYear - startYear ] += carbonDifference;
        }
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

    // Below ground carbon is released/sequestered slowly over time.
    // Land use emissions are based on actual carbon which varies with yield/land size
    // Actual carbon is calculated in each period. The getActual method linearly
    // interpolates between periods which can cause unexpected land use emissions.
    // (e.g., positive emissions when land mass grows ). To get around this problem,
    // we calculate the change in carbon over a full period and assume that the carbon
    // is released/sequestered evenly throughout that period.
    int currYear = aYear;
    int prevYear = aYear - 1;
    double prevCarbonDensity = mLandUseHistory->getHistoricBelowGroundCarbonDensity();
    double currCarbonDensity = mLandUseHistory->getHistoricBelowGroundCarbonDensity();
    const Modeltime* modeltime = scenario->getModeltime();
    if ( aYear >= modeltime->getStartYear() ) {
        const int currPeriod = modeltime->getyr_to_per( aYear );
        currYear = modeltime->getper_to_yr( currPeriod );
        currCarbonDensity = getActualBelowGroundCarbonDensity( currYear );
        if ( aYear >= modeltime->getStartYear() + modeltime->gettimestep( currPeriod ) ) {
            prevYear = currYear - modeltime->gettimestep( currPeriod );
            prevCarbonDensity = getActualBelowGroundCarbonDensity( prevYear );
        }
    }

    double soilCarbonPrev = CarbonModelUtils::getLandUse( prevYear, mLandUseHistory,
                                                        mHistoricalShare, mLandUse )
                        * prevCarbonDensity;
    
    double soilCarbonCurr = CarbonModelUtils::getLandUse( currYear, mLandUseHistory,   
                                                        mHistoricalShare, mLandUse )
                        * currCarbonDensity;

    // Calculate the difference in carbon between the previous period and the
    // current period. If this is negative, an uptake has occurred. If this is
    // positive an emissions has occurred.
    // double carbonDifference = ( soilCarbonPrev - soilCarbonCurr ) / modeltime->gettimestep( currPeriod );
    double carbonDifference = ( soilCarbonPrev - soilCarbonCurr ) / ( currYear - prevYear );

    // If the carbon content is equivalent than there are no emissions to
    // distribute.
    if( util::isEqual( carbonDifference, 0.0 ) ){
        return;
    }
 
    // Set emissions (or, if negative, uptake) from now until the end of the model.
    const int endYear = CarbonModelUtils::getEndYear();
    const int startYear = CarbonModelUtils::getStartYear();
    const double emissionRate = 100.0 / CarbonModelUtils::getSoilTimeScale() / 100.0;
    for( int currYear = aYear; currYear <= endYear; ++currYear ){
        double futureAnnualEmiss = carbonDifference * emissionRate;
        carbonDifference -= futureAnnualEmiss;
        // Only store annual emissions values if this is not a historical
        // emissions calculation. Historical emissions calculations only occur
        // once, unlike current emissions calculations which need to remove the
        // effect of the previous iteration.
        if( aIsCurrentYear ){
            int aPeriod = modeltime->getyr_to_per( aYear );
            mCurrentEmissions[ currYear ] += futureAnnualEmiss;            
            mStoredEmissions[ aPeriod ][ currYear - startYear ] += futureAnnualEmiss;
        }

        // Add to the total carbon emission for the year. This will be the sum
        // of the effects of all carbon emissions for the previous years.
        mTotalEmissions[ currYear ] += futureAnnualEmiss;
    } // for

}


/*!
* \brief Returns a discount factor for the carbon subsidy for soil carbon.
* \details This method approximates a discount factor to adjust the carbon subsidy
*          to reflect the slow uptake of soil carbons and forest vegetation
*          growth. The carbon subsidy is based on a constant carbon tax; thus,
*          carbon uptake in the future should be valued less than carbon uptake
*          in the initial period
* \return soil carbon subsidy discount factor
*/
double ASimpleCarbonCalc::getBelowGroundCarbonSubsidyDiscountFactor( ){
    // If carbon uptake occurs in the first year, we do not discount it.
    if ( mSoilTimeScale == 1 ) {
        return 1.0;
    }

    // We are approximating this curve as alpha / (SoilTimeScale - beta)
    // Alpha and beta are chosen by minimizing least squared error 
    // between actual carbon subsidy discount and functional estimate
    const double ALPHA = 20.47;
    const double BETA = -19.46;
    return ALPHA / ( mSoilTimeScale - BETA );
}

/*!
* \brief Returns a discount factor for the carbon subsidy for vegetation carbon.
* \details This method approximates a discount factor to adjust the carbon subsidy
*          to reflect the slow uptake of soil carbons and forest vegetation
*          growth. The carbon subsidy is based on a constant carbon tax; thus,
*          carbon uptake in the future should be valued less than carbon uptake
*          in the initial period
* \return above ground carbon subsidy discount factor
*/
double ASimpleCarbonCalc::getAboveGroundCarbonSubsidyDiscountFactor( ){
    // If carbon uptake occurs in the first year, we do not discount it.
    if ( mMatureAge == 1 ) {
        return 1.0;
    }

    // We are approximating this curve as a quadratic with an offset of
    // 250 (If the mature age is 250, all carbon uptake occurs far enough 
    // in the future that you wouldn't base decisions on it. So, for a 
    // mature age of 250 the multiplier is zero
    // quadCoef is chosen by minimizing least squared error 
    // between actual carbon subsidy discount and functional estimate
    const double QUADCOEF = 2.7e-10;
    const int MAXMATUREAGE = 250; // Mature age where carbon subsidy is zero
    return QUADCOEF * pow( double(mMatureAge - MAXMATUREAGE), 4);
}

void ASimpleCarbonCalc::setSoilTimeScale( const int aTimeScale ) {
    mSoilTimeScale = aTimeScale;
}



