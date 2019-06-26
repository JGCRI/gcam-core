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
 * \file asimple_carbon_calc.cpp
 * \ingroup Objects
 * \brief ASimpleCarbonCalc class source file.
 * \author James Blackwood
 */

#include "util/base/include/definitions.h"
#include <cassert>

#include "ccarbon_model/include/asimple_carbon_calc.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include "util/base/include/ivisitor.h"
#include "util/base/include/util.h"
#include "land_allocator/include/land_use_history.h"
#include "land_allocator/include/land_leaf.h"
#include "util/logger/include/ilogger.h"

using namespace std;
using namespace objects;

extern Scenario* scenario;

ASimpleCarbonCalc::ASimpleCarbonCalc():
mTotalEmissions( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() ),
mTotalEmissionsAbove( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() ),
mTotalEmissionsBelow( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() ),
mAboveGroundCarbonDensity( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear()),
mBelowGroundCarbonDensity( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() ),
mCarbonStock( scenario->getModeltime()->getStartYear(), CarbonModelUtils::getEndYear() )
{
    mLandUseHistory = 0;
    mLandLeaf = 0;
    mSoilTimeScale = CarbonModelUtils::getSoilTimeScale();
    mHasCalculatedHistoricEmiss = false;
}

//! Default destructor
ASimpleCarbonCalc::~ASimpleCarbonCalc() {
}

void ASimpleCarbonCalc::setLandUseObjects( const LandUseHistory* aHistory, const LandLeaf* aLandLeaf )
{
    mLandUseHistory = aHistory;
    mLandLeaf = aLandLeaf;
}

void ASimpleCarbonCalc::initCalc( const int aPeriod ) {
    if( aPeriod > 0 && mLandLeaf->hasLandAllocationCalculated( aPeriod ) ) {
        calc( aPeriod, CarbonModelUtils::getEndYear(), eReverseCalc );
    }
}

double ASimpleCarbonCalc::calc( const int aPeriod, const int aEndYear, const CarbonCalcMode aCalcMode ) {
    const Modeltime* modeltime = scenario->getModeltime();
    
    // KVC_IESM: First, construct the carbon density vectors, by appending the historic
    //           carbon density to the future carbon density. There may be a better way to
    //           do this, but I'm not seeing it right now.
    for( int year = CarbonModelUtils::getStartYear(); year <= CarbonModelUtils::getEndYear(); ++year ) {
        if ( year < modeltime->getStartYear() ) {
            mAboveGroundCarbonDensity[ year ] = mLandUseHistory->getHistoricAboveGroundCarbonDensity();
            mBelowGroundCarbonDensity[ year ] = mLandUseHistory->getHistoricBelowGroundCarbonDensity();
        }
        else {
            mAboveGroundCarbonDensity[ year ] = getActualAboveGroundCarbonDensity( year );
            mBelowGroundCarbonDensity[ year ] = getActualBelowGroundCarbonDensity( year );
        }
    }
    
    // If this is a land-use history year...
    if( aPeriod == 0 ) {
        /*!
         * \warning Land-use history emissions can only be calculated once regardless
         *          of how many times the model will be run.
         */
        if( !mHasCalculatedHistoricEmiss && aEndYear == CarbonModelUtils::getEndYear() ) {
            // This code requires our land use history to be accurate.
            // AboveGroundCarbon is overwritten in these years
            // BelowGroundCarbon affects future model periods that are not overwritten
            const double aboveGroundCarbonDensity = mLandUseHistory->getHistoricAboveGroundCarbonDensity();

            double currCarbonStock = aboveGroundCarbonDensity * mLandUseHistory->getAllocation( CarbonModelUtils::getStartYear() );
            
            double prevLand = mLandUseHistory->getAllocation( CarbonModelUtils::getStartYear() - 1 );
            for( int year = CarbonModelUtils::getStartYear(); year <= mLandUseHistory->getMaxYear(); ++year ) {
                double currLand = mLandUseHistory->getAllocation( year );
                double landDifference = prevLand - currLand;
                calcAboveGroundCarbonEmission( currCarbonStock, prevLand, currLand, mAboveGroundCarbonDensity, year, aEndYear, mTotalEmissionsAbove );
                calcBelowGroundCarbonEmission( landDifference, mBelowGroundCarbonDensity, year, aEndYear, mTotalEmissionsBelow );
                prevLand = currLand;
                currCarbonStock -= mTotalEmissionsAbove[ year ];
                mTotalEmissions[year] = mTotalEmissionsAbove[year] + mTotalEmissionsBelow[year];
            }
            mHasCalculatedHistoricEmiss = true;
            mCarbonStock[ modeltime->getStartYear() ] = currCarbonStock;
        }
    }
    else {
        // using model calculated allocations
        const int modelYear = modeltime->getper_to_yr(aPeriod);
        const int prevModelYear = modeltime->getper_to_yr(aPeriod-1);
        int year = prevModelYear + 1;
        YearVector<double> currEmissionsAbove( year, aEndYear, 0.0 );
        YearVector<double> currEmissionsBelow( year, aEndYear, 0.0 );
        
        year = prevModelYear;
        double currLand = aPeriod == 1 ? mLandUseHistory->getAllocation( prevModelYear ) :
            // we need to be careful about accessing the land allocation from a previous timestep
            // when we are intending to calculate in eReverseCalc as the previous timestep may have
            // already calculated in eStoreResults
            aCalcMode != eReverseCalc ? mLandLeaf->getLandAllocation( mLandLeaf->getName(), aPeriod - 1 ) : mSavedLandAllocation[ aPeriod - 1];
        const double avgAnnualChangeInLand = ( mLandLeaf->getLandAllocation( mLandLeaf->getName(), aPeriod ) - currLand )
            / modeltime->gettimestep( aPeriod );

        for( ++year; year <= modelYear; ++year ) {
            double prevLand = currLand;
            currLand += avgAnnualChangeInLand;
            // we need to be careful about accessing the carbon stock from a previous timestep
            // when we are intending to calculate in eReverseCalc as the previous timestep may have
            // already calculated in eStoreResults
            calcAboveGroundCarbonEmission( aCalcMode == eReverseCalc && (year - 1) == prevModelYear ?
                                          mSavedCarbonStock[ aPeriod - 1 ] :
                                          mCarbonStock[ year - 1 ], prevLand, currLand, mAboveGroundCarbonDensity, year, aEndYear, currEmissionsAbove );
            calcBelowGroundCarbonEmission( prevLand - currLand, mBelowGroundCarbonDensity, year, aEndYear, currEmissionsBelow );

            if( aCalcMode != eReverseCalc ) {
                mCarbonStock[ year ] = mCarbonStock[ year - 1 ] - ( mTotalEmissionsAbove[ year ] + currEmissionsAbove[ year ] );
            }
        }
        
        if( aCalcMode == eStoreResults ) {
            // add current emissions to the total
            for( year = prevModelYear + 1; year <= aEndYear; ++year ) {
                mTotalEmissionsAbove[ year ] += currEmissionsAbove[ year ];
                mTotalEmissionsBelow[ year ] += currEmissionsBelow[ year ];
                mTotalEmissions[ year ] = mTotalEmissionsAbove[ year ] + mTotalEmissionsBelow[ year ];
            }
            mSavedCarbonStock[ aPeriod - 1 ] = mCarbonStock[ prevModelYear ];
            mSavedLandAllocation[ aPeriod - 1 ] = mLandLeaf->getLandAllocation( mLandLeaf->getName(), aPeriod - 1 );
        }
        else if( aCalcMode == eReverseCalc ) {
            // back out the current emissions from the total
            for( year = prevModelYear + 1; year <= aEndYear; ++year ) {
                mTotalEmissionsAbove[ year ] -= currEmissionsAbove[ year ];
                mTotalEmissionsBelow[ year ] -= currEmissionsBelow[ year ];
                mTotalEmissions[ year ] = mTotalEmissionsAbove[ year ] + mTotalEmissionsBelow[ year ];
            }
        }
        else if( aCalcMode == eReturnTotal ) {
            // Since the flag to avoid storing the full emissions is set we will just calculate
            // and return the appropriate total emissions.
            return mTotalEmissions[ aEndYear ] + currEmissionsAbove[ aEndYear ] + currEmissionsBelow[ aEndYear ];
        }
    }
    
    return mTotalEmissions[ aEndYear ];
}

/*!
 * \brief Calculate the emission from above ground carbon for a given year.
 * \details Above ground carbon is emitted as a pulse, and will uptake over mature age.
 * \param aPrevCarbonStock The carbon stock from the previous year.
 * \param aPrevLandArea The land area during the previous year.
 * \param aCurrLandArea The land area which will expand/contract to.
 * \param aPrevCarbonDensity The potential carbon density for the previous year.
 * \param aYear The year to start the calculation.
 * \param aEndYear The last future year to calculate to.
 * \param aEmissVector A vector to accumulate emissions into.
 */
//template<typename DoubleType>
void ASimpleCarbonCalc::calcAboveGroundCarbonEmission( const double aPrevCarbonStock,
                                                       const double aPrevLandArea,
                                                       const double aCurrLandArea,
                                                       objects::YearVector<double>& aCarbonDensity,
                                                       const int aYear,
                                                       const int aEndYear,
                                                       YearVector<double>& aEmissVector)
{
    double landDiff = aPrevLandArea  - aCurrLandArea;
    // If no land change occurred, then exit.
    if( util::isEqual( landDiff, 0.0 ) ) {
        return;
    }
    
    // Finally, calculate net land use change emissions from changes in
    // above ground carbon.
    if ( getMatureAge() > 1 && landDiff < 0.0 ) {
        // If carbon content increases, then carbon was sequestered.
        // Carbon sequestration is stretched out in time, based on mMatureAge, because some
        // land cover types (notably forests) don't mature instantly.
        calcSigmoidCurve( landDiff, aCarbonDensity, aYear, aEndYear, aEmissVector, 0.0 );
    }
    else if( util::isEqual( aPrevLandArea, 0.0 ) ) {
        // If this land category didn't exist before, and now it does,
        // then the calculation below will generate a NaN.  Avoid that
        // by taking the appropriate limit here.
        aEmissVector[ aYear ] -= aCurrLandArea * aCarbonDensity[ aYear ];
    }
    else {
        // If carbon content decreases, then emissions have occurred.
        // Compute the carbon emission as the carbon stock pro rata to
        // the fraction of land converted.
        
        // If the mature age is just one year then sequestration
        // (negative emission) can just be added here as well (so we
        // don't have a separate branch for it).  (It's not obvious,
        // but you can show that the formula below just reduces to the
        // expression for carbonDiff at the top of the function.)
        aEmissVector[ aYear ] += ( aPrevCarbonStock / aPrevLandArea ) * ( aPrevLandArea - aCurrLandArea );
        if( getMatureAge() > 1 ) {
            // Back out the pending future sequestration for the land
            // that has been converted (i.e., that sequestration will
            // no longer happen).  This calculation is necessarily
            // approximate because we don't know how long the
            // destroyed vegetation has been growing.  We do know that
            // when the vegetation is fully mature,
            // carbonStock/LandArea == carbonDensity, so the
            // difference between those two quantities tells us how
            // much pending sequestration we have.  Distribute the
            // correction as a sigmoid between aYear and aEndYear.
            calcSigmoidCurve( landDiff, aCarbonDensity, aYear, aEndYear, aEmissVector, ( aPrevCarbonStock / aPrevLandArea ) );
        }
    }
}

/*!
 * \brief Calculate the emission from below ground carbon for the given year.
 * \details Below ground, or soil carbon, is not emitted as a pulse but at a
 *          exponential rate with a half-life computed from the soil time scale.
 * \param aLandDiff Difference in land area
 * \param aCarbonDensity Carbon density vector
 * \param aYear Year.
 * \param aEndYear The last future year to calculate to.
 * \param aEmissVector A vector to accumulate emissions into.
 */
//template<typename DoubleType>
void ASimpleCarbonCalc::calcBelowGroundCarbonEmission( const double aLandDiff,
                                                       YearVector<double>& aCarbonDensity,
                                                       const int aYear,
                                                       const int aEndYear,
                                                       YearVector<double>& aEmissVector )
{
    // If no land use change, then exit without calculating emissions.
    // KVC_IESM: This means we aren't capturing land use emissions (just LUC) -- that is, if
    //           carbon density changes but land area doesn't, we won't have emissions.
    if( util::isEqual( aLandDiff, 0.0 ) ){
        return;
    }
    
    // Exponential Soil carbon accumulation and decay, with half-life assumed to be
    // the soil time scale divided by ten.  At the half-life, half of the change will
    // have occured, at twice the half-life 75% would have occurred, etc.
    // Note also that the aLandDiff is passed here as previous land minus current land
    // so a positive difference means that emissions will occur and a negative means uptake.
    
    const double halfLife = mSoilTimeScale / 10.0;
    const double log2 = log( 2.0 );
    const double lambda = log2 / halfLife;
    int yearCounter = 0;
    double cumStockDiff_t1, cumStockDiff_t2;
    cumStockDiff_t1 = 0.0;
    for( int currYear = aYear; currYear <= aEndYear; ++currYear ) {
        yearCounter += 1;
        cumStockDiff_t2 = aLandDiff * aCarbonDensity[ currYear ] * ( 1.0 - exp( -1.0 * lambda * yearCounter ) );
        aEmissVector[ currYear ] += cumStockDiff_t2 - cumStockDiff_t1;
        cumStockDiff_t1 = cumStockDiff_t2;
    }
}

/*!
 * \brief    Calculate the sigmoidal sequestration curve.
 * \details  Called by calcAboveGroundCarbonEmission.
 * \param    carbonDifference Annual change in carbon for aYear
 * \param    aYear Year.
 * \param    aEndYear The last future year to calculate to.
 * \param    aEmissVector A vector to accumulate emissions into.
 */
//template<typename DoubleType>
void ASimpleCarbonCalc::calcSigmoidCurve( const double aLandDiff,
                                          objects::YearVector<double>& aCarbonDensity,
                                          const int aYear,
                                          const int aEndYear,
                                          YearVector<double>& aEmissVector,
                                          const double aCarbonAdjust )
{
    /*!
     * \pre This calculation will not be correct for a mature age of a single
     *      year.
     */
    assert( getMatureAge() > 1 );
    
    for( int currYear = aYear; currYear <= aEndYear; ++currYear ){
        // To avoid expensive calculations the difference in the sigmoid curve
        // has already been precomputed.
        aEmissVector[ currYear ] += precalc_sigmoid_diff.get()[ currYear - aYear ] * aLandDiff * ( aCarbonDensity[ currYear ] - aCarbonAdjust );
    }
}

double ASimpleCarbonCalc::getNetLandUseChangeEmission( const int aYear ) const {
    return mTotalEmissions[ aYear ];
}

double ASimpleCarbonCalc::getNetLandUseChangeEmissionAbove( const int aYear ) const {
    return mTotalEmissionsAbove[ aYear ];
}

double ASimpleCarbonCalc::getNetLandUseChangeEmissionBelow( const int aYear ) const {
    return mTotalEmissionsBelow[ aYear ];
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

    // Exponential soil carbon with a fixed discount rate set here/
    const double halfLife = mSoilTimeScale / 10.0;
    const double log2 = log( 2.0 );
    const double lambda = log2 / halfLife;
    return 1.0 - mPrivateDiscountRate / ( mPrivateDiscountRate + lambda );
        
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
    if ( getMatureAge() == 1 ) {
        return 1.0;
    }
    // We are approximating this curve as a polynomial with an offset of
    // 250 (If the mature age is 250, all carbon uptake occurs far enough
    // in the future that you wouldn't base decisions on it. So, for a
    // mature age of 250 the multiplier is zero
    // quadCoef is chosen by minimizing least squared error
    // between actual carbon subsidy discount and functional estimate
    // Note: This function was estimated offline assuming a discount rate of 0.1.
    // This discount rate is a private investors discount rate since it
    // applies to future profit streams of a land owner. This may differ from
    // the read in social discount rate used elsewhere in the land model.
    const double COEF = -8.57e-13;
    const int MAXMATUREAGE = 250; // Mature age where carbon subsidy is zero
    return COEF * pow( double( getMatureAge() - MAXMATUREAGE ), 5);
}

void ASimpleCarbonCalc::setSoilTimeScale( const int aTimeScale ) {
    mSoilTimeScale = aTimeScale;
}

double ASimpleCarbonCalc::getAboveGroundCarbonStock( const int aYear ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    return aYear >= modeltime->getStartYear() ? mCarbonStock[ aYear ] : 0;
}

double ASimpleCarbonCalc::getBelowGroundCarbonStock( const int aYear ) const {
    // TODO: decide what to do for carbon stock
    return 0;//mBelowGroundCarbonStock[ aYear ];
}
