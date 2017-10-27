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
using namespace xercesc;
using namespace objects;

extern Scenario* scenario;

ASimpleCarbonCalc::ASimpleCarbonCalc():
mTotalEmissions( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() ),
mTotalEmissionsAbove( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() ),
mTotalEmissionsBelow( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() ),
mCarbonStock( scenario->getModeltime()->getStartYear(), CarbonModelUtils::getEndYear() )
{
    int endYear = CarbonModelUtils::getEndYear();
    const Modeltime* modeltime = scenario->getModeltime();
    
    mLandUseHistory = 0;
    mLandLeaf = 0;
    mSoilTimeScale = CarbonModelUtils::getSoilTimeScale();
    mHasCalculatedHistoricEmiss = false;

    // Note we are not allocating space for period zero since that is historical
    // and can never be calculated more than once.
    mStoredEmissionsAbove[0] = 0;
    mStoredEmissionsBelow[0] = 0;
    
    for( int period = 1; period < mStoredEmissionsAbove.size(); ++period ){
        int currYear = modeltime->getper_to_yr( period ) - modeltime->gettimestep( period ) + 1;
        mStoredEmissionsAbove[period] = new YearVector<Value>( currYear, endYear );
        mStoredEmissionsBelow[period] = new YearVector<Value>( currYear, endYear );
    }
}

//! Default destructor
ASimpleCarbonCalc::~ASimpleCarbonCalc() {
    for( int period = 0; period < mStoredEmissionsAbove.size(); ++period ){
        // Both of these arrays have the same size.
        delete mStoredEmissionsAbove[period];
        delete mStoredEmissionsBelow[period];
    }
}

void ASimpleCarbonCalc::setLandUseObjects( const LandUseHistory* aHistory, const LandLeaf* aLandLeaf )
{
    mLandUseHistory = aHistory;
    mLandLeaf = aLandLeaf;
}

void ASimpleCarbonCalc::initCalc( const int aPeriod ) {
    if( aPeriod > 0 ) {
        const Modeltime* modeltime = scenario->getModeltime();
        const int prevModelYear = modeltime->getper_to_yr(aPeriod-1);
        int year = prevModelYear + 1;
        YearVector<Value>& currEmissionsAbove = *mStoredEmissionsAbove[ aPeriod ];
        YearVector<Value>& currEmissionsBelow = *mStoredEmissionsBelow[ aPeriod ];
        for( ; year <= CarbonModelUtils::getEndYear(); ++year ) {
            mTotalEmissionsAbove[ year ] -= currEmissionsAbove[ year ];
            mTotalEmissionsBelow[ year ] -= currEmissionsBelow[ year ];
            currEmissionsAbove[ year ] = 0.0;
            currEmissionsBelow[ year ] = 0.0;
        }
    }
}

double ASimpleCarbonCalc::calc( const int aPeriod, const int aEndYear, const bool aStoreFullEmiss ) {
    const Modeltime* modeltime = scenario->getModeltime();
    
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
            const double belowGroundCarbonDensity = mLandUseHistory->getHistoricBelowGroundCarbonDensity();
            
            double currCarbonStock = aboveGroundCarbonDensity * mLandUseHistory->getAllocation( CarbonModelUtils::getStartYear() );
            
            double prevLand = mLandUseHistory->getAllocation( CarbonModelUtils::getStartYear() - 1 );
            for( int year = CarbonModelUtils::getStartYear(); year <= mLandUseHistory->getMaxYear(); ++year ) {
                double currLand = mLandUseHistory->getAllocation( year );
                double landDifference = prevLand - currLand;
                calcAboveGroundCarbonEmission( currCarbonStock, prevLand, currLand, aboveGroundCarbonDensity, year, aEndYear, mTotalEmissionsAbove );
                calcBelowGroundCarbonEmission( landDifference * belowGroundCarbonDensity, year, aEndYear, mTotalEmissionsBelow );
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
        YearVector<Value>& currEmissionsAbove = *mStoredEmissionsAbove[ aPeriod ];
        YearVector<Value>& currEmissionsBelow = *mStoredEmissionsBelow[ aPeriod ];
        
        // clear the previously calculated emissions first
        for( ; year <= aEndYear; ++year ) {
            currEmissionsAbove[ year ] = 0.0;
            currEmissionsBelow[ year ] = 0.0;
        }
        
        year = prevModelYear;
        double currLand = aPeriod == 1 ? mLandUseHistory->getAllocation( prevModelYear ) : mLandLeaf->getLandAllocation( mLandLeaf->getName(), aPeriod - 1 );
        const double avgAnnualChangeInLand = ( mLandLeaf->getLandAllocation( mLandLeaf->getName(), aPeriod ) - currLand )
            / modeltime->gettimestep( aPeriod );
        double prevCarbonBelow = currLand * getActualBelowGroundCarbonDensity( year );
        for( ++year; year <= modelYear; ++year ) {
            double prevLand = currLand;
            currLand += avgAnnualChangeInLand;
            double currCarbonBelow = currLand * getActualBelowGroundCarbonDensity( year);
            calcAboveGroundCarbonEmission( mCarbonStock[ year - 1 ], prevLand, currLand, getActualAboveGroundCarbonDensity( year ), year, aEndYear, currEmissionsAbove );
            calcBelowGroundCarbonEmission( prevCarbonBelow - currCarbonBelow, year, aEndYear, currEmissionsBelow );

            mCarbonStock[ year ] = mCarbonStock[ year - 1 ] - ( mTotalEmissionsAbove[ year ] + currEmissionsAbove[ year ] );
            prevCarbonBelow = currCarbonBelow;
        }
        
        if( aStoreFullEmiss ) {
            // add current emissions to the total
            for( year = prevModelYear + 1; year <= aEndYear; ++year ) {
                mTotalEmissionsAbove[ year ] += currEmissionsAbove[ year ];
                mTotalEmissionsBelow[ year ] += currEmissionsBelow[ year ];
                mTotalEmissions[ year ] = mTotalEmissionsAbove[ year ] + mTotalEmissionsBelow[ year ];
            }
        }
        else {
            // Since the flag to avoid storing the full emissions is set we will just calculate
            // and return the appropriate total emissions.
            return mTotalEmissions[ aEndYear ] + currEmissionsAbove[ aEndYear ] + currEmissionsBelow[ aEndYear ];
        }
    }
    
    return mTotalEmissions[ aEndYear ];
}

double ASimpleCarbonCalc::getNetLandUseChangeEmission( const int aYear ) const {
    return mTotalEmissions[ aYear ];
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
    const double discountrate = 0.05;
    return 1.0 - discountrate / ( discountrate + lambda );
        
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

    // We are approximating this curve as a quadratic with an offset of
    // 250 (If the mature age is 250, all carbon uptake occurs far enough 
    // in the future that you wouldn't base decisions on it. So, for a 
    // mature age of 250 the multiplier is zero
    // quadCoef is chosen by minimizing least squared error 
    // between actual carbon subsidy discount and functional estimate
    // Note: these parameters assume a discount rate of 0.05
    /* const double QUADCOEF = 2.7e-10;
    const int MAXMATUREAGE = 250; // Mature age where carbon subsidy is zero
    return QUADCOEF * pow( double(getMatureAge() - MAXMATUREAGE), 4); */
    
    // We are approximating this curve as a quadratic with an offset of
    // 250 (If the mature age is 250, all carbon uptake occurs far enough 
    // in the future that you wouldn't base decisions on it. So, for a 
    // mature age of 250 the multiplier is zero
    // quadCoef is chosen by minimizing least squared error 
    // between actual carbon subsidy discount and functional estimate
    // Note: these parameters assume a discount rate of 0.025
    const double QUADCOEF = 1.53e-05;
    const int MAXMATUREAGE = 250; // Mature age where carbon subsidy is zero
    return QUADCOEF * pow( double(getMatureAge() - MAXMATUREAGE), 2);
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
