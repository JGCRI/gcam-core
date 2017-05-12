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
 * \file manage_state_variables.cpp
 * \ingroup util
 * \brief ManageStateVariables class source file.
 * \author Pralit Patel
 */

#include <cstring>

#include "util/base/include/manage_state_variables.hpp"
#include "util/base/include/value.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/gcam_fusion.hpp"
#include "util/base/include/gcam_data_containers.h"

using namespace std;

extern Scenario* scenario;

double** Value::mAltValue = 0;
bool* Value::mIsPartialDeriv = 0;

ManageStateVariables::ManageStateVariables( const int aPeriod ):
mStateData( new double*[2] ),
mPeriodToCollect( aPeriod ),
mYearToCollect( scenario->getModeltime()->getper_to_yr( aPeriod ) ),
mNumCollected( 0 )
{
    Value::mAltValue = mStateData;
    Value::mIsPartialDeriv = &scenario->getMarketplace()->mIsDerivativeCalc;
    collectState();
}

ManageStateVariables::~ManageStateVariables() {
    resetState();
    for( size_t stateInd = 0; stateInd < 2; ++stateInd ) {
        delete[] mStateData[ stateInd ];
    }
    delete[] mStateData;
    Value::mAltValue = 0;
}

void ManageStateVariables::copyState() {
    memcpy( mStateData[1], mStateData[0], (sizeof( double)) * mNumCollected );
}

void ManageStateVariables::collectState() {
    DoCollect doCollectProc;
    mNumCollected = 0;
    doCollectProc.mParentClass = this;
    doCollectProc.mIsCollect = true;
    doCollectProc.mMemIsAllocated = false;
    vector<FilterStep*> collectStateSteps( 2, 0 );
    collectStateSteps[ 0 ] = new FilterStep( "" );
    collectStateSteps[ 1 ] = new FilterStep( "", DataFlags::STATE );
    cout << "Num steps: " << collectStateSteps.size() << endl;
    for( auto filterStep : collectStateSteps ) {
        cout << "Name: " << filterStep->mDataName << ", NoFilter? " << filterStep->isDescendantStep() << endl;
    }
    GCAMFusion<DoCollect, true, true, true> gatherState( doCollectProc, collectStateSteps );
    gatherState.startFilter( scenario );
    cout << "Collected: " << mNumCollected << endl;
    for( size_t stateInd = 0; stateInd < 2; ++stateInd ) {
        mStateData[ stateInd ] = new double[ mNumCollected ];
    }
    cout << "Mem allocated" << endl;
    mNumCollected = 0;
    doCollectProc.mMemIsAllocated = true;
    gatherState.startFilter( scenario );
    
    for( auto filterStep : collectStateSteps ) {
        delete filterStep;
    }
}

void ManageStateVariables::resetState() {
    DoCollect doCollectProc;
    mNumCollected = 0;
    doCollectProc.mParentClass = this;
    doCollectProc.mIsCollect = false;
    doCollectProc.mMemIsAllocated = true;
    vector<FilterStep*> collectStateSteps( 2, 0 );
    collectStateSteps[ 0 ] = new FilterStep( "" );
    collectStateSteps[ 1 ] = new FilterStep( "", DataFlags::STATE );
    /*cout << "Num steps: " << collectStateSteps.size() << endl;
     for( auto filterStep : collectStateSteps ) {
     cout << "Name: " << filterStep->mDataName << ", NoFilter? " << filterStep->isDescendantStep() << endl;
     }*/
    GCAMFusion<DoCollect, true, true, true> gatherState( doCollectProc, collectStateSteps );
    gatherState.startFilter( scenario );
    for( auto filterStep : collectStateSteps ) {
        delete filterStep;
    }
}

/*void Value::doCheck() const {
    const bool isPartialDeriv = scenario->getMarketplace()->mIsDerivativeCalc;
    if( !mIsStateCopy && isPartialDeriv ) {
        cout << "Missed one" << endl;
        abort();
    }
}*/

template<typename DataType>
void ManageStateVariables::DoCollect::processData( DataType& aData ) {
    cout << "Found an unexpected state var type: " << typeid( aData ).name() << endl;
}

template<>
void ManageStateVariables::DoCollect::processData<Value>( Value& aData ) {
    if( !mIgnoreCurrValue ) {
        aData.mIsStateCopy = mIsCollect;
        //aData.mIsPartialDeriv = &scenario->getMarketplace()->mIsDerivativeCalc;
        if( mIsCollect ) {
            //aData.mAltValue = mParentClass->mStateData;
            aData.mAltValueIndex = mParentClass->mNumCollected;
            if( mMemIsAllocated ) {
                aData.mAltValue[0][aData.mAltValueIndex] = aData.mValue;
                aData.mAltValue[1][aData.mAltValueIndex] = aData.mValue;
            }
        }
        else {
            if( aData.mAltValueIndex != mParentClass->mNumCollected ) {
                cout << "Reset didn't match " << aData.mAltValueIndex << " != " << mParentClass->mNumCollected << endl;
                abort();
            }
            aData.mValue = aData.mAltValue[0][aData.mAltValueIndex];
        }
        ++mParentClass->mNumCollected;
    }
}

template<>
void ManageStateVariables::DoCollect::processData<objects::PeriodVector<Value> >( objects::PeriodVector<Value >& aData ) {
    if( !mIgnoreCurrValue ) {
        aData[mParentClass->mPeriodToCollect].mIsStateCopy = mIsCollect;
        //aData[mParentClass->mPeriodToCollect].mIsPartialDeriv = &scenario->getMarketplace()->mIsDerivativeCalc;
        if( mIsCollect ) {
            //aData[mParentClass->mPeriodToCollect].mAltValue = mParentClass->mStateData;
            aData[mParentClass->mPeriodToCollect].mAltValueIndex = mParentClass->mNumCollected;
            if( mMemIsAllocated ) {
                aData[mParentClass->mPeriodToCollect].mAltValue[0][aData[mParentClass->mPeriodToCollect].mAltValueIndex] = aData[mParentClass->mPeriodToCollect].mValue;
                aData[mParentClass->mPeriodToCollect].mAltValue[1][aData[mParentClass->mPeriodToCollect].mAltValueIndex] = aData[mParentClass->mPeriodToCollect].mValue;
            }
        }
        else {
            if( aData[mParentClass->mPeriodToCollect].mAltValueIndex != mParentClass->mNumCollected ) {
                cout << "Reset didn't match " << aData[mParentClass->mPeriodToCollect].mAltValueIndex << " != " << mParentClass->mNumCollected << endl;
                abort();
            }
            aData[mParentClass->mPeriodToCollect].mValue = aData[mParentClass->mPeriodToCollect].mAltValue[0][aData[mParentClass->mPeriodToCollect].mAltValueIndex];
        }
        ++mParentClass->mNumCollected;
    }
}

template<typename DataType>
void ManageStateVariables::DoCollect::pushFilterStep( const DataType& aData ) {
    // ignore most steps
}

template<typename DataType>
void ManageStateVariables::DoCollect::popFilterStep( const DataType& aData ) {
    // ignore most steps
}

template<>
void ManageStateVariables::DoCollect::pushFilterStep<ITechnology*>( ITechnology* const& aData ) {
    if( !aData->isOperating( mParentClass->mPeriodToCollect ) ) {
        mIgnoreCurrValue = true;
    }
}

template<>
void ManageStateVariables::DoCollect::popFilterStep<ITechnology*>( ITechnology* const& aData ) {
    mIgnoreCurrValue = false;
}

template<>
void ManageStateVariables::DoCollect::pushFilterStep<Market*>( Market* const& aData ) {
    if( aData->getYear() != mParentClass->mYearToCollect ) {
        mIgnoreCurrValue = true;
    }
}

template<>
void ManageStateVariables::DoCollect::popFilterStep<Market*>( Market* const& aData ) {
    mIgnoreCurrValue = false;
}

