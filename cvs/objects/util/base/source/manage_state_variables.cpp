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
#include "util/base/include/gcam_fusion.hpp"
#include "util/base/include/gcam_data_containers.h"

#if GCAM_PARALLEL_ENABLED
#include <tbb/concurrent_queue.h>
#endif

using namespace std;

extern Scenario* scenario;

Value::AltValueType Value::mAltValue( (double*)0 );
double* Value::mGoodValue( 0 );

#if GCAM_PARALLEL_ENABLED
struct AssignThreadStateFun {
    double** mArr;
    const int mMaxStates;
    tbb::concurrent_queue<int> mThreadStateIndex;
    AssignThreadStateFun( double** aArr, const int aMaxStates ):mArr( aArr ), mMaxStates( aMaxStates ) {
        for( int i = 1; i < mMaxStates; ++i ) {
            mThreadStateIndex.push( i );
        }
    }
    double* operator()() {
        int nextState;
        bool gotState = mThreadStateIndex.try_pop( nextState );
        if( !gotState ) {
            std::cout << "Failed to get state!!" << std::endl;
            abort();
        }
        
        return mArr[ nextState ];
    }
};
#endif


ManageStateVariables::ManageStateVariables( const int aPeriod ):
#if !GCAM_PARALLEL_ENABLED
mStateData( new double*[2] ),
#else
mThreadPool(),
mStateData( new double*[mThreadPool.max_concurrency()+1] ),
#endif
mPeriodToCollect( aPeriod ),
mYearToCollect( scenario->getModeltime()->getper_to_yr( aPeriod ) ),
mCCStartYear( mYearToCollect - scenario->getModeltime()->gettimestep( aPeriod ) + 1 ),
mNumCollected( 0 )
{
    collectState();
}

ManageStateVariables::~ManageStateVariables() {
    resetState();
#if !GCAM_PARALLEL_ENABLED
    const int maxStates = 2;
#else
    const int maxStates = mThreadPool.max_concurrency()+1;
#endif
    for( size_t stateInd = 0; stateInd < maxStates; ++stateInd ) {
        delete[] mStateData[ stateInd ];
    }
    delete[] mStateData;
#if !GCAM_PARALLEL_ENABLED
    Value::mAltValue = 0;
#else
    Value::mAltValue.clear();
#endif
    Value::mGoodValue = 0;
}

void ManageStateVariables::copyState() {
#if !GCAM_PARALLEL_ENABLED
    memcpy( mStateData[1], mStateData[0], (sizeof( double)) * mNumCollected );
#else
    memcpy( Value::mAltValue.local(), mStateData[0], (sizeof( double)) * mNumCollected );
#endif
}

void ManageStateVariables::collectState() {
    DoCollect doCollectProc;
    mNumCollected = 0;
    doCollectProc.mParentClass = this;
    vector<FilterStep*> collectStateSteps( 2, 0 );
    collectStateSteps[ 0 ] = new FilterStep( "" );
    collectStateSteps[ 1 ] = new FilterStep( "", DataFlags::STATE );
    GCAMFusion<DoCollect, true, true, true> gatherState( doCollectProc, collectStateSteps );
    gatherState.startFilter( scenario );
    cout << "Collected: " << mNumCollected << endl;
#if !GCAM_PARALLEL_ENABLED
    const int maxStates = 2;
#else
    const int maxStates = mThreadPool.max_concurrency()+1;
#endif
    for( size_t stateInd = 0; stateInd < maxStates; ++stateInd ) {
        mStateData[ stateInd ] = new double[ mNumCollected ];
    }
    setPartialDeriv( false );
    Value::mGoodValue = mStateData[0];
    cout << "Mem allocated" << endl;
    mNumCollected = 0;
    for( auto currValue : mStateValues ) {
        currValue->mIsStateCopy = true;
        currValue->mAltValueIndex = mNumCollected;
        currValue->mGoodValue[ mNumCollected ] = currValue->mValue;
        ++mNumCollected;
    }
    
    for( auto filterStep : collectStateSteps ) {
        delete filterStep;
    }
}

void ManageStateVariables::resetState() {
    //unsigned int count = 0;
    for( auto currValue : mStateValues ) {
        currValue->mIsStateCopy = false;
        /*if( currValue->mAltValueIndex != count ) {
            cout << "Reset didn't match " << currValue->mAltValueIndex << " != " << count << endl;
            abort();
        }*/
        currValue->mValue = currValue->mGoodValue[ currValue->mAltValueIndex ];
    }
}

void ManageStateVariables::setPartialDeriv( const bool aIsPartialDeriv ) {
#if !GCAM_PARALLEL_ENABLED
    Value::mAltValue = mStateData[ aIsPartialDeriv ? 1 : 0 ];
#else
    if( !aIsPartialDeriv ) {
        Value::mAltValue = Value::AltValueType( mStateData[0] );
    }
    else {
        Value::mAltValue = Value::AltValueType( AssignThreadStateFun( mStateData, mThreadPool.max_concurrency()+1 ) );
    }
#endif
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
        mParentClass->mStateValues.push_front( &aData );
        ++mParentClass->mNumCollected;
    }
}

template<>
void ManageStateVariables::DoCollect::processData<objects::PeriodVector<Value> >( objects::PeriodVector<Value>& aData ) {
    if( !mIgnoreCurrValue ) {
        mParentClass->mStateValues.push_front( &aData[ mParentClass->mPeriodToCollect ] );
        ++mParentClass->mNumCollected;
    }
}

template<>
void ManageStateVariables::DoCollect::processData<objects::PeriodVector<objects::YearVector<Value>*> >( objects::PeriodVector<objects::YearVector<Value>*>& aData ) {
    if( !mIgnoreCurrValue && mParentClass->mPeriodToCollect > 0 ) {
        objects::YearVector<Value>& currEmiss = *aData[ mParentClass->mPeriodToCollect ];
        for( int year = mParentClass->mCCStartYear; year <= mParentClass->mYearToCollect; ++year ) {
            mParentClass->mStateValues.push_front( &currEmiss[ year ] );
            ++mParentClass->mNumCollected;
        }
    }
}

template<>
void ManageStateVariables::DoCollect::processData<objects::YearVector<Value> >( objects::YearVector<Value>& aData ) {
    if( !mIgnoreCurrValue ) {
        for( int year = std::max( mParentClass->mCCStartYear, aData.getStartYear() ); year <= mParentClass->mYearToCollect; ++year ) {
            mParentClass->mStateValues.push_front( &aData[ year ] );
            ++mParentClass->mNumCollected;
        }
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

