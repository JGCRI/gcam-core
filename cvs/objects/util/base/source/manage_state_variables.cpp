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
#include <fstream>

#include "util/base/include/manage_state_variables.hpp"
#include "util/base/include/value.h"
#include "containers/include/scenario.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/configuration.h"
#include "util/base/include/gcam_fusion.hpp"
#include "util/base/include/gcam_data_containers.h"

#if GCAM_PARALLEL_ENABLED
#include <tbb/concurrent_queue.h>
#include <tbb/global_control.h>
#endif

using namespace std;

extern Scenario* scenario;

// Note we must static initialize static class member variables in a cpp file and
// since Value is header only and these particular fields are just as related to
// ManageStateVariables it seems appropriate to initialize them to NULL here.
Value::CentralValueType Value::sCentralValue( (double*)0 );
double* Value::sBaseCentralValue( 0 );

#if GCAM_PARALLEL_ENABLED
#define NUM_STATES tbb::global_control::active_value(tbb::global_control::max_allowed_parallelism)+1
#else
#define NUM_STATES 2
#endif

#if GCAM_PARALLEL_ENABLED
/*!
 * \brief A helper functor to assign a state slot in ManageStateVariables::mStateData
 *        to each worker thread in ManageStateVariables::mThreadPool.  This functor
 *        will get called the first time a new thread accesses the thread local
 *        storage Value::sCentralValue that provides access to mStateData by thread
 *        from with in the Value class.
 */
struct AssignThreadStateFun {
    //! A reference to ManageStateVariables::mStateData.
    double** mArr;
    
    //! The maximum number of states that have been allocated in mStateData.
    const int mMaxStates;
    
    //! A thread safe queue that will have as values each index into mStateData
    //! and as each new thread consumes it's next value implies that thread gets
    //! assigned that state slot.
    tbb::concurrent_queue<int> mThreadStateIndex;
    
    //! Constructor
    AssignThreadStateFun( double** aArr, const int aMaxStates ):mArr( aArr ), mMaxStates( aMaxStates ) {
        // initialize the state index slots starting from 1 as 0 is always the
        // "base" state.
        for( int i = 1; i < mMaxStates; ++i ) {
            mThreadStateIndex.push( i );
        }
    }
    
    /*!
     * \brief The functor that gets called when a new thread accesses the thread local
     *        storage Value::sCentralValue for the first time.  It will assign a unique
     *        slot into ManageStateVariables::sCentralValue for this thread to use
     *        for the duration of it's calculations.
     * \return The unique slot of state that this thread can be guaranteed to use
     *         free from interference from any other thread.
     */
    double* operator()() {
        int nextState;
        bool gotState = mThreadStateIndex.try_pop( nextState );
        if( !gotState ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::SEVERE );
            mainLog << "Failed to get an unused state to assign to a worker thread." << endl;
            abort();
        }
        
        return mArr[ nextState ];
    }
};
#endif

/*!
 * \brief Constructor which calls collectState() to begin the process to find all
 *        state data during the given model period and allocate memory to hold
 *        it in a "base" and "scratch" spaces.
 * \param aPeriod The model period to manage state in.
 */
ManageStateVariables::ManageStateVariables( const int aPeriod ):
#if !GCAM_PARALLEL_ENABLED
mStateData( new double*[ NUM_STATES ] ),
#else
mThreadPool(),
mStateData( new double*[ NUM_STATES ] ),
#endif
mPeriodToCollect( aPeriod ),
mYearToCollect( scenario->getModeltime()->getper_to_yr( aPeriod ) ),
mCCStartYear( mYearToCollect - scenario->getModeltime()->gettimestep( aPeriod ) + 1 ),
mNumCollected( 0 )
{
    collectState();
}

/*!
 * \brief On destruction we need to ensure we have copied back the state in the
 *        "base" state back into the Value objects before we deallocate that memory.
 */
ManageStateVariables::~ManageStateVariables() {
    resetState();
    for( size_t stateInd = 0; stateInd < NUM_STATES; ++stateInd ) {
        delete[] mStateData[ stateInd ];
    }
    delete[] mStateData;
#if !GCAM_PARALLEL_ENABLED
    Value::sCentralValue = 0;
#else
    Value::sCentralValue.clear();
#endif
    Value::sBaseCentralValue = 0;
}

/*!
 * \brief Search for all relevant STATE Values and allocate space for them in the
 *        central state data arrays.  The "base" state will get initialized as the
 *        actual value set in the individual Value objects before being collected.
 */
void ManageStateVariables::collectState() {
    // Set up the GCAM Fusion steps as well as the callback struct that will handle
    // the results from the search.
    DoCollect doCollectProc;
    doCollectProc.mParentClass = this;
    // Note an empty string for the data name indicates match any name.  The first
    // step that does not match any name nor value indicates a "descendant" step
    // allowing for GCAM fusion to search at any depth to find Data of any name
    // but with the STATE flag set.
    vector<FilterStep*> collectStateSteps( 2, 0 );
    collectStateSteps[ 0 ] = new FilterStep( "" );
    collectStateSteps[ 1 ] = new FilterStep( "", DataFlags::STATE );
    // DoCollect will handle all fusion callbacks thus their template boolean parameter
    // are set to true.
    GCAMFusion<DoCollect, true, true, true> gatherState( doCollectProc, collectStateSteps );
    gatherState.startFilter( scenario );
    
    // DoCollect has now gathered all active state into the mStateValues list to
    // allow faster/easier processing for the remaining tasks at hand.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::DEBUG );
    mainLog << "Number of active state values: " << mNumCollected << endl;
    // Allocate space for each active state value for each state slot.
    for( size_t stateInd = 0; stateInd < NUM_STATES; ++stateInd ) {
        mStateData[ stateInd ] = new double[ mNumCollected ];
    }
    
    // We can now initialize the static Value references into mStateData for fast
    // access from within each Value object.
    setPartialDeriv( false );
    Value::sBaseCentralValue = mStateData[0];

    // Take another pass through the Value objects and copy the original data from
    // each one into the corresponding "base" state to initialize it.
    mNumCollected = 0;
    uint64_t currEncodedId = Value::STATE_COPY_MASK;
    for( auto currValue : mStateValues ) {
        double realData = Value::convertToDouble(currValue->mBits);
        currValue->mBits = currEncodedId;
        currValue->sBaseCentralValue[ mNumCollected ] = realData;
        if(mNumCollected == Value::ID_MASK) {
            mainLog.setLevel( ILogger::SEVERE );
            mainLog << "The number of STATE values exceeded reserved ID space in ManageStateVariables." << endl;
            abort();
        }
        ++mNumCollected;
        ++currEncodedId;
    }
    
    // if configured, reset initial state data from a restart file
    // note because the value could be specified via restart-period or restart-year
    // we use the util::getConfigRunPeriod to reconcile the two.
    int newRestartPeriod = util::getConfigRunPeriod( "restart" );
    
    if( newRestartPeriod == Scenario::UNINITIALIZED_RUN_PERIODS ) {
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Could not determine restart period, no restart will be used." << endl;
        newRestartPeriod = -1;
    }
    
    if( newRestartPeriod != -1 && mPeriodToCollect < newRestartPeriod ) {
        loadRestartFile();
    }
    
    // clean up GCAMFusion related memory
    for( auto filterStep : collectStateSteps ) {
        delete filterStep;
    }
}

/*!
 * \brief Copy the "base" state back into each corresponding Value object before
 *        we move on from this model period and release the state memory.
 */
void ManageStateVariables::resetState() {
    if( Configuration::getInstance()->shouldWriteFile( "restart", false, false ) ) {
        saveRestartFile();
    }
    
#if DEBUG_STATE
    uint64_t count = Value::STATE_COPY_MASK;
#endif
    for( auto currValue : mStateValues ) {
#if DEBUG_STATE
        if( currValue->mBits != count ) {
            cout << "Reset didn't match " << currValue->mBits << " != " << count << endl;
            abort();
        }
        ++count;
#endif
        double realValue = currValue->getInternal();
        currValue->mBits = Value::convertToBits(realValue);
    }
}

/*!
 * \brief Copies the "base" state over the "scratch" space.
 * \details This method is typically called before starting a partial derivative
 *          calculation which will make changes in the "scratch" space.  Note when
 *          GCAM_PARALLEL_ENABLED the appropriate "scratch" space to reset is identified
 *          as the one assigned to the calling thread via the thread local Value::sCentralValue.
 */
void ManageStateVariables::copyState() {
#if !GCAM_PARALLEL_ENABLED
    memcpy( mStateData[1], mStateData[0], (sizeof( double)) * mNumCollected );
#else
    memcpy( Value::sCentralValue.local(), mStateData[0], (sizeof( double)) * mNumCollected );
#endif
}

/*!
 * \brief Set up the Value classes static references into mStateData to appropriately
 *        point to the "base" state if aIsPartialDeriv is false or a "scratch"
 *        space if aIsPartialDeriv is true.
 * \param aIsPartialDeriv The flag indicating if we are about to calculate a partial
 *                        derivative or not as set from the solution algorithm.
 */
void ManageStateVariables::setPartialDeriv( const bool aIsPartialDeriv ) {
#if !GCAM_PARALLEL_ENABLED
    Value::sCentralValue = mStateData[ aIsPartialDeriv ? 1 : 0 ];
#else
    if( !aIsPartialDeriv ) {
        // Initialize the thread local storage to always access the "base" state.
        Value::sCentralValue = Value::CentralValueType( mStateData[0] );
    }
    else {
        // Use the AssignThreadStateFun helper functor to uniquely assign a state
        // slot to each worker thread.
        Value::sCentralValue = Value::CentralValueType( AssignThreadStateFun( mStateData, NUM_STATES ) );
    }
#endif
}

/*!
 * \brief Generate the appropriate restart file name to use.
 * \details This method will append the model period this instance was created
 *          with to the base name as set in the Configuration.  It will also also
 *          follow the <Files> convetion, specificially obey append-scenario-name, when
 *          it generates the file name.
 * \return The correct filename to use for restarts
 */
string ManageStateVariables::getRestartFileName() const {
    Configuration* conf = Configuration::getInstance();
    const string fileName = conf->getFile( "restart", "restart/restart" );
    const string scnAppend = conf->shouldAppendScnToFile( "restart" ) ? "." + scenario->getName() : "";
    const string period = util::toString( mPeriodToCollect );
    return fileName + scnAppend + "." + period;
}

/*!
 * \brief Load a restart file from disk directly into the "base" state.
 * \warning Very little error checking is done to ensure the state read in was generated
 *          from the exact same scenario.  All we can do in terms of error checking is
 *          check that the size of the data coming in is exactly the same size as mNumCollected.
 * \sa ManageStateVariables::getRestartFileName
 * \sa ManageStateVariables::saveRestartFile
 */
void ManageStateVariables::loadRestartFile() {
    // read from the appropriate file which is in binary format
    const string restartFileName = getRestartFileName();
    fstream restartFile( restartFileName.c_str(), ios_base::in | ios_base::binary );
    
    if( !restartFile.is_open() ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Could not open restart file: " << restartFileName << " for read." << endl;
        abort();
    }
    
    uint64_t numStatesInRestart;
    restartFile.read( reinterpret_cast<char*>( &numStatesInRestart ), sizeof( uint64_t ) );
    if( numStatesInRestart != mNumCollected ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Restart file: " << restartFileName << " differs in size, read: " << numStatesInRestart
                << ", expected: " << mNumCollected << endl;
        abort();
    }
    
    // read the binary data directly into the "base" state
    restartFile.read( reinterpret_cast<char*>( mStateData[0] ), sizeof( double ) * numStatesInRestart );
    if( !restartFile ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Restart file: " << restartFileName << " has fewer states than expected, read: " << static_cast<size_t>( (restartFile.gcount() - sizeof(size_t)) / sizeof(double))
                << ", expected: " << numStatesInRestart << endl;
        abort();
    }
    if( restartFile.peek() != EOF ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Restart file: " << restartFileName << " has more states than expected: " << numStatesInRestart << endl;
        abort();
    }

    restartFile.close();
}

/*!
 * \brief Dump the contents of the "base" state array into a binary restart file.
 * \details The first value written is mNumCollected (size written: size_t) to help with do some error checking
 *          when we try to read it back in.  Then we just write the entire content of mStateData[0]
 *          (size written: double * mNumCollected).
 * \sa ManageStateVariables::getRestartFileName
 */
void ManageStateVariables::saveRestartFile() {
    const string restartFileName = getRestartFileName();
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::DEBUG );
    mainLog << "Writing restart file: " << restartFileName << "... ";
    fstream restartFile( restartFileName.c_str(), ios_base::out | ios_base::trunc | ios_base::binary );
    
    if( !restartFile.is_open() ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Could not open restart file: " << restartFileName << " for write." << endl;
        abort();
    }
    
    // first write the total number of entries that should be expected to help with error
    // checking on read in
    restartFile.write( reinterpret_cast<char*>( &mNumCollected ), sizeof( uint64_t ) );
    
    // write the entire contents of the "base" state
    restartFile.write( reinterpret_cast<char*>( mStateData[0] ), sizeof( double ) * mNumCollected );
    
    restartFile.close();
    
    mainLog << "Done." << endl;
}

#if DEBUG_STATE
void Value::doStateCheck() const {
    const bool isPartialDeriv = scenario->getMarketplace()->mIsDerivativeCalc;
    if( (mBits & STATE_COPY_MASK) != STATE_COPY_MASK && isPartialDeriv ) {
        cout << "Missed one" << endl;
        // use the debugger call stack from here to identify Values that were not
        // marked as STATE but should have been.
        abort();
    }
}
#endif

template<typename DataType>
void ManageStateVariables::DoCollect::processData( DataType& aData ) {
#if DEBUG_STATE
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::SEVERE );
    mainLog << "Found an unexpected state var type: " << typeid( aData ).name() << endl;
#endif
}

template<>
void ManageStateVariables::DoCollect::processData<Value>( Value& aData ) {
    // Any SINGLE value that is tagged is considered active so long as it is not
    // contained in a retired technology for instance.
    if( !mIgnoreCurrValue ) {
        mParentClass->mStateValues.push_front( &aData );
        ++mParentClass->mNumCollected;
    }
}

template<>
void ManageStateVariables::DoCollect::processData<objects::PeriodVector<Value> >( objects::PeriodVector<Value>& aData ) {
    // When an ARRAY of values are tagged only the Value in [ mPeriodToCollect] is
    // considered active.
    if( !mIgnoreCurrValue ) {
        mParentClass->mStateValues.push_front( &aData[ mParentClass->mPeriodToCollect ] );
        ++mParentClass->mNumCollected;
    }
}

template<>
void ManageStateVariables::DoCollect::processData<objects::TechVintageVector<Value> >( objects::TechVintageVector<Value>& aData ) {
    // When an ARRAY of values are tagged only the Value in [ mPeriodToCollect] is
    // considered active.
    
    // Note, mIgnoreCurrValue should take care of out of bounds here
    if( !mIgnoreCurrValue ) {
        mParentClass->mStateValues.push_front( &aData[ mParentClass->mPeriodToCollect ] );
        ++mParentClass->mNumCollected;
    }
}

template<>
void ManageStateVariables::DoCollect::processData<objects::YearVector<Value> >( objects::YearVector<Value>& aData ) {
    // When a year vector is tagged we only need to worry about values in the current
    // timestep (already calculated the years ahead of time in the interest of speed
    // to be from [mCCStartYear, mYearToCollect])
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
    // Ignore any data set within a Technology that is not operating in the current
    // model period.
    if( !aData->isOperating( mParentClass->mPeriodToCollect ) ) {
        mIgnoreCurrValue = true;
    }
}

template<>
void ManageStateVariables::DoCollect::popFilterStep<ITechnology*>( ITechnology* const& aData ) {
    // Moving out of the current Technology so reset the ignore flag.
    mIgnoreCurrValue = false;
}

template<>
void ManageStateVariables::DoCollect::pushFilterStep<Market*>( Market* const& aData ) {
    // Ignore any data set within a Market which is not for the current model year.
    if( aData->getYear() != mParentClass->mYearToCollect ) {
        mIgnoreCurrValue = true;
    }
}

template<>
void ManageStateVariables::DoCollect::popFilterStep<Market*>( Market* const& aData ) {
    // Moving out of the current Market so reset the ignore flag.
    mIgnoreCurrValue = false;
}

