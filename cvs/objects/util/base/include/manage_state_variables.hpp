#ifndef _MANAGE_STATE_VARIABLES_H_
#define _MANAGE_STATE_VARIABLES_H_
#if defined(_MSC_VER)
#pragma once
#endif

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
 * \file manage_state_variables.hpp
 * \ingroup util
 * \brief ManageStateVariables class header file.
 * \author Pralit Patel
 */

#include <cassert>
#include <forward_list>
#include <string>
#include "util/base/include/definitions.h"

class Value;

#if GCAM_PARALLEL_ENABLED
#include <tbb/task_arena.h>
#endif

/*!
 * \brief A utility for collecting all self declared GCAM state variables so that
 *        they can be managed and reset as appropriate.
 * \details All Data definitions marked as STATE will be searched for using GCAMFusion
 *          and only those that could possibly be changed during World.calc of the
 *          period for which this class was created will be managed as active state.
 *          The Value class is used in conjunction with this class such that the
 *          actual state data is stored in tightly packed arrays that can quickly
 *          get reset.  In addition the Value can be set at the same time from multiple
 *          threads.  All of this happens opaque to the rest of the GCAM code so
 *          developers do not need to worry about any of this.  All they have to do
 *          is ensure they appropriately tag their STATE Data.
 *
 * \author Pralit Patel
 */
class ManageStateVariables {
    friend class SolutionDebugger;
public:
    ManageStateVariables( const int aPeriod );
    ~ManageStateVariables();
    
    void copyState();
    
    void setPartialDeriv( const bool aIsPartialDeriv );
    
#if GCAM_PARALLEL_ENABLED
    //! A tbb task arena which is the closest tbb comes to a thread pool which we
    //! will insist parallel calculations use so that we can ensure that we have
    //! appropriately sized and allocated a slot in mStateData for each thread to
    //! have as "scratch" space for it's computations.
    tbb::task_arena mThreadPool;
#endif
    
private:
    //! The actual home of all state data.  This is a two dimensional array where
    //! the first is by state the second is for each GCAM Data marketed as STATE.
    //! Note the first state is the "base" state and the rest are "scratch" for
    //! partial derivatives where before a new partial derivative is performed the
    //! "scratch" space is copied over by the "base" state.  Without GCAM_PARALLEL_ENABLED
    //! only a single "scratch" state will be allocated, when it is enabled there
    //! will be as many as the max_concurrency the thread pool allows on the system
    //! running the code.
    double** mStateData;
    
    //! The period this state was collected for.
    int mPeriodToCollect;
    
    //! The year corresponding to mPeriodToCollect converted ahead of time in the
    //! interest of speed when collecting state.
    int mYearToCollect;
    
    //! The first year of the yearly carbon cycle arrays of data that may change
    //! while calculating World.calc( mPeriodToCollect ).
    unsigned int mCCStartYear;
    
    //! The total number of individual Values flagged as STATE that could possibly
    //! be changed during World.calc( mPeriodToCollect ).
    uint64_t mNumCollected;
    
    //! The list of individual Values flagged as STATE that could possibly be
    //! changed during World.calc( mPeriodToCollect ).  We store them in a list
    //! since searching via GCAMFusion is a relatively expensive operation and we
    //! will need to take three passes at them:
    //! - Figure out how many we have so what we can allocate enough memory for mStateData.
    //! - Copy the actual data from each Value to initialize the "base" state.
    //! - When we are done with this period copy the "base" state back into each Value.
    std::forward_list<Value*> mStateValues;
    
    void collectState();
    
    void resetState();
    
    std::string getRestartFileName() const;
    
    void loadRestartFile();
    
    void saveRestartFile();
    
    /*!
     * \brief A helper struct to provide a call back to GCAMFusion as it searches
     *        for data flagged STATE.
     * \details In addition to handling the processData call back we also are
     *          interested in the push/pop filter steps, particularly for Technology
     *          and MarketContainer to avoid collecting Data in a Technology or
     *          Market that is going to be inactive during mPeriodToCollect.
     */
    struct DoCollect {
        //! A reference to the containing class where each collected state data
        //! will be added.
        ManageStateVariables* mParentClass;
        
        //! A state variable while processing GCAMFusion which when set indicates
        //! we are in a Technology or MarketContainer that is inactive in the
        //! current period.  This flag gets reset when the corresponding popFilterStep
        //! is found.
        bool mIgnoreCurrValue = false;
        
        // Templated callbacks for GCAMFusion
        template<typename DataType>
        void processData( DataType& aData );
        template<typename DataType>
        void pushFilterStep( const DataType& aData );
        template<typename DataType>
        void popFilterStep( const DataType& aData );
    };
};

#endif // _MANAGE_STATE_VARIABLES_H_
