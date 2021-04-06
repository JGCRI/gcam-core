#ifndef _TIMER_H_
#define _TIMER_H_
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
* \file timer.h  
* \ingroup Objects
* \brief Header file for the Timer class.
* \author Josh Lurz
*/

#include <iosfwd>
#include <string>
#include <vector>
#include <map>
#include <boost/date_time/posix_time/posix_time_types.hpp>
#include <boost/core/noncopyable.hpp>

#include "util/base/include/definitions.h"

#if GCAM_PARALLEL_ENABLED
#include <tbb/spin_mutex.h>
#endif

/*!
* \ingroup Objects
* \brief A very basic class which times and prints events.
* \author Josh Lurz
*/
class Timer : private boost::noncopyable {
public:
    Timer();        
    void start();
    void stop();
    double getTotalTimeDifference() const;
    void print( std::ostream& aOut, const std::string& aTitle = "Time: " ) const;
private:
    //! Time the timer started
    boost::posix_time::ptime mStartTime;

    //! State flag
    int mRunning;
    
#if GCAM_PARALLEL_ENABLED
    tbb::spin_mutex mMutex;
#endif
    
    //! The total time measured by this timer between all starts and stops.
    double mTotalTime;
};

/*!
 * \brief A central timer repository which will keep track of named timers.
 * \details Registered timers will exist for the duration of the model and can be
 *          started and stopped multiple times while keeping a running total of
 *          the total time spent.  This can be very useful for debugging or profiling
 *          therefore it provides two interfaces.  One where timers are named by
 *          predefined enumerations where performance is critical and another they
 *          are named by string which is more convenient.
 * \author Pralit Patel and Robert Link
 */
class TimerRegistry {
public:
    //! Enumeration which describes predefined timers, to be used when performance
    //! is critical.
    enum PredefinedTimers {
        FULLSCENARIO,
        BISECT,
        SOLVER,
        JACOBIAN,
        EVAL_PART,
        EVAL_FULL,
        JAC_PRE,
        JAC_PRE_JAC,
        EDFUN_MISC,
        EDFUN_PRE,
        EDFUN_POST,
        EDFUN_AN_RESET,
        WRITE_DATA,
        END
    };
    
    static TimerRegistry& getInstance();
    
    Timer& getTimer( const std::string& aTimerName );
    
    Timer& getTimer( const PredefinedTimers aTimerName );
    
    void printAllTimers( std::ostream& aOut ) const;
private:
    //! Private constructor to prevent multiple registries
    TimerRegistry();
    //! Private undefined copy constructor to prevent copying
    TimerRegistry( const TimerRegistry& aTimerRegistry );
    //! Private undefined assignment operator to prevent copying
    TimerRegistry& operator=( const TimerRegistry& aTimerRegistry );
    
    //! A vector sized for the predefined timers for fast lookup.
    std::vector<Timer> mPredefinedTimers;
    
    //! A map for named timers.
    std::map<std::string, Timer> mNamedTimers;
};

#endif // _TIMER_H_
