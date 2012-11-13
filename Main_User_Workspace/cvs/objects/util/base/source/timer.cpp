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
* \file timer.cpp
* \ingroup Objects
* \brief Timer class source file.
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include "util/base/include/timer.h"

using namespace std;
using namespace boost::posix_time;

//! Constructor
Timer::Timer():mTotalTime( 0 )
{
}
        
/*! \brief Start the timer. 
* \details This function starts the timer. All times will be relative to this time.
*/     
void Timer::start(){
    mStartTime = microsec_clock::universal_time();
}

/*! \brief Stop the timer.
* \details This functions stops the timer so that the amount of time that has passed
* may be fetched or printed.
*/
void Timer::stop(){
    mStopTime = microsec_clock::universal_time();
    mTotalTime += getTimeDifference();
}

/*! \brief Get the differential between the start time and stop time.
* \return The difference between the stop and start time.
*/
double Timer::getTimeDifference() const {
    time_duration diff = mStopTime - mStartTime;
    return diff.total_seconds() + pow( 10.0, -time_duration::num_fractional_digits() ) * diff.fractional_seconds();
}

/*!
 * \brief Get the total time measured by this timer between all start and stops.
 * \return The total time in seconds.
 */
double Timer::getTotalTimeDifference() const {
    return mTotalTime;
}

/*! \brief Print the stored time.
* \details This function prints the time between the last call to stop() and the time
* start() was called.
* \param aOut The output stream to print to.
* \param aTitle The label to print in front of the time. Defaults to 'Time: '
*/
void Timer::print( std::ostream& aOut, const string& aLabel ) const {
    if( mTotalTime > 0 ) {
        aOut << aLabel << " " << mTotalTime << " seconds. " << endl;
    }
}

//! Constructor
TimerRegistry::TimerRegistry():mPredefinedTimers( END )
{
}

/*!
 * \brief Get the singleton instance of the TimerRegistry.
 * \return The TimerRegistry.
 */
TimerRegistry& TimerRegistry::getInstance() {
    static TimerRegistry TIMER_REGISTRY;
    return TIMER_REGISTRY;
}

/*!
 * \brief Get the underlying timer for the given identifier.
 * \details This version looks up the timer by enumeration and should be used when
 *          performance is critical.
 * \param aTimerName The identifier to use to lookup the timer.
 * \return The appropriate Timer to use.
 */
Timer& TimerRegistry::getTimer( const PredefinedTimers aTimerName ) {
    /*!
     * \pre aTimerName is a valid PredefinedTimers.
     */
    assert( aTimerName < END );
    
    return mPredefinedTimers[ aTimerName ];
}

/*!
 * \brief Get the underlying timer for the given identifier.
 * \details This version looks up the timer by name and is more convenient to use
 *          when debugging.
 * \param aTimerName The identifier to use to lookup the timer.
 * \return The appropriate Timer to use, note if a timer for the given name does
 *         not already exist it will be created
 */
Timer& TimerRegistry::getTimer( const string& aTimerName ) {
    return mNamedTimers[ aTimerName ];
}

/*!
 * \brief Have all registered timers print their current times using their names
 *        as a label.
 */
void TimerRegistry::printAllTimers( ostream& aOut ) const {
    for( int timer = 0; timer < END; ++timer ) {
        string timerName;
        switch( timer ) {
            case FULLSCENARIO:
                timerName = "Full Scenario";
                break;
            case BISECT:
                timerName = "Bisection solver";
                break;
            case SOLVER:
                timerName = "Broyden Solver";
                break;
            case JACOBIAN:
                timerName = "Jacobian calcs";
                break;
            case EVAL_PART:
                timerName = "Partial function evaluations";
                break;
            case EVAL_FULL:
                timerName = "Full function evaluations";
                break;
            case JAC_PRE:
                timerName = "Jacobian Preconditioner (overlaps with Jacobian)";
                break;
            case JAC_PRE_JAC:
                timerName = "Jacobian Preconditioner Jacobian overlap";
                break;
            case EDFUN_MISC:
                timerName = "EDFUN miscellaneous";
                break;
            case EDFUN_PRE:
                timerName = "EDFUN before world->calc";
                break;
            case EDFUN_POST:
                timerName = "EDFUN after world->calc";
                break;
            case EDFUN_AN_RESET:
                timerName = "EDFUN affected nodes reset";
                break;
                
            default: timerName = "Predefined timer";
        }
        mPredefinedTimers[ timer ].print( aOut, timerName );
    }
    
    for( map<string, Timer>::const_iterator it = mNamedTimers.begin(); it != mNamedTimers.end(); ++it ) {
        (*it).second.print( aOut, (*it).first );
    }
}
