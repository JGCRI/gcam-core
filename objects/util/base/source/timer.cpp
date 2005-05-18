/*! 
* \file timer.cpp
* \ingroup Objects
* \brief Timer class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <ctime>
#include <string>
#include "util/base/include/timer.h"

using namespace std;

//! Constructor
Timer::Timer(){
    mStartTime = 0;
    mStopTime = 0;
}
        
/*! \brief Start the timer. 
* \details This function starts the timer. All times will be relative to this time.
*/     
void Timer::start(){
    mStartTime = clock();
}

/*! \brief Stop the timer.
* \details This functions stops the timer so that the amount of time that has passed
* may be fetched or printed.
*/
void Timer::stop(){
    mStopTime = clock();
}

/*! \brief Get the differential between the start time and stop time.
* \return The difference between the stop and start time.
*/
double Timer::getTimeDifference() const {
    return (double)( mStopTime - mStartTime ) / CLOCKS_PER_SEC;
}
/*! \brief Print the stored time.
* \details This function prints the time between the last call to stop() and the time
* start() was called.
* \param aOut The output stream to print to.
* \param aLabel The label to print in front of the time. Defaults to 'Time: '
*/
void Timer::print( std::ostream& aOut, const string& aLabel ) const {
    aOut << aLabel << " " << getTimeDifference() << " seconds. " << endl; 
}
