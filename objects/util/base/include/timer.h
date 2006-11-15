#ifndef _TIMER_H_
#define _TIMER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file timer.h  
* \ingroup Objects
* \brief Header file for the Timer class.
* \author Josh Lurz
*/

#include <iosfwd>
#include <string>
#include <ctime>
/*!
* \ingroup Objects
* \brief A very basic class which times and prints events.
* \author Josh Lurz
*/
class Timer {
public:
    Timer();        
    void start();
    void stop();
    double getTimeDifference() const;
    void print( std::ostream& aOut, const std::string& aTitle = "Time: " ) const;
private:
    clock_t mStartTime; //!< Time the timer started
    clock_t mStopTime; //!< Time the timer stopped
};

#endif // _TIMER_H_
