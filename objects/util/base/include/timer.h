#ifndef _TIMER_H_
#define _TIMER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file timer.h  
* \ingroup CIAM
* \brief Header file for the timer class.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <iosfwd>
#include <string>
#include <ctime>
/*!
* \ingroup CIAM
* \brief A very basic class which times and prints events.
* \author Josh Lurz
*/
class Timer {
private:
    clock_t startTime;
    clock_t savedTime;

public:
    Timer();        
    ~Timer();
    void start();
    void save();
    void print( std::ostream& out, const std::string title = "Time: " ) const;
    };

#endif // _TIMER_H_