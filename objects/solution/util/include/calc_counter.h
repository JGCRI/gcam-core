#ifndef _CALC_COUNTER_H_
#define _CALC_COUNTER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file calc_counter.h
* \ingroup Solution
* \brief The header file for the CalcCounter class.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <iosfwd>
#include <map>

/*!
* \ingroup Solution
* \brief A class used to count iterations of world.calc.
* \detailed This function tracks calls to world.calc and keeps updated counts of the total 
* number of times world.calc has been called for the model, the total times for the period, 
* and the number of times in the current period by solution mechanism. The world has a pointer
* to this object, so it is updated automatically. Solution mechanisms need to notify it when they
* switch solution methods and when a new period is started. They can then use its accessor functions 
* at any time and are gaurunteed to have updated values. 
* \author Josh Lurz
*/
class CalcCounter {
    //! Function which allows the use of the << operator on the CalcCounter object.
     friend std::ostream& operator<<( std::ostream& os, const CalcCounter& calcCounter ){
        calcCounter.print( os );
        return os;
    }
public:
    CalcCounter();
    int getTotalCount() const;
    int getPeriodCount() const;
    int getMethodCount( const std::string methodName ) const;
    void incrementCount( const double additional = 1 );
    void setCurrentMethod( const std::string methodName );
    void startNewPeriod();
private:
    std::string currMethodName;
    std::map<std::string, double> methodCounts;
    double totalCount;
    double periodCount;
    static int convertToInt( double );
    void print( std::ostream& out ) const;
};

#endif // _CALC_COUNTER_H_
