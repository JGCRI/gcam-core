/*! 
* \file calc_counter.cpp
* \ingroup Solution
* \brief CalcCounter class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include <iostream>
#include <cmath>

#include "util/base/include/util.h"
#include "solution/util/include/calc_counter.h"

using namespace std;

//! Constructor
CalcCounter::CalcCounter() {
    totalCount = 0;
    periodCount = 0;
}

/* \brief Return the total number of iterations of world.calc called so far for all periods.
* \return Integer value of the number of calls of world.calc called so far for all periods.
*/
int CalcCounter::getTotalCount() const {
    return convertToInt( totalCount );
}

/* \brief Return the total number of iterations of world.calc called so far for the current periods.
* \return Integer value of the number of calls of world.calc called so far for the current periods.
*/
int CalcCounter::getPeriodCount() const {
    return convertToInt( periodCount );
}

/*! \brief Return the number of iterations of world.calc called so far by a given solution method in the current period.
* \param methodName The name of the method for which to get the number of world.calc calls.
* \return The number of times the given solution method has called world.calc in the current period.
*/
int CalcCounter::getMethodCount( const string methodName ) const {
    return convertToInt( util::searchForValue( methodCounts, methodName ) );
}

/*!\brief Increment the world.calc count by a given amount, 1 by default.
* \details This method increments the total count, period count and count for the current method
* by the amount passed as an argument. 
* \param additional Amount to increment the counts by, 1 is the default.
*/
void CalcCounter::incrementCount( const double additional ){
    totalCount += additional;
    periodCount += additional;
    methodCounts[ currMethodName ] += additional;
}

/*! \brief Set the name of the method currently being used to solve.
* \param methodName The name of the method now being used to solve.
*/
void CalcCounter::setCurrentMethod( const string methodName ){
    currMethodName = methodName;
}

/*! \brief Start a new period. 
* \details Starts a new period by resetting the period based counters.
*/
void CalcCounter::startNewPeriod(){
    periodCount = 0;
    methodCounts.clear();
}

/*! \brief Utility helper function to convert to an integer from the cieling of a double.
* \param value Double value to convert.
* \return Integer with the value of the cieling of the double passed in.
*/
int CalcCounter::convertToInt( const double value ) {
    return static_cast<int>( ceil( value ) );
}

/*! \brief Print out the information countained within the CalcCounter.
* \param out Outputstream to print to.
*/
void CalcCounter::print( ostream& out ) const {
    out << "Period Count: " << periodCount << endl;
    out << "Total Count: " << totalCount << endl;
    out << "Per Method Period Counts: " << endl;

    typedef map<string, double>::const_iterator MethodCountIterator;

    for( MethodCountIterator iter = methodCounts.begin(); iter != methodCounts.end(); ++iter ){
        out << "Method: " << iter->first << " Count: " << iter->second << endl;
    }
    out << endl;
}


