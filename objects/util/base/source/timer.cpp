/*! 
* \file timer.cpp
* \ingroup CIAM
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
    startTime = 0;
    savedTime = 0;
}
        
//! Destructor.
Timer::~Timer(){
}
        
/*! \brief Start the timer. 
* \detailed This function starts the timer. All times will be relative to this time.
*/     
void Timer::start(){
    startTime = clock();
}

/*! \brief Save the current time for printing. 
* \detailed This function saves a time for printing. This time will be used 
* for all call to print() until the next call to this function.  
*/
void Timer::save(){
    savedTime = clock();
}
        
/*! \brief Print the stored time.
* \detailed This function prints the time between the last call to save and the time
* start() was called.
* \param out The output stream to print to.
* \param label The label to print in front of the time. Defaults to 'Time: '
*/
void Timer::print( std::ostream& out, const string label ) const {
    out << label << " " << (double)( savedTime - startTime ) / CLOCKS_PER_SEC << " seconds. " << std::endl; 
}