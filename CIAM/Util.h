#ifndef _UTIL_H_
#define _UTIL_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file Util.h  
* \ingroup CIAM
* \brief A set of commonly used functions.
* 
* This is a set of functions which are frequently needed within the program.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include <limits>
#include <cmath>
#include <fstream>
#include <iostream>
#include <string>
#include <ctime>
#include <sstream>

namespace util {
    /*! \brief A function to determine the sign of a number.
    * \param number A templated parameter which must be comparable to 0.
    * \return Returns -1 if the number is less than 0, +1 otherwise.
    */
    template <class T>
    const int sign( const T number ) {
        return ( number < 0 )?(-1):(1);
    }

    /*! \brief A function to check for the validity of numbers. 
    *
    * Occasionally after calculations numbers are no longer in the range of
    * real numbers, however C++ will continue to perform calculations on them.
    * This function can then be used to check if numbers are not NaN or Infinity.
    *   
    * \warning Some compiler/platforms do not support these checks. In that case this function will always return true. 
    * \warning Do not try to perform this check without using this function. It will fail on some compilers.
    * \param number A templated parameter which must be comparable to 0.
    * \return Returns -1 if the number is less than 0, +1 otherwise.
    */
    template <class T>
    const bool isValidNumber( const T number ) {
        bool tempval = ( number == number );
        if ( std::numeric_limits<double>::infinity() != 0 ) {
            tempval = tempval && ( number != std::numeric_limits<T>::infinity() );
        }
        return tempval;
    }

    /*! \brief A function to determine if two doubles are equal.
    *
    * Due to inaccuracies in machine arithmatic, it is virtually impossible for two doubles with decimal values
    * that are calculated with different methods to be exactly equal. This function checks if the two values
    * are within a very small threshhold. 
    *
    * \warning Do not compare two doubles using the == operator. Use this function instead. 
    * \param firstNumber The first double to compare.
    * \param secondNumber The second double to compare.
    * \return Whether the two doubles are within SMALL_NUM of equivalence. 
    */
    inline const bool isEqual( const double firstNumber, const double secondNumber ) {
        const double SMALL_NUM = 1E-10;
        return ( fabs( firstNumber - secondNumber ) < SMALL_NUM );
    }

    /*! \brief A function to check if a file was opened successfully.
    *
    * When C++ opens a file, for input or output, it does not check whether it was opened successfully.
    * If the file has not been opened correctly, the program will often not fail as FORTRAN programs do,
    * but instead it will have unexpected behavior. This function will check if the file is open, and 
    * if it is not it will print an error message before calling abort. 
    * 
    * \todo This function should be more flexible, for use in cases where the missing file is a non-fatal problem.
    * \param streamIn A templated parameter that must have the function is_open. This was done so that one function could 
    * check both input files and output files.
    * \param fName The name of the file streamIn references, so that the error message can be more informative.
    */
    template <class T>
    inline void checkIsOpen( const T& streamIn, const std::string& fName ) {
        if( !streamIn.is_open() ) {
            std::cerr << "Severe Error: File could not be opened for writing." << std::endl;
            abort();
        }
    }

    /*! \brief A function to replace spaces with underscores.
    *
    * This function will replace all spaces in a string with underscores. Each space will be replaced by 
    * exactly one underscore, multiple spaces are not concatonated. Other whitespace characters are not
    * replaced. 
    * 
    * \todo This function currently generated a compiler warning, which should be fixed. 
    * \param stringIn The string in which spaces should be replaced by underscores.
    */
    inline void replaceSpaces( std::string& stringIn ) {
        static const std::basic_string<char>::size_type npos = -1;
        std::basic_string <char>::size_type index;

        while( stringIn.find_first_of( " " ) != npos ) {
            index = stringIn.find_first_of( " " );
            stringIn.replace( index, 1, "_" );
        }
    }

    //! Static function which returns SMALL_NUM. This avoid initialization problems. 
   static inline double getSmallNumber() {
      const double SMALL_NUM = 1e-6;
      return SMALL_NUM;
   }

   //! Static function which returns VERY_SMALL_NUM. This avoids initialization problems. 
   static inline double getVerySmallNumber() {
      const double VERY_SMALL_NUM = 1e-8;
      return VERY_SMALL_NUM;
   }

    /*! \brief Function which creates an XML compliant date time string.
    *
    * This function takes as an argument a time_t object and returns a string containing the date and time in the following format:
    * yyyy-mm-dd-Thh:mm-GMTOFFSET
    * ie: 2003-01-11T09:30:47-05:00
    * \param time time_t to convert to XML string form.
    * \return string The time converted to XML date string format.
    * \bug GMT offset does not work properly.
    */

   static std::string XMLCreateDate( const std::time_t& time ) {
       std::stringstream buffer;
       std::string retString;
       struct std::tm* timeInfo;
       struct std::tm* umtTimeInfo;
	
	    timeInfo = localtime( &time );
	    umtTimeInfo = gmtime( &time );
	
	    // Create the string
	    buffer << ( timeInfo->tm_year + 1900 ); // Set the year
	    buffer << "-";
	    buffer << timeInfo->tm_mday; // Set the day
	    buffer << "-";
	    buffer << ( timeInfo->tm_mon + 1 ); // Month's in ctime range from 0-11
	    buffer << "T";
	    buffer << timeInfo->tm_hour;
	    buffer << ":";
	    buffer << timeInfo->tm_min;
	    buffer << ":";
	    buffer << timeInfo->tm_sec;
	    buffer << "-";
	
	    int umtDiff = timeInfo->tm_hour - umtTimeInfo->tm_hour;
	    if( umtDiff < 10 ) {
		    buffer << "0";
	    }
	    buffer << umtDiff;
	    buffer << ":00";
	    // Completed creating the string;
	    buffer >> retString;
	
	    return retString;
    }
}
#endif // _UTIL_H_

