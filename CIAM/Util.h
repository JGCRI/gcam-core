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

namespace util {
   template <class T>
      //! Helper function to determine the sign of a number.
      const int sign( const T number ) {
      return ( number < 0 )?(-1):(1);
   }
   
   template <class T>
      //! Helper function to check for validity of numbers. 
      const bool isValidNumber( const T number ) {
      bool tempval =  ( number == number );
      if ( std::numeric_limits<double>::infinity() != 0 ) {
         tempval = tempval && ( number != std::numeric_limits<T>::infinity() );
      }
      return tempval;
   }

   //! Helper function to determine if two doubles are equal.
   inline const bool isEqual( const double firstNumber, const double secondNumber ) {
      const double SMALL_NUM = 1E-10;
      return ( fabs( firstNumber - secondNumber ) < SMALL_NUM );
   }
   
   //! Function to check for proper opening of an output file.
   inline void checkIsOpen( std::ofstream& streamIn ) {
      if( !streamIn.is_open() ) {
         abort();
      }
   }

   //! Function to check for proper opening of an input file.
   inline void checkIsOpen( std::ifstream& streamIn ) {
      if( !streamIn.is_open() ) {
         abort();
      }
   }
}

#endif // _UTIL_H_

