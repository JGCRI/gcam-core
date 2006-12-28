#ifndef _UTIL_H_
#define _UTIL_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
 * \file util.h  
 * \ingroup Objects
 * \brief A set of commonly used functions.
 * \details This is a set of functions which are frequently needed within the
 *          program.
 * \note These are static functions within a namespace, not static class
 *       functions. This is because partial template specialization cannot be
 *       done for classes. The functions are in the objects namespace, also the
 *       location of the utility container classes. The util namespace is
 *       aliased to the objects, namespace, which means it is an alternative
 *       name for it.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <boost/static_assert.hpp>
#include <limits>
#include <fstream>
#include <iostream>
#include <string>
#include <cmath>
#include <sstream>
#include <map>
#include <vector>
#include <cassert>

// Boost static asserts do not work when included from multiple namespaces.
// Seperate them into their own unique namespace.
namespace conditionsCheck {
    BOOST_STATIC_ASSERT( std::numeric_limits<double>::has_quiet_NaN );
    BOOST_STATIC_ASSERT( std::numeric_limits<double>::has_infinity );
}

namespace objects {
    
    /*! \brief Returns the value within this map associated with a given key. 
    * \details This function takes as its input a map and a key to search for.
    *          It will return the value associated with the key, or the default
    *          value for the class of the object if the key is not found.
    * \note Use this function instead of recoding a map search, as this function
    *       should be more efficient and handle errors more appropriately. 
    * \todo Evaluate returning a const reference.
    * \param currMap The map within which to search for the value.
    * \param key The key to find the value with which it is associated.
    * \return The value in the currMap associated with the key, the default
    *         constructed object otherwise. 
    */
    template <class K, class V>
    const V searchForValue( const std::map<K,V>& currMap, const K& key ){
        typedef typename std::map<K,V>::const_iterator CMapIterator;
        CMapIterator iter = currMap.find( key );
        if( iter != currMap.end() ){
            return iter->second;
        } else {
            return V(); // Returns default constructed value, 0 for doubles and
                        // ints
        }
    }

    /*! \brief Returns a constant iterator to a position in the vector which
    *          contains a pointer to an object with the given name. 
    * \details This function searches linearly from the beginning of the vector
    *          until it finds an object which has a getName() function which
    *          returns the given name. If the name is not found the end iterator
    *          will be returned.
    * \param aVector A vector to search.
    * \param aName A name for which to search.
    * \return An iterator to the position in the vector with the given name, the
    *         end iterator if that is not found.
    */
    template <class U>
    inline typename std::vector<U>::const_iterator searchForValue( const std::vector<U>& aVector, const std::string& aName ) {
        typename std::vector<U>::const_iterator iter = aVector.begin();
        for( ; iter!= aVector.end(); ++iter ){
            if( (*iter)->getName() == aName ){
                break;
            }
        }
        return iter;
    }

    /*! \brief Returns a mutable iterator to a position in the vector which
    *          contains a pointer to an object with the given name. 
    * \details This function searches linearly from the beginning of the vector
    *          until it finds an object which has a getName() function which
    *          returns the given name. If the name is not found the end iterator
    *          will be returned.
    * \param aVector A vector to search.
    * \param aName A name for which to search.
    * \return An iterator to the position in the vector with the given name, the
    *         end iterator if that is not found.
    */
    template <class U>
    inline typename std::vector<U>::iterator searchForValue( std::vector<U>& aVector, const std::string& aName ) {
        typename std::vector<U>::iterator iter = aVector.begin();
        for( ; iter!= aVector.end(); ++iter ){
            if( (*iter)->getName() == aName ){
                break;
            }
        }
        return iter;
    }

    /*! \brief Returns whether a value with the given key exists.
    * \details This function takes as its input a map and a key for which to
    *          search, and returns whether the key exists. 
    * \param aCurrMap The map within which to search for the value.
    * \param aKey The key of which to check for the existance.
    * \return Whether the key exists. 
    */
    template <class K, class V> bool hasValue( const std::map<K,V>& aCurrMap, const K& aKey ){
        return ( aCurrMap.find( aKey ) != aCurrMap.end() );
    }

    /*! \brief A function to determine the sign of a number.
    * \param number A templated parameter which must be comparable to 0.
    * \return Returns -1 if the number is less than 0, +1 otherwise.
    */
    template <class T>
    const int sign( const T number ) {
        return ( number < 0 )?(-1):(1);
    }

    /*! \brief Check the validity of a number.
    * \details Occasionally after calculations numbers are no longer in the
    *          range of real numbers, however C++ will continue to perform
    *          calculations on them. This function can then be used to check if
    *          numbers are not not a number or infinity.
    * \warning Some compiler/platforms do not support these checks. In that case
    *          this function will always return true. 
    * \warning Do not try to perform this check without using this function. It
    *          will fail on some compilers.
    * \param number A templated parameter which must be comparable to 0.
    * \return Returns whether the number is valid.
    */
    template <class T>
    inline bool isValidNumber( const T aNumber ) {

        // Need to check whether the type supports not-a-number and infinity.
        return ( !std::numeric_limits<T>::has_quiet_NaN || aNumber != std::numeric_limits<T>::quiet_NaN() )
            && ( !std::numeric_limits<T>::has_infinity ||
            ( aNumber != std::numeric_limits<T>::infinity()
            && std::negate<double>()( aNumber ) != std::numeric_limits<T>::infinity() ) );
    }

    /*!
     * \brief Specialization of isValidNumber for booleans.
     * \details Booleans are always valid so this overrides isValidNumber to 
     *          avoid conversion which would not be legal on booleans.
     * \param aBoolean Boolean to check for validity.
     * \return Whether the boolean is valid which is always true.
     */
    template <>
    inline bool isValidNumber<bool>( const bool aNumber ){
        return true;
    }

    /*!
     * \brief This is a template function which compares two values. 
     * \details This function very simply uses the == operator of the two
     *          arguments to compare them, and returns the return value of the
     *          == operator. The reason for this function is so that it can be
     *          overridden for doubles to perform special comparison not using
     *          the == operator.
     * \param aFirstValue The first value to compare.
     *  \param aSecondValue The second value to compare.
     * \param aTolerance This parameter is unused and only for compatability
     *        with the double specialization of the function.
     * \return Whether or not the two values are equal.
     */
    template<class T>
    inline bool isEqual( const T aFirstValue,
                         const T aSecondValue,
                         const double aTolerance = 1E-10 )
    {
        return ( aFirstValue == aSecondValue );
    }

    /*!
     * \brief A function to determine if two doubles are equal.
     * \details Due to inaccuracies in machine arithmatic, it is virtually
     *          impossible for two doubles with decimal values that are
     *          calculated with different methods to be exactly equal. This
     *          function checks if the two values are within a very small
     *          threshhold. This an explicit template specialization for doubles
     *          which allows isEqual to act differently for doubles. These
     *          function had to be declared inline to avoid linker errors.
     * \warning Do not compare two doubles using the == operator. Use this
     *          function instead. 
     * \param aFirstValue The first double to compare.
     * \param aSecondValue The second double to compare.
     * \param aTolerance Tolerance to use when comparing the numbers. Defaults
     *        to 1E-10.
     * \return Whether the two doubles are within aTolerance of each other. 
     */
    template<>
    inline bool isEqual<double>( const double aFirstValue,
                                 const double aSecondValue,
                                 const double aTolerance )
    {
        return ( std::fabs( aFirstValue - aSecondValue ) < aTolerance );
    }

    /*
    * \brief Interpolate a Y value based on two points and an X value.
    * \param aX X value for which to find an X value.
    * \param aX1 First X value.
    * \param aX2 Second X value.
    * \param aY1 First Y value.
    * \param aY2 Second Y value.
    * \return Linearly interpolated Y value for aX.
    */
    double linearInterpolateY( const double aX,
                               const double aX1,
                               const double aX2,
                               const double aY1,
                               const double aY2 );

    /*! \brief A function to check if a file was opened successfully.
    *
    * When C++ opens a file, for input or output, it does not check whether it
    * was opened successfully. If the file has not been opened correctly, the
    * program will often not fail as FORTRAN programs do, but instead it will
    * have unexpected behavior. This function will check if the file is open,
    * and if it is not it will print an error message before calling abort. 
    * 
    * \todo This function should be more flexible, for use in cases where the
    *       missing file is a non-fatal problem.
    * \param streamIn A templated parameter that must have the function is_open.
    *        This was done so that one function could 
    * check both input files and output files.
    * \param fName The name of the file streamIn references, so that the error
    *        message can be more informative.
    */
    template <class T>
    inline void checkIsOpen( T& streamIn, const std::string& fName ) {
        if( !streamIn.is_open() ) {
            std::cerr << "Severe Error: File " << fName << " could not be opened." << std::endl;
            abort();
        }
    }
    
    std::string replaceSpaces( const std::string& aString );

    /*! \brief Static function which returns SMALL_NUM. 
    * \details This is a static function which is used to find the value of the
    *          constant SMALL_NUM. This avoids the initialization problems of
    *          static variables. This function should be used instead of
    *          defining this constant in multiple locations in the code.
    * \return The constant SMALL_NUM.
    */
   static inline double getSmallNumber() {
      const double SMALL_NUM = 1e-6;
      return SMALL_NUM;
   }

    /*! \brief Static function which returns VERY_SMALL_NUM. 
    * \details This is a static function which is used to find the value of the
    *          constant VERY_SMALL_NUM. This avoids the initialization problems
    *          of static variables. This function should be used instead of
    *          defining this constant in multiple locations in the code.
    * \return The constant VERY_SMALL_NUM.
    */
   static inline double getVerySmallNumber() {
      const double VERY_SMALL_NUM = 1e-8;
      return VERY_SMALL_NUM;
   }

    /*! \brief Static function which returns EXTREMELY_SMALL_NUM. 
    * \details This is a static function which is used to find the value of the
    *          constant EXTREMELY_SMALL_NUM. This avoids the initialization
    *          problems of static variables. This function should be used
    *          instead of defining this constant in multiple locations in the
    *          code.
    * \return The constant EXTREMELY_SMALL_NUM.
    */
   static inline double getTinyNumber() {
      const double EXTREMELY_SMALL_NUM = 1e-16;
      return EXTREMELY_SMALL_NUM;
   }

    /*! \brief Static function which returns LARGE_NUM. 
    * \details This is a static function which is used to find the value of the
    *          constant LARGE_NUM. This avoids the initialization problems of
    *          static variables. This function should be used instead of
    *          defining this constant in multiple locations in the code.
    * \return The constant LARGE_NUM.
    */
   static inline double getLargeNumber() {
      const double LARGE_NUM = 1e+6;
      return LARGE_NUM;
   }

    /*! \brief Function which returns a vector of keys from a map.
    * \details This function takes a map as an argument and returns a vector of
    *           all the keys of the map. It uses the same order as the map
    *           iterator returns.
    * \param aMap A map to return all keys for.
    * \return A vector of all keys from the map in the same order as the map
    *         iterator returns.
    */
    template<class T, class U> const std::vector<T> getKeys( const std::map<T,U> aMap ) {
        typedef typename std::map<T,U>::const_iterator ConstMapIterator;
        std::vector<T> keys;
        for( ConstMapIterator mapIter = aMap.begin(); mapIter != aMap.end(); mapIter++ ){
            keys.push_back( ( *mapIter ).first );
        }
        return keys;
    }
    
   /* \brief Function which returns a vector of values from a map.
   * \details This function takes a map as an argument and returns a vector of
   *          all the values of the map. It uses the same order as the map
   *          iterator returns.
   * \param aMap A map to return all values for.
   * \return A vector of all values from the map in the same order as the map
   *         iterator returns.
   */
    template<class T, class U>
        const std::vector<U> getValues( const std::map<T,U> aMap )
    {
        typedef typename std::map<T,U>::const_iterator ConstMapIterator;
        std::vector<U> values;
        for( ConstMapIterator mapIter = aMap.begin(); mapIter != aMap.end(); mapIter++ ){
            values.push_back( ( *mapIter ).second );
        }
        return values;
    }

    /*! \brief Convert a value to a string using the built in stringstream.
    * \todo Remove this function and use the boost equivalent.
    */
    template<class T> std::string toString( const T& value ){
        static std::stringstream converter;
        converter << value;
        std::string output;
        converter >> output;
        converter.clear();
        return output;
    }

   long createMinicamRunID( const time_t& aTime );
   std::string XMLCreateDate( const time_t& time );

   tm* getGMTime( const time_t& aTime );
   tm* getLocalTime( const time_t& aTime );
   void printTime( const time_t& aTime, std::ostream& aOut );
} // End util namespace.

namespace util = objects;

#endif // _UTIL_H_
