#ifndef _VALUE_H_
#define _VALUE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial
 * Institute. Battelle has certain unperfected rights in the software which
 * should not be copied or otherwise disseminated outside your organization
 * without the express written authorization from Battelle. All rights to
 * the software are reserved by Battelle.  Battelle makes no warranty,
 * express or implied, and assumes no liability or responsibility for the use of
 * this software.
 */

/*! 
* \file value.h
* \ingroup Objects
* \brief Value class header file.
*
* \author Josh Lurz
*/
// Should only include these in debug.
#include <cassert>
#include "util/base/include/util.h"

/*! 
* \ingroup Objects
* \brief A class containing a single value in the model.
* \details Tracks a value, whether it has been initialized, and an associated
*          unit.
* \todo This is only a skeleton, much more to do.
* \author Josh Lurz
*/

class Value
{
    /*!
     * \brief Output stream operator to print a Value.
     * \details Output stream operators allow classes to be printed using the <<
     *          operator. This function must be defined in the global namespace.
     * \param aOut Output stream into which to print.
     * \param aValue Value to print.
     * \return aOut for chaining.
     */
    friend std::ostream& operator<<( std::ostream& aOut, const Value& aValue ){
        aValue.print( aOut );
        return aOut;
    }

    /*!
     * \brief Input operator to read doubles into a Value.
     * \param aIStream Input stream to read from.
     * \param aValue Value to read into.
     * \return Input stream for chaining.
     */
     friend std::istream& operator>>( std::istream& aIStream, Value& aValue ){
         return aValue.read( aIStream );
     }

public:
    enum Unit {
        DEFAULT,
        PETAJOULE,
		EXAJOULE
    };

    Value();
    Value( const double aValue, const Unit aUnit = DEFAULT );
    void init( const double aNewValue, const Unit aUnit = DEFAULT );
    void set( const double aNewValue, const Unit aUnit = DEFAULT );
    operator double() const;
    double get() const;
    bool isInited() const;
    Value& operator+=( const Value& aValue );
    Value& operator-=( const Value& aValue );
    Value& operator*=( const Value& aValue );
    Value& operator/=( const Value& aValue );

    // XML Function here.
private:
    void print( std::ostream& aOutputStream ) const;
    std::istream& read( std::istream& aIStream );

    double mValue;
    bool mIsInit;
    Unit mUnit;
};

inline Value::Value(): mValue( 0 ), mIsInit( false ), mUnit( DEFAULT ){}

/*! 
 * \brief Create a value from a double and a unit.
 * \param aValue Initial value.
 * \param aUnit Unit.
 */
inline Value::Value( const double aValue, const Unit aUnit ):
mValue( aValue ),
mIsInit( true ),
mUnit( aUnit )
{
}

//! Initialize the value, can only be done once.
inline void Value::init( const double aNewValue, const Unit aUnit ){
    assert( util::isValidNumber( aNewValue ) );
    if( !mIsInit ){
        mValue = aNewValue;
        mUnit = aUnit;
        mIsInit = true;
    }
}

//! Set the value.
inline void Value::set( const double aNewValue, const Unit aUnit ){
    assert( util::isValidNumber( aNewValue ) );
    mValue = aNewValue;
    mUnit = aUnit;
    mIsInit = true;
}

//! Get the value.
inline Value::operator double() const {
    return mValue;
}

/*!
 * \brief Get the value in an unambigous way.
 * \details Only use this function when using the Value directly gives an error
 *          or warning.
 * \return The value.
 */
double Value::get() const {
    return mValue;
}

/*! 
 * \brief Increment the value by the amount contained in another value.
 * \param aValue Value containing the amount to add.
 * \return This value by reference for chaining.
 */
inline Value& Value::operator+=( const Value& aValue ){
    // Assume that if this value is not initialized that adding to zero is
    // correct and the new value is valid.
    mIsInit = true;

    // Make sure the units match up.
    assert( mUnit == aValue.mUnit );
    mValue += aValue.mValue;
    return *this;
}

/*! 
 * \brief Decrement the value by the amount contained in another value.
 * \param aValue Value containing the amount to subtract.
 * \return This value by reference for chaining.
 */
inline Value& Value::operator-=( const Value& aValue ){
    // Assume that if this value is not initialized that subtracting from zero
    // is correct and the new value is valid.
    mIsInit = true;

    // Make sure the units match up.
    assert( mUnit == aValue.mUnit );
    mValue -= aValue.mValue;
    return *this;
}

/*! 
 * \brief Multiple the value by the amount contained in another value.
 * \param aValue Value containing the amount by which to multiply.
 * \return This value by reference for chaining.
 */
inline Value& Value::operator*=( const Value& aValue ){
    // If the value hasn't been initialized it should not be used.
    assert( mIsInit );

    // Make sure the units match up.
    assert( mUnit == aValue.mUnit );
    mValue *= aValue.mValue;
    return *this;
}

/*! 
 * \brief Divide the value by the amount contained in another value.
 * \param aValue Value containing the divisor.
 * \return This value by reference for chaining.
 */
inline Value& Value::operator/=( const Value& aValue ){
    // If the value hasn't been initialized it should not be used.
    assert( mIsInit );
    assert( aValue > util::getSmallNumber() );

    // Make sure the units match up.
    assert( mUnit == aValue.mUnit );
    mValue /= aValue.mValue;
    return *this;
}

//! Check if the value has been initialized.
inline bool Value::isInited() const {
    return mIsInit;
}

/*!
 * \brief Print the value.
 * \details Helper function for the global << operator which prints the Value
 *          into the output stream.
 * \param aOut Output stream into which to print.
 */
inline void Value::print( std::ostream& aOut ) const {
    aOut << mValue;
}

/*!
 * \brief Helper function to read a Value from a stream.
 * \param aIStream Input stream to read from.
 * \return The input stream for chaining.
 */
inline std::istream& Value::read( std::istream& aIStream ){
    double streamValue;
    if( aIStream >> streamValue ) {
        mIsInit = true;
        mValue = streamValue;
    }
    return aIStream;
}

#endif // _VALUE_H_

