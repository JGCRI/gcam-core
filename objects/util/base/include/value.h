#ifndef _VALUE_H_
#define _VALUE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Laboratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responsibility for the 
	use of this software.
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
public:
    enum Unit {
        DEFAULT,
        PETAJOULE,
		EXAJOULE
    };

    inline Value();
    inline Value( const double aValue, const Unit aUnit = DEFAULT );
    inline void init( const double aNewValue, const Unit aUnit = DEFAULT );
    inline void set( const double aNewValue, const Unit aUnit = DEFAULT );
    inline operator double() const;
    inline double get() const;
    inline bool isInited() const;
    inline Value& operator+=( const Value& aValue );
    inline Value& operator-=( const Value& aValue );
    // XML Function here.
private:
    double mValue;
    bool mIsInit;
    Unit mUnit;
};

Value::Value(): mValue( 0 ), mIsInit( false ), mUnit( DEFAULT ){}

/*! 
 * \brief Create a value from a double and a unit.
 * \param aValue Initial value.
 * \param aUnit Unit.
 */
Value::Value( const double aValue, const Unit aUnit ):
mValue( aValue ),
mIsInit( true ),
mUnit( aUnit )
{
}

//! Initialize the value, can only be done once.
void Value::init( const double aNewValue, const Unit aUnit ){
    assert( util::isValidNumber( aNewValue ) );
    if( !mIsInit ){
        mValue = aNewValue;
        mUnit = aUnit;
        mIsInit = true;
    }
}

//! Set the value.
void Value::set( const double aNewValue, const Unit aUnit ){
    assert( util::isValidNumber( aNewValue ) );
    mValue = aNewValue;
    mUnit = aUnit;
    mIsInit = true;
}

//! Get the value.
Value::operator double() const {
    return mValue;
}

//! Get the value. Only use this function to resolve ambiguities.
double Value::get() const {
    return mValue;
}

/*! 
 * \brief Increment the value by the amount contained in another value.
 * \param aValue Value containing the amount to add.
 * \return This value by reference for chaining.
 */
Value& Value::operator+=( const Value& aValue ){
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
Value& Value::operator-=( const Value& aValue ){
    // Assume that if this value is not initialized that subtracting from zero
    // is correct and the new value is valid.
    mIsInit = true;

    // Make sure the units match up.
    assert( mUnit == aValue.mUnit );
    mValue -= aValue.mValue;
    return *this;
}

//! Check if the value has been initialized.
bool Value::isInited() const {
    return mIsInit;
}

#endif // _VALUE_H_

