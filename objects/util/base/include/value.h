#ifndef _VALUE_H_
#define _VALUE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Labratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responisbility for the 
	use of this software.
*/

/*! 
* \file value.h
* \ingroup Objects
* \brief Value class header file.
*
*  Detailed description.
*
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
// Should only include these in debug.
#include <cassert>
#include "util/base/include/util.h"

/*! 
* \ingroup Objects
* \brief Change
* \details CHANGE
*
* \note CHANGE
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
    inline void init( const double aNewValue, const Unit aUnit = DEFAULT );
    inline void set( const double aNewValue, const Unit aUnit = DEFAULT );
    inline operator double() const;
    inline bool isInited() const;
    // XML Function here.
private:
    double mValue;
    bool mIsInit;
    Unit mUnit;
};

Value::Value(): mValue( 0 ), mIsInit( false ), mUnit( DEFAULT ){}

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

//! Check if the value has been initialized.
bool Value::isInited() const {
    return mIsInit;
}

#endif // _VALUE_H_

