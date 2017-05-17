#ifndef _VALUE_H_
#define _VALUE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
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

#if GCAM_PARALLEL_ENABLED
#include <tbb/enumerable_thread_specific.h>
#endif

/*! 
* \ingroup Objects
* \brief A class containing a single value in the model.
* \details Tracks a value, whether it has been initialized, and an associated
*          unit.
* \todo This is only a skeleton, much more to do.
* \author Josh Lurz
*/

class Value {
    friend class ManageStateVariables;
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
    /*enum Unit {
        DEFAULT,
        PETAJOULE,
		EXAJOULE
    };*/

    Value();
    explicit Value( const double aValue/*, const Unit aUnit = DEFAULT*/ );
    void init( const double aNewValue/*, const Unit aUnit = DEFAULT*/ );
    void set( const double aNewValue/*, const Unit aUnit = DEFAULT*/ );
    operator double() const;
    double get() const;
    double getDiff() const;
    bool isInited() const;
    //void setCopyState( const bool aIsStateCopy );
    Value& operator+=( const Value& aValue );
    Value& operator-=( const Value& aValue );
    Value& operator*=( const Value& aValue );
    Value& operator/=( const Value& aValue );
    Value& operator=( const Value& aValue );
    Value& operator+=( const double& aValue );
    Value& operator-=( const double& aValue );
    Value& operator*=( const double& aValue );
    Value& operator/=( const double& aValue );
    Value& operator=( const double& aDblValue );

    // XML Function here.
private:
    void print( std::ostream& aOutputStream ) const;
    std::istream& read( std::istream& aIStream );

    double mValue;
#if !GCAM_PARALLEL_ENABLED
    typedef double* AltValueType;
#else
    typedef tbb::enumerable_thread_specific<double*, tbb::cache_aligned_allocator<double*>, tbb::ets_key_per_instance> AltValueType;
#endif
    static AltValueType mAltValue;
    static double* mGoodValue;
    /*size_t*/unsigned int mAltValueIndex;
    bool mIsInit;
    //static bool* mIsPartialDeriv;
    //Unit mUnit;
    bool mIsStateCopy;
    
    //void doCheck() const;
    double& getInternal();
    const double& getInternal() const;
};

inline Value::Value(): mValue( 0 ), mIsInit( false )/*, mUnit( DEFAULT )*/, mIsStateCopy( false ){
}

/*! 
 * \brief Create a value from a double and a unit.
 * \param aValue Initial value.
 * \param aUnit Unit.
 */
inline Value::Value( const double aValue/*, const Unit aUnit*/ ):
mValue( aValue ),
mIsInit( true )/*,
mUnit( aUnit )*/,
mIsStateCopy( false )
{
}

//! Initialize the value, can only be done once.
inline void Value::init( const double aNewValue/*, const Unit aUnit*/ ){
    assert( util::isValidNumber( aNewValue ) );
    if( !mIsInit ){
        mValue = aNewValue;
        //mUnit = aUnit;
        mIsInit = true;
    }
}

inline double& Value::getInternal() {
    return mIsStateCopy ?
#if !GCAM_PARALLEL_ENABLED
        mAltValue[mAltValueIndex]
#else
        mAltValue.local()[mAltValueIndex]
#endif
        : mValue;
}

inline const double& Value::getInternal() const {
    return mIsStateCopy ?
#if !GCAM_PARALLEL_ENABLED
        mAltValue[mAltValueIndex]
#else
        mAltValue.local()[mAltValueIndex]
#endif
        : mValue;
}

//! Set the value.
inline void Value::set( const double aNewValue/*, const Unit aUnit*/ ){
    assert( util::isValidNumber( aNewValue ) );
    getInternal() = aNewValue;
    //mUnit = aUnit;
    mIsInit = true;
    //doCheck();
}

inline double Value::getDiff() const {
    assert( !mIsStateCopy );
    return getInternal() - mGoodValue[ mAltValueIndex ];
}

//! Get the value.
inline Value::operator double() const {
    return getInternal();
}

/*!
 * \brief Get the value in an unambiguous way.
 * \details Only use this function when using the Value directly gives an error
 *          or warning.
 * \return The value.
 */
inline double Value::get() const {
    return getInternal();
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
    //assert( mUnit == aValue.mUnit );
    getInternal() += aValue.getInternal();
    //doCheck();
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
    //assert( mUnit == aValue.mUnit );
    getInternal() -= aValue.getInternal();
    //doCheck();
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
    //assert( mUnit == aValue.mUnit );
    getInternal() *= aValue.getInternal();
    //doCheck();
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
    //assert( mUnit == aValue.mUnit );
    getInternal() /= aValue.getInternal();
    //doCheck();
    return *this;
}

inline Value& Value::operator=( const Value& aValue ) {
    getInternal() = aValue.getInternal();
    mIsInit = aValue.mIsInit;
    //mUnit = aValue.mUnit;
    /*
    if( aValue.mIsStateCopy ) {
        mAltValue = aValue.mAltValue;
        mAltValueIndex = aValue.mAltValueIndex;
        mIsPartialDeriv = aValue.mIsPartialDeriv;
        mIsStateCopy = aValue.mIsStateCopy;
    }*/
    
    return *this;
}

// TODO: supply these here or just make the users do it themselves
inline Value& Value::operator+=( const double& aValue ) {
    return operator+=( Value( aValue ) );
}

inline Value& Value::operator-=( const double& aValue ) {
    return operator-=( Value( aValue ) );
}

inline Value& Value::operator*=( const double& aValue ) {
    return operator*=( Value( aValue ) );
}

inline Value& Value::operator/=( const double& aValue ) {
    return operator/=( Value( aValue ) );
}

/*!
 * \breif An explicit assignment from type double which will just forward to the
 *        set() method.
 * \param aDblValue The value as double to try to set.
 * \return This value by reference for chaining.
 */
inline Value& Value::operator=( const double& aDblValue ) {
    set( aDblValue );
    
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
    aOut << getInternal();
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
        getInternal() = streamValue;
    }
    return aIStream;
}

#endif // _VALUE_H_
