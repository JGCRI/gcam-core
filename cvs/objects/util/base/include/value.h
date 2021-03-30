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
#include "util/base/include/definitions.h"

#if GCAM_PARALLEL_ENABLED
#include <tbb/enumerable_thread_specific.h>
#endif

/*! 
 * \ingroup Objects
 * \brief A class containing a single value in the model.
 * \details Tracks a value, whether it has been initialized.  It is also utilized
 *          as a layer of abstraction so that Value objects tagged as STATE (will
 *          be set during World.calc) can have it's actual data managed in a central
 *          location and can quickly be reset when needed.  Note that in addition
 *          when GCAM_PARALLEL_ENABLED it also allows the value to be set independently
 *          from different threads.  When taking both of these points together
 *          implies World.calc is thread safe and can be called without any resource
 *          contention.
 *
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

    Value();
    explicit Value( const double aValue );
    void init( const double aNewValue );
    void set( const double aNewValue );
    operator double() const;
    double get() const;
    double getDiff() const;
    bool isInited() const;
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

    //! The actual underly value of this class.
    double mValue;
    //! A flag to indicate if this Value has been set to any value besides the default.
    bool mIsInit;
#if !GCAM_PARALLEL_ENABLED
    typedef double* CentralValueType;
#else
    // When GCAM_PARALLEL_ENABLED each worker thread will have it's own slot of
    // state assigned to it.  Note it is important that we use tbb::ets_key_per_instance
    // which as it uses up a finite resource certainly qualifies as a performance
    // critical use.
    typedef tbb::enumerable_thread_specific<double*, tbb::cache_aligned_allocator<double*>, tbb::ets_key_per_instance> CentralValueType;
#endif
    //! A static reference into ManageStateVariables::mStateData only used if mIsStateCopy
    //! is true.  Note we make this field static so that we can quickly swap state
    //! between a "base" state or some "scratch" value from a central location.
    static CentralValueType sCentralValue;
    //! A static reference into the "base" state of ManageStateVariables::mStateData
    //! mostly for convenience.
    static double* sBaseCentralValue;
    //! The index into sCentralValue that contains the data for this instance.
    unsigned int mCentralValueIndex;
    //! A flag to indicate if this instance of Value has been identified as active
    //! state.  If so it can assume that mCentralValueIndex has been appropriately
    //! set and mValue gets copied in/out of sBaseCentralValue at the appropriate
    //! time.
    bool mIsStateCopy;
    
#if DEBUG_STATE
    void doStateCheck() const;
#endif
    double& getInternal();
    const double& getInternal() const;
};

inline Value::Value(): mValue( 0 ), mIsInit( false ), mIsStateCopy( false ){
}

/*! 
 * \brief Create a value from a double and a unit.
 * \param aValue Initial value.
 * \param aUnit Unit.
 */
inline Value::Value( const double aValue ):
mValue( aValue ),
mIsInit( true ),
mIsStateCopy( false )
{
}

//! Initialize the value, can only be done once.
inline void Value::init( const double aNewValue ){
    assert( util::isValidNumber( aNewValue ) );
    if( !mIsInit ){
        mValue = aNewValue;
        mIsInit = true;
    }
}

/*!
 * \brief An accessor method to get at the actual data held in this class.
 * \details This method will appropriately get the value locally or the centrally
 *          managed state if the mIsStateCopy flag is set.
 * \return A reference the the appropriate value represented by this class.
 */
inline double& Value::getInternal() {
    return mIsStateCopy ?
#if !GCAM_PARALLEL_ENABLED
        sCentralValue[mCentralValueIndex]
#else
        sCentralValue.local()[mCentralValueIndex]
#endif
        : mValue;
}

/*!
 * \brief An accessor method (const) to get at the actual data held in this class.
 * \details This method will appropriately get the value locally or the centrally
 *          managed state if the mIsStateCopy flag is set.
 * \return A const reference the the appropriate value represented by this class.
 */
inline const double& Value::getInternal() const {
    return mIsStateCopy ?
#if !GCAM_PARALLEL_ENABLED
        sCentralValue[mCentralValueIndex]
#else
        sCentralValue.local()[mCentralValueIndex]
#endif
        : mValue;
}

//! Set the value.
inline void Value::set( const double aNewValue ){
    assert( util::isValidNumber( aNewValue ) );
    getInternal() = aNewValue;
    mIsInit = true;
#if DEBUG_STATE
    doStateCheck();
#endif
}

/*!
 * \brief Get the difference in value between the current value of this class and
 *        the value of this class in the "base" state.
 * \details This method is helpful when adding to supply/demand of a market when
 *          we are calculating partial derivatives.
 * \return The difference in value between the current value of this class and
 *         the value of this class in the "base" state
 * \warning This method is only valid for instances that are mIsStateCopy.
 */
inline double Value::getDiff() const {
    assert( !mIsStateCopy );
    return getInternal() - sBaseCentralValue[ mCentralValueIndex ];
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

    getInternal() += aValue.getInternal();
#if DEBUG_STATE
    doStateCheck();
#endif
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

    getInternal() -= aValue.getInternal();
#if DEBUG_STATE
    doStateCheck();
#endif
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

    getInternal() *= aValue.getInternal();
#if DEBUG_STATE
    doStateCheck();
#endif
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

    getInternal() /= aValue.getInternal();
#if DEBUG_STATE
    doStateCheck();
#endif
    return *this;
}

inline Value& Value::operator=( const Value& aValue ) {
    getInternal() = aValue.getInternal();
    mIsInit = aValue.mIsInit;
    
    return *this;
}

inline Value& Value::operator+=( const double& aValue ) {
    // Assume that if this value is not initialized that adding to zero is
    // correct and the new value is valid.
    mIsInit = true;
    
    getInternal() += aValue;
#if DEBUG_STATE
    doStateCheck();
#endif
    return *this;
}

inline Value& Value::operator-=( const double& aValue ) {
    // Assume that if this value is not initialized that adding to zero is
    // correct and the new value is valid.
    mIsInit = true;
    
    getInternal() -= aValue;
#if DEBUG_STATE
    doStateCheck();
#endif
    return *this;
}

inline Value& Value::operator*=( const double& aValue ) {
    // Assume that if this value is not initialized that adding to zero is
    // correct and the new value is valid.
    mIsInit = true;
    
    getInternal() *= aValue;
#if DEBUG_STATE
    doStateCheck();
#endif
    return *this;
}

inline Value& Value::operator/=( const double& aValue ) {
    // Assume that if this value is not initialized that adding to zero is
    // correct and the new value is valid.
    mIsInit = true;
    
    getInternal() /= aValue;
#if DEBUG_STATE
    doStateCheck();
#endif
    return *this;
}

/*!
 * \brief An explicit assignment from type double which will just forward to the
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
