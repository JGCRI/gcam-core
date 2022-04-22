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

private:
    void print( std::ostream& aOutputStream ) const;
    std::istream& read( std::istream& aIStream );

    /*!
     * \brief The underlying value of this class which can be interpreted as potentially
     *        the double value of this class or an index into the sCentralValue.
     * \details We take advantage of several facts that lets us encode several pieces of information into a single block of 64 bits of memory.  In principle this class has four member variables:
     *  - bool mIsInit If the numerical value of this class has been set in *any* way. As
     *    soon as *any* value is set mIsInit is considered true.  Note: If a user accesses
     *    the value while uninitialized they get a zero value.
     *  - bool mIsStateCopy If this instance has been tagged STATE and ManageStateVariables
     *    has determined it should now be actively managed.
     *  - int mCentralValueIndex If mIsStateCopy is true this would be the index to use to
     *    lookup the actual value this class represents (in the context of the current thread).
     *    If mIsStateCopy is false the value of mCentralValueIndex is undefined.
     *  - mValue The double value this class represents *if* mIsStateCopy is false.  Otherwise
     *    accessing mValue is undefined.
     *
     *  Given the above it is clear only some of these member variables are active at any 
     *  time.  Using this and the fact that the IEE standard for floating point numbers
     *  allows us to encode additional information into NaN values we set up the following
     *  scheme to condense all of these member variables into a single 64 bit value:
     *  - We initialize mBits to UNINITIALIZED which is equivalent to -0.  Which maintains
     *    the property that if a user accessed it they will get a zero value.  And (in
     *    practice) a user would never attempt to actually initialize to such a value.
     *  - If mIsStateCopy is meant to be true we set the first 32 bits to true (STATE_COPY_MASK)
     *    which qualifies mBits as a NaN value but is distinct from any double value which
     *    is possible as a result of any calculation.  We then encode mCentralValueIndex into
     *    the last 32 bits (ID_MASK).  Note, this implies we can only address 2^32 values
     *    as STATE and checks are made in ManageStateVariables accordingly (not that we are
     *    even close to that many).
     *  - Finally if mBits does not start with the first 32 bits as true we can assume it
     *    is just mValue and we convert the bits in mBits to a double value (see convertToDouble).
     *
     * \warning The internals of this class is heavily optimized for speed and to conserve
     *          memory.  Users should avoid accessing mBits directly and instead utilize
     *          the public methods which will ensure the appropriate values are derived.
     */
    uint64_t mBits;

    // some static constants that help us determine how to interpret mBits

    //! The value when mBits is !isInited
    static const uint64_t UNINITIALIZED = 0x8000000000000000;

    //! The bit mask which if matches mBits indicates this is an active STATE Value
    static const uint64_t STATE_COPY_MASK = 0xffffffff00000000;

    //! A bit mask to help us extract out the look up index into sCentralValue
    static const uint64_t ID_MASK = 0x00000000ffffffff;

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
    
#if DEBUG_STATE
    void doStateCheck() const;
#endif
    double getInternal() const;
    void setInternal(double const aDblValue);

    /*!
     * \brief A utility method to convert from 64 bits of data to a double.
     * \details It is a bit more tricky than one might naively think.  Namely you can not 
     *          just reinterpret the bits as double which is un-allowed by the C++ standard.
     *          The reason being floating point values have different registers on the CPU
     *          thus a register move operation may be needed. The solution from:
     *          https://stackoverflow.com/questions/10351150/how-can-you-convert-a-stdbitset64-to-a-double
     *          And as noted there, while it looks like a bunch of copying overhead will be
     *          incurred in reality the compiler is going to optimize it all down to nothing.
     * \param aBits The bits to convert to double.
     * \return The bits converted to double.
     */
    static double convertToDouble(uint64_t const aBits) {
        static_assert(sizeof(uint64_t) == sizeof(double), "Expecting size of double to be 64 bits.");

        double asDouble;

        // Aliases to `char*` are explicitly allowed in the Standard (and only them)
        char const* bitsAsChar = reinterpret_cast<char const*>(&aBits);
        char* doubleAsChar = reinterpret_cast<char*>(&asDouble);

        // Copy the bitwise representation from bits to double
        memcpy(doubleAsChar, bitsAsChar, sizeof(aBits));

        return asDouble;
    }

    /*!
     * \brief convert a double to its "bits" representation.
     * \details Converts the double to a 64 bit unsigned int.  The process is a bit
     *          more complicated than one might expect.  See convertToDouble for
     *          details.
     * \param aDouble The double value to convert.
     * \return The "bits" of the given double.
     * \sa convertToDouble
     */
    static uint64_t convertToBits(double const aDouble) {
        static_assert(sizeof(uint64_t) == sizeof(double), "Expecting size of double to be 64 bits.");

        uint64_t asBits;

        // Aliases to `char*` are explicitly allowed in the Standard (and only them)
        char const* doubleAsChar = reinterpret_cast<char const*>(&aDouble);
        char* bitsAsChar = reinterpret_cast<char*>(&asBits);

        // Copy the bitwise representation from double to bits
        memcpy(bitsAsChar, doubleAsChar, sizeof(aDouble));

        return asBits;
    }
};

inline Value::Value(): mBits(UNINITIALIZED) {
}

/*! 
 * \brief Create a value from a double and a unit.
 * \param aValue Initial value.
 */
inline Value::Value( const double aValue ):
mBits(convertToBits(aValue))
{
}

//! Initialize the value, can only be done once.
inline void Value::init( const double aNewValue ){
    assert( util::isValidNumber( aNewValue ) );
    if( !isInited() ){
        mBits = convertToBits(aNewValue);
    }
}

/*!
 * \brief An accessor method to get at the actual data held in this class.
 * \details This method will appropriately get the value locally or the centrally
 *          managed state if the mIsStateCopy flag is set.
 * \return The appropriate double value represented by this class.
 */
inline double Value::getInternal() const {
    return (mBits & STATE_COPY_MASK) == STATE_COPY_MASK ?
#if !GCAM_PARALLEL_ENABLED
        sCentralValue[ID_MASK & mBits]
#else
        sCentralValue.local()[ID_MASK & mBits]
#endif
        : convertToDouble(mBits);
}


/*!
 * \brief A setter method to set the actual data held in this class.
 * \details This method will appropriately set the value locally or the centrally
 *          managed state if the mIsStateCopy flag is set.
 * \param aDblValue The double value that needs to be represented by this class.
 */
inline void Value::setInternal(double const aDblValue) {
    if((mBits & STATE_COPY_MASK) == STATE_COPY_MASK) {
#if !GCAM_PARALLEL_ENABLED
        sCentralValue[ID_MASK & mBits] = aDblValue;
#else
        sCentralValue.local()[ID_MASK & mBits] = aDblValue;
#endif
    }
    else {
        mBits = convertToBits(aDblValue);
    }
}

//! Set the value.
inline void Value::set( const double aNewValue ){
    assert( util::isValidNumber( aNewValue ) );
    setInternal(aNewValue);
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
    assert( (mBits & STATE_COPY_MASK) == STATE_COPY_MASK );
    return getInternal() - sBaseCentralValue[ ID_MASK & mBits ];
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

    setInternal(getInternal() + aValue.getInternal());
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

    setInternal(getInternal() - aValue.getInternal());
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
    assert( isInited() );

    setInternal(getInternal() * aValue.getInternal());
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
    assert( isInited() );
    assert( aValue > util::getSmallNumber() );

    setInternal(getInternal() / aValue.getInternal());
#if DEBUG_STATE
    doStateCheck();
#endif
    return *this;
}

inline Value& Value::operator=( const Value& aValue ) {
    setInternal(aValue.getInternal());
    
    return *this;
}

inline Value& Value::operator+=( const double& aValue ) {
    // Assume that if this value is not initialized that adding to zero is
    // correct and the new value is valid.
    
    setInternal(getInternal() + aValue);
#if DEBUG_STATE
    doStateCheck();
#endif
    return *this;
}

inline Value& Value::operator-=( const double& aValue ) {
    // Assume that if this value is not initialized that adding to zero is
    // correct and the new value is valid.
    
    setInternal(getInternal() - aValue);
#if DEBUG_STATE
    doStateCheck();
#endif
    return *this;
}

inline Value& Value::operator*=( const double& aValue ) {
    // Assume that if this value is not initialized that adding to zero is
    // correct and the new value is valid.
    
    setInternal(getInternal() * aValue);
#if DEBUG_STATE
    doStateCheck();
#endif
    return *this;
}

inline Value& Value::operator/=( const double& aValue ) {
    // Assume that if this value is not initialized that adding to zero is
    // correct and the new value is valid.
    
    setInternal(getInternal() / aValue);
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
    setInternal( aDblValue );
#if DEBUG_STATE
    doStateCheck();
#endif
    
    return *this;
}

//! Check if the value has been initialized.
inline bool Value::isInited() const {
    return mBits != UNINITIALIZED;
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
        setInternal(streamValue);
    }
    return aIStream;
}

#endif // _VALUE_H_
