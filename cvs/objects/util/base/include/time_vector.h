#ifndef _TIME_VECTOR_H_
#define _TIME_VECTOR_H_
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


#include <cassert>
#include <algorithm>
#include <vector>
#include <boost/iterator/iterator_adaptor.hpp>

// TODO: Reduce these includes
#include "util/base/include/model_time.h"
#include "containers/include/scenario.h"
#include "util/base/include/util.h"

extern Scenario* scenario;

template<typename T>
class TechVectorParseHelper;

/*! 
* \file time_vector.h  
* \ingroup util
* \brief Header file for the TimeVector class.
* \author Josh Lurz
*/
namespace objects {
    // Need to inject the util namespace into the objects namespace.
    
    /*!
     * \brief Base class of vectors indexed by year or period.
     * \details Provides common code for year and period vectors.
     */
    template<class T>
    class TimeVectorBase {
    public:

        /*!
         * \brief A random access iterator so we can utilize time vectors in
         *        STL algorithms.
         * \details Given all we really need to do is create an iterators over
         *          essentially a simple array we can utilize boost::iterator_adaptor
         *          to do all the work.  In this way we do not need to worry about
         *          the subtle requirements, and shifting requirements with subsequent
         *          C++ standards, of what the STL requires an iterator to be.
         *
         *          This implementation is basically straight out of the boost documentation's
         *          examples.  Again given our simple need we need to make no specializations
         *          of the boost::iterator_adaptor base class.
         *
         *          Note we make the "value" type templated so that we can automatically
         *          generate both the mutable (iterator) and non-mutable (const_iterator)
         *          types.
         */
        template<class Value>
        class TimeVectorBaseIter : public boost::iterator_adaptor<
                                       TimeVectorBaseIter<Value>,
                                       Value*,
                                       boost::use_default,
                                       boost::random_access_traversal_tag>
        {
            private:
                // helper struct for the enable_if below
                // a private type avoids misuse
                struct enabler {};

            public:
                /*!
                 * \brief Default constructor.
                 */
                TimeVectorBaseIter(): TimeVectorBaseIter::iterator_adaptor_( 0 ) { }

                /*!
                 * \brief Constructor which simply points to the memory location this
                 *        iterator should reference.
                 * \param aData the memory location this iterator points to.
                 */
                explicit TimeVectorBaseIter( Value* aData ): TimeVectorBaseIter::iterator_adaptor_( aData )
                {
                }

                /*!
                 * \brief An "interoperability" constructor, essentially to allow us to
                 *        downgrade an iterator to const_interator.
                 * \details The syntax is quite complicated due to the use of the enable_if
                 *          the purpose of which is to only allow non-const to const and
                 *          not the other way around.
                 * \param aOther The iterator to downgrade.
                 */
                template <class OtherValue>
                TimeVectorBaseIter( TimeVectorBaseIter<OtherValue> const& aOther
                  , typename boost::enable_if<
                    boost::is_convertible<OtherValue*,Value*>
                  , enabler
                >::type = enabler() ): TimeVectorBaseIter::iterator_adaptor_( aOther.base() ) {}
        };

        // Generate the actual iterator types by templating just T for the mutable iterator
        // and T const for the non-mutable iterator.
        typedef TimeVectorBaseIter<T> iterator;
        typedef TimeVectorBaseIter<T const> const_iterator;


        TimeVectorBase( const unsigned int aSize, const T aDefaultValue );
        virtual ~TimeVectorBase();
        TimeVectorBase( const TimeVectorBase& aOther );
        TimeVectorBase& operator=( const TimeVectorBase& aOther );

        bool operator==( TimeVectorBase& aOther ) const;
        
        bool operator!=( TimeVectorBase& aOther ) const;

        virtual T& operator[]( const size_t aIndex ) = 0;
        virtual const T& operator[]( const size_t aIndex ) const = 0;

        size_t size() const;
        void assign( const size_t aPositions, const T& aValue );
        const_iterator begin() const;
        const_iterator end() const;
        const_iterator last() const;
        iterator begin();
        iterator end();
        iterator last();
        typedef T value_type;
    protected:
        //! Dynamic array containing the data.
        T* mData;

        //! Size of the array.
        size_t mSize;
    private:
        void init( const unsigned int aSize,
                   const T aDefaultValue );

        void clear();
    };
    

    /*!
     * \brief Constructor.
     * \param aSize Size of the TimeVectorBase. The size is immutable once
     *              constructed.
     * \param aDefaultValue Default for all values.
     */
    template<class T>
        TimeVectorBase<T>::TimeVectorBase( const unsigned int aSize,
                                           const T aDefaultValue )
    {
            init( aSize, aDefaultValue );
    }

    /*!
     * \brief Destructor which deallocates the array.
     */
    template<class T>
        TimeVectorBase<T>::~TimeVectorBase(){
            clear();
        }

    /*!
     * \brief Private member function to deallocate the memory.
     */
   template<class T>
       void TimeVectorBase<T>::clear(){
            delete[] mData;
       }

   /*! 
    * \brief Initialize the TimeVectorBase.
     * \param aSize Size of the TimeVectorBase. The size is immutable once
     *              constructed.
     * \param aDefaultValue Default for all values.
    */
   template<class T>
       void TimeVectorBase<T>::init( const unsigned int aSize,
                                     const T aDefaultValue )
   {
           mSize = aSize;
           mData = new T[ mSize ];

           // Initialize the data to the default value.
           std::uninitialized_fill( &mData[ 0 ], &mData[ 0 ] + mSize, aDefaultValue );
    }

    /*!
     * \brief Copy constructor.
     * \param aOther TimeVectorBase to copy.
     */
    template<class T>
        TimeVectorBase<T>::TimeVectorBase( const TimeVectorBase<T>& aOther ){
            init( aOther.mSize, T() );
            std::copy( aOther.begin(), aOther.end(), begin() );
        }

    /*!
     * \brief Assignment operator.
     * \param aOther TimeVectorBase to copy.
     * \return The newly constructed TimeVectorBase by reference(for chaining
     *         assignment).
     */
    template<class T>
        TimeVectorBase<T>& TimeVectorBase<T>::operator=( const TimeVectorBase<T>& aOther ){
            // Check for self-assignment.
            if( this != &aOther ){
                clear();
                init( aOther.size(), T() );
                std::copy( aOther.begin(), aOther.end(), begin() );
            }
            return *this;
        }

    /*!
     * \brief Equals operator.
     * \param aOther TimeVectorBase to check for equivalence.
     * \details TimeVectorBases are equivalent if they have the same size and
     *          all elements at respective positions are equal.
     * \return Whether the two vectors are equal.
     */
    template<class T>
        bool  TimeVectorBase<T>::operator==( TimeVectorBase& aOther ) const {
            return size() == aOther.size() && equal( begin(), end(), aOther.begin() );
        }

    /*!
     * \brief Not-equals operator.
     * \param aOther TimeVectorBase to check for dis-equivalence.
     * \details TimeVectorBases are not equivalent if they have the different
     *          sizes or all elements at respective positions are not equal.
     * \return Whether the two vectors are equal.
     */      
    template<class T>
        bool  TimeVectorBase<T>::operator!=( TimeVectorBase& aOther ) const {
            return !( *this == aOther );
        }

   /*!
    * \brief Return the size of the vector.
    * \return Size of the vector.
    */
    template<class T>
        size_t TimeVectorBase<T>::size() const {
            return mSize;
        }

    /*!
     * \brief Assign a single value to a number of positions in the vector
     *        start at position zero.
     * \param aPositions Number of positions to assign the value to.
     * \param aValue Value to assign to each position.
     */
    template<class T>
        void TimeVectorBase<T>::assign( const size_t aPositions, const T& aValue ){
            assert( aPositions <= size() );
            for( unsigned int i = 0; i < aPositions; ++i ){
                mData[ i ] = aValue;
            }
        }

   /*!
    * \brief Return the constant iterator to the first position in the vector.
    * \return Constant iterator to the first position.
    */
   template<class T>
       typename TimeVectorBase<T>::const_iterator TimeVectorBase<T>::begin() const {
           return typename TimeVectorBase<T>::const_iterator( mData );
       }

   /*!
    * \brief Return the constant iterator to the position past the last position
    *        in the vector.
    * \return Constant iterator to one position past the last position.
    */
   template<class T>
       typename TimeVectorBase<T>::const_iterator TimeVectorBase<T>::end() const {
           return typename TimeVectorBase<T>::const_iterator( mData + size() );
       }

   /*!
    * \brief Return the constant iterator to the last position in the vector.
    * \return Constant iterator to the last position
    */
   template<class T>
       typename TimeVectorBase<T>::const_iterator TimeVectorBase<T>::last() const {
           // Return the last position. If the vector is empty make sure this is
           // the zeroth position.
           return typename TimeVectorBase<T>::const_iterator( mData + std::max( int( size() ) - 1, 0 ) );
       }

  /*!
   * \brief Return a mutable iterator to the first position in the vector.
   * \return Mutable iterator to the first position.
   */
   template<class T>
       typename TimeVectorBase<T>::iterator TimeVectorBase<T>::begin() {
           return typename TimeVectorBase<T>::iterator( mData );
       }

   /*!
    * \brief Return a mutable iterator to the position past the last position in
    *        the vector.
    * \return Mutable iterator to one position past the last position.
    */
   template<class T>
       typename TimeVectorBase<T>::iterator TimeVectorBase<T>::end() {
           return typename TimeVectorBase<T>::iterator( mData + size() );
       }
    
   /*!
    * \brief Return a mutable iterator to the last position in the vector.
    * \return Mutable iterator to the last position
    */
   template<class T>
       typename TimeVectorBase<T>::iterator TimeVectorBase<T>::last() {
           // Return the last position. If the vector is empty make sure this is
           // the zeroth position.
           return typename TimeVectorBase<T>::iterator( mData + std::max( static_cast<int>( size() ) - 1, 0 ) );
       }

   /*
    * \brief YearVector is a fixed size vector which contains a value for each
    *        year within a range, and is indexed by year.
    * \details A YearVector is initialized with a start and end year and is
    *          constructed so that it has a value for each year between and
    *          including the start and end years. The size and end points of the
    *          YearVector cannot be changed once it is initialized. The
    *          YearVector is indexed by year, not vector position. Indexing
    *          outside the start and end years is invalid.
    */
   template<class T>
   class YearVector: public TimeVectorBase<T> {
   public:
        using TimeVectorBase<T>::begin;
        using TimeVectorBase<T>::end;
        using TimeVectorBase<T>::last;
        using TimeVectorBase<T>::size;
        using TimeVectorBase<T>::assign;
        using typename TimeVectorBase<T>::const_iterator;
        using typename TimeVectorBase<T>::iterator;

        YearVector( const unsigned int aStartYear,
                    const unsigned int aEndYear,
                    const T aDefaultValue = T() );

        const YearVector& operator=( const YearVector& aOther );

        virtual T& operator[]( const size_t aIndex );
        virtual const T& operator[]( const size_t aIndex ) const;
        typename TimeVectorBase<T>::const_iterator find( const unsigned int aIndex ) const;
        typename TimeVectorBase<T>::iterator find( const unsigned int aIndex );

        const unsigned int getStartYear() const;
        const unsigned int getEndYear() const;

   protected:
        //! First year of the array. This year is included in the array.
        unsigned int mStartYear;

        //! End year of the array. This year is included in the array.
        unsigned int mEndYear;
        
        // Declare that this class is using several base class members.
        using TimeVectorBase<T>::mData;
        using TimeVectorBase<T>::mSize;
   };

    /*!
     * \brief Constructor which sizes the vector to the specied number of years.
     * \param aStartYear First year of the array.
     * \param aEndYear End year of the array.
     * \param aDefaultValue Default value for each year. This argument is
     *        optional and defaults to the value created by the default
     *        constructor of the type.
     */
    template<class T>
    YearVector<T>::YearVector( const unsigned int aStartYear,
                               const unsigned int aEndYear,
                               const T aDefaultValue )
                               : TimeVectorBase<T>( aEndYear - aStartYear + 1,
                                                    aDefaultValue ),
                                 mStartYear( aStartYear ),
                                 mEndYear( aEndYear )
    {
    }

    /*!
     * \brief Assignment operator.
     * \param aOther YearVector to copy.
     * \return The newly constructed YearVector by reference(for chaining
     *         assignment).
     */
    template<class T>
    const YearVector<T>& YearVector<T>::operator=( const YearVector<T>& aOther ){
            // Check for self-assignment.
            if( this != &aOther ){
                mStartYear = aOther.mStartYear;
                mEndYear = aOther.mEndYear;
                TimeVectorBase<T>::operator=( aOther );
            }
            return *this;
        }

    /*!
     * \brief Operator which references data in the array.
     * \param aYear Year of the value to return.
     * \return Mutable value at the year by reference.
     */
    template<class T>
        T& YearVector<T>::operator[]( const size_t aYear ){
            /*! \pre The index must be between the start year and end year
            *        inclusive. 
            */
            assert( aYear >= mStartYear && aYear <= mEndYear );
            assert( isValidNumber( mData[ aYear - mStartYear ] ) );
            return mData[ aYear - mStartYear ];
        }
    
    /*!
     * \brief Operator which references data in the array.
     * \param aYear Year of the value to return.
     * \return Constant value at the year by reference.
     */
    template<class T>
        const T& YearVector<T>::operator[]( const size_t aYear ) const {
            /*! \pre The index must be between the start year and end year
            *        inclusive. 
            */
            assert( aYear >= mStartYear && aYear <= mEndYear );
            assert( isValidNumber( mData[ aYear - mStartYear ] ) );
            return mData[ aYear - mStartYear ];
        }

    /*!
     * \brief Find a specific year in the vector and return a constant iterator
     *        to it.
     * \param aYear Year for which to return a constant iterator.
     * \return Constant iterator for the year, the end iterator if it is not
     *         found.
     */
    template<class T>
        typename TimeVectorBase<T>::const_iterator YearVector<T>::find( const unsigned int aYear ) const {
            // Check if the year is valid.
            if( aYear < mStartYear || aYear > mEndYear ){
                return end();
            }
            // Return an iterator to the year.
            return typename TimeVectorBase<T>::const_iterator( aYear - mStartYear );
        }

    /*!
     * \brief Find a specific year in the vector and return a mutable iterator
     *        to it.
     * \param aYear Year for which to return a mutable iterator.
     * \return Mutable iterator for the year, the end iterator if it is not
     *         found.
     */
    template<class T>
        typename TimeVectorBase<T>::iterator YearVector<T>::find( const unsigned int aYear ) {
            // Check if the year is valid.
            if( aYear < mStartYear || aYear > mEndYear ){
                return end();
            }
            // Return an iterator to the year.
            return typename TimeVectorBase<T>::iterator( aYear - mStartYear, this );
        }

    /*!
     * \brief Get the start year of the YearVector.
     * \return The start year.
     */
    template<class T>
        const unsigned int YearVector<T>::getStartYear() const {
            return mStartYear;
        }

    /*!
     * \brief Get the end year of the YearVector.
     * \return The end year.
     */
    template<class T>
        const unsigned int YearVector<T>::getEndYear() const {
            return mEndYear;
        }

    /*!
     * \brief Array which when constructed automatically sizes to the maximum
     *          number of periods.
     * \details Array which when created will automatically have one position
     *          per period as defined by the Modeltime object.
     * \note This class is especially useful when using arrays by period within
     *       maps, since maps automatically construct their elements.
     */
    template<class T>
    class PeriodVector: public TimeVectorBase<T> {
    public:
        using typename TimeVectorBase<T>::const_iterator;
        using typename TimeVectorBase<T>::iterator;
        using TimeVectorBase<T>::begin;
        using TimeVectorBase<T>::end;
        using TimeVectorBase<T>::last;
        using TimeVectorBase<T>::size;
        using TimeVectorBase<T>::assign;

        PeriodVector( const T aDefaultValue = T() );
        virtual T& operator[]( const size_t aIndex );
        virtual const T& operator[]( const size_t aIndex ) const;
    protected:
        // Declare that this class is using the base class data and size.
        using TimeVectorBase<T>::mData;
        using TimeVectorBase<T>::mSize;
    };

    /*!
     * \brief Constructor which sizes the vector to the number of periods in the
     *        model.
     * \param aDefaultValue Default value for each year. This argument is
     *        optional and defaults to the value created by the default
     *        constructor of the type.
     */
    template<class T>
        PeriodVector<T>::PeriodVector( const T aDefaultValue )
        :TimeVectorBase<T>( scenario->getModeltime()->getmaxper(),
                            aDefaultValue )
    {
    }

    /*!
     * \brief Operator which references data in the array.
     * \param aIndex Index of the value to return.
     * \return Mutable value at the index by reference.
     */
    template<class T>
        T& PeriodVector<T>::operator[]( const size_t aIndex ){
            assert( aIndex < size() );
            assert( isValidNumber( mData[ aIndex ] ) );
            return mData[ aIndex ];
        }
    
    /*!
     * \brief Operator which references data in the array.
     * \param aIndex Index of the value to return.
     * \return Constant value at the index by reference.
     */
    template<class T>
        const T& PeriodVector<T>::operator[]( const size_t aIndex ) const {
            assert( aIndex < size() );
            assert( isValidNumber( mData[ aIndex ] ) );
            return mData[ aIndex ];
        }
    
    /*!
     * \brief Base class of vectors indexed by period that will automatically get resized
     *        to only include enough space for however many years an encolsing technology
     *        will operate.
     * \details Since Technologies are two dimensional, a new vintage for every model period,
     *          and each vintage may operate for many model periods depending on the tech
     *          lifetime.  Thus some technology member varariables must be saved in a
     *          vector of some sort, however if we size that vector for all model periods then
     *          that will lead to a lot of wasted memory.  That is where the TechVintageVector
     *          comes into play.  It should be used for any member variable vectors that are
     *          contained in an object that is ultimately contained in a technology.
     *
     *          Note that since technology lifetimes may not be known these vectors when first
     *          created will be left uninitialized.  All instances will get automatically sized
     *          and initialized during completeInit.  If a user needs to store values prior to
     *          completeInit they may use the TechVectorParseHelper helper class to do so and those
     *          stored values will get automatically copied over once the TechVintageVector is
     *          initialized.
     * \warning Attempting to access data prior to initialization will lead to an abort().
     * \sa TechVectorParseHelper for how to set default values, etc prior to completeInit.
     * \sa InitializeTechVectorHelper to see how instances of TechVintageVector are found and
     *     initialized.
     */
    template<class T>
    class TechVintageVector {
        friend class TechVectorParseHelper<T>;
    public:
        
        /*!
         * \brief A random access iterator so we can utilize time vectors in
         *        STL algorithms.
         * \details Given all we really need to do is create an iterators over
         *          essentially a simple array we can utilize boost::iterator_adaptor
         *          to do all the work.  In this way we do not need to worry about
         *          the subtle requirements, and shifting requirements with subsequent
         *          C++ standards, of what the STL requires an iterator to be.
         *
         *          This implementation is basically straight out of the boost documentation's
         *          examples.  Again given our simple need we need to make no specializations
         *          of the boost::iterator_adaptor base class.
         *
         *          Note we make the "value" type templated so that we can automatically
         *          generate both the mutable (iterator) and non-mutable (const_iterator)
         *          types.
         */
        template<class Value>
        class TechVintageVectorIter : public boost::iterator_adaptor<
        TechVintageVectorIter<Value>,
        Value*,
        boost::use_default,
        boost::random_access_traversal_tag>
        {
        private:
            // helper struct for the enable_if below
            // a private type avoids misuse
            struct enabler {};
            
        public:
            /*!
             * \brief Default constructor.
             */
            TechVintageVectorIter(): TechVintageVectorIter::iterator_adaptor_( 0 ) { }
            
            /*!
             * \brief Constructor which simply points to the memory location this
             *        iterator should reference.
             * \param aData the memory location this iterator points to.
             */
            explicit TechVintageVectorIter( Value* aData ): TechVintageVectorIter::iterator_adaptor_( aData )
            {
            }
            
            /*!
             * \brief An "interoperability" constructor, essentially to allow us to
             *        downgrade an iterator to const_interator.
             * \details The syntax is quite complicated due to the use of the enable_if
             *          the purpose of which is to only allow non-const to const and
             *          not the other way around.
             * \param aOther The iterator to downgrade.
             */
            template <class OtherValue>
            TechVintageVectorIter( TechVintageVectorIter<OtherValue> const& aOther
                               , typename boost::enable_if<
                               boost::is_convertible<OtherValue*,Value*>
                               , enabler
                               >::type = enabler() ): TechVintageVectorIter::iterator_adaptor_( aOther.base() ) {}
        };
        
        // Generate the actual iterator types by templating just T for the mutable iterator
        // and T const for the non-mutable iterator.
        typedef TechVintageVectorIter<T> iterator;
        typedef TechVintageVectorIter<T const> const_iterator;
        
        
        TechVintageVector();
        TechVintageVector( const unsigned int aStartPeriod, const unsigned int aSize, const T aDefaultValue );
        ~TechVintageVector();
        TechVintageVector( const TechVintageVector& aOther );
        TechVintageVector& operator=( const TechVintageVector& aOther );
        
        bool operator==( TechVintageVector& aOther ) const;
        
        bool operator!=( TechVintageVector& aOther ) const;
        
        T& operator[]( const unsigned int aIndex );
        const T& operator[]( const unsigned int aIndex ) const;
        
        unsigned int getStartPeriod() const;
        unsigned int size() const;
        const_iterator begin() const;
        const_iterator end() const;
        const_iterator last() const;
        iterator begin();
        iterator end();
        iterator last();
        typedef T value_type;
    protected:
        //! After initialization the dynamic array containing the data.
        //! Prior to initialization we use it as a unique ID of this object as a means
        //! for TechVectorParseHelper to associated temporarily stored data back to this
        //! instance for permanent storage.
        //! Note we use just a single variable for these two purposes to keep the memory
        //! footprint of this object as small as possible.
        T* mData;
        
        //! Initial period the tech is available.
        int mStartPeriod;
        
        //! Size of the array, enough space to accomodate the tech lifetime.
        unsigned int mSize;
        
        /*!
         * \brief a Helper to check if this instance has been initialized (memory allocated).
         * \return True if this instance has been initialized.
         */
        bool isInitialized() const {
            return mStartPeriod >= 0;
        }
        
    private:
        void init( const unsigned int aSize,
                  const T aDefaultValue );
        
        void clear();
    };
    
    /*!
     * \brief Default constructor, by necessarily left un-initialized as that will
     *        happen automatically during completeInit when technology lifetimes
     *        will be known.
     */
    template<class T>
    TechVintageVector<T>::TechVintageVector():
    mStartPeriod( -1 )
    {
        // WARNING: this could be very bad if someone tried to dereference this
        // we will stash a temporary look up ID in mData
        mData = reinterpret_cast<T*>( this );
    }
    
    /*!
     * \brief Constructor. Users should use this constructor *only* if they need
     *        to override the start period and size that would have been calculated
     *        automatically using the enclosing technology's parameters.
     * \param aSize Size of the TimeVectorBase. The size is immutable once
     *              constructed.
     * \param aDefaultValue Default for all values.
     */
    template<class T>
    TechVintageVector<T>::TechVintageVector( const unsigned int aStartPeriod,
                                             const unsigned int aSize,
                                             const T aDefaultValue ):
    mData( new T[ aSize ] ),
    mStartPeriod( aStartPeriod ),
    mSize( aSize )
    {
        // Initialize the data to the default value.
        std::uninitialized_fill( &mData[ 0 ], &mData[ 0 ] + mSize, aDefaultValue );
    }
    
    /*!
     * \brief Destructor which deallocates the array.
     */
    template<class T>
    TechVintageVector<T>::~TechVintageVector(){
        clear();
    }
    
    /*!
     * \brief Private member function to deallocate the memory.
     */
    template<class T>
    void TechVintageVector<T>::clear(){
        if( isInitialized() ) {
            delete[] mData;
        }
    }
    
    /*!
     * \brief Copy constructor.
     * \details If this aOther has not been initialized the copy will get the same
     *          unique ID reference as aOther thus once initialization does occur it
     *          will get the same values as aOther.  If it has already been initialized
     *          then it will just be a deep copy including the start period and size.
     * \param aOther TimeVectorBase to copy.
     */
    template<class T>
    TechVintageVector<T>::TechVintageVector( const TechVintageVector<T>& aOther ):
    mData( aOther.mData ),
    mStartPeriod( aOther.mStartPeriod ),
    mSize( aOther.mSize )
    {
        if( aOther.isInitialized() ) {
            mData = new T[ mSize ];
            std::copy( aOther.begin(), aOther.end(), &mData[ 0 ] );
        }
    }
    
    /*!
     * \brief Assignment operator.
     * \details If this aOther has not been initialized the copy will get the same
     *          unique ID reference as aOther thus once initialization does occur it
     *          will get the same values as aOther.  If it has already been initialized
     *          then it will just be a deep copy including the start period and size.
     * \param aOther TimeVectorBase to copy.
     * \return The newly constructed TimeVectorBase by reference(for chaining
     *         assignment).
     */
    template<class T>
    TechVintageVector<T>& TechVintageVector<T>::operator=( const TechVintageVector<T>& aOther ){
        // Check for self-assignment.
        if( this != &aOther ){
            if( !aOther.isInitialized() ) {
                mStartPeriod = -1;
                mData = aOther.mData;
            }
            else {
                clear();
                mStartPeriod = aOther.mStartPeriod;
                mSize = aOther.mSize;
                mData = new T[ mSize ];
                std::copy( aOther.begin(), aOther.end(), &mData[ 0 ] );
            }
        }
        return *this;
    }
    
    /*!
     * \brief Equals operator.
     * \param aOther TechVintageVector to check for equivalence.
     * \details TechVintageVector are equivalent if:
     *          - Unitialized then they share the same lookup ID.
     *          - Initialized then they have the same start period, size, and
     *            all elements at respective positions are equal.
     * \return Whether the two vectors are equal.
     */
    template<class T>
    bool  TechVintageVector<T>::operator==( TechVintageVector& aOther ) const {
        if( !isInitialized() ) {
            return !aOther.isInitialized() && mData == aOther.mData;
        }
        else {
            return mStartPeriod == aOther.mStartPeriod && size() == aOther.size() && equal( begin(), end(), aOther.begin() );
        }
    }
    
    /*!
     * \brief Not-equals operator.
     * \param aOther TimeVectorBase to check for dis-equivalence.
     * \details Just the not of the equals operator.
     * \return Whether the two vectors are equal.
     */
    template<class T>
    bool  TechVintageVector<T>::operator!=( TechVintageVector& aOther ) const {
        return !( *this == aOther );
    }
    
    /*!
     * \brief Array index operator, where the index is a model period.
     * \details The vector is index by model period similar to PeriodVector however
     *          only periods >= the start year and <= the last model period the
     *          technology operators are valid.  Also using this operator prior to
     *          initialization is invalid.
     * \param aIndex The model period to retrieve the value for.
     * \return The data stored in the vector at aIndex.
     * \warning Array index out of bounds checks are only enabled in debug mode.
     */
    template<class T>
    T& TechVintageVector<T>::operator[]( const unsigned int aIndex ) {
        if( !isInitialized() ) {
            abort();
        }
#ifndef NDEBUG
        
        if( aIndex < mStartPeriod ) {
            std::cerr << "Access before: " << aIndex << " < " << mStartPeriod << std::endl;
            abort();
        }
        else if( aIndex > (mStartPeriod + mSize) ) {
            std::cerr << "Access before: " << aIndex << " > " << mStartPeriod + mSize << std::endl;
            abort();
        }
#endif // !NDEBUG
        
        return mData[ aIndex - mStartPeriod ];
    }
    
    /*!
     * \brief Const array index operator, where the index is a model period.
     * \details The vector is index by model period similar to PeriodVector however
     *          only periods >= the start year and <= the last model period the
     *          technology operators are valid.  Also using this operator prior to
     *          initialization is invalid.
     * \param aIndex The model period to retrieve the value for.
     * \return The data stored in the vector at aIndex.
     * \warning Array index out of bounds checks are only enabled in debug mode.
     */
    template<class T>
    const T& TechVintageVector<T>::operator[]( const unsigned int aIndex ) const {
        if( !isInitialized() ) {
            abort();
        }
#ifndef NDEBUG
        
        if( aIndex < mStartPeriod ) {
            std::cerr << "Access before: " << aIndex << " < " << mStartPeriod << std::endl;
            abort();
        }
        else if( aIndex > (mStartPeriod + mSize) ) {
            std::cerr << "Access before: " << aIndex << " > " << mStartPeriod + mSize << std::endl;
            abort();
        }
#endif // !NDEBUG
        
        return mData[ aIndex - mStartPeriod ];
    }
    
    /*!
     * \brief The the start period of this vector.
     * \return The first model period for which data is stored.
     */
    template<class T>
    unsigned int TechVintageVector<T>::getStartPeriod() const {
        return mStartPeriod;
    }
    
    /*!
     * \brief Return the size of the vector.
     * \return Size of the vector.
     */
    template<class T>
    unsigned int TechVintageVector<T>::size() const {
        return mSize;
    }
    
    /*!
     * \brief Return the constant iterator to the first position in the vector.
     * \return Constant iterator to the first position.
     */
    template<class T>
    typename TechVintageVector<T>::const_iterator TechVintageVector<T>::begin() const {
        return typename TechVintageVector<T>::const_iterator( mData );
    }
    
    /*!
     * \brief Return the constant iterator to the position past the last position
     *        in the vector.
     * \return Constant iterator to one position past the last position.
     */
    template<class T>
    typename TechVintageVector<T>::const_iterator TechVintageVector<T>::end() const {
        return typename TechVintageVector<T>::const_iterator( mData + size() );
    }
    
    /*!
     * \brief Return the constant iterator to the last position in the vector.
     * \return Constant iterator to the last position
     */
    template<class T>
    typename TechVintageVector<T>::const_iterator TechVintageVector<T>::last() const {
        // Return the last position. If the vector is empty make sure this is
        // the zeroth position.
        return typename TechVintageVector<T>::const_iterator( mData + std::max( int( size() ) - 1, 0 ) );
    }
    
    /*!
     * \brief Return a mutable iterator to the first position in the vector.
     * \return Mutable iterator to the first position.
     */
    template<class T>
    typename TechVintageVector<T>::iterator TechVintageVector<T>::begin() {
        return typename TechVintageVector<T>::iterator( mData );
    }
    
    /*!
     * \brief Return a mutable iterator to the position past the last position in
     *        the vector.
     * \return Mutable iterator to one position past the last position.
     */
    template<class T>
    typename TechVintageVector<T>::iterator TechVintageVector<T>::end() {
        return typename TechVintageVector<T>::iterator( mData + size() );
    }
    
    /*!
     * \brief Return a mutable iterator to the last position in the vector.
     * \return Mutable iterator to the last position
     */
    template<class T>
    typename TechVintageVector<T>::iterator TechVintageVector<T>::last() {
        // Return the last position. If the vector is empty make sure this is
        // the zeroth position.
        return typename TechVintageVector<T>::iterator( mData + std::max( static_cast<int>( size() ) - 1, 0 ) );
    }
}

#endif // _TIME_VECTOR_H_
