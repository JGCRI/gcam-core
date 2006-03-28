#ifndef _TIME_VECTOR_H_
#define _TIME_VECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
 */

#include <cassert>
// TODO: Reduce these includes
#include "util/base/include/model_time.h"
#include "containers/include/scenario.h"

extern Scenario* scenario;

/*! 
* \file time_vector.h  
* \ingroup util
* \brief Header file for the TimeVector class.
* \author Josh Lurz
*/
namespace objects {
    /*!
     * \brief Base class of vectors indexed by year or period.
     * \details Provides common code for year and period vectors.
     */
    template<class T>
    class TimeVectorBase {
    public:
        /*!
        * \brief Constant input iterator.
        */
        class const_iterator {
        public:
            /*! \brief Required default constructor.
            */
            const_iterator(): mPos( 0 ), mParent( 0 ) {
            }

            /*! \brief Constructor.
            * \param aPos Position of the iterator.
            */
            const_iterator( const unsigned int aPos, 
                            const TimeVectorBase* aParent )
                : mPos( aPos ), mParent( aParent ){
            }

            //! Get the contents of the iterator.
            const T& operator*() const {
                assert( mParent );
                assert( mPos <= mParent->size() );
                return mParent->mData[ mPos ];
            }

            //! Equality operator.
            bool operator==( const_iterator& aOther ) const {
                return mPos == aOther.mPos;
            }

            //! Inequality operator.
            bool operator!=( const_iterator& aOther ) const {
                return !( *this == aOther );
            }
            
            //! Prefix increment.
            const_iterator& operator++(){
                ++mPos;
                return *this;
            }

            //! Postfix increment.
            const_iterator operator++(int){
                const_iterator prev = this;
                ++mPos;
                return prev;
            }

            //! Prefix decrement.
            const_iterator& operator--(){
                --mPos;
                return *this;
            }

            //! Postfix decrement.
            const_iterator operator--(int){
                const_iterator prev = this;
                --mPos;
                return prev;
            }
        protected:
            //! Current index into the array.
            unsigned int mPos;

            //! Parent container.
            const TimeVectorBase* mParent;
        };

        /*!
        * \brief Mutable input iterator.
        */
        class iterator: public const_iterator {
        public:
            /*! \brief Required default constructor.
            */
            iterator(){
            }
            /*! \brief Constructor.
            * \param aPos Position of the iterator.
            */
            iterator( const unsigned int aPos, TimeVectorBase* aParent )
                :const_iterator( aPos, aParent )
            {
            }

            //! Get the contents of the iterator.
            T& operator*() {
                assert( mParent );
                assert( mPos <= mParent->size() );
                return mParent->mData[ mPos ];
            }
                   //! Equality operator.
            bool operator==( iterator& aOther ) const {
                return mPos == aOther.mPos;
            }

            //! Inequality operator.
            bool operator!=( iterator& aOther ) const {
                return !( *this == aOther );
            }
            
            //! Prefix increment.
            iterator& operator++(){
                ++mPos;
                return *this;
            }

            //! Postfix increment.
            iterator operator++(int){
                iterator prev = this;
                ++mPos;
                return prev;
            }

            //! Prefix decrement.
            iterator& operator--(){
                --mPos;
                return *this;
            }

            //! Postfix decrement.
            iterator operator--(int){
                const_iterator prev = this;
                --mPos;
                return prev;
            }
        };
        TimeVectorBase( const unsigned int aSize, const T aDefaultValue );
        ~TimeVectorBase();
        TimeVectorBase( const TimeVectorBase& aOther );
        const TimeVectorBase& operator=( const TimeVectorBase& aOther );

        virtual T& operator[]( const size_t aIndex ) = 0;
        virtual const T& operator[]( const size_t aIndex ) const = 0;

        const size_t size() const;
        void assign( const size_t aPositions, const T& aValue );
        const_iterator begin() const;
        const_iterator end() const;
        const_iterator last() const;
        iterator begin();
        iterator end();
        iterator last();
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

    // TimeVectorBase implementation.

    /*!
     * \brief Constructor.
     * \param Size of the TimeVectorBase. The size is immutable once
     *        constructed.
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
     * \param Size of the TimeVectorBase. The size is immutable once
     *        constructed.
     * \param aDefaultValue Default for all values.
    */
   template<class T>
       void TimeVectorBase<T>::init( const unsigned int aSize,
                                     const T aDefaultValue )
   {
           mSize = aSize;
           mData = new T[ mSize ];

           // Initialize the data to the default value.
           for( unsigned int i = 0; i < mSize; ++i ){
               mData[ i ] = aDefaultValue;
           }
    }

    /*!
     * \brief Copy constructor.
     * \param aOther TimeVectorBase to copy.
     */
    template<class T>
        TimeVectorBase<T>::TimeVectorBase( const TimeVectorBase<T>& aOther ){
            init( aOther.mSize, T() );
            copy( aOther );
        }

    /*!
     * \brief Assignment operator.
     * \param aOther TimeVectorBase to copy.
     * \return The newly constructed TimeVectorBase by reference(for chaining
     *         assignment).
     */
    template<class T>
        const TimeVectorBase<T>& TimeVectorBase<T>::operator =( const TimeVectorBase<T>& aOther ){
            // Check for self-assignment.
            if( this != &aOther ){
                clear();
                init( aOther.mSize(), T() );
                copy( aOther );
            }
            return *this;
        }

   /*!
    * \brief Return the size of the vector.
    * \return Size of the vector.
    */
    template<class T>
        const size_t TimeVectorBase<T>::size() const {
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
           return const_iterator( 0, this );
       }

   /*!
    * \brief Return the constant iterator to the position past the last position
    *        in the vector.
    * \return Constant iterator to one position past the last position.
    */
   template<class T>
       typename TimeVectorBase<T>::const_iterator TimeVectorBase<T>::end() const {
           return const_iterator( size(), this );
       }

   /*!
    * \brief Return the constant iterator to the last position in the vector.
    * \return Constant iterator to the last position
    */
   template<class T>
       typename TimeVectorBase<T>::const_iterator TimeVectorBase<T>::last() const {
           // Return the last position. If the vector is empty make sure this is
           // the zeroth position.
           return const_iterator( max( int( size() ) - 1, 0 ), this );
       }

  /*!
   * \brief Return a mutable iterator to the first position in the vector.
   * \return Mutable iterator to the first position.
   */
   template<class T>
       typename TimeVectorBase<T>::iterator TimeVectorBase<T>::begin() {
           return iterator( 0, this );
       }

   /*!
    * \brief Return a mutable iterator to the position past the last position in
    *        the vector.
    * \return Mutable iterator to one position past the last position.
    */
   template<class T>
       typename TimeVectorBase<T>::iterator TimeVectorBase<T>::end() {
           return iterator( size(), this );
       }
    
   /*!
    * \brief Return a mutable iterator to the last position in the vector.
    * \return Mutable iterator to the last position
    */
   template<class T>
       typename TimeVectorBase<T>::iterator TimeVectorBase<T>::last() {
           // Return the last position. If the vector is empty make sure this is
           // the zeroth position.
           return iterator( max( size() - 1, 0 ), this );
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
        YearVector( const unsigned int aStartYear,
                    const unsigned int aEndYear,
                    const T aDefaultValue = T() );

        virtual T& operator[]( const size_t aIndex );
        virtual const T& operator[]( const size_t aIndex ) const;
        const_iterator find( const unsigned int aIndex ) const;
        iterator find( const unsigned int aIndex );
   protected:
        //! First year of the array. This year is included in the array.
        const unsigned int mStartYear;

        //! End year of the array. This year is included in the array.
        const unsigned int mEndYear;
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
        typename YearVector<T>::const_iterator YearVector<T>::find( const unsigned int aYear ) const {
            // Check if the year is valid.
            if( aYear < mStartYear || aYear > mEndYear ){
                return end();
            }
            // Return an iterator to the year.
            return const_iterator( aYear - mStartYear );
        }

    /*!
     * \brief Find a specific year in the vector and return a mutable iterator
     *        to it.
     * \param aYear Year for which to return a mutable iterator.
     * \return Mutable iterator for the year, the end iterator if it is not
     *         found.
     */
    template<class T>
        typename YearVector<T>::iterator YearVector<T>::find( const unsigned int aYear ) {
            // Check if the year is valid.
            if( aYear < mStartYear || aYear > mEndYear ){
                return end();
            }
            // Return an iterator to the year.
            return iterator( aYear - mStartYear, this );
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
        PeriodVector( const T aDefaultValue = T() );
        virtual T& operator[]( const size_t aIndex );
        virtual const T& operator[]( const size_t aIndex ) const;
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
            return mData[ aIndex ];
        }
   }

#endif // _TIME_VECTOR_H_
