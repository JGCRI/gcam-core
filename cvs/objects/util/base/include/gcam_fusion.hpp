#ifndef _GCAM_FUSION_H_
#define _GCAM_FUSION_H_
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
 * \file gcam_fusion.hpp
 * \ingroup util
 * \brief GCAMFusion and related utilities header file.
 * \author Pralit Patel
 */

#include <regex>

#include <boost/algorithm/string.hpp>
#include <boost/lexical_cast.hpp>

#include <boost/fusion/include/pair.hpp>

#include "util/base/include/expand_data_vector.h"
#include "util/base/include/time_vector.h"
#include "util/base/include/model_time.h"
#include "containers/include/scenario.h"

extern Scenario* scenario;

struct DoSomeThingNotSure {
    typedef int ProcessPushStep;
    typedef int ProcessPopStep;
    typedef int ProcessData;
    template<typename DataType, typename IDType>
    void pushFilterStep( const DataType& aData, const IDType* aIDValue )  {
        if( aIDValue ) {
            std::cout << "Pushed: " << *aIDValue << std::endl;
        }
    }
    template<typename DataType>
    void popFilterStep( const DataType& aData )  {
        std::cout << "Popped" << std::endl;
    }
    /*template<typename T>
    typename boost::disable_if<boost::is_same<T, Value>, void>::type processData( T& aData ) {
        std::cout << "WARNING got data of unexpected type: " << typeid(aData).name() << std::endl;
    }
    template<typename T>
    typename boost::enable_if<boost::is_same<T, Value>, void>::type processData( T& aData ) {
        std::cout << "Saw: " << aData << std::endl;
    }*/
    template<typename T>
    void processData( T& aData ) {
        std::cout << "WARNING got data of unexpected type: " << typeid(aData).name() << std::endl;
        //RegionMiniCAM a = aData;
    }
    void processData( Value& aData ) {
        std::cout << "Saw: " << aData << std::endl;
        aData *= 1.2;
    }
};

struct GetIndexAsYear {
    template<typename T>
    static int convertIterToYear( const std::vector<T>& aArray, const typename std::vector<T>::const_iterator& aIter ) {
        // TODO: what to do here?  We could assume if the length of the array is the same as model periods
        // then we can do the conversion.
        return -1;
    }
    template<typename T>
    static int convertIterToYear( const objects::PeriodVector<T>& aArray, const typename objects::TimeVectorBase<T>::const_iterator& aIter ) {
        return scenario->getModeltime()->getper_to_yr( aIter - aArray.begin() );
    }
    template<typename T>
    static int convertIterToYear( const objects::YearVector<T>& aArray, const typename objects::TimeVectorBase<T>::const_iterator& aIter ) {
        return aArray.getStartYear() + ( aIter - aArray.begin() );
    }
    template<typename T>
    static int convertIterToYear( const std::map<unsigned int, T>& aArray, const typename std::map<unsigned int, T>::const_iterator& aIter ) {
        return (*aIter).first;
    }
};

class AMatchesValue {
public:
    virtual ~AMatchesValue() {}
    virtual bool matchesString( const std::string& aStrToTest ) const {
        return false;
    }
    virtual bool matchesInt( const int aIntToTest ) const {
        return false;
    }
};

class StringEquals : public AMatchesValue {
public:
    StringEquals( const std::string& aStr ):mStr( aStr ) {}
    virtual ~StringEquals() {}
    virtual bool matchesString( const std::string& aStrToTest ) const {
        return mStr == aStrToTest;
    }
private:
    const std::string mStr;
};

class StringRegexMatches : public AMatchesValue {
public:
    StringRegexMatches( const std::regex& aRegex ):mRegex( aRegex ) {}
    // TODO: which matching style to use by default?
    StringRegexMatches( const std::string& aRegexStr ):mRegex( aRegexStr, std::regex::nosubs | std::regex::optimize | std::regex::egrep ) {}
    virtual ~StringRegexMatches() {}
    virtual bool matchesString( const std::string& aStrToTest ) const {
        return std::regex_search( aStrToTest, mRegex );
    }
private:
    const std::regex mRegex;
};

class IntEquals : public AMatchesValue {
public:
    IntEquals( const int aInt ):mInt( aInt ) {}
    virtual ~IntEquals() {}
    virtual bool matchesInt( const int aIntToTest ) const {
        return mInt == aIntToTest;
    }
private:
    const int mInt;
};

class IntGreaterThan : public AMatchesValue {
public:
    IntGreaterThan( const int aInt ):mInt( aInt ) {}
    virtual ~IntGreaterThan() {}
    virtual bool matchesInt( const int aIntToTest ) const {
        return aIntToTest > mInt;
    }
private:
    const int mInt;
};
class IntGreaterThanEq : public AMatchesValue {
public:
    IntGreaterThanEq( const int aInt ):mInt( aInt ) {}
    virtual ~IntGreaterThanEq() {}
    virtual bool matchesInt( const int aIntToTest ) const {
        return aIntToTest >= mInt;
    }
private:
    const int mInt;
};
class IntLessThan : public AMatchesValue {
public:
    IntLessThan( const int aInt ):mInt( aInt ) {}
    virtual ~IntLessThan() {}
    virtual bool matchesInt( const int aIntToTest ) const {
        return aIntToTest < mInt;
    }
private:
    const int mInt;
};
class IntLessThanEq : public AMatchesValue {
public:
    IntLessThanEq( const int aInt ):mInt( aInt ) {}
    virtual ~IntLessThanEq() {}
    virtual bool matchesInt( const int aIntToTest ) const {
        return aIntToTest <= mInt;
    }
private:
    const int mInt;
};


struct NoFilter {
    using filter_value_type = int*;
    template<typename T>
    bool operator()( const T* aContainer ) {
        return false;
    }
    filter_value_type getCurrValue() const {
        return 0;
    }
    void reset() {
    }
};

struct IndexFilter {
    using filter_value_type = const int*;
    IndexFilter( const AMatchesValue* aMatcher ):mMatcher( aMatcher ), mCurrFilterValue( 0 ) {}
    ~IndexFilter() {
        delete mMatcher;
    }
    const AMatchesValue* mMatcher;
    filter_value_type mCurrFilterValue;
    bool operator()( const int aIndex ) {
        if( mMatcher->matchesInt( aIndex ) ) {
            mCurrFilterValue = &aIndex;
            return true;
        }
        else {
            return false;
        }
    }
    filter_value_type getCurrValue() const {
        return mCurrFilterValue;
    }
    void reset() {
        mCurrFilterValue = 0;
    }
};

struct NamedFilter {
    using filter_value_type = const std::string*;
    NamedFilter( const AMatchesValue* aMatcher ):mMatcher( aMatcher ), mCurrFilterValue( 0 ) {}
    ~NamedFilter() {
        delete mMatcher;
    }
    const AMatchesValue* mMatcher;
    filter_value_type mCurrFilterValue;
    template<typename T>
    bool operator()( const T* aContainer ) {
        if( mMatcher->matchesString( aContainer->getName() ) ) {
            mCurrFilterValue = &aContainer->getName();
            return true;
        }
        else {
            return false;
        }
    }
    filter_value_type getCurrValue() const {
        return mCurrFilterValue;
    }
    void reset() {
        mCurrFilterValue = 0;
    }
};

struct YearFilter {
    using filter_value_type = const int*;
    YearFilter( const AMatchesValue* aMatcher ):mMatcher( aMatcher ), mCurrFilterValue( 0 ) {}
    ~YearFilter() {
        delete mMatcher;
    }
    const AMatchesValue* mMatcher;
    int mCurrFilterValueCopy;
    filter_value_type mCurrFilterValue;
    template<typename T>
    bool operator()( const T* aContainer ) {
        if( mMatcher->matchesInt( aContainer->getYear() ) ) {
            // getYear will likely return an int by value which we can not store
            // a reference to.  Instead we will copy the value into a member variable
            // and reference that.
            mCurrFilterValueCopy = aContainer->getYear();
            mCurrFilterValue = &mCurrFilterValueCopy;
            return true;
        }
        else {
            return false;
        }
    }
    // specialization where the year has been converted for us
    bool operator()( const int* aYear ) {
        if( mMatcher->matchesInt( *aYear ) ) {
            mCurrFilterValue = aYear;
            return true;
        }
        else {
            return false;
        }
    }
    filter_value_type getCurrValue() const {
        return mCurrFilterValue;
    }
    void reset() {
        mCurrFilterValue = 0;
    }
};

typedef boost::mpl::vector<NoFilter, IndexFilter, NamedFilter, YearFilter> FilterTypes;

BOOST_MPL_HAS_XXX_TRAIT_DEF( filter_type );
BOOST_MPL_HAS_XXX_TRAIT_DEF( key_type );
BOOST_MPL_HAS_XXX_TRAIT_DEF( iterator );

struct FilterStep {
    using FilterPtrTypes = typename boost::mpl::transform<FilterTypes, boost::add_pointer<boost::mpl::_> >::type;
    using FilterMapType = typename boost::fusion::result_of::as_map<typename boost::fusion::result_of::as_vector<typename boost::mpl::transform_view<boost::mpl::zip_view< boost::mpl::vector<FilterTypes, FilterPtrTypes> >, boost::mpl::unpack_args<boost::fusion::pair<boost::mpl::_1, boost::mpl::_2> > > >::type>::type;
    FilterStep( const std::string& aDataName ):mDataName( aDataName ), mFilterMap(), mNoFilters( true ) {}
    FilterStep( const std::string& aDataName, NoFilter* aFilter ):mDataName( aDataName ), mFilterMap(), mNoFilters( false ) {
        boost::fusion::at_key<NoFilter>( mFilterMap ) = aFilter;
    }
    FilterStep( const std::string& aDataName, IndexFilter* aFilter ):mDataName( aDataName ), mFilterMap(), mNoFilters( false ) {
        boost::fusion::at_key<IndexFilter>( mFilterMap ) = aFilter;
    }
    FilterStep( const std::string& aDataName, NamedFilter* aFilter ):mDataName( aDataName ), mFilterMap(), mNoFilters( false ) {
        boost::fusion::at_key<NamedFilter>( mFilterMap ) = aFilter;
    }
    FilterStep( const std::string& aDataName, YearFilter* aFilter ):mDataName( aDataName ), mFilterMap(), mNoFilters( false ) {
        boost::fusion::at_key<YearFilter>( mFilterMap ) = aFilter;
    }
    ~FilterStep() {
        boost::fusion::for_each( mFilterMap, [] ( auto& aPair ) {
            delete aPair.second;
        } );
    }
    const std::string mDataName;
    FilterMapType mFilterMap;
    bool mNoFilters;
    bool isDescendantStep() const {
        return mDataName.empty() && mNoFilters;
    }
    void reset() {
        boost::fusion::for_each( mFilterMap, [] ( auto& aPair ) {
            if( aPair.second ) {
                aPair.second->reset();
            }
        } );
    }
    template<typename DataType>
    bool matchesDataName( const DataType& aData ) {
        return mDataName.empty() || mDataName == aData.mDataName;
    }

    // Specializations for containers
    // TODO: this looks delicate, we need to make sure we know how to iterate over whatever types of data
    // is in DataType.mData

    // Specialization for a "Single" container
    template<typename DataType, typename DataVectorHandler>
    typename boost::enable_if<
        boost::mpl::and_<
            boost::is_same<DataType, ContainerData<typename DataType::value_type, typename DataType::filter_type> >,
            boost::is_pointer<typename DataType::value_type>
        >,
    void>::type applyFilter( DataType& aData, DataVectorHandler& aHandler, const bool aIsLastStep ) {
        //assert( matchesDataName( aData ) );
        // Do not process null data.
        if( !aData.mData ) {
            return;
        }
        // Note compiler error of the type Incomeplete type 'XYZ' named in nesed name specifier
        // here may indicate that the header file for that class was not included in
        // util/base/include/gcam_data_containers.h
        ExpandDataVector<typename boost::remove_pointer<decltype( aData.mData )>::type::SubClassFamilyVector> getDataVector;
        if( mNoFilters ) {
            // No filters accept all containers
            // use NULL as the current filter value as no filter is set
            typename DataType::filter_type::filter_value_type nullFilterValue = 0;
            if( !aIsLastStep ) {
                aHandler.pushFilterStep( aData.mData, nullFilterValue );
                aData.mData->doDataExpansion( getDataVector );
                getDataVector.getFullDataVector( aHandler );
                aHandler.popFilterStep( aData.mData );
            }
            else {
                aHandler.processData( aData.mData );
            }
        }
        else if( boost::fusion::at_key<typename DataType::filter_type>( mFilterMap ) ) {
            // Apply the filter that was set to determine if the container
            // matches and take a step on.
            auto& filterPred = *boost::fusion::at_key<typename DataType::filter_type>( mFilterMap );
            if( filterPred( aData.mData ) ) {
                if( !aIsLastStep ) {
                    aHandler.pushFilterStep( aData.mData, filterPred.getCurrValue() );
                    aData.mData->doDataExpansion( getDataVector );
                    getDataVector.getFullDataVector( aHandler );
                    aHandler.popFilterStep( aData.mData );
                }
                else {
                    aHandler.processData( aData.mData );
                }
            }
        }
        else if( boost::fusion::at_key<IndexFilter>( mFilterMap ) ) {
            // A single element cannot be filtered by position
            // TODO: or should we allow the filter only checking position 0?
        }
        else {
            // The filter set does not match the filter aData can be filtered by
            // so no element containers will be accepted
        }
    }
    // Specialization for a vector (or any iteratable array that is not a map) container
    template<typename DataType, typename DataVectorHandler>
    typename boost::enable_if<
        boost::mpl::and_<
            boost::is_same<DataType, ContainerData<typename DataType::value_type, typename DataType::filter_type> >,
            boost::mpl::and_<has_iterator<typename DataType::value_type>, boost::mpl::not_<has_key_type<typename DataType::value_type> > >
        >,
    void>::type applyFilter( DataType& aData, DataVectorHandler& aHandler, const bool aIsLastStep ) {
        //assert( matchesDataName( aData ) );
        // Note compiler error of the type Incomeplete type 'XYZ' named in nesed name specifier
        // here may indicate that the header file for that class was not included in
        // util/base/include/gcam_data_containers.h
        ExpandDataVector<typename boost::remove_pointer<typename decltype( aData.mData )::value_type>::type::SubClassFamilyVector> getDataVector;
        if( mNoFilters ) {
            // No filters accept all element containers
            // use NULL as the current filter value as no filter is set
            typename DataType::filter_type::filter_value_type nullFilterValue = 0;
            for( auto container : aData.mData ) {
                if( !aIsLastStep ) {
                    aHandler.pushFilterStep( container, nullFilterValue );
                    container->doDataExpansion( getDataVector );
                    getDataVector.getFullDataVector( aHandler );
                    aHandler.popFilterStep( container );
                }
                else {
                    aHandler.processData( container );
                }
            }
        }
        else if( boost::fusion::at_key<typename DataType::filter_type>( mFilterMap ) ) {
            // Apply the filter that was set to determine which element containers to
            // accept and take a step on
            auto& filterPred = *boost::fusion::at_key<typename DataType::filter_type>( mFilterMap );
            for( auto iter = aData.mData.begin(); iter != aData.mData.end(); ++iter ) {
                if( filterPred( *iter ) ) {
                    if( !aIsLastStep ) {
                        aHandler.pushFilterStep( *iter, filterPred.getCurrValue() );
                        (*iter)->doDataExpansion( getDataVector );
                        getDataVector.getFullDataVector( aHandler );
                        aHandler.popFilterStep( *iter );
                    }
                    else {
                        aHandler.processData( *iter );
                    }
                }
            }
        }
        else if( boost::fusion::at_key<IndexFilter>( mFilterMap ) ) {
            // Apply the filter that was set to determine which element containers to
            // accept and take a step on
            auto& filterPred = *boost::fusion::at_key<IndexFilter>( mFilterMap );
            for( auto iter = aData.mData.begin(); iter != aData.mData.end(); ++iter ) {
                if( filterPred( iter - aData.mData.begin() ) ) {
                    if( !aIsLastStep ) {
                        aHandler.pushFilterStep( *iter, filterPred.getCurrValue() );
                        (*iter)->doDataExpansion( getDataVector );
                        getDataVector.getFullDataVector( aHandler );
                        aHandler.popFilterStep( *iter );
                    }
                    else {
                        aHandler.processData( *iter );
                    }
                }
            }
        }
        else {
            // The filter set does not match the filter aData can be filtered by
            // so no element containers will be accepted
        }
    }
    // Specialization for a map container
    template<typename DataType, typename DataVectorHandler>
    typename boost::enable_if<
        boost::mpl::and_<
            boost::is_same<DataType, ContainerData<typename DataType::value_type, typename DataType::filter_type> >,
            has_key_type<typename DataType::value_type>
        >,
    void>::type applyFilter( const DataType& aData, DataVectorHandler& aHandler, const bool aIsLastStep ) {
        //assert( matchesDataName( aData ) );
        if( aData.mData.empty() ) {
            return;
        }
        // Note compiler error of the type Incomeplete type 'XYZ' named in nesed name specifier
        // here may indicate that the header file for that class was not included in
        // util/base/include/gcam_data_containers.h
        ExpandDataVector<typename boost::remove_pointer<typename decltype( aData.mData )::mapped_type>::type::SubClassFamilyVector> getDataVector;
        if( mNoFilters ) {
            // No filters accept all element containers
            // use NULL as the current filter value as no filter is set
            typename DataType::filter_type::filter_value_type nullFilterValue = 0;
            for( auto iter = aData.mData.begin(); iter != aData.mData.end(); ++iter ) {
                if( !aIsLastStep ) {
                    aHandler.pushFilterStep( (*iter).second, nullFilterValue );
                    (*iter).second->doDataExpansion( getDataVector );
                    getDataVector.getFullDataVector( aHandler );
                    aHandler.popFilterStep( (*iter).second );
                }
                else {
                    aHandler.processData( (*iter).second );
                }
            }
        }
        else if( boost::fusion::at_key<typename DataType::filter_type>( mFilterMap ) ) {
            // Apply the filter that was set to determine which element containers to
            // accept and take a step on
            auto& filterPred = *boost::fusion::at_key<typename DataType::filter_type>( mFilterMap );
            for( auto iter = aData.mData.begin(); iter != aData.mData.end(); ++iter ) {
                if( filterPred( (*iter).second ) ) {
                    if( !aIsLastStep ) {
                        aHandler.pushFilterStep( (*iter).second, filterPred.getCurrValue() );
                        (*iter).second->doDataExpansion( getDataVector );
                        getDataVector.getFullDataVector( aHandler );
                        aHandler.popFilterStep( (*iter).second );
                    }
                    else {
                        aHandler.processData( (*iter).second );
                    }
                }
            }
        }
        else if( boost::fusion::at_key<IndexFilter>( mFilterMap ) ) {
            // Apply the filter that was set to determine which element containers to
            // accept and take a step on
            auto& filterPred = *boost::fusion::at_key<IndexFilter>( mFilterMap );
            // we can not take the difference between map iterators so we will have
            // to keep track of the index location explicitly
            int index = 0;
            for( auto iter = aData.mData.begin(); iter != aData.mData.end(); ++iter ) {
                if( filterPred( index ) ) {
                    if( !aIsLastStep ) {
                        aHandler.pushFilterStep( (*iter).second, filterPred.getCurrValue() );
                        (*iter).second->doDataExpansion( getDataVector );
                        getDataVector.getFullDataVector( aHandler );
                        aHandler.popFilterStep( (*iter).second );
                    }
                    else {
                        aHandler.processData( (*iter).second );
                    }
                }
                ++index;
            }
        }
        else {
            // The filter set does not match the filter aData can be filtered by
            // so no element containers will be accepted
        }
    }

    // Specializations for arrays of non-containers i.e. actual data
    template<typename DataType, typename DataVectorHandler>
    typename boost::enable_if<boost::is_same<DataType, ArrayData<typename DataType::value_type> >, void>::type applyFilter( DataType& aData, DataVectorHandler& aHandler, const bool aIsLastStep ) {
        //assert( matchesDataName( aData ) );
        if( !aIsLastStep ) {
            // error?
            return;
        }
        if( mNoFilters ) {
            // No filters gets the full array
            aHandler.processData( aData.mData );
        }
        else if( boost::fusion::at_key<YearFilter>( mFilterMap ) ) {
            // Apply the year filter to accept only elements corresponding to that year
            auto& filterPred = *boost::fusion::at_key<YearFilter>( mFilterMap );
            for( auto iter = aData.mData.begin(); iter != aData.mData.end(); ++iter ) {
                const int year = GetIndexAsYear::convertIterToYear( aData.mData, iter );
                if( filterPred( &year ) ) {
                    aHandler.processData( *iter );
                }
            }
        }
        else if( boost::fusion::at_key<IndexFilter>( mFilterMap ) ) {
            // Apply the index filter to accept only elements in those indices
            auto& filterPred = *boost::fusion::at_key<IndexFilter>( mFilterMap );
            // The array type may be a map and we can not take the difference between
            // map iterators so we will have to keep track of the index location explicitly
            int index = 0;
            for( auto iter = aData.mData.begin(); iter != aData.mData.end(); ++iter ) {
                if( filterPred( index ) ) {
                    aHandler.processData( *iter );
                }
                ++index;
            }
        }
        else {
            // The filter set does not match the filter aData can be filtered by
            // so no element containers will be accepted
        }
    }
    // Specializations for non-containers i.e. actual data that is a single value
    template<typename DataType, typename DataVectorHandler>
    typename boost::enable_if<boost::is_same<DataType, Data<typename DataType::value_type> >, void>::type applyFilter( DataType& aData, DataVectorHandler& aHandler, const bool aIsLastStep ) {
        //assert( matchesDataName( aData ) );
        if( !mNoFilters ) {
            // There was a filter set however a simple DataType can not be filtered
            // so this data will not be processed.
            return;
        }
        else if( aIsLastStep ) {
            aHandler.processData( aData.mData );
        }
        else {
            // error?
        }
    }
};

BOOST_MPL_HAS_XXX_TRAIT_DEF( ProcessPushStep );
BOOST_MPL_HAS_XXX_TRAIT_DEF( ProcessPopStep );
BOOST_MPL_HAS_XXX_TRAIT_DEF( ProcessData );

template<typename DataProcessor>
class GCAMFusion {
    public:
    GCAMFusion( DataProcessor& aDataProcessor, std::vector<FilterStep*> aFilterSteps ):mDataProcessor( aDataProcessor ), mFilterSteps( aFilterSteps), mCurrStep( 0 ) {}
    ~GCAMFusion() {
        // TODO: decide who will own what memory.
        /*
        for( auto filterStep : mFilterSteps ) {
            delete filterStep;
        }
         */
    }
    template<typename ContainerType>
    void startFilter( ContainerType* aContainer ) {
        if( mFilterSteps.empty() ) {
            return;
        }
        ExpandDataVector<typename ContainerType::SubClassFamilyVector> getDataVector;
        aContainer->doDataExpansion( getDataVector );
        getDataVector.getFullDataVector( *this );
    }

    template<typename DataVectorType>
    void processDataVector( DataVectorType aDataVector ) {
        //assert( mCurrStep < mFilterSteps.size() );
        bool isAtLastStep = this->isAtLastStep();
        //std::cout << "Curr step is: " << mCurrStep << ", at end? " << isAtLastStep << std::endl;
        boost::fusion::for_each( aDataVector, [this, isAtLastStep] ( auto& aData ) {
            if( this->mFilterSteps[ mCurrStep ]->matchesDataName( aData ) ) {
                //std::cout << "Matches!!!! " << aData.mDataName << std::endl;
                this->mFilterSteps[ mCurrStep ]->applyFilter( aData, *this, isAtLastStep );
            }
        } );
    }

    template<typename DataType, typename IDType>
    typename boost::enable_if<boost::mpl::and_<boost::is_same<DataType, DataType>, has_ProcessPushStep<DataProcessor> >,
    void>::type pushFilterStep( const DataType& aData, const IDType* aIDValue )  {
        ++mCurrStep;
        //std::cout << "Pushed step: " << mCurrStep << std::endl;
        mDataProcessor.pushFilterStep( aData, aIDValue );
    }
    template<typename DataType, typename IDType>
    typename boost::disable_if<boost::mpl::and_<boost::is_same<DataType, DataType>, has_ProcessPushStep<DataProcessor> >,
    void>::type pushFilterStep( const DataType& aData, const IDType* aIDValue )  {
        ++mCurrStep;
        //std::cout << "Pushed step: " << mCurrStep << std::endl;
    }

    template<typename DataType>
    typename boost::enable_if<boost::mpl::and_<boost::is_same<DataType, DataType>, has_ProcessPopStep<DataProcessor> >,
    void>::type popFilterStep( const DataType& aData )  {
        --mCurrStep;
        //std::cout << "Popped step: " << mCurrStep << std::endl;
        mDataProcessor.popFilterStep( aData );
    }
    template<typename DataType>
    typename boost::disable_if<boost::mpl::and_<boost::is_same<DataType, DataType>, has_ProcessPopStep<DataProcessor> >,
    void>::type popFilterStep( const DataType& aData )  {
        --mCurrStep;
        //std::cout << "Popped step: " << mCurrStep << std::endl;
    }

    template<typename DataType>
    typename boost::enable_if<boost::mpl::and_<boost::is_same<DataType, DataType>, has_ProcessData<DataProcessor> >,
    void>::type processData( DataType& aData )  {
        //std::cout << "Done! 1" << std::endl;
        mDataProcessor.processData( aData );
    }
    template<typename DataType>
    typename boost::disable_if<boost::mpl::and_<boost::is_same<DataType, DataType>, has_ProcessData<DataProcessor> >,
    void>::type processData( DataType& aData )  {
        //std::cout << "Done! 2" << std::endl;
    }

    bool isAtLastStep() const {
        return ( mCurrStep + 1 ) == mFilterSteps.size();
    }

    protected:
    DataProcessor& mDataProcessor;

    std::vector<FilterStep*> mFilterSteps;

    int mCurrStep;
};

FilterStep* parseFilterStepStr( const std::string& aFilterStepStr ) {
    auto openBracketIter = std::find( aFilterStepStr.begin(), aFilterStepStr.end(), '[' );
    if( openBracketIter == aFilterStepStr.end() ) {
        std::cout << "Descendant filter step" << std::endl;
        // no filter just the data name
        return new FilterStep( aFilterStepStr );
    }
    else {
        std::string dataName( aFilterStepStr.begin(), openBracketIter );
        std::cout << "Data name is: >" << dataName << "<" << std::endl;
        std::string filterStr( openBracketIter + 1, std::find( openBracketIter, aFilterStepStr.end(), ']' ) );
        std::vector<std::string> filterOptions;
        boost::split( filterOptions, filterStr, boost::is_any_of( "," ) );
        std::cout << "matcher is: >" << filterOptions[ 1 ] << "< with value >" << filterOptions[ 2 ] << "< on filter >" << filterOptions[ 0 ] << "<" << std::endl;
        // [0] = filter type (name, year, index)
        // [1] = match type
        // [2:] = match type options
        AMatchesValue* matcher = 0;
        if( filterOptions[ 1 ] == "StringEquals" ) {
            matcher = new StringEquals( filterOptions[ 2 ] );
        }
        else if( filterOptions[ 1 ] == "StringRegexMatches" ) {
            matcher = new StringRegexMatches( filterOptions[ 2 ] );
        }
        else if( filterOptions[ 1 ] == "IntEquals" ) {
            matcher = new IntEquals( boost::lexical_cast<int>( filterOptions[ 2 ] ) );
        }
        else if( filterOptions[ 1 ] == "IntGreaterThan" ) {
            matcher = new IntGreaterThan( boost::lexical_cast<int>( filterOptions[ 2 ] ) );
        }
        else if( filterOptions[ 1 ] == "IntGreaterThanEq" ) {
            matcher = new IntGreaterThanEq( boost::lexical_cast<int>( filterOptions[ 2 ] ) );
        }
        else if( filterOptions[ 1 ] == "IntLessThan" ) {
            matcher = new IntLessThan( boost::lexical_cast<int>( filterOptions[ 2 ] ) );
        }
        else if( filterOptions[ 1 ] == "IntLessThanEq" ) {
            matcher = new IntLessThanEq( boost::lexical_cast<int>( filterOptions[ 2 ] ) );
        }
        else {
            std::cout << "Didn't match matcher" << std::endl;
        }

        FilterStep* filterStep = 0;
        if( filterOptions[ 0 ] == "NoFilter" ) {
            filterStep = new FilterStep( dataName, new NoFilter() );
        }
        else if( filterOptions[ 0 ] == "IndexFilter" ) {
            filterStep = new FilterStep( dataName, new IndexFilter( matcher ) );
        }
        else if( filterOptions[ 0 ] == "NamedFilter" ) {
            filterStep = new FilterStep( dataName, new NamedFilter( matcher ) );
        }
        else if( filterOptions[ 0 ] == "YearFilter" ) {
            filterStep = new FilterStep( dataName, new YearFilter( matcher ) );
        }
        else {
            std::cout << "Didn't match filter" << std::endl;
        }
        return filterStep;
    }
}

std::vector<FilterStep*> parseFilterString( const std::string& aFilterStr ) {
    std::vector<std::string> filterStepsStr;
    boost::split( filterStepsStr, aFilterStr, boost::is_any_of( "/" ) );
    std::vector<FilterStep*> filterSteps( filterStepsStr.size() );
    for( size_t i = 0; i < filterStepsStr.size(); ++i ) {
        filterSteps[ i ] = parseFilterStepStr( filterStepsStr[ i ] );
    }
    return filterSteps;
}

#endif // _GCAM_FUSION_H_
