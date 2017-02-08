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
#include "util/base/include/inamed.h"
#include "util/base/include/iyeared.h"
#include "util/base/include/time_vector.h"
#include "util/base/include/model_time.h"
#include "containers/include/scenario.h"

extern Scenario* scenario;

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
    template<typename T>
    bool operator()( const T* aContainer ) {
        return false;
    }
};

struct IndexFilter {
    IndexFilter( const AMatchesValue* aMatcher ):mMatcher( aMatcher ) {}
    ~IndexFilter() {
        delete mMatcher;
    }
    const AMatchesValue* mMatcher;
    bool operator()( const int aIndex ) const {
        return mMatcher->matchesInt( aIndex );
    }
};

struct NamedFilter {
    NamedFilter( const AMatchesValue* aMatcher ):mMatcher( aMatcher ) {}
    ~NamedFilter() {
        delete mMatcher;
    }
    const AMatchesValue* mMatcher;
    bool operator()( const INamed* aContainer ) const {
        return aContainer && mMatcher->matchesString( aContainer->getName() );
    }
};

struct YearFilter {
    YearFilter( const AMatchesValue* aMatcher ):mMatcher( aMatcher ) {}
    ~YearFilter() {
        delete mMatcher;
    }
    const AMatchesValue* mMatcher;
    bool operator()( const IYeared* aContainer ) const {
        return aContainer && mMatcher->matchesInt( aContainer->getYear() );
    }
    // specialization where the year has been converted for us
    bool operator()( const int aYear ) const {
        return mMatcher->matchesInt( aYear );
    }
};

template<typename Container, typename Enable=void>
struct GetFilterForContainer {
    typedef NoFilter filter_type;
};

template<typename Container>
struct GetFilterForContainer<Container, typename boost::enable_if<boost::is_base_of<INamed, Container> >::type> {
    typedef NamedFilter filter_type;
};

template<typename Container>
struct GetFilterForContainer<Container, typename boost::enable_if<boost::is_base_of<IYeared, Container> >::type> {
    typedef YearFilter filter_type;
};

typedef boost::mpl::vector<NoFilter, IndexFilter, NamedFilter, YearFilter> FilterTypes;

BOOST_MPL_HAS_XXX_TRAIT_DEF( key_type );
BOOST_MPL_HAS_XXX_TRAIT_DEF( iterator );

struct FilterStep {
    using FilterPtrTypes = typename boost::mpl::transform<FilterTypes, boost::add_pointer<boost::mpl::_> >::type;
    using FilterMapType = typename boost::fusion::result_of::as_map<typename boost::fusion::result_of::as_vector<typename boost::mpl::transform_view<boost::mpl::zip_view< boost::mpl::vector<FilterTypes, FilterPtrTypes> >, boost::mpl::unpack_args<boost::fusion::pair<boost::mpl::_1, boost::mpl::_2> > > >::type>::type;
    FilterStep( const std::string& aDataName ):mDataName( aDataName ), mFilterMap(), mNoFilters( true ) {}
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
            boost::integral_constant<bool, DataType::hasDataFlag( CONTAINER )>,
            boost::is_pointer<typename DataType::value_type>
        >,
    void>::type applyFilter( DataType& aData, DataVectorHandler& aHandler, const bool aIsLastStep ) {
        //assert( matchesDataName( aData ) );

        // Note compiler error of the type Incomeplete type 'XYZ' named in nesed name specifier
        // here may indicate that the header file for that class was not included in
        // util/base/include/gcam_data_containers.h
        using ContainerType = typename boost::remove_pointer<typename DataType::value_type>::type;
        ExpandDataVector<typename ContainerType::SubClassFamilyVector> getDataVector;
        // Construct what the appropriate Filter type would be to identify this
        // type of container such as by name or year.
        using ContainerIDFilterType = typename GetFilterForContainer<ContainerType>::filter_type;
        if( mNoFilters ) {
            // No filters accept all containers
            if( !aIsLastStep ) {
                if( aData.mData ) {
                    aHandler.pushFilterStep( aData.mData );
                    aData.mData->doDataExpansion( getDataVector );
                    getDataVector.getFullDataVector( aHandler );
                    aHandler.popFilterStep( aData.mData );
                }
            }
            else {
                aHandler.processData( aData.mData );
            }
        }
        else if( boost::fusion::at_key<ContainerIDFilterType>( mFilterMap ) ) {
            // Apply the filter that was set to determine if the container
            // matches and take a step on.
            auto& filterPred = *boost::fusion::at_key<ContainerIDFilterType>( mFilterMap );
            if( filterPred( aData.mData ) ) {
                if( !aIsLastStep ) {
                    aHandler.pushFilterStep( aData.mData );
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
            boost::integral_constant<bool, DataType::hasDataFlag( CONTAINER )>,
            boost::mpl::and_<has_iterator<typename DataType::value_type>, boost::mpl::not_<has_key_type<typename DataType::value_type> > >
        >,
    void>::type applyFilter( DataType& aData, DataVectorHandler& aHandler, const bool aIsLastStep ) {
        //assert( matchesDataName( aData ) );
        // Note compiler error of the type Incomeplete type 'XYZ' named in nesed name specifier
        // here may indicate that the header file for that class was not included in
        // util/base/include/gcam_data_containers.h
        using ContainerType = typename boost::remove_pointer<typename DataType::value_type::value_type>::type;
        ExpandDataVector<typename ContainerType::SubClassFamilyVector> getDataVector;
        // Construct what the appropriate Filter type would be to identify this
        // type of container such as by name or year.
        using ContainerIDFilterType = typename GetFilterForContainer<ContainerType>::filter_type;
        if( mNoFilters ) {
            // No filters accept all element containers
            for( auto container : aData.mData ) {
                if( !aIsLastStep ) {
                    if( container ) {
                        aHandler.pushFilterStep( container );
                        container->doDataExpansion( getDataVector );
                        getDataVector.getFullDataVector( aHandler );
                        aHandler.popFilterStep( container );
                    }
                }
                else {
                    aHandler.processData( container );
                }
            }
        }
        else if( boost::fusion::at_key<ContainerIDFilterType>( mFilterMap ) ) {
            // Apply the filter that was set to determine which element containers to
            // accept and take a step on
            auto& filterPred = *boost::fusion::at_key<ContainerIDFilterType>( mFilterMap );
            for( auto iter = aData.mData.begin(); iter != aData.mData.end(); ++iter ) {
                if( filterPred( *iter ) ) {
                    if( !aIsLastStep ) {
                        aHandler.pushFilterStep( *iter );
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
                        aHandler.pushFilterStep( *iter );
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
            boost::integral_constant<bool, DataType::hasDataFlag( CONTAINER )>,
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
        using ContainerType = typename boost::remove_pointer<typename DataType::value_type::mapped_type>::type;
        ExpandDataVector<typename ContainerType::SubClassFamilyVector> getDataVector;
        // Construct what the appropriate Filter type would be to identify this
        // type of container such as by name or year.
        using ContainerIDFilterType = typename GetFilterForContainer<ContainerType>::filter_type;
        if( mNoFilters ) {
            // No filters accept all element containers
            for( auto iter = aData.mData.begin(); iter != aData.mData.end(); ++iter ) {
                if( !aIsLastStep ) {
                    if( (*iter).second ) {
                        aHandler.pushFilterStep( (*iter).second );
                        (*iter).second->doDataExpansion( getDataVector );
                        getDataVector.getFullDataVector( aHandler );
                        aHandler.popFilterStep( (*iter).second );
                    }
                }
                else {
                    aHandler.processData( (*iter).second );
                }
            }
        }
        else if( boost::fusion::at_key<ContainerIDFilterType>( mFilterMap ) ) {
            // Apply the filter that was set to determine which element containers to
            // accept and take a step on
            auto& filterPred = *boost::fusion::at_key<ContainerIDFilterType>( mFilterMap );
            for( auto iter = aData.mData.begin(); iter != aData.mData.end(); ++iter ) {
                if( filterPred( (*iter).second ) ) {
                    if( !aIsLastStep ) {
                        aHandler.pushFilterStep( (*iter).second );
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
                        aHandler.pushFilterStep( (*iter).second );
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
    typename boost::enable_if<
        boost::integral_constant<bool, DataType::hasDataFlag( ARRAY )>,
    void>::type applyFilter( DataType& aData, DataVectorHandler& aHandler, const bool aIsLastStep ) {
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
                if( filterPred( year ) ) {
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
    typename boost::enable_if<
        boost::integral_constant<bool, DataType::hasDataFlag( SIMPLE )>,
    void>::type applyFilter( DataType& aData, DataVectorHandler& aHandler, const bool aIsLastStep ) {
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

FilterStep* parseFilterStepStr( const std::string& aFilterStepStr );
std::vector<FilterStep*> parseFilterString( const std::string& aFilterStr );

template<typename DataProcessor, bool ProcessData=false, bool ProcessPushStep=false, bool ProcessPopStep=false>
class GCAMFusion {
    public:
    GCAMFusion( DataProcessor& aDataProcessor, std::vector<FilterStep*> aFilterSteps ):mDataProcessor( aDataProcessor ), mFilterSteps( aFilterSteps), mCurrStep( 0 ) {}
    ~GCAMFusion() {
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
                // explicitly handle the "descendant" step case where we need to
                // both handle taking one step down and no steps down.
                if( !isAtLastStep && this->mFilterSteps[ mCurrStep ]->isDescendantStep() ) {
                    ++this->mCurrStep;
                    this->mFilterSteps[ mCurrStep ]->applyFilter( aData, *this, this->isAtLastStep() );
                    --this->mCurrStep;
                }
            }
        } );
    }

    template<typename DataType>
    typename boost::enable_if<
        boost::mpl::and_<boost::is_same<DataType, DataType>, boost::integral_constant<bool, ProcessPushStep> >,
    void>::type pushFilterStep( const DataType& aData )  {
        if( !mFilterSteps[ mCurrStep ]->isDescendantStep() ) {
            ++mCurrStep;
        }
        //std::cout << "Pushed step: " << mCurrStep << std::endl;
        mDataProcessor.pushFilterStep( aData );
    }
    template<typename DataType>
    typename boost::disable_if<boost::mpl::and_<boost::is_same<DataType, DataType>, boost::integral_constant<bool, ProcessPushStep> >,
    void>::type pushFilterStep( const DataType& aData )  {
        if( !mFilterSteps[ mCurrStep ]->isDescendantStep() ) {
            ++mCurrStep;
        }
        //std::cout << "Pushed step: " << mCurrStep << std::endl;
    }

    template<typename DataType>
    typename boost::enable_if<boost::mpl::and_<boost::is_same<DataType, DataType>, boost::integral_constant<bool, ProcessPopStep> >,
    void>::type popFilterStep( const DataType& aData )  {
        if( !mFilterSteps[ mCurrStep ]->isDescendantStep() ) {
            --mCurrStep;
        }
        //std::cout << "Popped step: " << mCurrStep << std::endl;
        mDataProcessor.popFilterStep( aData );
    }
    template<typename DataType>
    typename boost::disable_if<boost::mpl::and_<boost::is_same<DataType, DataType>, boost::integral_constant<bool, ProcessPopStep> >,
    void>::type popFilterStep( const DataType& aData )  {
        if( !mFilterSteps[ mCurrStep ]->isDescendantStep() ) {
            --mCurrStep;
        }
        //std::cout << "Popped step: " << mCurrStep << std::endl;
    }

    template<typename DataType>
    typename boost::enable_if<boost::mpl::and_<boost::is_same<DataType, DataType>, boost::integral_constant<bool, ProcessData> >,
    void>::type processData( DataType& aData )  {
        //std::cout << "Done! 1" << std::endl;
        mDataProcessor.processData( aData );
    }
    template<typename DataType>
    typename boost::disable_if<boost::mpl::and_<boost::is_same<DataType, DataType>, boost::integral_constant<bool, ProcessData> >,
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

#endif // _GCAM_FUSION_H_
