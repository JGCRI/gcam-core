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
#include "util/logger/include/ilogger.h"

extern Scenario* scenario;

/*!
 * \brief A helper struct to call the constexpr function hasDataFlag to check if a Data
 *        is declared as SIMPLE, ARRAY, or CONTAINER.  This hack is necessary to work
 *        around Visual Studio's lack of support for expressions in SFINAE (enable_if)
 *        which as of Visual Studio 2015 Update 3 still has not been implemented.
 */
template<typename DataType>
struct CheckDataFlagHelper {
    using is_simple = boost::integral_constant<bool, DataType::hasDataFlag(SIMPLE)>;
    using is_array = boost::integral_constant<bool, DataType::hasDataFlag(ARRAY)>;
    using is_container = boost::integral_constant<bool, DataType::hasDataFlag(CONTAINER)>;
    using is_not_parsable = boost::integral_constant<bool, DataType::hasDataFlag(NOT_PARSABLE)>;
};

/*!
 * \brief A helper struct to look up into some kind of array by converting an iterator
 *        of that array into a "year" index.
 * \details The conversion depends entirely on which type of array we are dealing with:
 *            - A std::vector will be treated as a "PeriodVector" if the size of the
 *              array is equal to the number of model periods.  If not -1 is returned.
 *            - A PeriodVector will convert the iterator into an index offset which is
 *              then equivalent to the model period and can the be converted to year
 *              using Modeltime::getper_to_yr.
 *            - A YearVector is index by year so but since we are looking up using an
 *              iterator we must convert into a index offset than add the "Start Year"
 *              of the array.
 *            - A std::map<int, T> we assume the key is the year thus we can just get
 *              the .first of from the iterator.
 */
struct GetIndexAsYear {
    template<typename T>
    static int convertIterToYear( const std::vector<T>& aArray, const typename std::vector<T>::const_iterator& aIter ) {
        const Modeltime* modeltime = scenario->getModeltime();
        if( aArray.size() == modeltime->getmaxper() ) {
            return modeltime->getper_to_yr( aIter - aArray.begin() );
        }
        else {
            // TODO: raise an error?
            return -1;
        }
    }
    template<typename T>
    static int convertIterToYear( const objects::PeriodVector<T>& aArray, const typename objects::TimeVectorBase<T>::const_iterator& aIter ) {
        return scenario->getModeltime()->getper_to_yr( aIter - aArray.begin() );
    }
    template<typename T>
    static int convertIterToYear( const objects::TechVintageVector<T>& aArray, const typename objects::TechVintageVector<T>::const_iterator& aIter ) {
        return scenario->getModeltime()->getper_to_yr( aArray.getStartPeriod() + ( aIter - aArray.begin() ) );
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

/*!
 * \brief Converting an array index to model period is trivial in most cases as they
 *        are one in the same.  However we do have a special case which is the TechVintageVector
 */
struct GetIndexAsPeriod {
    // specialization for TechVintageVector for which we need to account for the
    // start period to convert the index to period
    template<typename VecType>
    static typename boost::enable_if<
        typename boost::is_same<
            VecType,
            typename objects::TechVintageVector<typename VecType::value_type>
        >::type,
    int>::type convertIndexToPeriod( const VecType& aArray, const int aIndex ) {
        return aArray.getStartPeriod() + aIndex;
    }
    // all other container types for which the index to period is one to one
    template<typename VecType>
    static typename boost::disable_if<
        typename boost::is_same<
            VecType,
            typename objects::TechVintageVector<typename VecType::value_type>
        >::type,
    int>::type convertIndexToPeriod( const VecType& aArray, const int aIndex ) {
        return aIndex;
    }
};

/*!
 * \brief The base class for the matcher predicates which may be used in a FilterStep.
 * \details These predicates will be used to determine if for instance any GCAM
 *          container's name matches some string or if the container's year matches
 *          some int.  If the container, such as Region is "INamed" then it doesn't
 *          make sense that it should ever match if it is being compared to an int.
 *          Thus this class has two methods: for comparing strings and for ints.
 *          The base implementations would always simply never match and subclasses
 *          would typically only override one or the other as appropriate.
 */
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

/*!
 * \brief A matcher predicate that returns true if a given string to check exactly
 *        matches the string this class was created with.
 */
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

/*!
 * \brief A matcher predicate that returns true if a given string to check matches
 *        a regular expression that this class was created with.
 */
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

/*!
 * \brief A matcher predicate that returns true if a given int to check exactly
 *        matches the int this class was created with.
 */
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

/*!
 * \brief A matcher predicate that returns true if a given int to check is strictly
 *        greater than the int this class was created with.
 */
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

/*!
 * \brief A matcher predicate that returns true if a given int to check is greater
 *        than or equal to the int this class was created with.
 */
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

/*!
 * \brief A matcher predicate that returns true if a given int to check is strictly
 *        less than the int this class was created with.
 */
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

/*!
 * \brief A matcher predicate that returns true if a given int to check is less
 *        than or equal to the int this class was created with.
 */
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

/*!
 * \brief A helper struct to help determine how a GCAM container may be filtered.
 *        This struct in particular will be used for GCAM containers that can not
 *        be filtered in any way.
 * \details Such an example would be an IDiscreteChoice object which single container
 *          of data.  Any attempt to filter such an object by for instance checking
 *          if it's name is equal to some value should never match.
 */
struct NoFilter {
    template<typename T>
    bool operator()( const T* aContainer ) {
        return false;
    }
    
    template<typename T>
    static bool matchesXMLAttr( const T* aContainer, const std::map<std::string, std::string>& aAttrs ) {
        return false;
    }
};

/*!
 * \brief A helper struct to help determine how a GCAM container may be filtered.
 *        This struct in particular can be used for any GCAM container that are stored
 *        in some sort of array.
 * \details This filter is a bit different than the others as it may be employed
 *          irrespective of the type of the GCAM container and also for ARRAY Data.
 *          An example would be to filter a std::vector<Subsector*> or a
 *          PeriodVector<double> to select the first value in the array.
 */
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

/*!
 * \brief A helper struct to help determine how a GCAM container may be filtered.
 *        This struct in particular will be used for GCAM containers that can be
 *        filtered by name.
 * \details This filter assumes such GCAM containers are a subclass of the INamed
 *          interface.  Such an example would be std::vector<Subsector*> where as
 *          we loop over the array any Subsector instance would be selected if it's
 *          value returned by Subsector::getName() matches the predicate this filter
 *          was created with.
 */
struct NamedFilter {
    NamedFilter( const AMatchesValue* aMatcher ):mMatcher( aMatcher ) {}
    ~NamedFilter() {
        delete mMatcher;
    }
    const AMatchesValue* mMatcher;
    bool operator()( const INamed* aContainer ) const {
        return aContainer && mMatcher->matchesString( aContainer->getName() );
    }
    
    static const std::string& getXMLAttrKey() {
        const static std::string KEY = "name";
        return KEY;
    }
    
    static bool matchesXMLAttr( const INamed* aContainer, const std::map<std::string, std::string>& aAttrs ) {
        auto iter = aAttrs.find(getXMLAttrKey());
        return iter != aAttrs.end() && aContainer->getName() == (*iter).second;
    }
};

/*!
 * \brief A helper struct to help determine how a GCAM container may be filtered.
 *        This struct in particular will be used for GCAM containers that can be
 *        filtered by year.
 * \details This filter assumes such GCAM containers are a subclass of the IYeared
 *          interface.  Such an example would be std::vector<Market*> where as
 *          we loop over the array any Market instance would be selected if it's
 *          value returned by Market::getYear() matches the predicate this filter
 *          was created with.
 */
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
    
    static const std::string& getXMLAttrKey() {
        const static std::string KEY = "year";
        return KEY;
    }
    
    static bool matchesXMLAttr( const IYeared* aContainer, const std::map<std::string, std::string>& aAttrs ) {
        auto iter = aAttrs.find(getXMLAttrKey());
        return iter != aAttrs.end() && aContainer->getYear() == boost::lexical_cast<int>((*iter).second);
    }
};

/*
 * \brief A helper struct to figure out which type of Filter is appropriate for a
 *        given GCAM container using template specialization.
 * \details This base template struct indicate that the GCAM container can not be
 *          be filtered and will be used if none of the other specializations match.
 */
template<typename Container, typename Enable=void>
struct GetFilterForContainer {
    typedef NoFilter filter_type;
};

/*
 * \brief A helper struct to figure out which type of Filter is appropriate for a
 *        given GCAM container using template specialization.
 * \details This template specialization will match any GCAM container which is a
 *          subclass of the INamed interface thus using the NamedFilter.
 */
template<typename Container>
struct GetFilterForContainer<Container, typename boost::enable_if<boost::is_base_of<INamed, Container> >::type> {
    typedef NamedFilter filter_type;
};

/*
 * \brief A helper struct to figure out which type of Filter is appropriate for a
 *        given GCAM container using template specialization.
 * \details This template specialization will match any GCAM container which is a
 *          subclass of the IYeared interface thus using the YearFilter.
 */
template<typename Container>
struct GetFilterForContainer<Container, typename boost::enable_if<boost::is_base_of<IYeared, Container> >::type> {
    typedef YearFilter filter_type;
};

// Create some type traits which we will use to help create specializations of
// FilterStep::applyFilter so as to detect and know how to iterate over different
// types of arrays.
BOOST_MPL_HAS_XXX_TRAIT_DEF( key_type );
BOOST_MPL_HAS_XXX_TRAIT_DEF( iterator );

/*!
 * \brief A helper struct which does most of the hard work of applying some search
 *        criteria to traverse the GCAM container hierarchy.
 * \details This struct ties together filters and GCAM fusion to determine how to step
 *          through the multitude of containers and data types that may be found
 *          in GCAM.  An instance of this class has two primary elements:
 *            - A data name which filters which element of the Data Vector of a CONTAINER
 *              will be selected such as "price"
 *            - A "filter" to further refine the element of the Data Vector that
 *              was selected.  For instance if the Data type of "price" was a PeriodVector
 *              we may only want to select values that are prior to the year 2050.
 *              These filters can be any of the ones listed in FilterTypes or a
 *              DataTypeFlag.  Each filter would have been instantiated with it's
 *              own AMatchesValue predicate.
 *          It is valid to provide no data name which is interpreted as a "wild card"
 *          match on all elements of the DataVector however a "filter" may still refine
 *          which of those elements ultimately get selected.
 *          If no data name or filter is provided then that indicates this FilterStep
 *          is a "descendant step" which enables in GCAMFusion special behavior that
 *          allows this FilterStep to continue to match any number of steps down the
 *          CONTAINER hierarchy.  See GCAMFusion for more details about descendant steps.
 *
 *          Even though the FilterStep has to deal with all of these different
 *          types we must avoid templating the struct itself otherwise the combinations
 *          of FilterStep arrays and GCAMFusion types would explode.  Thus we use
 *          a boost::fusion::map to allow for any type of filter / predicate to be
 *          used in any instance of FilterStep and only template the applyFilter
 *          method only generating a copy of that method for each Data<T> type used
 *          in GCAM X GCAMFusion<T> types.
 */
struct FilterStep {
    //! The data name to select an individual element of a Data vector by comparing it
    //! against that element's Data::mDataName.  Note an empty name indicates a
    //! "wild card" and matches any element of a Data vector.
    const std::string mDataName;

    //! A "filter" that can be used to select if Data::hasTypeFlag( mDataFlag ) is
    //! true.
    const int mDataFlag;

    //! A flag to quickly indicate if no "filters" have been set.
    const bool mNoFilters;

    //! Counter to help with descendant steps
    int mNumDescendantSteps;

    //! A type list of filter types which may be used in a FilterStep.
    using FilterTypes = boost::mpl::vector<NoFilter, IndexFilter, NamedFilter, YearFilter>;

    //! A type list of filter types as pointers, such as NoFilter*, IndexFilter*, etc.
    using FilterPtrTypes = typename boost::mpl::transform<FilterTypes, boost::add_pointer<boost::mpl::_> >::type;

    //! The type for the boost fusion map which maps a Filter type to a pointer of an instance.
    using FilterMapType = typename boost::fusion::result_of::as_map<
        typename boost::fusion::result_of::as_vector<
            typename boost::mpl::transform_view<
                boost::mpl::zip_view<
                    boost::mpl::vector<FilterTypes, FilterPtrTypes> >,
                    boost::mpl::unpack_args<boost::fusion::pair<boost::mpl::_1, boost::mpl::_2> >
                >
            >::type
        >::type;

    //! The map which is used to track which, if any, "filter" has been set.  If
    //! at least one filter type has been set (mNoFilters is false) and mFilterMap
    //! gets looked up to a null value then no data will match.  For instance a Data
    //! type Subsector must be filtered with NamedFilter however if no instance of
    //! NamedFilter is set then that indicates some other filter such as YearFilter
    //! was set thus the FilterStep should not match.
    FilterMapType mFilterMap;

    /*!
     * \brief Constructor which (potentially) sets the data name and data flag filters.
     */
    FilterStep( const std::string& aDataName, const int aDataFlag = 0 ):mDataName( aDataName ), mDataFlag( aDataFlag ), mNumDescendantSteps( 0 ), mFilterMap(), mNoFilters( true ) {}

    /*!
     * \brief Constructor which (potentially) sets the data name and an IndexFilter.
     * \note The memory for aFilter will be managed by this class.
     */
    FilterStep( const std::string& aDataName, IndexFilter* aFilter ):mDataName( aDataName ), mDataFlag( 0 ), mNumDescendantSteps( 0 ), mFilterMap(), mNoFilters( false ) {
        boost::fusion::at_key<IndexFilter>( mFilterMap ) = aFilter;
    }

    /*!
     * \brief Constructor which (potentially) sets the data name and a NamedFilter.
     * \note The memory for aFilter will be managed by this class.
     */
    FilterStep( const std::string& aDataName, NamedFilter* aFilter ):mDataName( aDataName ), mDataFlag( 0 ), mNumDescendantSteps( 0 ), mFilterMap(), mNoFilters( false ) {
        boost::fusion::at_key<NamedFilter>( mFilterMap ) = aFilter;
    }

    /*!
     * \brief Constructor which (potentially) sets the data name and a YearFilter.
     * \note The memory for aFilter will be managed by this class.
     */
    FilterStep( const std::string& aDataName, YearFilter* aFilter ):mDataName( aDataName ), mDataFlag( 0 ), mNumDescendantSteps( 0 ), mFilterMap(), mNoFilters( false ) {
        boost::fusion::at_key<YearFilter>( mFilterMap ) = aFilter;
    }

    /*!
     * \brief Destructors, free memory from any set Filter objects.
     */
    ~FilterStep() {
        boost::fusion::for_each( mFilterMap, [] ( auto& aPair ) {
            delete aPair.second;
        } );
    }

    /*!
     * \brief Determine if this FilterStep was created as a "descendant step".
     * \details A FilterStep is a descendant step if the data name is empty and no
     *          other filters were set.  Descendant filter steps enable special
     *          stepping behavior in GCAMFusion that allows this FilterStep to
     *          continue to match any number of steps down the CONTAINER hierarchy.
     * \return True if configured as a descendant step.
     */
    bool isDescendantStep() const {
        return mDataName.empty() && mDataFlag == 0 && mNoFilters;
    }

    /*!
     * \brief A first cut check to see if an element of a Data vector matches.
     * \details Check the Data element to see if the Data::mDataName or Data::hasDataFlag()
     *          matches.  This provides a quick first cut to GCAMFusion to see if
     *          the current Data element should be inspected further using applyFilter.
     *          Calling matchesDataName first helps speed the search along as this
     *          method is much faster than applyFilter and can usually filter out
     *          most Data.
     * \return True if the Data passes the first cut checks and should be inspected
     *         further using applyFilter.
     */
    template<typename DataType>
    bool matchesDataName( const DataType& aData ) {
        return ( mDataName.empty() || mDataName == aData.mDataName ) && ( mDataFlag == 0 || aData.hasDataFlag( mDataFlag ) );
    }

    // Specializations for containers
    // TODO: this looks delicate, we need to make sure we know how to iterate over whatever types of data
    // is in DataType.mData

    // Specialization for a "Single" container
    template<typename DataType, typename DataVectorHandler>
    typename boost::enable_if<
        boost::mpl::and_<
            typename CheckDataFlagHelper<DataType>::is_container,
            boost::is_pointer<typename DataType::value_type>
        >,
    void>::type applyFilter( DataType& aData, DataVectorHandler& aHandler, const bool aIsLastStep ) {
        assert( matchesDataName( aData ) );

        // Note compiler error of the type Incomplete type 'XYZ' named in nested name specifier
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
                // avoid stepping into null containers
                if( aData.mData ) {
                    // We still have more steps to take so have GCAMFusion take
                    // one on this container data.
                    aHandler.pushFilterStep( aData.mData );
                    aData.mData->doDataExpansion( getDataVector );
                    getDataVector.getFullDataVector( aHandler );
                    aHandler.popFilterStep( aData.mData );
                }
            }
            else {
                // There are no more steps to take which implies the user was searching
                // for the entire container so have GCAMFusion trigger the processData
                // callback.
                aHandler.processData( aData.mData );
            }
        }
        else if( boost::fusion::at_key<ContainerIDFilterType>( mFilterMap ) ) {
            // Apply the filter that was set to determine if the container
            // matches and take a step on.
            auto& filterPred = *boost::fusion::at_key<ContainerIDFilterType>( mFilterMap );
            if( filterPred( aData.mData ) ) {
                if( !aIsLastStep ) {
                    // We still have more steps to take so have GCAMFusion take
                    // one on this container data.
                    aHandler.pushFilterStep( aData.mData );
                    aData.mData->doDataExpansion( getDataVector );
                    getDataVector.getFullDataVector( aHandler );
                    aHandler.popFilterStep( aData.mData );
                }
                else {
                    // There are no more steps to take which implies the user was searching
                    // for the entire container so have GCAMFusion trigger the processData
                    // callback.
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
            typename CheckDataFlagHelper<DataType>::is_container,
            boost::mpl::and_<has_iterator<typename DataType::value_type>, boost::mpl::not_<has_key_type<typename DataType::value_type> > >
        >,
    void>::type applyFilter( DataType& aData, DataVectorHandler& aHandler, const bool aIsLastStep ) {
        assert( matchesDataName( aData ) );
        // Note compiler error of the type Incomplete type 'XYZ' named in nested name specifier
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
                    // avoid stepping into null containers
                    if( container ) {
                        // We still have more steps to take so have GCAMFusion take
                        // one on this element of the array of container data.
                        aHandler.pushFilterStep( container );
                        container->doDataExpansion( getDataVector );
                        getDataVector.getFullDataVector( aHandler );
                        aHandler.popFilterStep( container );
                    }
                }
                else {
                    // There are no more steps to take which implies the user was searching
                    // for the entire container so have GCAMFusion trigger the processData
                    // callback on this element of the array of container data.
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
                        // We still have more steps to take so have GCAMFusion take
                        // one on this element of the array of container data.
                        aHandler.pushFilterStep( *iter );
                        (*iter)->doDataExpansion( getDataVector );
                        getDataVector.getFullDataVector( aHandler );
                        aHandler.popFilterStep( *iter );
                    }
                    else {
                        // There are no more steps to take which implies the user was searching
                        // for the entire container so have GCAMFusion trigger the processData
                        // callback on this element of the array of container data.
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
                int period = GetIndexAsPeriod::convertIndexToPeriod( aData.mData, iter - aData.mData.begin() );
                if( filterPred( period ) ) {
                    if( !aIsLastStep ) {
                        // We still have more steps to take so have GCAMFusion take
                        // one on this element of the array of container data.
                        aHandler.pushFilterStep( *iter );
                        (*iter)->doDataExpansion( getDataVector );
                        getDataVector.getFullDataVector( aHandler );
                        aHandler.popFilterStep( *iter );
                    }
                    else {
                        // There are no more steps to take which implies the user was searching
                        // for the entire container so have GCAMFusion trigger the processData
                        // callback on this element of the array of container data.
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
            typename CheckDataFlagHelper<DataType>::is_container,
            has_key_type<typename DataType::value_type>
        >,
    void>::type applyFilter( const DataType& aData, DataVectorHandler& aHandler, const bool aIsLastStep ) {
        assert( matchesDataName( aData ) );
        if( aData.mData.empty() ) {
            return;
        }
        // Note compiler error of the type Incomplete type 'XYZ' named in nested name specifier
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
                    // avoid stepping into null containers
                    if( (*iter).second ) {
                        // We still have more steps to take so have GCAMFusion take
                        // one on this element of the map of container data.
                        aHandler.pushFilterStep( (*iter).second );
                        (*iter).second->doDataExpansion( getDataVector );
                        getDataVector.getFullDataVector( aHandler );
                        aHandler.popFilterStep( (*iter).second );
                    }
                }
                else {
                    // There are no more steps to take which implies the user was searching
                    // for the entire container so have GCAMFusion trigger the processData
                    // callback on this element of the map of container data.
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
                        // We still have more steps to take so have GCAMFusion take
                        // one on this element of the map of container data.
                        aHandler.pushFilterStep( (*iter).second );
                        (*iter).second->doDataExpansion( getDataVector );
                        getDataVector.getFullDataVector( aHandler );
                        aHandler.popFilterStep( (*iter).second );
                    }
                    else {
                        // There are no more steps to take which implies the user was searching
                        // for the entire container so have GCAMFusion trigger the processData
                        // callback on this element of the map of container data.
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
                        // We still have more steps to take so have GCAMFusion take
                        // one on this element of the map of container data.
                        aHandler.pushFilterStep( (*iter).second );
                        (*iter).second->doDataExpansion( getDataVector );
                        getDataVector.getFullDataVector( aHandler );
                        aHandler.popFilterStep( (*iter).second );
                    }
                    else {
                        // There are no more steps to take which implies the user was searching
                        // for the entire container so have GCAMFusion trigger the processData
                        // callback on this element of the map of container data.
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
        typename CheckDataFlagHelper<DataType>::is_array,
    void>::type applyFilter( DataType& aData, DataVectorHandler& aHandler, const bool aIsLastStep ) {
        assert( matchesDataName( aData ) );
        if( !aIsLastStep ) {
            if( !isDescendantStep() ) {
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::WARNING );
                mainLog << "Attempting to take a step into " << aData.mDataName << " but Data is of type ARRAY" << std::endl;
            }
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
                int period = GetIndexAsPeriod::convertIndexToPeriod( aData.mData, index );
                if( filterPred( period ) ) {
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
        typename CheckDataFlagHelper<DataType>::is_simple,
    void>::type applyFilter( DataType& aData, DataVectorHandler& aHandler, const bool aIsLastStep ) {
        assert( matchesDataName( aData ) );
        if( !mNoFilters ) {
            // There was a filter set however a simple DataType can not be filtered
            // so this data will not be processed.
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Attempting to filter " << aData.mDataName << " but Data is of type SIMPLE" << std::endl;
            return;
        }
        else if( aIsLastStep ) {
            aHandler.processData( aData.mData );
        }
        else if( !isDescendantStep() ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Attempting to take a step into " << aData.mDataName << " but Data is of type SIMPLE" << std::endl;
        }
    }
};

// helper functions to convert a XPath-esque search string into FilterSteps
FilterStep* parseFilterStepStr( const std::string& aFilterStepStr );
std::vector<FilterStep*> parseFilterString( const std::string& aFilterStr );

/*!
 * \brief The GCAMFusion object is used to search any Data member at any level of
 *        the GCAM hierarchical nesting structure.  Such an object gives a foundation
 *        to build dynamic feedbacks to any part of the model.
 * \details GCAMFusion can then be used by essentially giving it a search term, list
 *          of "FilterSteps", an object that can process the results, and a GCAM
 *          CONTAINER object to start the search from.
 *          The GCAMFusion object takes four template parameters:
 *            - The type of the object that can handle the results of the search.
 *            - A boolean flag to indicate if said object will process the start of
 *              each step taken into a CONTAINER object (default is false).
 *            - A boolean flag to indicate if said object will process stepping out
 *              of a CONTAINER object (default is false).
 *            - A boolean flag to indicate if said object will process the data being
 *              found (default is true).
 *
 *          GCAMFusion will then perform the search by essentially performing a depth
 *          first search on the DataVectors of the GCAM CONTAINER objects.  At each
 *          container filtering the DataVector by the current filter step.  If the
 *          filtered value is itself a CONTAINER of more data a "step" is taken (firing
 *          the call back for pushFilterStep if configured) and activating the next
 *          FilterStep in the list.  GCAMFusion continues to recursively process like
 *          this until we reach the last FilterStep in the list.  When a FilterStep
 *          is filtering at the last step we consider this the Data the user is searching
 *          for and we fire the processData callback (if configured) giving a reference
 *          to the actual data thus allowing the callers to modify the value if they so
 *          choose.  Next as we are finished with the current CONTAINER we popFilterStep
 *          (firing the call back if so configured) and so on until the search if finished.
 *          Note GCAMFusion must provide for the special case that a FilterStep is set as
 *          a "descendant step" in which case any step into a subsequent CONTAINER is treated
 *          as taking no steps into the list of FilterSteps AND taking the typical one
 *          step into the list of FilterSteps.
 */
template<typename DataProcessor, bool ProcessPushStep=false, bool ProcessPopStep=false, bool ProcessData=true>
class GCAMFusion {
    public:
    /*
     * \brief Constructor defining how to search and process data.
     * \param aDataProcessor An instance of an object that can handle the results of the search
     *                       and any other callback configured through the template arguments.
     * \param aFilterSteps A list of FilterStep objects which provides the definition of the
     *                     search terms by filtering each CONTAINER's DataVector at each step.
     */
    GCAMFusion( DataProcessor& aDataProcessor, std::vector<FilterStep*> aFilterSteps ):mDataProcessor( aDataProcessor ), mFilterSteps( aFilterSteps), mCurrStep( 0 )
    {
        // Perform some error checking to ensure we do not have two descendant steps
        // back to back which will result in infinite recursion.
        bool wasLastDescendant = false;
        for( auto currFilter : mFilterSteps ) {
            bool isCurrDescendant = currFilter->isDescendantStep();
            if( wasLastDescendant && isCurrDescendant ) {
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::ERROR );
                mainLog << "Back to back descendant steps will cause infinite recursion." << std::endl;
                abort();
            }
            wasLastDescendant = isCurrDescendant;
        }
    }

    /*!
     * \brief Destructor
     * \warning This object does not manage any of the memory passed to it.  This
     *          is to allow users to re-use filter steps as necessary.
     */
    ~GCAMFusion() {
    }

    /*!
     * \brief Kickoff the search process starting at the given GCAM CONTAINER object.
     * \param aContainer Any CONTAINER object from which we will start applying the
     *                   filter steps.
     */
    template<typename ContainerType>
    void startFilter( ContainerType* aContainer ) {
        if( mFilterSteps.empty() ) {
            return;
        }
        // We must use the ExpandDataVector visitor pattern to ensure we get the full
        // DataVector taking into account the data vectors inherited from any base classes.
        ExpandDataVector<typename ContainerType::SubClassFamilyVector> getDataVector;
        aContainer->doDataExpansion( getDataVector );
        getDataVector.getFullDataVector( *this );
    }

    /*!
     * \brief The public call back method for ExpandDataVector.  This method is being
     *        indirectly called from startFilter or FilterStep::applyFilter.
     * \details Note that since the full data vector's actual type is determined at runtime
     *          we can not store it and instead must make this call back publicly available
     *          to ExpandDataVector.  This method is really the start of applying the current
     *          active FilterStep as it will loop over each element in the full DataVector
     *          and see if any match the filter step.  If it does the FilterStep will take
     *          it from there determining what if any steps / callbacks to mDataProccessor
     *          need to be made.
     * \param aDataVector The full DataVector to apply the current FilterStep to.
     */
    template<typename DataVectorType>
    void processDataVector( DataVectorType aDataVector ) {
        assert( mCurrStep < mFilterSteps.size() );
        bool isAtLastStep = this->isAtLastStep();
        boost::fusion::for_each( aDataVector, [this, isAtLastStep] ( auto& aData ) {
            // perform the first cut check to see if this Data element matches
            if( this->mFilterSteps[ mCurrStep ]->matchesDataName( aData ) ) {
                // Further apply more filtering to the current Data element to check
                // for instance if the element is a std::vector<Subsector*> and we
                // only want the elements of that vector for which the name is equal to
                // "gas".  The FilterStep will facilitate any further processing /
                // recursive steps to take.
                this->mFilterSteps[ mCurrStep ]->applyFilter( aData, *this, isAtLastStep );

                // explicitly handle the "descendant" step case where we need to
                // both handle taking one step down and no steps down.
                if( !isAtLastStep && this->mFilterSteps[ mCurrStep ]->isDescendantStep() ) {
                    ++this->mCurrStep;
                    if( this->mFilterSteps[ mCurrStep ]->matchesDataName( aData ) ) {
                        this->mFilterSteps[ mCurrStep ]->applyFilter( aData, *this, this->isAtLastStep() );
                    }
                    --this->mCurrStep;
                }
            }
        } );
    }

    /*!
     * \brief Callback from FilterStep to indicate that we are about to take a
     *        step into another CONTAINER and need to update the current FilterStep
     *        accordingly.
     * \details This specialization is for when GCAMFusion was instantiated with the
     *          template parameter ProcessPushStep as true and therefore will call
     *          mDataProcessor.pushFilterStep to notify it as well.
     * \param aData An instance of a GCAM object that we are about to step into.
     */
    template<typename DataType>
    typename boost::enable_if<
        boost::mpl::and_<boost::is_same<DataType, DataType>, boost::integral_constant<bool, ProcessPushStep> >,
    void>::type pushFilterStep( const DataType& aData )  {
        if( !mFilterSteps[ mCurrStep ]->isDescendantStep() ) {
            ++mCurrStep;
        }
        else {
            ++mFilterSteps[ mCurrStep ]->mNumDescendantSteps;
        }
        mDataProcessor.pushFilterStep( aData );
    }

    /*!
     * \brief Callback from FilterStep to indicate that we are about to take a
     *        step into another CONTAINER and need to update the current FilterStep
     *        accordingly.
     * \details This specialization is for when GCAMFusion was instantiated with the
     *          template parameter ProcessPushStep as false and therefore mDataProcessor
     *          is not made aware.
     * \param aData An instance of a GCAM object that we are about to step into.
     */
    template<typename DataType>
    typename boost::disable_if<boost::mpl::and_<boost::is_same<DataType, DataType>, boost::integral_constant<bool, ProcessPushStep> >,
    void>::type pushFilterStep( const DataType& aData )  {
        if( !mFilterSteps[ mCurrStep ]->isDescendantStep() ) {
            ++mCurrStep;
        }
        else {
            ++mFilterSteps[ mCurrStep ]->mNumDescendantSteps;
        }
    }

    /*!
     * \brief Callback from FilterStep to indicate that we are finished with the
     *        step into the current CONTAINER and need to update the current FilterStep
     *        accordingly.
     * \details This specialization is for when GCAMFusion was instantiated with the
     *          template parameter ProcessPopStep as true and therefore will call
     *          mDataProcessor.popFilterStep to notify it as well.
     * \param aData An instance of a GCAM object have now finished searching.
     */
    template<typename DataType>
    typename boost::enable_if<boost::mpl::and_<boost::is_same<DataType, DataType>, boost::integral_constant<bool, ProcessPopStep> >,
    void>::type popFilterStep( const DataType& aData )  {
        if( mFilterSteps[ mCurrStep ]->mNumDescendantSteps == 0 ) {
            --mCurrStep;
        }
        else {
            --mFilterSteps[ mCurrStep ]->mNumDescendantSteps;
        }
        mDataProcessor.popFilterStep( aData );
    }

    /*!
     * \brief Callback from FilterStep to indicate that we are finished with the
     *        step into the current CONTAINER and need to update the current FilterStep
     *        accordingly.
     * \details This specialization is for when GCAMFusion was instantiated with the
     *          template parameter ProcessPopStep as false and therefore mDataProcessor
     *          is not made aware.
     * \param aData An instance of a GCAM object have now finished searching.
     */
    template<typename DataType>
    typename boost::disable_if<boost::mpl::and_<boost::is_same<DataType, DataType>, boost::integral_constant<bool, ProcessPopStep> >,
    void>::type popFilterStep( const DataType& aData )  {
        if( mFilterSteps[ mCurrStep ]->mNumDescendantSteps == 0 ) {
            --mCurrStep;
        }
        else {
            --mFilterSteps[ mCurrStep ]->mNumDescendantSteps;
        }
    }

    /*!
     * \brief Callback from FilterStep to indicate that we have found some Data we
     *        have been searching for.
     * \details This specialization is for when GCAMFusion was instantiated with the
     *          template parameter ProcessData as true and therefore will call
     *          mDataProcessor.processData to notify it as well.
     * \param aData A reference to some piece of data or potentially an entire GCAM
     *              container object that is the result of the search.  Since it is
     *              given as reference users are free to modify it at their own risk.
     */
    template<typename DataType>
    typename boost::enable_if<boost::mpl::and_<boost::is_same<DataType, DataType>, boost::integral_constant<bool, ProcessData> >,
    void>::type processData( DataType& aData )  {
        mDataProcessor.processData( aData );
    }

    /*!
     * \brief Callback from FilterStep to indicate that we have found some Data we
     *        have been searching for.
     * \details This specialization is for when GCAMFusion was instantiated with the
     *          template parameter ProcessData as false and therefore mDataProcessor
     *          is not made aware.
     * \param aData A reference to some piece of data or potentially an entire GCAM
     *              container object that is the result of the search.  Since it is
     *              given as reference users are free to modify it at their own risk.
     */
    template<typename DataType>
    typename boost::disable_if<boost::mpl::and_<boost::is_same<DataType, DataType>, boost::integral_constant<bool, ProcessData> >,
    void>::type processData( DataType& aData )  {
        // nothing to do
    }

    /*!
     * \brief A helpful function to determine if the current active FilterStep is
     *        the last step in the search.
     * \return True if the current step is the last step in the search.
     */
    bool isAtLastStep() const {
        return ( mCurrStep + 1 ) == mFilterSteps.size();
    }

    protected:
    //! Any object that will handle the call backs pushFilterStep, popFilterStep,
    //! and processData as configured in the template arguments to GCAMFusion.
    DataProcessor& mDataProcessor;

    //! A list of FilterStep objects which provides the definition of the search
    //! terms by filtering each CONTAINER's DataVector at each step.
    std::vector<FilterStep*> mFilterSteps;

    //! An index into mFilterSteps which identifies which FilterStep is currently
    //! active.
    int mCurrStep;
};

#endif // _GCAM_FUSION_H_
