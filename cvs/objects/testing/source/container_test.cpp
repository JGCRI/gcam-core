#include <iostream>

#include <boost/fusion/include/pair.hpp>

#include "util/base/include/factory.h"
#include "util/base/include/expand_data_vector.h"

#include "testing_classes_def.hpp"

typedef AbstractBase::SubClassFamilyVector BaseFamily;

struct GetIndexAsYear {
    template<typename T>
    static int convertIterToYear( const std::vector<T>& aArray, const typename std::vector<T>::iterator& aIter ) {
        return -1;
    }
};

struct NoFilter {
    using filter_value_type = void*;
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
    IndexFilter( const int aIndex ):mIndex( aIndex ), mCurrFilterValue( 0 ) {}
    const int mIndex;
    filter_value_type mCurrFilterValue;
    bool operator()( const int aIndex ) {
        if( mIndex == aIndex ) {
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
    NamedFilter( const std::string& aName ):mName( aName ), mCurrFilterValue( 0 ) {}
    const std::string& mName;
    filter_value_type mCurrFilterValue;
    template<typename T>
    bool operator()( const T* aContainer ) {
        if( mName == aContainer->getName() ) {
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
    YearFilter( const int aYear ):mYear( aYear ), mCurrFilterValue( 0 ) {}
    const int mYear;
    filter_value_type mCurrFilterValue;
    template<typename T>
    bool operator()( const T* aContainer ) {
        if( mYear == aContainer->getYear() ) {
            mCurrFilterValue = &aContainer->getYear();
            return true;
        }
        else {
            return false;
        }
    }
    // specialization where the year has been converted for us
    bool operator()( const int* aYear ) {
        if( mYear == *aYear ) {
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
    const std::string& mDataName;
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
    void>::type applyFilter( const DataType& aData, DataVectorHandler& aHandler, const bool aIsLastStep ) {
        //assert( matchesDataName( aData ) );
        // Do not process null data.
        if( !aData.mData ) {
            return;
        }
        ExpandDataVector<typename boost::remove_pointer<decltype( aData.mData )>::type::SubClassFamilyVector> getDataVector;
        if( mNoFilters ) {
            // No filters accept all containers
            // use NULL as the current filter value as no filter is set
            typename DataType::filter_type::filter_value_type nullFilterValue = 0;
            if( !aIsLastStep ) {
                aHandler.pushFilterStep( aData.mData, nullFilterValue );
                aData.mData->accept( getDataVector );
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
                    aData.mData->accept( getDataVector );
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
            boost::mpl::not_<has_key_type<typename DataType::value_type> >
        >,
    void>::type applyFilter( const DataType& aData, DataVectorHandler& aHandler, const bool aIsLastStep ) {
        //assert( matchesDataName( aData ) );
        if( aData.mData.empty() ) {
            return;
        }
        ExpandDataVector<typename boost::remove_pointer<typename decltype( aData.mData )::value_type>::type::SubClassFamilyVector> getDataVector;
        if( mNoFilters ) {
            // No filters accept all element containers
            // use NULL as the current filter value as no filter is set
            typename DataType::filter_type::filter_value_type nullFilterValue = 0;
            for( auto container : aData.mData ) {
                if( !aIsLastStep ) {
                    aHandler.pushFilterStep( container, nullFilterValue );
                    container->accept( getDataVector );
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
                        (*iter)->accept( getDataVector );
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
                        (*iter)->accept( getDataVector );
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
        ExpandDataVector<typename boost::remove_pointer<typename decltype( aData.mData )::mapped_type>::type::SubClassFamilyVector> getDataVector;
        if( mNoFilters ) {
            // No filters accept all element containers
            // use NULL as the current filter value as no filter is set
            typename DataType::filter_type::filter_value_type nullFilterValue = 0;
            for( auto iter = aData.mData.begin(); iter != aData.mData.end(); ++iter ) {
                if( !aIsLastStep ) {
                    aHandler.pushFilterStep( (*iter).second, nullFilterValue );
                    (*iter).second->accept( getDataVector );
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
                        (*iter).second->accept( getDataVector );
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
            for( auto iter = aData.mData.begin(); iter != aData.mData.end(); ++iter ) {
                if( filterPred( iter - aData.mData.begin() ) ) {
                    if( !aIsLastStep ) {
                        aHandler.pushFilterStep( (*iter).second, filterPred.getCurrValue() );
                        (*iter).second->accept( getDataVector );
                        getDataVector.getFullDataVector( aHandler );
                        aHandler.popFilterStep( (*iter).second );
                    }
                    else {
                        aHandler.processData( (*iter).second );
                    }
                }
            }
        }
        else {
            // The filter set does not match the filter aData can be filtered by
            // so no element containers will be accepted
        }
    }

    // Specializations for arrays of non-containers i.e. actual data
    template<typename DataType, typename DataVectorHandler>
    typename boost::enable_if<boost::is_same<DataType, ArrayData<typename DataType::value_type> >, void>::type applyFilter( const DataType& aData, DataVectorHandler& aHandler, const bool aIsLastStep ) {
        //assert( matchesDataName( aData ) );
        if( aData.mData.empty() ) {
            return;
        }
        if( !aIsLastStep ) {
            // error?
            return;
        }
        if( mNoFilters ) {
            // No filters accept all element
            for( auto dataElement : aData.mData ) {
                aHandler.processData( dataElement );
            }
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
            for( auto iter = aData.mData.begin(); iter != aData.mData.end(); ++iter ) {
                if( filterPred( iter - aData.mData.begin() ) ) {
                    aHandler.processData( *iter );
                }
            }
        }
        else {
            // The filter set does not match the filter aData can be filtered by
            // so no element containers will be accepted
        }
    }
    // Specializations for non-containers i.e. actual data that is a single value
    template<typename DataType, typename DataVectorHandler>
    typename boost::enable_if<boost::is_same<DataType, Data<typename DataType::value_type> >, void>::type applyFilter( const DataType& aData, DataVectorHandler& aHandler, const bool aIsLastStep ) {
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

template<typename DataProcessor>
class GCAMParamResetAPI {
    public:
    GCAMParamResetAPI( DataProcessor& aDataProcessor, std::vector<FilterStep*> aFilterSteps ):mDataProcessor( aDataProcessor ), mFilterSteps( aFilterSteps), mCurrStep( 0 ) {}
    template<typename ContainerType>
    void startFilter( ContainerType* aContainer ) {
        if( mFilterSteps.empty() ) {
            return;
        }
        ExpandDataVector<typename ContainerType::SubClassFamilyVector> getDataVector;
        aContainer->accept( getDataVector );
        getDataVector.getFullDataVector( *this );
    }

    template<typename DataVectorType>
    void processDataVector( DataVectorType aDataVector ) {
        //assert( mCurrStep < mFilterSteps.size() );
        bool isAtLastStep = this->isAtLastStep();
        std::cout << "Curr step is: " << mCurrStep << ", at end? " << isAtLastStep << std::endl;
        boost::fusion::for_each( aDataVector, [this, isAtLastStep] ( auto& aData ) {
            if( this->mFilterSteps[ mCurrStep ]->matchesDataName( aData ) ) {
                std::cout << "Matches!!!! " << aData.mDataName << std::endl;
                this->mFilterSteps[ mCurrStep ]->applyFilter( aData, *this, isAtLastStep );
            }
        } );
    }

    template<typename DataType, typename IDType>
    void pushFilterStep( const DataType& aData, const IDType* aIDValue )  {
        ++mCurrStep;
        std::cout << "Pushed step: " << mCurrStep << std::endl;
        mDataProcessor.pushFilterStep( aData, aIDValue );
    }

    template<typename DataType>
    void popFilterStep( const DataType& aData )  {
        --mCurrStep;
        std::cout << "Popped step: " << mCurrStep << std::endl;
        mDataProcessor.popFilterStep( aData );
    }

    template<typename DataType>
    void processData( DataType& aData )  {
        std::cout << "Done!" << std::endl;
        mDataProcessor.processData( aData );
    }

    bool isAtLastStep() const {
        return ( mCurrStep + 1 ) == mFilterSteps.size();
    }

    protected:
    DataProcessor mDataProcessor;

    std::vector<FilterStep*> mFilterSteps;

    int mCurrStep;
};

struct DoSomeThingNotSure {
    template<typename DataType, typename IDType>
    void pushFilterStep( const DataType& aData, const IDType* aIDValue )  {
        if( aIDValue ) {
            std::cout << "Pushed: " << *aIDValue << std::endl;
        }
    }
    template<typename DataType>
    void popFilterStep( const DataType& aData )  {
    }
    template<typename T>
    void processData( T& aData ) {
        std::cout << "Saw: " << aData << std::endl;
    }
};

void addCalc( Container* aContainer, const std::string& aXMLName, const std::string& aName ) {
    AbstractBase* newCalc = Factory<BaseFamily>::createType( aXMLName );
    newCalc->setName( aName );
    aContainer->addCalc( newCalc );
}

int main() {
    Container container;
    std::string calcs[] = { "Base", "D1", "D2", "D3" };
    std::string names[] = { "first", "second", "third", "fourth" };
    for( int i = 0; i < 4; ++i ) {
        addCalc( &container, calcs[i], names[i] );
    }

    NamedFilter nFilter( "second" );
    YearFilter yFilter( 1975 );
    FilterStep aStep("calculator", &nFilter );
    FilterStep aStep2("", &nFilter );
    FilterStep wildCardStep("");
    FilterStep yearStep("", &yFilter);
    std::cout << "isDescendantStep? " << aStep.isDescendantStep() << std::endl;
    std::cout << "isDescendantStep? " << aStep2.isDescendantStep() << std::endl;
    std::cout << "isDescendantStep? " << wildCardStep.isDescendantStep() << std::endl;

    std::vector<FilterStep*> steps( 2 );
    steps[0] = &aStep;
    steps[1] = &wildCardStep;
    DoSomeThingNotSure notSure;
    GCAMParamResetAPI<DoSomeThingNotSure> api(notSure, steps);
    api.startFilter( &container );
    std::cout << nFilter.mCurrFilterValue << std::endl;
    return 0;
}

