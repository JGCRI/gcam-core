#include <iostream>
#include <algorithm>

#include <boost/fusion/include/pair.hpp>

#include "util/base/include/factory.h"
#include "util/base/include/expand_data_vector.h"

#include "testing_classes_def.hpp"

typedef AbstractBase::SubClassFamilyVector BaseFamily;

struct NamedFilter {
    NamedFilter( const std::string& aName ):mName( aName ), mCurrFilterValue( 0 ) {}
    const std::string& mName;
    const std::string* mCurrFilterValue;
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
    void reset() {
        mCurrFilterValue = 0;
    }
};

struct YearFilter {
    YearFilter( const int aYear ):mYear( aYear ), mCurrFilterValue( 0 ) {}
    const int mYear;
    int* mCurrFilterValue;
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
    void reset() {
        mCurrFilterValue = 0;
    }
};

BOOST_MPL_HAS_XXX_TRAIT_DEF( filter_type );

struct FilterStep {
    FilterStep( const std::string& aDataName ):mDataName( aDataName ), mFilterMap( boost::fusion::make_pair<NamedFilter>( static_cast<NamedFilter*>( 0 ) ), boost::fusion::make_pair<YearFilter>( static_cast<YearFilter*>( 0 ) ) ), mNoFilters( true ) {}
    FilterStep( const std::string& aDataName, NamedFilter* aFilter ):mDataName( aDataName ), mFilterMap( boost::fusion::make_pair<NamedFilter>( aFilter ), boost::fusion::make_pair<YearFilter>( static_cast<YearFilter*>( 0 ) ) ), mNoFilters( false ) {}
    FilterStep( const std::string& aDataName, YearFilter* aFilter ):mDataName( aDataName ), mFilterMap( boost::fusion::make_pair<NamedFilter>( static_cast<NamedFilter*>( 0 ) ), boost::fusion::make_pair<YearFilter>( aFilter ) ), mNoFilters( false ) {}
    const std::string& mDataName;
    boost::fusion::map<boost::fusion::pair<NamedFilter, NamedFilter*>, boost::fusion::pair<YearFilter, YearFilter*> > mFilterMap;
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
    typename boost::enable_if<has_filter_type<DataType>, bool>::type matchesDataType( const DataType& aData ) {
        return ( mDataName.empty() || mDataName == aData.mDataName ) && ( mNoFilters || boost::fusion::at_key<typename DataType::filter_type>( mFilterMap ) );
    }
    template<typename DataType>
    typename boost::enable_if<boost::mpl::not_<has_filter_type<DataType> >, bool>::type matchesDataType( const DataType& aData ) {
        return ( mDataName.empty() || mDataName == aData.mDataName ) && ( mNoFilters );
    }
    template<typename DataType, typename DataVectorHandler>
    typename boost::enable_if<has_filter_type<DataType>, void>::type applyFilter( const DataType& aData, DataVectorHandler& aHandler ) {
        //assert( matchesDataType( aData ) );
        if( aData.mData.empty() ) {
            return;
        }
        ExpandDataVector<typename boost::remove_pointer<typename decltype( aData.mData )::value_type>::type::SubClassFamilyVector> getDataVector;
        auto iter = aData.mData.begin();
        while( ( iter = std::find_if( iter, aData.mData.end(), *boost::fusion::at_key<typename DataType::filter_type>( mFilterMap ) ) ) != aData.mData.end() ) {
            (*iter)->accept( getDataVector );
            getDataVector.getFullDataVector( aHandler );
            ++iter;
        }
    }
    template<typename DataType, typename DataVectorHandler>
    typename boost::enable_if<boost::mpl::not_<has_filter_type<DataType> >, void>::type applyFilter( const DataType& aData, DataVectorHandler& aHandler ) {
        //assert( matchesDataType( aData ) );
        //aHandler.processDataVector( aData );
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
        boost::fusion::for_each( aDataVector, [this] ( auto& aData ) {
            if( this->mFilterSteps[ mCurrStep ]->matchesDataType( aData ) ) {
                std::cout << "Begin mCurrStep is now: " << mCurrStep << std::endl;
                std::cout << "Matches!!!! " << aData.mDataName << std::endl;
                if( isAtLastStep() ) {
                    std::cout << "Done!" << std::endl;
                    mDataProcessor.processData( aData );
                    ++mCurrStep;
                }
                else {
                    std::cout << "Taking step" << std::endl;
                    this->mFilterSteps[ mCurrStep++ ]->applyFilter( aData, *this );
                }
                --mCurrStep;
                std::cout << "End mCurrStep is now: " << mCurrStep << std::endl;
            }
        } );
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
    template<typename T>
    void processData( T& aData ) {
        // something
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

