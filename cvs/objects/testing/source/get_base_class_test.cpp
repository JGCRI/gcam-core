#include <iostream>

#include "util/base/include/expand_data_vector.h"
#include <boost/fusion/include/filter_view.hpp>

#include "testing_classes_def.hpp"

//template<typename T> struct PrintError;
//PrintError<boost::mpl::front<ExpandDataVector<BaseFamily>::FusionMap>::type> asdf;

struct PrintDataHandler {
    template<typename DataVectorType>
    void processDataVector( DataVectorType aDataVector ) {
        boost::fusion::for_each( aDataVector, [] ( const auto& aData ) { std::cout << aData.mDataName << " = " << aData.mData << std::endl; } );
    }
};

template<typename T>
class GetDataHandler {
    public:
    GetDataHandler( const std::string& aDataName ): mDataName( aDataName ), mNumFound( 0 ), mData( 0 ) { }
    void reset() {
        mNumFound = 0;
        mData = 0;
    }
    bool wasFound() const {
        return mNumFound > 0;
    }
    T& getData() {
        if( mNumFound == 0 ) {
            std::cout << mDataName << " was not found." << std::endl;
            abort();
        }
        else if( mNumFound > 1 ) {
            std::cout << mDataName << " was not found " << mNumFound << " times." << std::endl;
        }
        return *mData;
    }
    template<typename DataVectorType>
    void processDataVector( DataVectorType aDataVector ) {
        // We need to filter here to make sure mData's type is assignable however maybe is_same is too strict?
        // TODO: consider allowing is_convertable instead?
        boost::fusion::filter_view< DataVectorType, boost::is_same< boost::mpl::_, Data<T, SIMPLE> > > simpleDataVec( aDataVector );
        boost::fusion::for_each( simpleDataVec, *this );
    }
    void operator()( Data<T, SIMPLE>& aData ) const {
        if( aData.mDataName == mDataName ) {
            ++mNumFound;
            mData = &aData.mData;
        }
    }

    protected:
    const std::string& mDataName;
    mutable int mNumFound;
    mutable T* mData;
};

template<typename T>
void runTest( ExpandDataVector<AbstractBase::SubClassFamilyVector>& aDataVector, const std::string& aDataName, const T& aNewValue ) {
    GetDataHandler<T> getDataHandler( aDataName );
    aDataVector.getFullDataVector( getDataHandler );
    std::cout << "Was " << aDataName << " found? " <<  getDataHandler.wasFound() << " = ";
    if( getDataHandler.wasFound() ) {
        std::cout << getDataHandler.getData() << std::endl;
        getDataHandler.getData() = aNewValue;
    }
    else {
        std::cout << "NA" << std::endl;
    }
}

void runTests( AbstractBase* aContainer ) {
    std::cout << "Testing " << aContainer->getXMLName() << std::endl;
    ExpandDataVector<AbstractBase::SubClassFamilyVector> expandDataVector;
    aContainer->doDataExpansion( expandDataVector );

    // should be found
    runTest<std::string>( expandDataVector, "name", "new name" );
    runTest<int>( expandDataVector, "year", 1975 );
    runTest<double>( expandDataVector, "coef-0", 2.0 );
    // might be found based on the AbstractBase sub class
    runTest<double>( expandDataVector, "coef-1", 5.0 );
    runTest<double>( expandDataVector, "coef-2", 10.0 );
    // should not find due to unknown name
    runTest<int>( expandDataVector, "unknown", 1000 );
    // should not find due to wrong type
    runTest<double>( expandDataVector, "year", 10000.0 );
    std::cout << "Doing print all: " << std::endl;

    // Print all data
    PrintDataHandler printer;
    expandDataVector.getFullDataVector( printer );

    // should not find due to reset
    expandDataVector.reset();
    runTest<std::string>( expandDataVector, "name", "should not set name" );

    std::cout << "calc(10): " << aContainer->calc( 10.0 ) << std::endl;
}

int main() {
#if BOOST_FUSION_HAS_VARIADIC_VECTOR
    std::cout << "Has variadic" << std::endl;
#else
    std::cout << "Doesn't have variadic" << std::endl;
#endif
    runTests( new Base() );
    runTests( new D1() );
    runTests( new D2() );
    runTests( new D3() );

    return 0;
}

