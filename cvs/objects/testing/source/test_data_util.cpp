#include <iostream>
#include <map>
#include <functional>
#include <algorithm>

#include <boost/type_traits/is_same.hpp>
#include <boost/fusion/include/filter_view.hpp>

#include <boost/fusion/include/for_each.hpp>
#include <boost/fusion/include/any.hpp>
#include <boost/fusion/include/count_if.hpp>
#include <boost/fusion/include/joint_view.hpp>

#include <boost/mpl/for_each.hpp>
#include <boost/mpl/quote.hpp>
#include <boost/mpl/apply.hpp>
#include <boost/mpl/inherit.hpp>
#include <boost/mpl/inherit_linearly.hpp>

#include "util/base/include/data_definition_util.h"

std::ostream& operator<<( std::ostream& out, const std::map<int, double>& m ) {
    return out << "MAPasdfasdf";
}

typedef boost::mpl::vector<std::string, int, double, std::map<int, double>> ValidDataPrimatives;

template<typename T>
struct add_value_type {
    using type=typename T::value_type;
};


template<typename DataType>
class GetDataWrapper {
    public:
    GetDataWrapper():mNumFound( 0 ), mFoundData( 0 ) { }
    GetDataWrapper( const GetDataWrapper& aOther ):mNumFound( 0 ), mFoundData( 0 ) {
        std::cout << "Creating copy wrapper!" << std::endl;
    }

    virtual const std::string& getDataNameToFind() const = 0;

    void reset() {
        mNumFound = 0;
        mFoundData = 0;
    }

    bool wasFound() const {
        return mNumFound > 0;
    }

    DataType& getData() const {
        if( mNumFound == 0 ) {
            std::cout << getDataNameToFind() << " was not found." << std::endl;
            abort();
        }
        else if( mNumFound > 1 ) {
            std::cout << getDataNameToFind() << " was not found " << mNumFound << " times." << std::endl;
        }
        return *mFoundData;
    }

    mutable int mNumFound;

    mutable DataType* mFoundData;
};

class GetDataVisitor : public boost::mpl::inherit_linearly< ValidDataPrimatives, boost::mpl::inherit< boost::mpl::_1, GetDataWrapper< boost::mpl::_2 > > >::type {
    public:
    GetDataVisitor( const std::string& aDataNameToFind ):mDataNameToFind ( aDataNameToFind ) { }
    GetDataVisitor( const GetDataVisitor& aOther ):mDataNameToFind ( aOther.mDataNameToFind ) {
        std::cout << "Creating copy visitor!" << std::endl;
    }

    virtual const std::string& getDataNameToFind() const {
        return mDataNameToFind;
    }

    template<typename T>
    void process( T* aContainer ) {
        using namespace boost::mpl;
        auto fullDataVec = aContainer->getDataVector();
        boost::fusion::filter_view< decltype( fullDataVec ),
            boost::is_same<_,
                           bind<quote1<Data>, bind<quote1<add_value_type>, _> >
                        > > simpleDataVec( fullDataVec );
        boost::fusion::for_each( simpleDataVec, *this );
    }

    template<typename DataType>
    void operator()( Data<DataType>& aData ) const {
        if( aData.mDataName == mDataNameToFind ) {
            static_cast<const GetDataWrapper<DataType>*>( this )->mFoundData = &aData.mData;
            static_cast<const GetDataWrapper<DataType>*>( this )->mNumFound++;
            // TODO: track the number of foudn across all types too?
        }
    }

    void reset() {
        GetDataVisitor* thisWrapper = this;
        boost::mpl::for_each<ValidDataPrimatives>( [&thisWrapper] (auto d) {
            typedef decltype( d ) DataType;
            static_cast<GetDataWrapper<DataType>*>( thisWrapper )->reset();
        } );
    }

    protected:
    const std::string& mDataNameToFind;
};

class Base {
    public:
    Base();
    virtual ~Base();

    const std::string& getName() const;
    int getYear() const;
    virtual double calc(const double value) const;

    struct printData {
        printData( std::ostream& aOut ):mOut( aOut ) {}
        std::ostream& mOut;
        template<typename T>
        void operator()( const T& aData ) const {
            mOut << aData.mDataName << " = " << aData.mData << std::endl;
        }
    };

    virtual void print(std::ostream& aOut) {
        aOut << typeid(this).name() << ':' << std::endl;
        //boost::fusion::for_each( mDataVector, printData( aOut ) );
        boost::fusion::for_each( mDataVector, [&aOut] ( const auto& aData ) { aOut << aData.mDataName << " = " << aData.mData << std::endl; } );
    }

    protected:
    DEFINE_DATA(
        CREATE_SIMPLE_VARIABLE( mName, std::string, "name" ),
        CREATE_SIMPLE_VARIABLE( mYear, int, "year" ),
        CREATE_SIMPLE_VARIABLE( mCoefA, double, "coef-a" ),
        CREATE_SIMPLE_VARIABLE( mMap, std::map<int, double>, "some-map" )
    )

    auto getDataVector() -> boost::fusion::joint_view< decltype( mDataVector ), boost::fusion::vector<> > {
        boost::fusion::vector<> emptyVec;
        return boost::fusion::joint_view< decltype( mDataVector ), decltype( emptyVec )>( mDataVector, emptyVec );
    }
    public:
    friend class GetDataVisitor;
    virtual void accept( GetDataVisitor& aDataVisitor ) {
        aDataVisitor.process( this );
    }
};

Base::Base() {
    mCoefA = 0.5;
}

Base::~Base() {
}

const std::string& Base::getName() const {
    return mName;
}

int Base::getYear() const {
    return mYear;
}

double Base::calc(const double value) const {
    return value * mCoefA;
}

class Derived : public Base {
    public:
    Derived();
    virtual ~Derived();

    virtual double calc( const double value ) const;

    protected:

    DEFINE_DATA_WITH_PARENT(
        Base,
        CREATE_SIMPLE_VARIABLE( mCoefB, double, "coef-b" )
    )

    auto getDataVector() -> boost::fusion::joint_view< decltype( mDataVector ), decltype( Base::getDataVector() ) > {
        auto baseView = Base::getDataVector();
        boost::fusion::joint_view< decltype( mDataVector ), decltype( baseView ) > ret( mDataVector, baseView );
        return ret;
    }
    public:
    friend class GetDataVisitor;
    virtual void accept( GetDataVisitor& aDataVisitor ) {
        aDataVisitor.process( this );
    }
};

Derived::Derived() {
    mCoefB = 0.2;
}

Derived::~Derived() {}

double Derived::calc( const double value ) const {
    return value * mCoefA * mCoefB;
}

template<typename T>
void runTest( Base* aContainer, const std::string& aDataNameToFind, T aNewValue ) {
    GetDataVisitor visitor( aDataNameToFind );
    GetDataWrapper<T>& visitorTypeT = static_cast<GetDataWrapper<T>&>( visitor );
    aContainer->accept( visitor );
    std::cout << "Was found? " <<  visitorTypeT.wasFound() << " = ";
    if( visitorTypeT.wasFound() ) {
        std::cout << visitorTypeT.getData() << std::endl;
        visitorTypeT.getData() = aNewValue;
    }
    else {
        std::cout << "NA" << std::endl;
    }
}


void runTests(Base* aContainer) {
    aContainer->print( std::cout );
    runTest<std::string>( aContainer, "name", "new-name" );
    runTest<int>( aContainer, "year", 1975 );
    runTest<double>( aContainer, "coef-a", 2 );
    runTest<double>( aContainer, "coef-b", 5 );

    runTest<int>( aContainer, "coef-a", 100 );
    runTest<int>( aContainer, "wrong-name", 101 );
    aContainer->print( std::cout );

    std::cout << "calc(10): " << aContainer->calc( 10.0 ) << std::endl;
}


int main() {
    std::cout << "Working on Base" << std::endl;
    Base* b = new Base();
    runTests( b );

    std::cout << "Working on Derived" << std::endl;
    Base* d = new Derived();
    runTests( d );

    return 0;
}

