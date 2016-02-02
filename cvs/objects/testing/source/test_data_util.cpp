#include <iostream>
#include <functional>
#include <algorithm>

#include <boost/type_traits/is_same.hpp>
#include <boost/fusion/include/filter_view.hpp>

#include <boost/fusion/include/for_each.hpp>
#include <boost/fusion/include/any.hpp>
#include <boost/fusion/include/count_if.hpp>
#include <boost/fusion/include/joint_view.hpp>

#include <boost/mpl/inherit.hpp>
#include <boost/mpl/inherit_linearly.hpp>

#include "util/base/include/data_definition_util.h"

typedef boost::mpl::vector<std::string, int, double> ValidDataPrimatives;

template<typename DataType>
class GetDataVisitor {
    public:
    GetDataVisitor( const std::string& aDataNameToFind ):mDataNameToFind ( aDataNameToFind ), mNumFound( 0 ), mFoundData( 0 ) { }
    GetDataVisitor( const GetDataVisitor& aOther ):mDataNameToFind ( aOther.mDataNameToFind ), mNumFound( 0 ), mFoundData( 0 ) {
        std::cout << "Creating copy!" << std::endl;
    }

    bool wasFound() const {
        return mNumFound > 0;
    }

    DataType& getData() const {
        if( mNumFound == 0 ) {
            std::cout << mDataNameToFind << " was not found." << std::endl;
            abort();
        }
        else if( mNumFound > 1 ) {
            std::cout << mDataNameToFind << " was not found " << mNumFound << " times." << std::endl;
        }
        return *mFoundData;
    }

    void operator()( Data<DataType>& aData ) const {
        if( aData.mDataName == mDataNameToFind ) {
            mFoundData = &aData.mData;
            ++mNumFound;
        }
    }

    template<typename T>
    void process( T* aContainer ) {
        auto fullDataVec = aContainer->getDataVector();
        boost::fusion::filter_view< decltype( fullDataVec ), boost::is_same<boost::mpl::_, Data<DataType> > > dataOfDataType( fullDataVec );
        boost::fusion::for_each( dataOfDataType, *this );
    }

    private:
    const std::string& mDataNameToFind;

    mutable int mNumFound;

    mutable DataType* mFoundData;
};

class Master : public boost::mpl::inherit_linearly< ValidDataPrimatives, boost::mpl::inherit< boost::mpl::_1, GetDataVisitor< boost::mpl::_2 > > >::type {
};

class Base {
    public:
    Base();
    virtual ~Base();

    const std::string& getName() const;
    int getYear() const;
    virtual double calc(const double value) const;

    template<typename T>
    bool setData( const std::string& aDataNameToFind, const T& aDataToSet ); 

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
        CREATE_SIMPLE_VARIABLE( mCoefA, double, "coef-a" )
    )

    auto getDataVector() -> boost::fusion::joint_view< decltype( mDataVector ), boost::fusion::vector<> > {
        boost::fusion::vector<> emptyVec;
        return boost::fusion::joint_view< decltype( mDataVector ), decltype( emptyVec )>( mDataVector, emptyVec );
    }
    public:
    friend class GetDataVisitor<std::string>;
    virtual void accept( GetDataVisitor<std::string>& aDataVisitor ) {
        aDataVisitor.process( this );
    }
    friend class GetDataVisitor<int>;
    virtual void accept( GetDataVisitor<int>& aDataVisitor ) {
        aDataVisitor.process( this );
    }
    friend class GetDataVisitor<double>;
    virtual void accept( GetDataVisitor<double>& aDataVisitor ) {
        aDataVisitor.process( this );
    }

    template<typename T>
    struct setDataHelper {
        setDataHelper( const std::string& aDataNameToFind, const T& aDataToSet ): mDataNameToFind( aDataNameToFind ), mDataToSet( aDataToSet ) {}
        const std::string& mDataNameToFind;
        const T& mDataToSet;
        void operator()( Data<T>& aData ) const {
            if( aData.mDataName == mDataNameToFind ) {
                aData.mData = mDataToSet;
            }
        }
    };
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

template<typename T>
bool Base::setData( const std::string& aDataNameToFind, const T& aDataToSet ) {
    boost::fusion::filter_view< decltype( mDataVector ), boost::is_same<boost::mpl::_, Data<T> > > dataOfTypeT( mDataVector );
    bool wasSet = boost::fusion::any( dataOfTypeT, [&aDataNameToFind, &aDataToSet] ( auto& aData ) -> bool {
        if( aData.mDataName == aDataNameToFind ) {
            aData.mData = aDataToSet;
            return true;
        } else {
            return false;
        }
    } );
    //boost::fusion::for_each( dataOfTypeT, setDataHelper<T>( aDataNameToFind, aDataToSet ) );

    return wasSet;
}

class Derived : public Base {
    public:
    Derived();
    virtual ~Derived();

    virtual double calc( const double value ) const;

    protected:

    DEFINE_DATA(
        CREATE_SIMPLE_VARIABLE( mCoefB, double, "coef-b" )
    )

    auto getDataVector() -> boost::fusion::joint_view< decltype( mDataVector ), decltype( Base::getDataVector() ) > {
        auto baseView = Base::getDataVector();
        boost::fusion::joint_view< decltype( mDataVector ), decltype( baseView ) > ret( mDataVector, baseView );
        return ret;
    }
    public:
    friend class GetDataVisitor<std::string>;
    virtual void accept( GetDataVisitor<std::string>& aDataVisitor ) {
        aDataVisitor.process( this );
    }
    friend class GetDataVisitor<int>;
    virtual void accept( GetDataVisitor<int>& aDataVisitor ) {
        aDataVisitor.process( this );
    }
    friend class GetDataVisitor<double>;
    virtual void accept( GetDataVisitor<double>& aDataVisitor ) {
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
    GetDataVisitor<T> visitor( aDataNameToFind );
    aContainer->accept( visitor );
    std::cout << "Was found? " <<  visitor.wasFound() << " = ";
    if( visitor.wasFound() ) {
        std::cout << visitor.getData() << std::endl;
        visitor.getData() = aNewValue;
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
    /*

    b->accept( visitor );

    bool wasSet = false;
    wasSet = b->setData( "name", std::string( "this is my name" ) );
    std::cout << "Was able to set name? " << wasSet << std::endl;
    wasSet = b->setData( "year", 1975 );
    std::cout << "Was able to set year? " << wasSet << std::endl;
    wasSet = b->setData( "year-wrong", 1990 );
    std::cout << "Was able to set year-wrong? " << wasSet << std::endl;
    wasSet = b->setData( "dont-have-type", *b );
    std::cout << "Was able to set dont-have-type? " << wasSet << std::endl;

    b->print( std::cout );
    std::cout << "calc(10): " << b->calc( 10.0 ) << std::endl;
    */

    std::cout << "Working on Derived" << std::endl;
    Base* d = new Derived();
    runTests( d );

    /*
    d->accept( visitor );

    wasSet = d->setData( "coef-a", 2.0 );
    std::cout << "Was able to set coef-a? " << wasSet << std::endl;
    wasSet = d->setData( "coef-b", 5.0 );
    std::cout << "Was able to set coef-b? " << wasSet << std::endl;

    d->print( std::cout );
    std::cout << "calc(10): " << d->calc( 10.0 ) << std::endl;
    */

    return 0;
}

