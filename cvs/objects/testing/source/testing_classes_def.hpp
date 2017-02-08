#include <iostream>
#include <vector>

#include <boost/core/noncopyable.hpp>

#include "util/base/include/data_definition_util.h"

class AbstractBase;
class Base;
class D1;
class D2;
class D3;

class AbstractBase : private boost::noncopyable {
    public:

    virtual ~AbstractBase() {}
    virtual const std::string& getXMLName() const = 0;
    virtual const std::string& getName() const = 0;
    virtual void setName( const std::string& aName ) = 0;
    virtual int doSomethingVirtual() const = 0;
    virtual double calc(const double value) const = 0;

    protected:
    DEFINE_DATA(
        DEFINE_SUBCLASS_FAMILY(AbstractBase, Base, D1, D2, D3)
    )
};

class Base : public AbstractBase {
    public:
    virtual ~Base() {}
    static const std::string& getXMLNameStatic() {
        static const std::string& XML_NAME = "Base";
        return XML_NAME;
    }
    virtual const std::string& getXMLName() const { return getXMLNameStatic(); }
    virtual const std::string& getName() const { return mName; }
    virtual void setName( const std::string& aName ) { mName = aName; }
    void print() { std::cout << "In " << getXMLNameStatic() << std::endl; }
    virtual int doSomethingVirtual() const { return 0; };
    virtual double calc(const double value) const  { return value * mCoef0; }

    protected:
    DEFINE_DATA_WITH_PARENT(
        AbstractBase,
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),
        DEFINE_VARIABLE( SIMPLE, "year", mYear, int ),
        DEFINE_VARIABLE( SIMPLE, "coef-0", mCoef0, double )
    )
};

class D1 : public Base {
    public:
    virtual ~D1() {}
    static const std::string& getXMLNameStatic() {
        static const std::string& XML_NAME = "D1";
        return XML_NAME;
    }
    virtual const std::string& getXMLName() const { return getXMLNameStatic(); }
    void print() { std::cout << "In " << getXMLNameStatic() << std::endl; }
    virtual int doSomethingVirtual() const { return 1; };
    virtual double calc(const double value) const  { return value * mCoef0 * mCoef1; }

    protected:
    DEFINE_DATA_WITH_PARENT(
        Base,
        DEFINE_VARIABLE( SIMPLE, "coef-1", mCoef1, double )
    )
};

class D2 : public D1 {
    public:
    virtual ~D2() {}
    static const std::string& getXMLNameStatic() {
        static const std::string& XML_NAME = "D2";
        return XML_NAME;
    }
    virtual const std::string& getXMLName() const { return getXMLNameStatic(); }
    void print() { std::cout << "In " << getXMLNameStatic() << std::endl; }
    virtual int doSomethingVirtual() const { return 2; };
    virtual double calc(const double value) const  { return value * mCoef0 * mCoef1 * mCoef2; }

    protected:
    DEFINE_DATA_WITH_PARENT(
        D1,
        DEFINE_VARIABLE( SIMPLE, "coef-2", mCoef2, double )
    )
};

class D3 : public Base {
public:
    virtual ~D3() {}
    static const std::string& getXMLNameStatic() {
        static const std::string& XML_NAME = "D3";
        return XML_NAME;
    }
    virtual const std::string& getXMLName() const { return getXMLNameStatic(); }
    void print() { std::cout << "In " << getXMLNameStatic() << std::endl; }
    virtual int doSomethingVirtual() const { return 3; };
    virtual double calc(const double value) const  { return value * mCoef0 * 100.0; }

    protected:
    DEFINE_DATA_WITH_PARENT(
        Base
    )
};

class Container: private boost::noncopyable {
    public:
    virtual ~Container() {}
    static const std::string& getXMLNameStatic() {
        static const std::string& XML_NAME = "container";
        return XML_NAME;
    }
    virtual const std::string& getXMLName() const { return getXMLNameStatic(); }
    void print() { std::cout << "In " << getXMLNameStatic() << std::endl; }
    void addCalc( AbstractBase* aCalc ) { mCalculators.push_back( aCalc ); }
    double doAllCalcs( const double value ) {
        double ret = 0;
        for(auto it = mCalculators.begin(); it != mCalculators.end(); ++it ) {
            ret += (*it)->calc( value );
        }
        return ret;
    }

    protected:
    DEFINE_DATA(
        DEFINE_SUBCLASS_FAMILY( Container ),
        DEFINE_VARIABLE( CONTAINER, "calculator", mCalculators, std::vector<AbstractBase*> )
    )
};

