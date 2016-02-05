#include <iostream>

#include "util/base/include/data_definition_util.h"

class AbstractBase;
class Base;
class D1;
class D2;
class D3;
typedef boost::mpl::vector<AbstractBase, Base, D1, D2, D3> BaseFamily;
//template<> class Factory<BaseFamily> {};
//typedef Factory<BaseFamily> BaseFactory;

class AbstractBase {
    public:
    virtual ~AbstractBase() {}
    virtual int doSomethingVirtual() const = 0;
    virtual double calc(const double value) const = 0;
};

class Base : public AbstractBase {
    public:
    virtual ~Base() {}
    static const std::string& getXMLNameStatic() {
        static const std::string& XML_NAME = "Base";
        return XML_NAME;
    }
    void print() { std::cout << "In " << getXMLNameStatic() << std::endl; }
    virtual int doSomethingVirtual() const { return 0; };
    virtual double calc(const double value) const  { return value * mCoef0; }

    //protected:
    DEFINE_DATA(
        CREATE_SIMPLE_VARIABLE( mName, std::string, "name" ),
        CREATE_SIMPLE_VARIABLE( mYear, int, "year" ),
        CREATE_SIMPLE_VARIABLE( mCoef0, double, "coef-0" )
    )
    // TOOD: how to put in macro
    //friend class Factory<BaseFamily>;
};

class D1 : public Base {
    public:
    virtual ~D1() {}
    static const std::string& getXMLNameStatic() {
        static const std::string& XML_NAME = "D1";
        return XML_NAME;
    }
    void print() { std::cout << "In " << getXMLNameStatic() << std::endl; }
    virtual int doSomethingVirtual() const { return 1; };
    virtual double calc(const double value) const  { return value * mCoef0 * mCoef1; }

    //protected:
    DEFINE_DATA_WITH_PARENT(
        Base,
        CREATE_SIMPLE_VARIABLE( mCoef1, double, "coef-1" )
    )
    // TOOD: how to put in macro
    //friend class BaseFactory;
};

class D2 : public D1 {
    public:
    virtual ~D2() {}
    static const std::string& getXMLNameStatic() {
        static const std::string& XML_NAME = "D2";
        return XML_NAME;
    }
    void print() { std::cout << "In " << getXMLNameStatic() << std::endl; }
    virtual int doSomethingVirtual() const { return 2; };
    virtual double calc(const double value) const  { return value * mCoef0 * mCoef1 * mCoef2; }

    //protected:
    DEFINE_DATA_WITH_PARENT(
        D1,
        CREATE_SIMPLE_VARIABLE( mCoef2, double, "coef-2" )
    )
    // TOOD: how to put in macro
    //friend class BaseFactory;
};

class D3 : public Base {
public:
    virtual ~D3() {}
    static const std::string& getXMLNameStatic() {
        static const std::string& XML_NAME = "D3";
        return XML_NAME;
    }
    void print() { std::cout << "In " << getXMLNameStatic() << std::endl; }
    virtual int doSomethingVirtual() const { return 3; };
    virtual double calc(const double value) const  { return value * mCoef0 * mCoef3; }

    //protected:
    DEFINE_DATA_WITH_PARENT(
        Base,
        CREATE_SIMPLE_VARIABLE( mCoef3, double, "coef-3" )
    )
    // TOOD: how to put in macro
    //friend class BaseFactory;
};

