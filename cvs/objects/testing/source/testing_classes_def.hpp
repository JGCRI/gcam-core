#include <iostream>

#include "util/base/include/data_definition_util.h"

class AbstractBase;
class Base;
class D1;
class D2;
class D3;

template<typename T>
class ExpandDataVector;

class AbstractBase {
    public:
    virtual ~AbstractBase() {}
    virtual const std::string& getXMLName() const = 0;
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
    void print() { std::cout << "In " << getXMLNameStatic() << std::endl; }
    virtual int doSomethingVirtual() const { return 0; };
    virtual double calc(const double value) const  { return value * mCoef0; }

    protected:
    DEFINE_DATA_WITH_PARENT(
        AbstractBase,
        CREATE_SIMPLE_VARIABLE( mName, std::string, "name" ),
        CREATE_SIMPLE_VARIABLE( mYear, int, "year" ),
        CREATE_SIMPLE_VARIABLE( mCoef0, double, "coef-0" )
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
        CREATE_SIMPLE_VARIABLE( mCoef1, double, "coef-1" )
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
        CREATE_SIMPLE_VARIABLE( mCoef2, double, "coef-2" )
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

