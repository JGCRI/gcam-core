#ifndef _RESOURCE_H_
#define _RESOURCE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file resource.h
* \ingroup CIAM
* \brief The Resource, DepletableResource, FixedResource, and RenewableResource classes header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/
#include <xercesc/dom/DOMNode.hpp>
#include <vector>
#include <map>

// Forward declaration.
class SubResource;
class GDP;

/*! 
* \ingroup CIAM
* \brief An abstract class which defines a Resource object, which is a container for multiple Subresource objects.
* \author Sonny Kim
*/

class Resource {
public:
    Resource(); // default construtor
    virtual ~Resource();
    virtual const std::string& getXMLName() const = 0;
    void clear();
    void XMLParse( const xercesc::DOMNode* node );
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* node ) = 0; // the = 0 makes this an abstract method
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toOutputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream &out, Tabs* tabs ) const;
    std::string getName() const; 
    void completeInit();
    void setMarket( const std::string& regionName );
    double getPrice(int per); 
    void cumulsupply(double prc,int per);
    double getCummProd(int per);
    void annualsupply( int per, const GDP* gdp, double price1, double price2 );
    double getAnnualProd(int per);
    double getAvailable(int per);
    double getSubAvail( const std::string& subResourceName, const int per);
    int getNoSubrsrc();
    void show();
    void dbOutput( const std::string& regname ); 
    void csvOutputFile( const std::string& regname ); 
    void addToDependencyGraph( std::ostream& outStream, const int period ) const;
protected:
    std::string name; //!< Resource name
    std::string market; //!< regional market
    int nosubrsrc; //!< number of subsectors for each Resource
    std::vector<SubResource*> subResource; //!< subsector objects for each Resource
    std::vector<double> rscprc; //!< Resource price
    std::vector<double> available; //!< total Resource available
    std::vector<double> annualprod; //!< annual production rate of Resource
    std::vector<double> cummprod; //!< cummulative production of Resource
    std::map<std::string,int> subResourceNameMap; //!< Map of subResource name to integer position in vector. 
    void printStyle( std::ostream& outStream ) const;
};

/*! 
* \ingroup CIAM
* \brief A class which defines a DepletableResource object, which is a container for multiple Subresource objects.
* \author Josh Lurz
* \date $ Date $
* \version $ Revision $
*/
class DepletableResource: public Resource {
public: 
    const std::string& getXMLName() const;
    static const std::string& getXMLNameStatic();
    bool XMLDerivedClassParse( const std::string& nodename, const xercesc::DOMNode* node );
private:
    static const std::string XML_NAME; //!< node name for toXML methods
};

/*! 
* \ingroup Objects
* \brief A class which defines a FixedResource object, which is a container for multiple Subresource objects.
* \author Josh Lurz
* \date $ Date $
* \version $ Revision $
*/
class FixedResource: public Resource {
public: 
    const std::string& getXMLName() const;
    static const std::string& getXMLNameStatic();
    bool XMLDerivedClassParse( const std::string& nodename, const xercesc::DOMNode* node );
private:
    static const std::string XML_NAME; //!< node name for toXML methods
};

/*! 
* \ingroup Objects
* \brief A class which defines a RenewableResource object, which is a container for multiple Subresource objects.
* \author Josh Lurz
* \date $ Date $
* \version $ Revision $
*/
class RenewableResource: public Resource {
public: 
    const std::string& getXMLName() const;
    static const std::string& getXMLNameStatic();
    bool XMLDerivedClassParse( const std::string& nodename, const xercesc::DOMNode* node );
private:
    static const std::string XML_NAME; //!< node name for toXML methods
};

#endif // _RESOURCE_H_

