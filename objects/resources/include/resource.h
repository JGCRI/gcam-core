#ifndef _RESOURCE_H_
#define _RESOURCE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file resource.h
* \ingroup Objects
* \brief The Resource, DepletableResource, FixedResource, and RenewableResource classes header file.
* \author Sonny Kim
*/
#include <xercesc/dom/DOMNode.hpp>
#include <vector>
#include <map>
#include "resources/include/aresource.h"

// Forward declaration.
class SubResource;

/*! 
* \ingroup Objects
* \brief An abstract class which defines a single resource containing multiple
*        subresources.
* \todo This class needs much more documentation.
* \todo This class and AResource need refactoring and cleaning up. FixedResource
*       should be removed, DeplatableResource and Resource should be merged, and
*       RenewableResource should inherit from AResource and be moved to its own
*       files.
* \author Sonny Kim
*/
class Resource: public AResource {
    friend class XMLDBOutputter;
public:
    Resource();
    virtual ~Resource();
    void XMLParse( const xercesc::DOMNode* node );
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream &out, Tabs* tabs ) const;
    const std::string& getName() const; 
    void completeInit( const std::string& aRegionName, const IInfo* aRegionInfo );
    
    virtual void initCalc( const std::string& aRegionName, const int aPeriod );
    virtual void postCalc( const std::string& aRegionName, const int aPeriod );
    
    void calcSupply( const std::string& regionName, const GDP* gdp, const int period );
    virtual double getAnnualProd( const std::string& aRegionName, const int aPeriod ) const;
    void dbOutput( const std::string& regname ); 
    void csvOutputFile( const std::string& regname ); 
	virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    std::string name; //!< Resource name
    std::string mOutputUnit; //!< unit of good or service produced by sector
    std::string mPriceUnit; //!< price unit of good or service produced by sector
    std::string market; //!< regional market
    int nosubrsrc; //!< number of subsectors for each Resource
    std::auto_ptr<IInfo> mResourceInfo; //!< Pointer to the sector's information store.
    std::vector<SubResource*> subResource; //!< subsector objects for each Resource
    std::vector<double> rscprc; //!< Resource price
    std::vector<double> available; //!< total Resource available
    std::vector<double> annualprod; //!< annual production rate of Resource
    std::vector<double> cummprod; //!< cummulative production of Resource
    std::map<std::string,int> subResourceNameMap; //!< Map of subResource name to integer position in vector. 
    virtual bool XMLDerivedClassParse( const std::string& aNodeName,
                                       const xercesc::DOMNode* aNode ) = 0;
    virtual const std::string& getXMLName() const = 0;
    void setMarket( const std::string& aRegionName );
    virtual void annualsupply( const std::string& regionName, int per, const GDP* gdp, double price, double prev_price );
    void cumulsupply( double prc, int per );
};

/*! 
* \ingroup Objects
* \brief A class which defines a DepletableResource object, which is a container for multiple Subresource objects.
* \author Josh Lurz
*/
class DepletableResource: public Resource {
public: 
    static const std::string& getXMLNameStatic();
protected:
    const std::string& getXMLName() const;
    bool XMLDerivedClassParse( const std::string& nodename, const xercesc::DOMNode* node );
private:
    static const std::string XML_NAME; //!< node name for toXML methods
};

/*! 
* \ingroup Objects
* \brief A class which defines a FixedResource object, which is a container for multiple Subresource objects.
* \author Josh Lurz
*/
class FixedResource: public Resource {
public: 

    static const std::string& getXMLNameStatic();
protected:
    const std::string& getXMLName() const;
    bool XMLDerivedClassParse( const std::string& nodename, const xercesc::DOMNode* node );
private:
    static const std::string XML_NAME; //!< node name for toXML methods
};

/*! 
* \ingroup Objects
* \brief A class which defines a RenewableResource object, which is a container for multiple Subresource objects.
* \author Josh Lurz
*/
class RenewableResource: public Resource {
public: 
    RenewableResource();
    static const std::string& getXMLNameStatic();
protected:
    std::vector<double> resourceVariance; //!< average resource variance computed from subresources
    std::vector<double> resourceCapacityFactor; //!< average resource capacity factor computed from subresources
    bool XMLDerivedClassParse( const std::string& nodename, const xercesc::DOMNode* node );
    const std::string& getXMLName() const;
    void annualsupply( const std::string& regionName, int per, const GDP* gdp, double price, double prev_price );
private:
    static const std::string XML_NAME; //!< node name for toXML methods
};

#endif // _RESOURCE_H_

