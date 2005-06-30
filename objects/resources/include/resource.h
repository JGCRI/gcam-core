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
* \ingroup Objects
* \brief An abstract class which defines a Resource object, which is a container for multiple Subresource objects.
* \author Sonny Kim
*/

class Resource {
public:
    Resource(); // default construtor
    virtual ~Resource();
    void XMLParse( const xercesc::DOMNode* node );
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toOutputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream &out, Tabs* tabs ) const;
    const std::string& getName() const; 
    void completeInit( const std::string& aRegionName );
    double getPrice(int per);
    void calcSupply( const std::string& regionName, const GDP* gdp, const int period );
    double getAnnualProd(int per);
    void dbOutput( const std::string& regname ); 
    void csvOutputFile( const std::string& regname ); 
    void addToDependencyGraph( std::ostream& outStream, const int period ) const;
    void setCalibratedSupplyInfo( const int period, const std::string& regionName );
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
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* node ) = 0;
    virtual const std::string& getXMLName() const = 0;
    void setMarket( const std::string& aRegionName );
	virtual void annualsupply( const std::string& regionName, int per, const GDP* gdp, double price, double prev_price );
    void cumulsupply( double prc, int per );
    void printStyle( std::ostream& outStream ) const;
};

/*! 
* \ingroup Objects
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
	RenewableResource();
	const std::string& getXMLName() const;
	static const std::string& getXMLNameStatic();
	bool XMLDerivedClassParse( const std::string& nodename, const xercesc::DOMNode* node );
protected:
	std::vector<double> resourceVariance; //!< average resource variance computed from subresources
	std::vector<double> resourceCapacityFactor; //!< average resource capacity factor computed from subresources
	void annualsupply( const std::string& regionName, int per, const GDP* gdp, double price, double prev_price );
private:
	static const std::string XML_NAME; //!< node name for toXML methods
};

#endif // _RESOURCE_H_

