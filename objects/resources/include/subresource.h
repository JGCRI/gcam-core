#ifndef _SUBRSRC_H_
#define _SUBRSRC_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file subresource.h
* \ingroup Objects
* \brief The SubResource class header file.
* \author Sonny Kim
*/
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/ivisitable.h"

// Forward declarations.
class Grade;
class GDP;
class IInfo;
class IVisitor;

/*! 
* \ingroup Objects
* \brief SubResource is a class that contains grades.
* \author Sonny Kim
*/

class SubResource: public IVisitable
{
	friend class XMLDBOutputter;
public:
    SubResource();
    virtual ~SubResource();
    std::string getName() const;
    void XMLParse( const xercesc::DOMNode* tempnode );
    virtual void completeInit( const IInfo* aResourceInfo );
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    static const std::string& getXMLNameStatic();
    virtual void cumulsupply(double prc,int per);
    virtual void initCalc( const std::string& aRegionName, const std::string& aResourceName, const int aPeriod );
    virtual void postCalc( const std::string& aRegionName, const std::string& aResourceName, const int aPeriod );
    double getCumulProd( const int aPeriod ) const;
    virtual void annualsupply( int per, const GDP* gdp, double price1, double price2 );
    double getAnnualProd(int per) const;
    double getAvailable(int per) const;
    void dbOutput( const std::string& regname, const std::string& secname); 
    void csvOutputFile(const std::string &regname, const std::string& sname); 
    void updateAvailable( const int period );
	virtual double getVariance() const;
	virtual double getAverageCapacityFactor() const;
	virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    virtual const std::string& getXMLName() const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* node ) = 0;
    virtual void toXMLforDerivedClass( std::ostream& out, Tabs* tabs ) const;

    std::string name; //!< SubResource name
    std::auto_ptr<IInfo> mSubresourceInfo; //!< The subsector's information store.
    double priceElas; //!< price elasticity for short-term supply limit
    double minShortTermSLimit; //!< short-term supply limit.
    std::vector<double> techChange; //!< technical change
    std::vector<double> environCost; //!< Environmental costs
    std::vector<double> severanceTax; //!< Severence Tax (exogenous)
    std::vector<double> available; //!< total available resource
    std::vector<double> annualprod; //!< annual production of SubResource
    std::vector<double> cumulprod; //!< cumulative production of SubResource
    std::vector<double> gdpExpans; //!< short-term supply limit expansion elasticity w/ gdp
    std::vector<double> cumulativeTechChange; //!< Cumulative Technical Change for this sub-sector
    // Cumulative technical change needs to be in sub-resource sector 
    std::vector<Grade*> grade; //!< amount of SubResource for each grade
    std::map<std::string,int> gradeNameMap; //!< Map of grade name to integer position in vector. 

private:
    static const std::string XML_NAME; //!< node name for toXML methods
};


/*! 
* \ingroup Objects
* \brief A class which defines a SubDepletableResource object, which is a container for multiple grade objects.
* \author Steve Smith
*/
class SubDepletableResource: public SubResource {
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* node );
};

/*! 
* \ingroup Objects
* \brief A class which defines a SubFixedResource object, which is a container for multiple grade objects.
* \author Steve Smith
*/
class SubFixedResource: public SubResource {
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* node );
};

#endif // _SUBRSRC_H_
