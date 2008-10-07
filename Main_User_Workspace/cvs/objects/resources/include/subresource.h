#ifndef _SUBRSRC_H_
#define _SUBRSRC_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * LEGAL NOTICE
 * This computer software was prepared by Battelle Memorial Institute,
 * hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
 * with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
 * CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
 * sentence must appear on any copies of this computer software.
 * 
 * EXPORT CONTROL
 * User agrees that the Software will not be shipped, transferred or
 * exported into any country or used in any manner prohibited by the
 * United States Export Administration Act or any other applicable
 * export laws, restrictions or regulations (collectively the "Export Laws").
 * Export of the Software may require some form of license or other
 * authority from the U.S. Government, and failure to obtain such
 * export control license may result in criminal liability under
 * U.S. laws. In addition, if the Software is identified as export controlled
 * items under the Export Laws, User represents and warrants that User
 * is not a citizen, or otherwise located within, an embargoed nation
 * (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
 *     and that User is not otherwise prohibited
 * under the Export Laws from receiving the Software.
 * 
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */


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
    std::vector<double> severanceTax; //!< Severance Tax (exogenous)
    std::vector<double> available; //!< total available resource
    std::vector<double> annualprod; //!< annual production of SubResource
    std::vector<double> cumulprod; //!< cumulative production of SubResource
    std::vector<double> gdpExpans; //!< short-term supply limit expansion elasticity w/ gdp
    std::vector<double> cumulativeTechChange; //!< Cumulative Technical Change for this subsector
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
