#ifndef _BUILDING_SUPPLY_TECHNOLOGY_H_
#define _BUILDING_SUPPLY_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file building_supply_technology.h
* \ingroup CIAM
* \brief The building supply technology
* \author Steve Smith
*/

#include <xercesc/dom/DOMNode.hpp>
#include "technologies/include/technology.h"

// Forward declarations
class Demographic;
class NationalAccount;

/*! 
* \ingroup CIAM
* \brief This building supply technology class incorporates internal gains calcuation
*
* Internal gains are added to a specifed market.
*
* Temporarilly, the internal gain market is specified via data, but this will eventually be passed through 
* Info
*
* \author Steve Smith
*/

class BuildingSupplyTechnology : public technology
{
public:
    BuildingSupplyTechnology( const std::string& aName, const int aYear );
    BuildingSupplyTechnology* clone() const;
    ~BuildingSupplyTechnology();
    const std::string& getXMLName1D() const;
    static const std::string& getXMLNameStatic1D();
    
    virtual void production( const std::string& aRegionName,
                             const std::string& aSectorName,
                             const double aDemand,
                             const GDP* aGDP,
                             const int aPeriod );
protected:
    const std::string& getXMLName() const; 
    bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ); 
    void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
 
    double internalLoadFraction; //!< fraction of losses from this subsector that contribute to internal loads
private:
    static const std::string XML_NAME1D; //!< tag name for toInputXML
};
#endif // _BUILDING_SUPPLY_TECHNOLOGY_H_

