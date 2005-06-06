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
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include "technologies/include/technology.h"

/*! 
* \ingroup CIAM
* \brief This building supply technology class incorporates internal gains calcuation
*
* Internal gains are added to a specifed market.
*
* Temporarilly, the internal gain market is specified via data, but this will eventually be passed through 
* marketInfo
*
* \author Steve Smith
*/

class BuildingSupplyTechnology : public technology
{
public:
    BuildingSupplyTechnology(); // default construtor
    BuildingSupplyTechnology* clone() const;
    ~BuildingSupplyTechnology();
    const std::string& getXMLName1D() const;
    static const std::string& getXMLNameStatic1D();
    void production(const std::string& regionName,const std::string& prodName,double dmd, const GDP* gdp, const int per);
protected:
    const std::string& getXMLName() const; 
    bool XMLDerivedClassParse( const std::string nodeName, const xercesc::DOMNode* curr ); 
    void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toOutputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
 
    double internalLoadFraction; //!< fraction of losses from this subsector that contribute to internal loads
    std::string intGainsMarketName; //!< Name of internal gains market that this sector should use
private:
    static const std::string XML_NAME1D; //!< tag name for toInputXML
};
#endif // _BUILDING_SUPPLY_TECHNOLOGY_H_

