#ifndef _BUILDING_GENERIC_TECHNOLOGY_H_
#define _BUILDING_GENERIC_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file building_generic_dmd_technology.h
* \ingroup CIAM
* \brief The building service demand technology.
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>

#include "technologies/include/technology.h"

// Forward declaration
class GDP;
class MarketInfo;

/*! 
* \ingroup CIAM
* \brief This building technology class calculates demand for building energy services.
*
* Building demand technology objects, act differently than normal technology objects in that they 
* each generate a demand for a 
* specific building service (heating, cooling, lighting, etc.), which is then provided by 
* a supply sector.
* These technologies do not consume fuels or generate GHG emissions. These come from the supply sectors.
*
* \author Steve Smith
*/

class BuildingGenericDmdTechnology : public technology
{
public:
    BuildingGenericDmdTechnology(); // default construtor
    virtual BuildingGenericDmdTechnology* clone() const;
    virtual ~BuildingGenericDmdTechnology();
    virtual const std::string& getXMLName1D() const;
    static const std::string& getXMLNameStatic1D();
    virtual void calcShare( const std::string& regionName, const GDP* gdp, const int per ); 
    virtual void production(const std::string& regionName,const std::string& prodName,double dmd, const GDP* gdp, const int per);
    virtual void adjustForCalibration( double subSectorDemand, const std::string& regionName, const MarketInfo* subSectorInfo, const int period ); // Adjust share weights for calibration
protected:
    virtual bool XMLDerivedClassParse( const std::string nodeName, const xercesc::DOMNode* curr ); // for derived classes
    void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toOutputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual double getDemandFnPrefix( const std::string& regionName, const int period );
    virtual double getEffectiveInternalGains( const std::string& regionName, const int period );
    double saturation; //!< penetration level for this technology
    double priceElasticity; //!< Price elasticity for this demand
private:
    static const std::string XML_NAME1D; //!< tag name for toInputXML
};
#endif // _BUILDING_GENERIC_TECHNOLOGY_H_

