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
*/

#include <xercesc/dom/DOMNode.hpp>

#include "technologies/include/technology.h"

// Forward declaration
class GDP;
class IInfo;

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
    BuildingGenericDmdTechnology();
    virtual BuildingGenericDmdTechnology* clone() const;
    virtual ~BuildingGenericDmdTechnology();
    virtual const std::string& getXMLName1D() const;
    static const std::string& getXMLNameStatic1D();
    
    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const IInfo* aSubsectorIInfo,
                           const Demographic* aDemographics,
                           const int aPeriod );

    virtual void calcShare( const std::string& aRegionName,
                            const std::string& aSectorName,
                            const GDP* aGDP,
                            const int aPeriod ); 
    
    virtual void production( const std::string& aRegionName,
                             const std::string& aSectorName,
                             const double aDemand,
                             const GDP* aGDP,
                             const int aPeriod );

    virtual void adjustForCalibration( double subSectorDemand, const std::string& regionName,
		                               const IInfo* subSectorInfo, const int period );
protected:
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual double getDemandFnPrefix( const std::string& regionName, const int period );
    virtual double getEffectiveInternalGains( const std::string& regionName, const int period );
    double saturation; //!< penetration level for this technology
    double priceElasticity; //!< Price elasticity for this demand
private:
    static const std::string XML_NAME1D; //!< tag name for toInputXML
};
#endif // _BUILDING_GENERIC_TECHNOLOGY_H_

