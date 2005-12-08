#ifndef _BUILDING_HEAT_COOL_DMD_TECHNOLOGY_H_
#define _BUILDING_HEAT_COOL_DMD_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file building_heat_cool_dmd_technology.h
* \ingroup CIAM
* \brief The building heating service demand technology.
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>

#include "technologies/include/building_generic_dmd_technology.h"

// Forward declaration
class IInfo;

/*! 
* \ingroup CIAM
* \brief Abstract base class for  building heating or cooling service demand.
*
* Building demand technology objects, act differently than normal technology objects in that they 
* each generate a demand for a specific building service (heating, cooling, lighting, etc.), which is 
* then provided by a supply sector.
* These technologies do not consume fuels or generate GHG emissions. These come from the supply sectors.
*
* The building heating and cooling services are different from the generic energy service in that it has a different set of 
* coefficients for the demand function and internal gains are taken into account.
*
* \author Steve Smith
*/

class BuildingHeatCoolDmdTechnology : public BuildingGenericDmdTechnology
{
public:
    BuildingHeatCoolDmdTechnology(); // default construtor
    virtual const std::string& getXMLName1D() const = 0;
    virtual void initCalc( const IInfo* aSubsectorInfo );
    void adjustForCalibration( double subSectorDemand, const std::string& regionName,
		                       const IInfo* subsectorInfo, const int period );
private:
    static const std::string XML_NAME1D; //!< tag name for toInputXML
protected:
    bool XMLDerivedClassParse( const std::string nodeName, const xercesc::DOMNode* curr );
    void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual double getDemandFnPrefix( const std::string& regionName, const int period ) = 0;
    virtual double getEffectiveInternalGains( const std::string& regionName, const int period );
    virtual double getInternalGainsSign() const = 0;
    std::string intGainsMarketName; // !< temporary string for internal gains market name
    double aveInsulation; // !< Average insulation level -- cached from subSector
    double floorToSurfaceArea; // !< Conversion from floor space to surface area -- cached from subSector
    double fractionOfYearActive; // !< Fraction of year that this service is active
};
#endif // _BUILDING_HEAT_COOL_DMD_TECHNOLOGY_H_

