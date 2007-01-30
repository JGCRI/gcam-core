#ifndef _BUILDING_COOLING_DMD_TECHNOLOGY_H_
#define _BUILDING_COOLING_DMD_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file building_cooling_dmd_technology.h
* \ingroup CIAM
* \brief BuildingCoolingDmdTechnology class header file.
* \author Steve Smith
*/
#include <xercesc/dom/DOMNode.hpp>

#include "technologies/include/building_heating_dmd_technology.h"

// Forward declaration
class GDP;
class IInfo;


/*! 
* \ingroup CIAM
* \brief This building technology class calculates demand for building cooling services.
*
* Building demand technology objects, act differently than normal technology objects in that they 
* each generate a demand for a 
* specific building service (heating, cooling, lighting, etc.), which is then provided by 
* a supply sector.
* These technologies do not consume fuels or generate GHG emissions. These come from the supply sectors.
*
* The building cooling service is identical to the building heating service except for the use of cooling degree days
* instead of heating degree days and a change of sign on the internal gains calculation.
* \author Steve Smith
*/

class BuildingCoolingDmdTechnology : public BuildingHeatCoolDmdTechnology
{
public:
    BuildingCoolingDmdTechnology( const std::string& aName, const int aYear );
    virtual BuildingCoolingDmdTechnology* clone() const;
    virtual const std::string& getXMLName1D() const;
    static const std::string& getXMLNameStatic1D();
    
    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const IInfo* aSubsectorIInfo,
                           const Demographic* aDemographics,
                           const int aPeriod );
protected:
    double getInternalGainsSign() const;
    double getDemandFnPrefix( const std::string& regionName, const int period );
    double coolingDegreeDays; // !< Heating degree days -- cached from Sector
private:
    static const std::string XML_NAME1D; //!< tag name for toInputXML
};
#endif // _BUILDING_COOLING_DMD_TECHNOLOGY_H_

