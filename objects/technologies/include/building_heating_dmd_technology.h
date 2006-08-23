#ifndef _BUILDING_HEATING_DMD_TECHNOLOGY_H_
#define _BUILDING_HEATING_DMD_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file building_heating_dmd_technology.h
* \ingroup CIAM
* \brief BuildingHeatingDmdTechnology header file
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/
#include <xercesc/dom/DOMNode.hpp>

#include "technologies/include/building_heat_cool_dmd_technology.h"

// Forward declaration
class IInfo;

/*! 
* \ingroup CIAM
* \brief This building technology class calculates demand for building heating services.
*
* Building demand technology objects, act differently than normal technology objects in that they 
* each generate a demand for a specific building service (heating, cooling, lighting, etc.), which is 
* then provided by a supply sector.
* These technologies do not consume fuels or generate GHG emissions. These come from the supply sectors.
*
* The building heating service is different from the generic energy service in that it has a different set of 
* coefficients for the demand function and internal gains are taken into account.
*
* \author Steve Smith
*/

class BuildingHeatingDmdTechnology : public BuildingHeatCoolDmdTechnology
{
public:
    BuildingHeatingDmdTechnology( const std::string& aName, const int aYear );
    BuildingHeatingDmdTechnology* clone() const;
    const std::string& getXMLName1D() const;
    static const std::string& getXMLNameStatic1D();
    
    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const IInfo* aSubsectorIInfo,
                           const Demographic* aDemographics,
                           const int aPeriod );
protected:
    double getInternalGainsSign() const;
    double getDemandFnPrefix( const std::string& regionName, const int period );
    double heatingDegreeDays; // !< Heating degree days -- cached from Sector
private:
    static const std::string XML_NAME1D; //!< tag name for toInputXML
};
#endif // _BUILDING_HEATING_DMD_TECHNOLOGY_H_

