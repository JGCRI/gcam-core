#ifndef _BLDG_HEATING_DMD_TECHNOLOGY_H_
#define _BLDG_HEATING_DMD_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file technology.h
* \ingroup CIAM
* \brief The technology class header file.
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <map>
#include <xercesc/dom/DOMNode.hpp>

#include "technologies/include/building_dmd_technology.h"

// Forward declaration
class GDP;

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

class BuildingHeatingDmdTechnology : public BuildingDmdTechnology
{
public:
    BuildingHeatingDmdTechnology(); // default construtor
    BuildingHeatingDmdTechnology& operator=( const BuildingHeatingDmdTechnology& techIn ); // assignment operator.
    virtual BuildingHeatingDmdTechnology* clone() const;
    virtual ~BuildingHeatingDmdTechnology();
    virtual void production(const std::string& regionName,const std::string& prodName,double dmd, const GDP* gdp, const int per);
    virtual void adjustForCalibration( double subSectorDemand );
    bool XMLDerivedClassParse( const std::string nodeName, const xercesc::DOMNode* curr );
    void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toOutputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual void initCalc( std::auto_ptr<MarketInfo> mSubsectorInfo );
private:
    static const std::string XML_NAME1D; //!< tag name for toInputXML
    static const std::string XML_NAME2D; //!< tag name for toInputXML
    double internalGainFraction; //!< fraction of year that internalGains are relevant for this technology
    double heatingDegreeDays; // !< Heating degree days -- cached from Sector
    double aveInsulation; // !< Average insulation level -- cached from subSector
    double floorToSurfaceArea; // !< Conversion from floor space to surface area -- cached from subSector
protected:
};
#endif // _BLDG_HEATING_DMD_TECHNOLOGY_H_

