#ifndef _BUILDING_GENERIC_TECHNOLOGY_H_
#define _BUILDING_GENERIC_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file building_generic_dmd_technology.h
* \ingroup CIAM
* \brief BuildingGenericDmdTechnology class header file
* \author Steve Smith
*/

#include <xercesc/dom/DOMNode.hpp>

#include "technologies/include/technology.h"

// Forward declaration
class GDP;
class IInfo;

/*! 
* \ingroup CIAM
* \brief This building technology class calculates demand for building energy
*        services.
* \details Building demand technology objects, act differently than normal
*          technology objects in that they each generate a demand for a specific
*          building service (heating, cooling, lighting, etc.), which is then
*          provided by a supply sector. These technologies do not consume fuels
*          or generate GHG emissions. These come from the supply sectors.
* \author Steve Smith
*/

class BuildingGenericDmdTechnology : public Technology
{
public:
    BuildingGenericDmdTechnology( const std::string& aName, const int aYear );
    virtual BuildingGenericDmdTechnology* clone() const;
    virtual ~BuildingGenericDmdTechnology();
    virtual const std::string& getXMLName1D() const;
    static const std::string& getXMLNameStatic1D();
	
    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               DependencyFinder* aDepFinder,
                               const IInfo* aSubsectorInfo,
                               ILandAllocator* aLandAllocator,
                               const GlobalTechnologyDatabase* aGlobalTechDB );

    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const IInfo* aSubsectorIInfo,
                           const Demographic* aDemographics,
                           const int aPeriod );

    virtual void postCalc( const std::string& aRegionName,
                           const int aPeriod );	
	
    virtual void production( const std::string& aRegionName,
                             const std::string& aSectorName, 
		                     double aVariableDemand,
                             double aFixedOutputScaleFactor,
                             const GDP* aGDP,
                             const int aPeriod );

	virtual double getFuelCost( const std::string& aRegionName,
                                const std::string& aSectorName,
		                        const int aPeriod ) const;
    
	virtual void calcCost( const std::string& aRegionName,
		                   const std::string& aSectorName,
		                   const int aPeriod );

    virtual double getNonEnergyCost( const int aPeriod ) const;
	
    virtual double calcShare( const std::string& aRegionName,
                              const std::string& aSectorName, 
		                      const GDP* aGDP,
                              const int aPeriod ) const;

    virtual double getEfficiency( const int aPeriod ) const;

    virtual void adjustForCalibration( double aTechnologyDemand,
                                       const std::string& aRegionName,
                                       const IInfo* aSubSectorInfo,
                                       const int aPeriod );

protected:
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual double getDemandFnPrefix( const std::string& regionName, const int period );
    virtual double getEffectiveInternalGains( const std::string& regionName, const int period );
    double saturation; //!< penetration level for this technology
    double priceElasticity; //!< Price elasticity for this demand
};
#endif // _BUILDING_GENERIC_TECHNOLOGY_H_

