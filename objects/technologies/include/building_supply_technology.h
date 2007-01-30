#ifndef _BUILDING_SUPPLY_TECHNOLOGY_H_
#define _BUILDING_SUPPLY_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file building_supply_technology.h
* \ingroup CIAM
* \brief BuildingSupplyTechnology header file
* \author Steve Smith
*/

#include <xercesc/dom/DOMNode.hpp>
#include "technologies/include/technology.h"

// Forward declarations
class Demographic;
class NationalAccount;

/*! 
* \ingroup CIAM
* \brief This building supply technology class incorporates internal gains calculation
*
* Internal gains are added to a specified market.
*
* Temporarily, the internal gain market is specified via data, but this will eventually be passed through 
* Info
*
* \author Steve Smith
*/

class BuildingSupplyTechnology : public Technology
{
public:
    BuildingSupplyTechnology( const std::string& aName, const int aYear );
    virtual BuildingSupplyTechnology* clone() const;
    virtual const std::string& getXMLName1D() const;

    static const std::string& getXMLNameStatic1D();
	
    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               DependencyFinder* aDepFinder,
                               const IInfo* aSubsectorIInfo,
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

