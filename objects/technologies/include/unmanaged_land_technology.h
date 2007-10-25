#ifndef _UNMANAGED_LAND_TECHNOLOGY_H_
#define _UNMANAGED_LAND_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
* \file unmanaged_land_technology.h
* \ingroup Objects
* \brief The UnmanagedLandTechnology class header file.
* \author Steve Smith
*/  

#include <xercesc/dom/DOMNode.hpp>

#include "technologies/include/food_production_technology.h"
#include "util/base/include/value.h"

// Forward declaration
class Tabs;
class DependencyFinder;

/*! 
* \ingroup Objects
* \brief Unmanaged land "production" technology.
* \details 
* This class allows products to be produced from unmanaged land leaves
* as well as ghg emissions. "Input" GHG objects will be driven by land area
* while "Output" GHG objects will be driven by land-area change (e.g. deforestation)
* \author Steve Smith
*/

class UnmanagedLandTechnology : public FoodProductionTechnology {
public:
    UnmanagedLandTechnology( const std::string& aName,
                                const int aYear );

    ~UnmanagedLandTechnology();
    static const std::string& getXMLNameStatic1D();
    const std::string& getXMLName1D() const;
    UnmanagedLandTechnology* clone() const;

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

    virtual double calcShare( const std::string& aRegionName,
                              const std::string& aSectorName,
                              const GDP* aGDP,
                              const int aPeriod ) const; 

    virtual void calcCost( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const int aPeriod );

    virtual void production( const std::string& aRegionName,
                             const std::string& aSectorName, 
                             double aVariableDemand,
                             double aFixedOutputScaleFactor,
                             const GDP* aGDP,
                             const int aPeriod );

    virtual void calcEmissionsAndOutputs( const std::string& aRegionName,
                                  const double aInput,
                                  const double aPrimaryOutput,
                                  const GDP* aGDP,
                                  const int aPeriod );

protected:
    bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
private:
    //! Name of leaf to use as driver for this technology
    std::string mLeafName;

    //! Conversion factor from unmanaged land area to energy
    double mCarbonToEnergy;

    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
 };

#endif // _UNMANAGED_LAND_TECHNOLOGY_H_

