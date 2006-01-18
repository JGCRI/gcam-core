#ifndef _FOREST_PRODUCTION_TECHNOLOGY_H_
#define _FOREST_PRODUCTION_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file forest_production_technology.h
* \ingroup CIAM
* \brief The ForestProductionTechnology class header file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/  

#include <xercesc/dom/DOMNode.hpp>

#include "technologies/include/food_production_technology.h"

// Forward declaration
class Tabs;
class DependencyFinder;

/*! 
* \ingroup CIAM
* \brief This technology class is based on the MiniCAM description of technology.
*
* The technology class is where all fuels are either consumed or transformed. The default technology class is 
* based on a MiniCAM-style logit representation. This class has options for capacity limits, calibration, 
* and fixed output technologies (for supply sectors) -- although these capabilities depend on interaction with 
* the sub-sector and sector classes. 
*
* \author James Blackwood
*/

class ForestProductionTechnology : public FoodProductionTechnology {
public:
    ForestProductionTechnology();
    ~ForestProductionTechnology();
    static const std::string& getXMLNameStatic1D();
    const std::string& getXMLName1D() const;
    ForestProductionTechnology* clone() const;

    virtual void completeInit( const std::string& aSectorName,
                               DependencyFinder* aDepFinder,
                               const IInfo* aSubsectorIInfo,
                               ILandAllocator* aLandAllocator );
    
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

protected:
    bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
private:
    double interestRate;
    int rotationPeriod; //!< rotation period for forests
    double futureProduction;
    double forestLandAside;
    static const std::string prefix;
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    void setCalLandValues( );

    virtual double calcProfitRate( const std::string& aRegionName,
                                   const std::string& aProductName,
                                   const int aPeriod ) const;

    double calcDiscountFactor() const;

    int getHarvestPeriod( const int aCurrentPeriod ) const;

    const std::string getFutureMarket( const std::string& aProductName ) const;
};

#endif // _FOREST_PRODUCTION_TECHNOLOGY_H_

