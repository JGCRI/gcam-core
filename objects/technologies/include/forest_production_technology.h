#ifndef _FOREST_PRODUCTION_TECHNOLOGY_H_
#define _FOREST_PRODUCTION_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
* \file forest_production_technology.h
* \ingroup Objects
* \brief The ForestProductionTechnology class header file.
* \author James Blackwood
*/  

#include <xercesc/dom/DOMNode.hpp>

#include "technologies/include/food_production_technology.h"
#include "util/base/include/value.h"

// Forward declaration
class Tabs;
class DependencyFinder;

/*! 
* \ingroup Objects
* \brief Forest production technology.
* \details TODO
* \author James Blackwood
*/

class ForestProductionTechnology : public FoodProductionTechnology {
public:
    ForestProductionTechnology( const std::string& aName, const int aYear );
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

    // TODO: Rename this to years.
    //! Cached rotation period for forests.
    unsigned int mRotationPeriod;

    //! Specified calibrated value for future production of the technology.
    Value mFutureProduction;

    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    void setCalLandValues( );

    virtual double calcProfitRate( const std::string& aRegionName,
                                   const std::string& aProductName,
                                   const int aPeriod ) const;

    virtual double calcDiscountFactor() const;

    int getHarvestPeriod( const int aCurrentPeriod ) const;

    const std::string getFutureMarket( const std::string& aProductName ) const;
};

#endif // _FOREST_PRODUCTION_TECHNOLOGY_H_

