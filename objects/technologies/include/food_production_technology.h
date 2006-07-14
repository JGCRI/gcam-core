#ifndef _FOOD_PRODUCTION_TECHNOLOGY_H_
#define _FOOD_PRODUCTION_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file food_production_technology.h
* \ingroup Objects
* \brief The FoodProductionTechnology class header file.
* \author James Blackwood
*/

#include <xercesc/dom/DOMNode.hpp>
#include "technologies/include/technology.h"

// Forward declaration
class Tabs;
class DependencyFinder;
class ILandAllocator;

/*!
* \ingroup Objects
* \brief A technology which supplies food products.
* \details TODO
* \author James Blackwood
*/

class FoodProductionTechnology : public technology {
public:
    FoodProductionTechnology( const std::string& aName, const int aYear );
    ~FoodProductionTechnology();
    static const std::string& getXMLNameStatic1D();
    FoodProductionTechnology* clone() const;    

    bool outputFixed() const;

    virtual double getCalibrationOutput( const int aPeriod ) const;

    virtual bool getCalibrationStatus() const;
    
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

    virtual void calcCost( const std::string& regionName, const std::string& sectorName, const int per );

    virtual double getFuelcost() const;
protected:
    std::string landType; //!< Type of land that will be used for this product
    double variableCost;
    double calYield; //!< optional input of calibration yield -- used only for sectors with no current production
    double calLandUsed; //!< input calibration value for land use 
    double calProduction; //!< input calibrationvalue for land production
    double calObservedYield; //!< the calibrated observed yield
    double agProdChange;  //!< the technological change factor

    //! Amount of above ground carbon(unit?).
    double mAboveGroundCarbon;

    //! Amount of below ground carbon(unit?).
    double mBelowGroundCarbon;

    ILandAllocator* mLandAllocator;
    
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual const std::string& getXMLName1D() const;
    
    virtual double calcProfitRate( const std::string& aRegionName,
                                   const std::string& aProductName,
                                   const int aPeriod ) const;

    virtual double calcDiscountFactor() const;

    double calcSupply( const std::string& aRegionName,
                       const std::string& aProductName,
                       const int aPeriod ) const;
};

#endif // _FOOD_PRODUCTION_TECHNOLOGY_H_

