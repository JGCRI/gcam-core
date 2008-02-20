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

class FoodProductionTechnology : public Technology {
    friend class XMLDBOutputter;
public:
    FoodProductionTechnology( const std::string& aName,
                              const int aYear );
    ~FoodProductionTechnology();
    static const std::string& getXMLNameStatic1D();
    FoodProductionTechnology* clone() const;    
    
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

    virtual double calcShare( const std::string& aRegionName,
                              const std::string& aSectorName,
                              const GDP* aGDP,
                              const int aPeriod ) const; 
    
    virtual void production( const std::string& aRegionName,
                             const std::string& aSectorName, 
                             double aVariableDemand,
                             double aFixedOutputScaleFactor,
                             const GDP* aGDP,
                             const int aPeriod );

    virtual void calcCost( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const int aPeriod );

    virtual double getFuelCost( const std::string& aRegionName,
                                const std::string& aSectorName,
                                const int aPeriod ) const;

    virtual double getNonEnergyCost( const int aPeriod ) const;

    virtual double getEfficiency( const int aPeriod ) const;

    virtual void adjustForCalibration( double aTechnologyDemand,
                                       const std::string& aRegionName,
                                       const IInfo* aSubsectorInfo,
                                       const int aPeriod );
protected:
    std::string landType; //!< Type of land that will be used for this product
    double variableCost;
    double calYield; //!< optional input of calibration yield -- used only for sectors with no current production
    double calLandUsed; //!< input calibration value for land use

    double calObservedYield; //!< the calibrated observed yield
    double agProdChange;  //!< the technological change factor

    //! Measure of multiple cropping
    double mHarvestedToCroppedLandRatio;

    //! Multiplier on profit penalty due to LUC emissions
    double mLUCPenaltyMultiplier;

    //! Amount of above ground carbon(unit?).
    double mAboveGroundCarbon;

    //! Amount of below ground carbon(unit?).
    double mBelowGroundCarbon;

    ILandAllocator* mLandAllocator;

    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual void derivedVisitorAccept( IVisitor* aVisitor, const int aPeriod ) const;    
    virtual const std::string& getXMLName1D() const;
    
    virtual double calcProfitRate( const std::string& aRegionName,
                                   const std::string& aProductName,
                                   const int aPeriod ) const;

    virtual double calcDiscountFactor() const;

    double calcSupply( const std::string& aRegionName,
                       const std::string& aProductName,
                       const int aPeriod ) const;

    void setCalLandValues( );
};

#endif // _FOOD_PRODUCTION_TECHNOLOGY_H_

