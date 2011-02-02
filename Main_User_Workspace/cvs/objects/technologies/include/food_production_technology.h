#ifndef _FOOD_PRODUCTION_TECHNOLOGY_H_
#define _FOOD_PRODUCTION_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * LEGAL NOTICE
 * This computer software was prepared by Battelle Memorial Institute,
 * hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
 * with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
 * CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
 * sentence must appear on any copies of this computer software.
 * 
 * EXPORT CONTROL
 * User agrees that the Software will not be shipped, transferred or
 * exported into any country or used in any manner prohibited by the
 * United States Export Administration Act or any other applicable
 * export laws, restrictions or regulations (collectively the "Export Laws").
 * Export of the Software may require some form of license or other
 * authority from the U.S. Government, and failure to obtain such
 * export control license may result in criminal liability under
 * U.S. laws. In addition, if the Software is identified as export controlled
 * items under the Export Laws, User represents and warrants that User
 * is not a citizen, or otherwise located within, an embargoed nation
 * (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
 *     and that User is not otherwise prohibited
 * under the Export Laws from receiving the Software.
 * 
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */


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
    static const std::string& getXMLNameStatic();
    FoodProductionTechnology* clone() const;    
    
    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               const std::string& aSubsectorName,
                               DependencyFinder* aDepFinder,
                               const IInfo* aSubsectorIInfo,
                               ILandAllocator* aLandAllocator );
    
    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const IInfo* aSubsectorInfo,
                           const Demographic* aDemographics,
                           PreviousPeriodInfo& aPrevPeriodInfo,
                           const int aPeriod );

    virtual void postCalc( const std::string& aRegionName,
                           const int aPeriod );

    virtual double calcShare( const std::string& aRegionName,
                              const std::string& aSectorName,
                              const GDP* aGDP,
                              const double aLogitExp,
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

    virtual double getNonEnergyCost( const std::string& aRegionName,
                                     const int aPeriod ) const;
    virtual void doInterpolations( const Technology* aPrevTech, const Technology* aNextTech );
    
    virtual Value getParsedShareWeight() const;
protected:
    std::string landType; //!< Type of land that will be used for this product
    double variableCost; //!< The non-land cost of producing a unit of product
    //! Flag to indicate if the variableCost was set exogenously
    bool mReadInVariableCost;
    double mNonLandCostTechChange; //!< Annual percent reduction in non-land variableCost
    double calYield; //!< optional input of calibration yield -- used only for sectors with no current production
    double calLandUsed; //!< input calibration value for land use

    double calObservedYield; //!< the calibrated observed yield
    double mMaxYield; //!< the maximum possible yield
    double agProdChange;  //!< the technological change factor

    //! Measure of multiple cropping
    double mHarvestedToCroppedLandRatio;

    //! Multiplier on profit penalty due to LUC emissions
    double mLUCPenaltyMultiplier;

    //! Amount of above ground carbon(unit?).
    double mAboveGroundCarbon;

    //! Amount of below ground carbon(unit?).
    double mBelowGroundCarbon;
    
    //! Age to mature (optional; normally used only for forests)
    int mMatureAge;        

    ILandAllocator* mLandAllocator;

    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual void acceptDerived( IVisitor* aVisitor, const int aPeriod ) const;    
    virtual const std::string& getXMLName() const;
    
    virtual double calcProfitRate( const std::string& aRegionName,
                                   const std::string& aProductName,
                                   const int aPeriod ) const;

    virtual double getTotalInputCost( const std::string& aRegionName,
                                      const std::string& aSectorName,
                                      const int aPeriod ) const;


    virtual double calcDiscountFactor() const;

    double calcSupply( const std::string& aRegionName,
                       const std::string& aProductName,
                       const int aPeriod ) const;

    void setCalLandValues( );
};

#endif // _FOOD_PRODUCTION_TECHNOLOGY_H_

