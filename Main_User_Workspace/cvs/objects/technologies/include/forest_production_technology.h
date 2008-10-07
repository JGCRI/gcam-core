#ifndef _FOREST_PRODUCTION_TECHNOLOGY_H_
#define _FOREST_PRODUCTION_TECHNOLOGY_H_
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
    ForestProductionTechnology( const std::string& aName,
                                const int aYear );

    ~ForestProductionTechnology();
    static const std::string& getXMLNameStatic1D();
    const std::string& getXMLName1D() const;
    ForestProductionTechnology* clone() const;

    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               const std::string& aSubsectorName,
                               DependencyFinder* aDepFinder,
                               const IInfo* aSubsectorIInfo,
                               ILandAllocator* aLandAllocator,
                               const GlobalTechnologyDatabase* aGlobalTechDB );
    
    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const IInfo* aSubsectorInfo,
                           const Demographic* aDemographics,
                           PreviousPeriodInfo& aPrevPeriodInfo,
                           const int aPeriod );

    virtual void calcCost( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const int aPeriod );

    virtual void production( const std::string& aRegionName,
                             const std::string& aSectorName, 
                             double aVariableDemand,
                             double aFixedOutputScaleFactor,
                             const GDP* aGDP,
                             const int aPeriod );

    int getHarvestPeriod( const int aCurrentPeriod ) const;

protected:
    bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual void acceptDerived( IVisitor* aVisitor, const int aPeriod ) const;    
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

    int getNRotationPeriodSteps( ) const;

    const std::string getFutureMarket( const std::string& aProductName ) const;

    //! Future yield stored for debugging
    double mFutureYield;

    //! Future land allocation stored for debugging
    double mFutureLand;

    //! Stored Ag productivity Change
    double mSavedAgProdChange;

};

#endif // _FOREST_PRODUCTION_TECHNOLOGY_H_

