#ifndef _DEMAND_SECTOR_H_
#define _DEMAND_SECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file demand_sector.h
* \ingroup Objects
* \brief The DemandSector class header file.
* \author Sonny Kim
*/

#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/sector.h"

// Forward declarations
class GDP;
class NationalAccount;
class Demographic;
class DependencyFinder;
class IInfo;

/*! 
* \ingroup Objects
* \brief A class which defines a single demand sector.
*
*  The demand sector is derived from the sector class.  The demand sector
*  is similar to the supply sector except that it represents a service sector
*  and incorporates a demand function that determines the total demand for the
*  service.
*  The demand sector is not a Final Demand sector, but combines a service sector with 
*  a final demand for the service.
*
*  In the future, the demand sector should be treated as a supply sector and a 
*  separate Final Demand Sector class should be created to drive the demand for
*  the service.  This is representative of the general equilibrium framework.
*
* \author Sonny Kim
*/

class DemandSector: public Sector
{
public:
    explicit DemandSector( const std::string& aRegionName );
    virtual ~DemandSector();
    void calcFinalSupplyPrice( const GDP* aGDP, const int aPeriod );
    void supply( const GDP* aGDP, const int aPeriod );
    static const std::string& getXMLNameStatic();
    
    virtual void completeInit( const IInfo* aRegionInfo,
                               DependencyFinder* aDepFinder,
                               ILandAllocator* aLandAllocator,
                               const GlobalTechnologyDatabase* aGlobalTechDB );
    
    virtual void initCalc( NationalAccount* aNationalAccount,
                           const Demographic* aDemographics,
                           const int aPeriod );
    
    virtual void calcAggregateDemand( const GDP* aGDP,
                                      const Demographic* aDemographic,
                                      const int aPeriod );

    virtual void operate( NationalAccount& aNationalAccount, const Demographic* aDemographic,
                          const int aPeriod ){}; // Passing demographic here is not good.

    virtual void dbOutput( const GDP* aGDP,
                           const IndirectEmissionsCalculator* aIndEmissCalc ) const;

    virtual void csvOutputFile( const GDP* aGDP,
                                const IndirectEmissionsCalculator* aIndirectEmissCalc ) const;

    virtual void calibrateSector( const GDP* aGDP, const int aPeriod );
    
    double getWeightedEnergyPrice( const GDP* aGDP, const int aPeriod ) const;

    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    //! Demand equation based on per capita GNP, true or false.
    // TODO: Use derived classes to remove this attribute.
    bool mIsPerCapitaBased;

    //! Whether the demand function is PPP.
    bool mIsPPP;

    //! Total end-use sector service. This parameter is read-in for base years
    //! and calculated by the aggregate demand equation for all other years.
    std::vector<double> mService;
    
    //! Read-in income elasticity.
    std::vector<double> mIncomeElasticity;
    
    //! Calculated price elasticity.
    std::vector<double> mPriceElasticity;
    
    //! Autonomous end-use energy intensity parameter.
    std::vector<double> mAEEI;
    
    //! Final energy to calibrate to.
    std::vector<double> mCalFinalEnergy;

    //! Scaler for determining demand for future years.
    std::vector<double> mBaseScaler;

    void scaleOutput( const int period, double scaleFactor );
    double getService( const int period ) const;
    double getEnergyInput( const int period ) const;

    virtual void setMarket();
    virtual void MCoutput_subsec( const GDP* aGDP,
                                  const IndirectEmissionsCalculator* aIndirectEmissCalc ) const;

    virtual double getOutput( const int aPeriod ) const;
    virtual double getPrice( const GDP* aGDP, const int aPeriod ) const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ); 
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;

    virtual const std::string& getXMLName() const; 
    double getAdjustedPriceElasticity( const GDP* aGDP, const int aPeriod ) const;
    double getTechnicalChange( const int aPeriod ) const;
    double getFuelPriceRatio( const GDP* aGDP, const int aPeriod ) const;
    
    virtual void setOutput( const double aDemand,
                            const GDP* aGDP,
                            const int aPeriod );
};

#endif // _DEMAND_SECTOR_H_

