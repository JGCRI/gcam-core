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
    DemandSector( const std::string aRegionName );
    virtual ~DemandSector();
    void calcFinalSupplyPrice( const GDP* aGDP, const int aPeriod );
    void supply( const GDP* aGDP, const int aPeriod );
    static const std::string& getXMLNameStatic();
    
    virtual void completeInit( const IInfo* aRegionInfo,
                               DependencyFinder* aDepFinder,
                               ILandAllocator* aLandAllocator,
                               const GlobalTechnologyDatabase* aGlobalTechDB );
    
    virtual void initCalc( NationalAccount& aNationalAccount,
                           const Demographic* aDemographics,
                           const int aPeriod );

    virtual void calcPriceElasticity( const int period );
    virtual void aggdemand( const GDP* gdp, const int period ); 
    virtual void operate( NationalAccount& aNationalAccount, const Demographic* aDemographic,
                          const int aPeriod ){}; // Passing demographic here is not good.
    virtual void csvOutputFile() const;

    virtual void dbOutput( const IndirectEmissionsCalculator* aIndEmissCalc ) const;

    virtual void calibrateSector( const int period );

    virtual double getWeightedEnergyPrice( const int aPeriod ) const;

    double getService( const int period ) const;

    double getServiceWoTC( const int period ) const;
    void scaleOutput( const int period, double scaleFactor );
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    bool perCapitaBased; //!< demand equation based on per capita GNP, true or false.
    double pElasticityBase; //!< base year energy price elasticity
    std::vector<double> service; //!< total end-use sector service
    std::vector<double> iElasticity; //!< income elasticity 
    std::vector<double> pElasticity; //!< price elasticity.
    std::vector<double> aeei; //!< autonomous end-use energy intensity parameter
    std::vector<double> techChangeCumm; //!< cummulative technical change on end-use service
    
    virtual void setMarket();
    void MCoutput_subsec( const IndirectEmissionsCalculator* aIndirectEmissCalc ) const;
    virtual double getOutput( const int aPeriod ) const;
    virtual double getPrice( const int aPeriod ) const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ); 
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual const std::string& getXMLName() const; 
    
    virtual void setOutput( const double aDemand,
                            const GDP* aGDP,
                            const int aPeriod );
private:
    static const std::string XML_NAME; //!< node name for toXML methods
};

#endif // _DEMAND_SECTOR_H_

