#ifndef _FOREST_DEMAND_SECTOR_H_
#define _FOREST_DEMAND_SECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file forest_demand_sector.h
* \ingroup CIAM
* \brief The ForestDemandSector class header file.
* \author James Blackwood
*/

#include "sectors/include/demand_sector.h"

// Forward declarations
class GDP;

/*! 
 * \brief A sector which demands forestry products.
 */
class ForestDemandSector: public DemandSector {
public:
    explicit ForestDemandSector( const std::string& aRegionName );
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
protected:
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ); 
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual const std::string& getXMLName() const;
    double calcForestDemand ( const GDP* gdp, const int period, const int normPeriod, double priceRatio );
private:
    int rotationPeriod;
    static const std::string prefix;
    std::string demandedGoodName;
    double perCapitaBaseOutput;
};

#endif // _FOREST_DEMAND_SECTOR_H_

