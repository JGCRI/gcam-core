#ifndef _INTERM_SUPPLY_SECTOR_H_
#define _INTERM_SUPPLY_SECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
* \file interm_supply_sector.h
* \ingroup Objects
* \brief The Intermittent SupplySector class header file.
* \details Intended for computing and demanding a backup supply sector to add to
*          the supply of an intermittent resource.
* \author Marshall Wise
*/
#include <string>
#include "sectors/include/supply_sector.h"
class IInfo;
class DependencyFinder;

/*!
* \ingroup Objects
* \brief The Intermittent Resource Supply Sector.
* \details Intended for wind and solar. Takes an intermittent resource and
*          determines the demand for supply from a back-up sector, especially if
*          needed for electricity.
* \author Marshall Wise
*/
class IntermittentSupplySector: public SupplySector
{
public:
    explicit IntermittentSupplySector( const std::string& aRegionName );
    static const std::string& getXMLNameStatic();
    
    virtual void completeInit( const IInfo* aRegionInfo,
                               DependencyFinder* aDepFinder,
                               ILandAllocator* aLandAllocator,
                               const GlobalTechnologyDatabase* aGlobalTechDB );

    virtual void initCalc( NationalAccount* nationalAccount,
                           const Demographic* aDemographics,
                           const int aPeriod );
protected:
    bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    
	const std::string& getXMLName() const;
    
	//! Electricity reserve margin for regional electricity sector.
    double elecReserveMargin;
    
	//! Resource backup cost in 1975 $/kW/yr(value is and should be annualized)
    double backupCost;
    
	//! Average capacity factor of total electric system to convert to total
    //! grid capacity.
    double aveGridCapacityFactor;
    
	//! Capacity factor for backup capacity (to convert energy output to
    //! capacity.
    double backupCapacityFactor; 
};

#endif // _INTERM_SUPPLY_SECTOR_H_
