#ifndef _FINAL_DEMAND_SECTOR_H_
#define _FINAL_DEMAND_SECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Laboratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responsibility for the 
	use of this software.
*/

/*! 
* \file final_demand_sector.h
* \ingroup Objects
* \brief FinalDemandSector class header file.
* \author Pralit Patel
* \author Sonny Kim
*/

#include <string>
#include <xercesc/dom/DOMNode.hpp>

#include "sectors/include/sector.h"

class Tabs;
class IInfo;
class DependencyFinder;

/*!
 * \brief A sector which calculates the final demands for a region using a
 *        series of consumers.
 */
class FinalDemandSector : public Sector
{
public:
	FinalDemandSector( const std::string& aRegionName );
	virtual ~FinalDemandSector();
	virtual void calcFinalSupplyPrice( const GDP* aGDP, const int aPeriod ){};
	virtual void supply( const GDP* aGDP, const int aPeriod ){};
    virtual void operate( NationalAccount& aNationalAccount, const Demographic* aDemographic, 
        const int aPeriod );

	static const std::string& getXMLNameStatic();
    
    virtual void initCalc( NationalAccount* aNationalAccount,
                           const Demographic* aDemographics,
                           const int aPeriod );

    virtual void completeInit( const IInfo* aRegionInfo,
                               DependencyFinder* aDependencyFinder,
                               ILandAllocator* aLandAllocator,
                               const GlobalTechnologyDatabase* aGlobalTechDB );

    virtual void dbOutput( const IndirectEmissionsCalculator* aIndEmissCalc ) const {}
protected:
    virtual double getOutput( const int aPeriod ) const { return 0; }
    virtual double getPrice( const int aPeriod ) const;
    virtual void setMarket();
    virtual const std::string& getXMLName() const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
	virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
};

#endif // _FINAL_DEMAND_SECTOR_H_

