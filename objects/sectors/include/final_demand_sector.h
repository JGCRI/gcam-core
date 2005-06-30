#ifndef _FINAL_DEMAND_SECTOR_H_
#define _FINAL_DEMAND_SECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Labratory operated by Battelle Memorial
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
* \brief Final Demand Sector class header file.
*
*  Detailed description.
*
* \author Pralit Patel
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <iosfwd>
#include <xercesc/dom/DOMNode.hpp>

#include "sectors/include/sector.h"

class Tabs;
class MarketInfo;
class DependencyFinder;

class FinalDemandSector : public Sector
{
public:
	FinalDemandSector( const std::string& aRegionName );
	virtual ~FinalDemandSector();
	virtual void calcFinalSupplyPrice( const GDP* aGDP, const int aPeriod ){};
	virtual void supply( const int aPeriod, const GDP* aGDP ){};
    virtual void operate( NationalAccount& aNationalAccount, const Demographic* aDemographic, 
        const int aPeriod );
    double getOutput( const int aPeriod ) const { return 0; }
	static const std::string& getXMLNameStatic();
    virtual void initCalc( const int period, const MarketInfo* aMarketInfo,
                           NationalAccount& nationalAccount, Demographic* aDemographics );
    virtual void completeInit( DependencyFinder* aDependencyFinder );
    virtual void setCalibratedSupplyInfo( const int aPeriod ) const {};
protected:
    virtual void setMarket();
    virtual const std::string& getXMLName() const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
	virtual bool XMLDerivedClassParseAttr( const xercesc::DOMNode* node );
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
	virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
	virtual void toOutputXMLDerived( std::ostream& out, Tabs* tabs ) const;
};

#endif // _FINAL_DEMAND_SECTOR_H_

