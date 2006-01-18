#ifndef _FOOD_SUPPLY_SECTOR_H_
#define _FOOD_SUPPLY_SECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file food_supply_sector.h
* \ingroup CIAM
* \brief The FoodSupplySector class header file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/supply_sector.h"

// Forward declarations
class ILandAllocator;

class FoodSupplySector : public SupplySector {
public:
	FoodSupplySector( std::string& aRegionName );
	virtual ~FoodSupplySector();
	static const std::string& getXMLNameStatic();
    virtual void completeInit( const IInfo* aRegionInfo,
                               DependencyFinder* aDepFinder,
                               ILandAllocator* aLandAllocator );
    virtual double getPrice( const int aPeriod ) const;
protected:
	virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
	virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
	virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
	virtual const std::string& getXMLName() const;
	virtual void setMarket();
	double calPrice;

    //! Name of the market for this good.
    std::string mMarketName;
};

#endif // _FOOD_SUPPLY_SECTOR_H_
