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
*/

#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/supply_sector.h"

// Forward declarations
class ILandAllocator;

/*!
 * \brief A sector which supplies food products.
 */
class FoodSupplySector : public SupplySector {
public:
    explicit FoodSupplySector( std::string& aRegionName );
    virtual ~FoodSupplySector();
    static const std::string& getXMLNameStatic();
    virtual void completeInit( const IInfo* aRegionInfo,
                               DependencyFinder* aDepFinder,
                               ILandAllocator* aLandAllocator,
                               const GlobalTechnologyDatabase* aGlobalTechDB );

    virtual void supply( const GDP* aGDP, const int aPeriod );
protected:
	virtual double getPrice( const GDP* aGDP,
                             const int aPeriod ) const;

    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual const std::string& getXMLName() const;
    virtual void setMarket();

    // TODO: Should this be a vector?
    double calPrice;

    //! Name of the market for this good.
    std::string mMarketName;
};

#endif // _FOOD_SUPPLY_SECTOR_H_
