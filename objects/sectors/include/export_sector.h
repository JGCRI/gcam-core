#ifndef _EXPORT_SECTOR_H_
#define _EXPORT_SECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file export_sector.h
* \ingroup Objects
* \brief The ExportSector class header file.
* \author Josh Lurz
*/
#include <string>
#include "sectors/include/supply_sector.h"
class DependencyFinder;
class IInfo;

/*! 
* \ingroup Objects
* \brief A SupplySector which can export a good to other regions.
* \details The ExportSector currently is a SupplySector with all fixed output
*          and a read-in international market. It also reads-in prices which are
*          used as the market prices for the good. It creates a non-regional
*          market for the ExportGood, and ensures that simultaneities are not
*          created for it. The ExportSector does not reset the market price.
* \todo Improve this class so more dynamic behavior is possible.
* \author Josh Lurz
*/
class ExportSector: public SupplySector
{
public:
	explicit ExportSector( const std::string& aRegionName );

	virtual void calcFinalSupplyPrice( const GDP* aGDP,
                                       const int aPeriod );

    virtual void supply( const GDP* aGDP,
                         const int aPeriod );

    static const std::string& getXMLNameStatic();
protected:
	void setMarket();
        
	virtual double getPrice( const GDP* aGDP,
                             const int aPeriod ) const;

    virtual bool XMLDerivedClassParse( const std::string& aNodeName,
                                       const xercesc::DOMNode* aCurr ); 
    
    virtual void toInputXMLDerived( std::ostream& aOut,
                                    Tabs* aTabs ) const;
    
    virtual void toDebugXMLDerived( const int aPeriod,
                                    std::ostream& aOut,
                                    Tabs* aTabs ) const;
	
    const std::string& getXMLName() const;
    
    //! Vector of read-in fixed prices.
    std::vector<double> mFixedPrices;

	//! The market region into which the sector is exporting.
	std::string mMarketName;
};

#endif // _EXPORT_SECTOR_H_
