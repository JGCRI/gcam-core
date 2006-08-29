#ifndef _SUPPLY_SECTOR_H_
#define _SUPPLY_SECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file supply_sector.h
* \ingroup Objects
* \brief The SupplySector class header file.
* \author James Blackwood
*/
#include <string>
#include "sectors/include/sector.h"
class NationalAccount;
class IInfo;
class DependencyFinder;
/*! 
* \ingroup Objects
* \brief This class represents a single supply sector.
* \author James Blackwood
*/
class SupplySector: public Sector
{
public:
	explicit SupplySector( const std::string& aRegionName );
    virtual ~SupplySector(){};
    static const std::string& getXMLNameStatic();
    
    virtual void completeInit( const IInfo* aRegionInfo,
                               DependencyFinder* aDepFinder,
                               ILandAllocator* aLandAllocator,
                               const GlobalTechnologyDatabase* aGlobalTechDB );

    
    virtual void initCalc( NationalAccount& aNationalAccount,
                           const Demographic* aDemographics,
                           const int aPeriod );

	virtual void calcFinalSupplyPrice( const GDP* aGDP, const int aPeriod );
	
    virtual void supply( const GDP* aGDP,
                         const int aPeriod );

    virtual void operate( NationalAccount& aNationalAccount, const Demographic* aDemographic,
                          const int aPeriod ){};
protected:
    virtual double getOutput( const int aPeriod ) const;
    virtual double getPrice( const int aPeriod ) const;
    virtual void setMarket();
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ); 
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const {};
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const {};
	virtual const std::string& getXMLName() const;
	void adjustForFixedOutput( const double aMarketDemand, const int aPeriod );
private:
	const static std::string XML_NAME; //!< node name for toXML methods	
};

#endif // _SUPPLY_SECTOR_H_
