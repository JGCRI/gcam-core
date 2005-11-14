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
* \date $Date$
* \version $Revision$
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
    virtual void completeInit( const IInfo* aRegionInfo, DependencyFinder* aDependencyFinder );
    
    virtual void initCalc( NationalAccount& aNationalAccount,
                           const Demographic* aDemographics,
                           const int aPeriod );
    
    void setCalibratedSupplyInfo( const int aPeriod ) const;
    double getOutput( const int aPeriod ) const;
	virtual void calcFinalSupplyPrice( const GDP* aGDP, const int aPeriod );
	virtual void supply( const int aPeriod, const GDP* aGDP );
    virtual void operate( NationalAccount& aNationalAccount, const Demographic* aDemographic,
                          const int aPeriod ){};
protected:
    virtual void setMarket();
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ); 
    virtual void toOutputXMLDerived( std::ostream& out, Tabs* tabs ) const {};
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const {};
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const {};
	virtual const std::string& getXMLName() const;
	void adjustForFixedOutput( const double aMarketDemand, const int aPeriod );
private:
	const static std::string XML_NAME; //!< node name for toXML methods	
};

#endif // _SUPPLY_SECTOR_H_
