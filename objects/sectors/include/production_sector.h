#ifndef _PRODUCTION_SECTOR_H_
#define _PRODUCTION_SECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file production_sector.h
* \ingroup Objects
* \brief The ProductionSector class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/
#include <string>
#include <memory>
#include <map>

#include "sectors/include/sector.h"
// There are some missing includes here!
class IInvestor;
class Demographic;
class NationalAccount;
class GDP;
class MarketInfo;

/*! 
* \ingroup Objects
* \brief This class represents a single production sector.
* \author Sonny Kim
*/
class ProductionSector: public Sector
{

public:
	ProductionSector ( const std::string& aRegionName );
	virtual ~ProductionSector();
    static const std::string& getXMLNameStatic();
    virtual void completeInit();
    double getOutput( const int aPeriod ) const;
    virtual void initCalc( const int period, const MarketInfo* aMarketInfo, 
                           NationalAccount& nationalAccount, Demographic* aDemographics );
    virtual void operate( NationalAccount& aNationalAccount, const Demographic* aDemographic, const int aPeriod ); // Passing demographic here is not good.
    virtual void updateOutputContainer( OutputContainer* aOutputContainer, const int aPeriod ) const;
protected:
	std::map<std::string,double> ghgEmissCoefMap; //! Map of ghg name to emission coefficent
    void setMarket();
    virtual const std::string& getXMLName() const;
    virtual bool XMLDerivedClassParseAttr( const xercesc::DOMNode* node ); 
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ); 
    virtual void toOutputXMLDerived( std::ostream& out, Tabs* tabs ) const {};
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
private:
    bool mIsFixedPrice; //!< Whether this sector is on a fixed price path.
    bool mIsEnergyGood; //!< If the sector has an energy product.
    bool mIsPrimaryEnergyGood; //!< If the sector has a primary energy product.
    bool mIsSecondaryEnergyGood; //!< If the sector has a secondary energy product.
    std::auto_ptr<IInvestor> mInvestor; //!< Investment object.
    
    void calcInvestment( const Demographic* aDemographic, NationalAccount& aNationalAccount, const int period );
    void operateOldCapital( const Demographic* aDemographic, NationalAccount& aNationalAccount, const int period );
    void operateNewCapital( const Demographic* aDemographic, NationalAccount& aNationalAccount, const int period );
    void calcPriceReceived( const int period );
};

#endif // _PRODUCTION_SECTOR_H_
