#ifndef _MARKETPLACE_H_
#define _MARKETPLACE_H_
#if defined(_MSC_VER)
#pragma once
#endif
/*! 
* \file marketplace.h
* \ingroup Objects
* \brief The Marketplace class header file.
* \author Sonny Kim
*/

#include <vector>
#include <iosfwd>
#include <string>
#include "marketplace/include/imarket_type.h"
#include "util/base/include/ivisitable.h"

class Tabs;
class Market;
class MarketLocator;
class IVisitor;
class IInfo;

/*! 
* \ingroup Objects
* \brief A class which describes the single global marketplace.
* \author Sonny Kim
* \todo The naming of get(set)MarketInfo and the (re)storeInfo needs fixing. 
*/

class Marketplace: public IVisitable
{
public:
    Marketplace();
    ~Marketplace();
    void solve( const int per );
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    bool createMarket( const std::string& regionName, const std::string& marketName,
                       const std::string& goodName, const IMarketType::Type aMarketType );
    void initPrices();
    void nullSuppliesAndDemands( const int period );
    void setPrice( const std::string& goodName, const std::string& regionName, const double value,
                   const int period, bool aMustExist = true );
    void setPriceVector( const std::string& goodName, const std::string& regionName,
                         const std::vector<double>& prices );
    void addToSupply( const std::string& goodName, const std::string& regionName, const double value,
                      const int period, bool aMustExist = true );
    void addToDemand( const std::string& goodName, const std::string& regionName, const double value,
                      const int period, bool aMustExist = true );
    double getPrice( const std::string& goodName, const std::string& regionName, const int period,
                     bool aMustExist = true ) const;
    double getSupply( const std::string& goodName, const std::string& regionName, const int period ) const;
    double getDemand( const std::string& goodName, const std::string& regionName, const int period ) const;
    void init_to_last( const int period );
    void dbOutput() const; 
    void csvOutputFile( std::string marketsToPrint = "" ) const; 
    void resetToPriceMarket( const std::string& goodName, const std::string& regionName );
    void setMarketToSolve( const std::string& goodName, const std::string& regionName, const int period );
    void unsetMarketToSolve( const std::string& goodName, const std::string& regionName, const int period );
    void storeinfo( const int period );
    void restoreinfo( const int period );

    const IInfo* getMarketInfo( const std::string& aGoodName, const std::string& aRegionName,
                                const int aPeriod, const bool aMustExist ) const;

    IInfo* getMarketInfo( const std::string& aGoodName, const std::string& aRegionName,
                         const int aPeriod, const bool aMustExist );

    void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
    void accept( IVisitor* aVisitor, const int aPeriod ) const;

    std::vector<Market*> getMarketsToSolve( const int period ) const;
    static const std::string& getXMLNameStatic();
    
    //! The price to return if no market exists.
    const static double NO_MARKET_PRICE;
private:
    std::vector< std::vector<Market*> > markets; //!< no of market objects by period
    std::auto_ptr<MarketLocator> mMarketLocator; //!< An object which determines the correct market number.
};

#endif
