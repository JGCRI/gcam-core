#ifndef _MARKETPLACE_H_
#define _MARKETPLACE_H_
#if defined(_MSC_VER)
#pragma once
#endif
/*! 
* \file marketplace.h
* \ingroup CIAM
* \brief The Marketplace class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <map>
#include <iosfwd>
#include <string>
#include "marketplace/include/imarket_type.h"
class Tabs;
class Market;
/*! 
* \ingroup CIAM
* \brief A class which describes the single global marketplace.
* \author Sonny Kim
* \todo The naming of get(set)MarketInfo and the (re)storeInfo needs fixing. 
*/

class Marketplace
{	
public:
    Marketplace();
    ~Marketplace();
    void solve( const int per );
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    bool createMarket( const std::string& regionName, const std::string& marketName, const std::string& goodName, const IMarketType::Type aMarketType );
    void initPrices();
    void nullSuppliesAndDemands( const int period );
    void setPrice( const std::string& goodName, const std::string& regionName, const double value , const int period );
    void setPriceVector( const std::string& goodName, const std::string& regionName, const std::vector<double>& prices );
    void addToSupply( const std::string& goodName, const std::string& regionName, const double value, const int period );
    void addToDemand( const std::string& goodName, const std::string& regionName, const double value, const int period );
    double getPrice( const std::string& goodName, const std::string& regionName, const int period ) const;
    double getSupply( const std::string& goodName, const std::string& regionName, const int period ) const;
    double getDemand( const std::string& goodName, const std::string& regionName, const int period ) const;
    void init_to_last( const int period );
    void storeto_last( const int period );
    void dbOutput() const; 
    void csvOutputFile( std::string marketsToPrint = "" ) const; 
    void resetToPriceMarket( const std::string& goodName, const std::string& regionName );
    void setMarketToSolve( const std::string& goodName, const std::string& regionName, const int period );
    void unsetMarketToSolve( const std::string& goodName, const std::string& regionName, const int period );
    void storeinfo( const int period );
    void restoreinfo( const int period );
    void setMarketInfo( const std::string& goodName, const std::string& regionName, const int period, const std::string itemName, const double itemValue );
    double getMarketInfo( const std::string& goodName, const std::string& regionName, const int period, const std::string& itemName ) const;
    bool doesMarketExist( const std::string& goodName, const std::string& regionName, const int period ) const;
    std::vector<Market*> getMarketsToSolve( const int period ) const;
private:
    unsigned int uniqueNo; //!< number for creating markets
    unsigned int numMarkets;  //!< number of markets
    std::vector< std::vector<Market*> > markets; //!< no of market objects by period
    typedef std::pair<const size_t, const size_t> MarketHashKey;
    typedef std::map<MarketHashKey,int> MarketMap;
    typedef MarketMap::iterator MarketMapIterator;
    typedef MarketMap::const_iterator CMarketMapIterator;
    MarketMap marketMap; //!< map of unique market id from good and market-region names
    MarketMap regionToMarketMap; //!< map of market lookup from good and region names

    static const Marketplace::MarketHashKey createMarketKey( const std::string& aMarketName,
        const std::string& aGoodName );
    int getMarketNumber( const std::string& goodName, const std::string& regionName ) const;
    size_t static hash( const std::string& aString );
};

#endif
