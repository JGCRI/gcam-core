#ifndef _MARKET_H_
#define _MARKET_H_
#if defined(_MSC_VER_)
#pragma once
#endif

/*! 
* \file market.h
* \ingroup CIAM
* \brief The Market class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <memory>
#include "marketplace/include/imarket_type.h"

class MarketInfo;
class Tabs;

/*!
* \ingroup CIAM
* \brief A class which defines a single market object.
* \author Sonny Kim
*/

class Market
{
public:
    Market( const std::string& goodNameIn, const std::string& regionNameIn, const int periodIn );
    virtual ~Market();
    static std::auto_ptr<Market> createMarket( const IMarketType::Type aMarketType, const std::string& aGoodName, const std::string& aRegionName, const int aPeriod );
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;

    /*! \brief Add additional information to the debug xml stream for derived classes.
    *
    * This method is inherited from by derived class if they which to add any additional information to the printout of the class.
    * \param out Output stream to print to.
    * \param tabs A tabs object responsible for printing the correct number of tabs. 
    */ 
    virtual void derivedToDebugXML( std::ostream& out, Tabs* tabs ) const = 0;
	const std::string& getXMLName() const;
	static const std::string& getXMLNameStatic();
    void addRegion( const std::string& regionNameIn );
    const std::vector<std::string> getContainedRegions() const;

    virtual void initPrice() = 0;
    virtual void setPrice( const double priceIn ) = 0;
    void setRawPrice( const double priceIn );
    virtual void setPriceFromLast( const double lastPrice ) = 0;
    virtual double getPrice() const = 0;
    double getRawPrice() const;
    double getStoredRawPrice() const;

    virtual void nullDemand();
    void setRawDemand( const double value );
    virtual void addToDemand( const double demandIn ) = 0;
    void removeFromRawDemand( const double demandIn );
    double getRawDemand() const;
    double getStoredRawDemand() const;
    virtual double getDemand() const = 0;

    virtual void nullSupply() = 0;
    double getRawSupply() const;
    double getStoredRawSupply() const;
    void setRawSupply( const double supplyIn );
    void removeFromRawSupply( const double supplyIn );
    virtual double getSupply() const = 0;
    virtual double getSupplyForChecking() const = 0;
    virtual void addToSupply( const double supplyIn );

    std::string getName() const;
    std::string getRegionName() const;
    std::string getGoodName() const;
    void setMarketInfo( const std::string& itemName, const double itemValue );
    double getMarketInfo( const std::string& itemName ) const;
    void storeInfoFromLast( const double lastDemand, const double lastSupply, const double lastPrice );
    void storeInfo();
    void restoreInfo();

    void setSolveMarket( const bool doSolve );
    virtual bool meetsSpecialSolutionCriteria() const = 0;
    virtual bool shouldSolve() const = 0;
    virtual bool shouldSolveNR() const = 0;
    virtual std::string getType() const = 0;
protected:
    Market( const Market& aMarket );
    std::string good;  //!< The good the market represents
    std::string region;  //!< The region of the market.
    bool solveMarket; //!< Whether to solve the market given other constraints are satisfied.
    int period; //!< The period the market is valid in.
    double price;  //!< The market price.
    double storedPrice;  //!< The stored market price.
    double demand; //!< The market demand.
    double storedDemand; //!< The stored demand.
    double supply; //!< The market supply.
    double storedSupply; //!< The stored supply.
    std::vector <std::string> containedRegionNames; //!< Vector of names of all regions contained within this market.
    std::auto_ptr<MarketInfo> mMarketInfo; //!< Object containing information related to the market. 

private:
	const static std::string XML_NAME; //!< node name for toXML methods
};

#endif // _MARKET_H_
