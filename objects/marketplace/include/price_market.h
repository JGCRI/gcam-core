#ifndef _PRICE_MARKET_H_
#define _PRICE_MARKET_H_
#if defined(_MSC_VER_)
#pragma once
#endif

/*! 
* \file price_market.h
* \ingroup CIAM
* \brief The PriceMarket class header file.
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/

#include "marketplace/include/market.h"

class Tabs;

/*!
* \ingroup CIAM
* \brief A derived class which defines a single PriceMarket object for use in solving simultinaity markets.
* \author Steve Smith
*/

class PriceMarket: public Market {
public:
    PriceMarket( const std::string& goodNameIn, const std::string& regionNameIn, const int periodIn, Market* demandMarketIn );
    PriceMarket( const Market& marketIn, Market* demandMarketIn );
    virtual void derivedToDebugXML( std::ostream& out, Tabs* tabs ) const;
    virtual std::string getType() const;

    virtual void initPrice();
    virtual void setPrice( const double priceIn );
    virtual void setPriceFromLast( const double lastPrice );
    virtual double getPrice() const;

    virtual void addToDemand( const double demandIn );
    virtual double getDemand() const;

    virtual void nullSupply();
    virtual double getSupply() const;
    virtual double getSupplyForChecking() const;
    virtual void addToSupply( const double supplyIn );

    virtual bool shouldSolve() const;
    virtual bool shouldSolveNR() const;

private:
    Market* demandMarketPointer; //!< A pointer to the companion DemandMarket
    double priceMultiplier; //!< Price multiplier.
};

#endif // _PRICE_MARKET_H_
