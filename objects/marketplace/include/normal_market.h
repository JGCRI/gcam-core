#ifndef _NORMAL_MARKET_H_
#define _NORMAL_MARKET_H_
#if defined(_MSC_VER_)
#pragma once
#endif

/*! 
* \file normal_market.h
* \ingroup Objects
* \brief The NormalMarket class header file.
* \author Sonny Kim
*/

#include "marketplace/include/market.h"

/*!
* \ingroup Objects
* \brief A class which defines the normal supply-demand type market.
* \author Sonny Kim
*/

class NormalMarket: public Market {
public:
    NormalMarket( const std::string& goodNameIn, const std::string& regionNameIn, const int periodIn );
    virtual IMarketType::Type getType() const;

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
    
    virtual bool meetsSpecialSolutionCriteria() const;
    virtual bool shouldSolve() const;
    virtual bool shouldSolveNR() const;
protected:
    virtual void toDebugXMLDerived( std::ostream& out, Tabs* tabs ) const;
};

#endif // _NORMAL_MARKET_H_
