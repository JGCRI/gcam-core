#ifndef _GHG_MARKET_H_
#define _GHG_MARKET_H_
#if defined(_MSC_VER_)
#pragma once
#endif

/*! 
* \file ghg_market.h
* \ingroup CIAM
* \brief The GHGMarket class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "Market.h"

/*!
* \ingroup CIAM
* \brief A class which defines a GHGMarket object which is used for GHG constraint markets. 
* \author Sonny Kim
*/

class GHGMarket: public Market {
public:
    GHGMarket( const std::string& goodNameIn, const std::string& regionNameIn, const int periodIn );
    virtual void derivedToDebugXML( std::ostream& out ) const;
    virtual std::string getType() const;
    virtual void setCompanionMarketPointer( Market* pointerIn );

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
};

#endif // _GHG_MARKET_H_