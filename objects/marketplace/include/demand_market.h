#ifndef _DEMAND_MARKET_H_
#define _DEMAND_MARKET_H_
#if defined(_MSC_VER_)
#pragma once
#endif

/*! 
* \file demand_market.h
* \ingroup CIAM
* \brief The DemandMarket class header file.
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/

#include "marketplace/include/market.h"

/*!
* \ingroup CIAM
* \brief A class which defines a DemandMarket object for use in solving simultinaity markets.
* \author Steve Smith
*/

class DemandMarket: public Market {
public:
    DemandMarket( const std::string& goodNameIn, const std::string& regionNameIn, const int periodIn );
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
    double demMktSupply; //!< Raw supply
};

#endif // _DEMAND_MARKET_H_