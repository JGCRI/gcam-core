#ifndef _CALIBRATION_MARKET_H_
#define _CALIBRATION_MARKET_H_
#if defined(_MSC_VER_)
#pragma once
#endif

/*! 
* \file calibration_market.h
* \ingroup CIAM
* \brief The CalibrationMarket class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "Market.h"

/*!
* \ingroup CIAM
* \brief A class which defines a CalibrationMarket object for use in solving calibration markets.
* \author Josh Lurz
*/

class CalibrationMarket: public Market {
public:
    CalibrationMarket( const std::string& goodNameIn, const std::string& regionNameIn, const int periodIn );
    ~CalibrationMarket();
    virtual void derivedToDebugXML( std::ostream& out ) const;
    virtual std::string getType() const;

    virtual void initPrice();
    virtual void setPrice( const double priceIn );
    virtual void setPriceFromLast( const double lastPriceIn );
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

#endif // _CALIBRATION_MARKET_H_