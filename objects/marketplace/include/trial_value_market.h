#ifndef _TRIALVALUE_MARKET_H_
#define _TRIALVALUE_MARKET_H_
#if defined(_MSC_VER_)
#pragma once
#endif

/*! 
* \file trial_value_market.h
* \ingroup CIAM
* \brief The trial_value_market class header file.
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/

#include "marketplace/include/market.h"

/*!
* \ingroup CIAM
* \brief A class which defines the trail value type market.
* This market type sets up the solution mechanism to solve for a trial value of some quantity. 
* The quantity here need not be associated with any particular supply or demand. The object using 
* this market must set this market to solved, and then call addToDemand to set the trial value, 
* and call getPrice to get the trial value.
*
* \author Steve Smith
*/

class TrialValueMarket: public Market {
public:
    TrialValueMarket( const std::string& goodNameIn, const std::string& regionNameIn, const int periodIn );
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
    
    virtual bool meetsSpecialSolutionCriteria() const;
    virtual bool shouldSolve() const;
    virtual bool shouldSolveNR() const;

private:
};

#endif // _TRIALVALUE_MARKET_H_
