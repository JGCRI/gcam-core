#ifndef _MARKET_H_
#define _MARKET_H_
#pragma once

/*! 
* \file Market.h
* \ingroup CIAM
* \brief The Market class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <vector>

using namespace std;
/*!
* \ingroup CIAM
* \brief A class which defines a single market object.
* \author Sonny Kim
*/

class Market
{
public:
	enum marketType	// RHS and LHS below may not be consistant
	{ 
		NORMAL, //!< RHS = Supply; LHS = Demand; Solution Value = Price.
		PRICE, //!< RHS = Logit Price; LHS = Solution Price; Solution Value = Price.
		DEMAND, //!< RHS = Demand; LHS = Solution Demand; Solution Value = Solution Demand.
		GHG,	//!< RHS = Emissions ; LHS = Constraint; Solution Value = Price.
		CALIBRATION, //!< LHS = Constraint; RHS = Value; Solution Value = Calibration Parameter.
	};
	
	Market( const string& goodNameIn = "", const string& regionNameIn = "", const marketType typeIn = NORMAL );
	
   void toDebugXML( const int period, ostream& out ) const;
   void addRegion( const string& regionNameIn );
   
   void initPrice();
   void nullPrice();
   void setPrice( const double priceIn );
   void setActualPrice( const double priceIn );
   void setPriceToLast( const double lastPrice );
   double getPrice() const;
   double getActualPrice() const;
   void restorePrice();
   double getChangeInPrice() const;
   double getLogChangeInPrice( const double SMALL_NUM ) const;

   void nullDemand();
   void setDemand( const double value );
   void incrementDemand( const double value );
   double getDemand() const;
   void calcLogDemand( const double SMALL_NUM );
   double getLogDemand() const;
   double getChangeInDemand() const;
   double getLogChangeInDemand( const double SMALL_NUM ) const;

   void nullSupply();
   double getSupply() const;
   double getActualSupply() const;
   void calcLogSupply( const double SMALL_NUM );
   double getLogSupply() const;
   double getChangeInSupply() const;
   double getLogChangeInSupply( const double SMALL_NUM ) const;
   void calcExcessDemand();
   double getExcessDemand() const;
   
   void calcLogExcessDemand( const double SMALL_NUM );
   double getLogExcessDemand() const;

   void setPeriod( const int periodIn );
   string getName() const;
   string getRegionName() const;
   string getGoodName() const;
   marketType getType() const;
   void storeInfoFromLast( const double lastDemand, const double lastSupply, const double lastPrice );
   void storeInfo();
   void restoreInfo();
   
   void setSolveMarket( const bool doSolve );
   void print( ostream& out ) const;
	
   string name;  //! name of market (fuel or good) !not right
	string region;  //! market region
	bool solveMarket; //! Toggle for markets that should be solved
	marketType type; //! market type: normal, price, demand, etc.
	int period; //! Model period
   int priceMult; //! Price Multiplier.
	vector<string> containedRegionNames; //! Vector of names of all regions within this vector.
	double price;  //! market price
	double tprice;  //! store market price
	double demand; //! demand for market solution
	double tdemand; //! store previous demand
	double supply; //! supply for market solution
	double demMktSupply; //! actual supply for a demand market solution
	double tsupply; //! store previous supply
	double exdmd; //! excess demand for each market
	double lexdmd; //! log of excess demand for each market
	double texdmd; //! store excess demand
	double dexdmd; //! derivative of excess demand
	double ldem; //! log of demand for each market
	double lsup; //! log of supply for each market
};

#endif // _MARKET_H_
