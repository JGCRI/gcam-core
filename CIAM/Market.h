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
		NORMAL, //!< RHS = Supply; LHS = Demand; Solution Value = Price;
		PRICE, //!< RHS = Logit Price; LHS = Solution Price; Solution Value = Price;
		DEMAND, //!< RHS = Demand; LHS = Solution Demand; Solution Value = Solution Demand;
		GHG,	//!< RHS = Emissions ; LHS = Constraint; Solution Value = Price;
		GDP 
	};
	
	Market();
	void toDebugXML( const int period, ostream& out ) const;
	string name;  //! name of market (fuel or good) !not right
	string region;  //! market region
	bool solveMarket; //! Toggle for markets that should be solved
	marketType type; //! market type: normal, price, demand, etc.
	int year; //! period
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
