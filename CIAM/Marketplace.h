#ifndef _MARKETPLACE_H_
#define _MARKETPLACE_H_

/*! 
* \file Marketplace.h
* \ingroup CIAM
* \brief The Marketplace class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#if defined(_MSC_VER)
#pragma once
#pragma warning( disable: 4275 )
#endif

#include <string>
#include <vector>
#include <map>
#include <mtl/matrix.h>
#include "Market.h"

using namespace std;
using namespace mtl;

typedef matrix<double, rectangle<>, dense<>, row_major>::type Matrix;

/*! 
* \ingroup CIAM
* \brief A class which describes the single global marketplace.
* \author Sonny Kim
*/

class Marketplace
{	
	
private:
	//! Structure which contains the solution information for a single market.
	struct solinfo {
		double X;		//!< unknown, prices
		double ED;		//!< excess demand for X
        double demand;  //!< demand for X.
		double dX;		//!< change in excess demand
		double XL;		//!< left bracket
		double XR;		//!< right bracket
		double EDL;		//!< excess demand for left bracket
		double EDR;		//!< excess demand for right bracket
		double XL_org;	//!< original left bracket
		double XR_org;	//!< original right bracket
		double EDL_org; //!< original excess demand for left bracket
		double EDR_org; //!< original excess demand for right bracket
		bool bracketed;	//!< 1 or 0 for bracketed or unbrackted.
	};
	
	private:
		int uniqueNo; //!< number for creating markets
		int nomrks;  //!< number of markets
		int nomrks_t; //!< number of markets that require solving
		int nomrks_t_NR; //!< number of markets for Newton-Rhapson
		int nodrscmrks;  //!< number of depletable resource markets
		int nossecmrks;  //!< number of supply sector markets
		int noDemandMarkets; //!< number of demand sector markets.
		int noghgmrks;	//!< number of GHG markets
		double priceMult; //!< resolution enhancement for price markets
        double SMALL_NUM; //!< constant small number to replace for null
        double VERY_SMALL_NUM; //!< constant small number to replace for null
        bool bugTracking; //!< Turn on to enable bugout tracking in various solution routines
        bool bugMinimal; //!< Turn on minimal tracking of solution results
        bool trackED; //!< Turn on solution mechanism tracking (to cout)
        int TotIter; //!< Cumulative number of interations
		vector< vector<Market> > mrk; //!< no of market objects by period
		vector<int> mrk_isol; //!< index look up for markets that require solving
		vector<int> mrk_isol_NR; //!< index look up for markets for Newton-Rhapson
		map<string,int> marketMap; //!< map of unique market id from good and market-region names
		map<string,int> region_marketMap; //!< map of market lookup from good and region names
		
		// Private Functions
		int NR_Ron( const double Tol, const double excessDemandSolutionFloor, vector<solinfo>& sol, int& n, const int per );
		int NewtRap( const double Tol,  const double excessDemandSolutionFloor, vector<solinfo>& sol, int& n, const int per );
		void Derivatives( vector<double> prices, Matrix& JFDM, Matrix& JFSM, int& n, const int per );
		void JFunction( vector<double> prices, Matrix& JFDM, int& n, int const per );
		int Secant_all( const double Tol,vector<solinfo>& sol,int& n, const int per );
		int FalsePos_all( const double Tol,vector<solinfo>& sol,int& n, const int per );
		int Bisection_i( const int i, const double Tol, vector<solinfo>& sol,int& n, const int per );
		int Bisection_all( const double Tol, const double excessDemandSolutionFloor, const int IterLimit, vector<solinfo>& sol, int& n, const int per );
		void CheckBracket( const double Tol, const double excessDemandSolutionFloor, vector<solinfo>& sol, bool& allbracketed );
		int Bracket( const double Tol, const double excessDemandSolutionFloor, vector<solinfo>& sol, bool& allbracketed, bool& firsttime,int& n, const int per );
		int getMarketNumber( const string& goodName, const string& regionName ) const; // get the market number
		double marketDemand( const int mktNumber, const int period);
		double checkSupply( const string& goodName, const string& regionName, const int period ) const;
		double checkSupply( const int marketNumber, const int period ) const;
		string getName( const int marketNumber ) const;
        string getRegionName( const int marketNumber ) const;
		string getGoodName( const int marketNumber) const;
		double getRawSupply( const int marketNumber, const int period ) const;
		double getRawDemand( const int marketNumber, const int period ) const;
		double getRawPrice( const int marketNumber, const int period ) const;
		Market::marketType getType( const int marketNumber, const int period ) const;
		const vector<double> jacobian( const vector<int>& indices, const int marketNumber, const int period ) const;
		const vector<double> calcDemandElas( const vector<int>& indices, const int marketSolutionNumber, const int period ) const;
		const vector<double> calcSupplyElas( const vector<int>& indices, const int marketSolutionNumber, const int period ) const;
		const vector<double> getPrices( const vector<int>& indices, const int period ) const;
		void setPrices( const vector<double>& prices, const vector<int>& indices, const int period );
		const vector<double> getExcessDemands( const vector<int>& indices, const int period ) const;
		const vector<double> getLogExcessDemands( const vector<int>& indices, const int period ) const; 
		const vector<double> getDemands( const vector<int>& indices, const int per ) const;
        const vector<double> getLogDemands( const vector<int>& indices, const int period ) const;
		
        const vector<double> getLogSupplies( const vector<int>& indices, const int period ) const;
		static void invertMatrix( Matrix& A );
        bool isWithinTolerance( const double excessDemand, const double demand, const double solutionTolerance, const double minimum ) const;
	    double getRelativeED( const double excessDemand, const double demand, const double excessDemandFloor ) const ;
   
   public:
		Marketplace();
		void solve( const int per );
		void toDebugXML( const int period, ostream& out ) const;
		bool setMarket( const string& regionName, const string& marketName, const string& goodName, const Market::marketType type ); // sets number of markets
		void initXMLPrices();
		void nullprc( const int period ); // initialize all market prices to 0
		void nulldem( const int period ); // initialize all market demands to 0
		void nulldem( const string& goodName, const string& regionName, const int period ); // set one market demand to 0
        void nullsup( const int period ); // initialize all market supplies to 0
		void nullsup( const string& goodName, const string& regionName, const int period ); // set one market supply to 0
		
        void showmrks( const int period ) const; // show market information
		
		// set market price
		void setprice( const string& goodName, const string& regionName, const double value , const int period );
		void setPriceVector( const string& goodName, const string& regionName, const vector<double>& prices );
		
		// set market supply.
		void setsupply( const string& goodName, const string& regionName, const double value, const int period );
		
		// set market demand
		void setdemand( const string& goodName, const string& regionName, const double value, const int period );
		
		// return market price
		double showprice( const string& goodName, const string& regionName, const int period ) const;
		
		// return market supply
		double showsupply( const string& goodName, const string& regionName, const int period ) const;
		
		// return market demand
		double showdemand( const string& goodName, const string& regionName, const int period ) const;
		
		void excessdemand( const int period ); // calculates excess demand for all markets
		void logED( const int period ); // calculates log of excess demand for all markets
		void logDem( const int period ); // calculates log of demand for all markets
		void logSup( const int period ); // calculates log of supply for all markets
		int setMarketsToSolve( const int period );
		int setMarketsToSolveNR( const int period );
		int worstED( const vector<int>& indices, const double excessDemandSolutionFloor, const int per ) const;
		double maxED( const vector<int>& indices, const double excessDemandSolutionFloor, const int per ) const;

		
		void init_to_last( const int period ); // initialize current market info to last period
		void storeto_last( const int period ); // initialize current market t info to last period
		void storeinfo( const int period ); // stores original market information
		void restoreinfo( const int period ); // restores original market information
		void restoreprc( const vector<int>& indices, const int period );
		void MCoutput() const; 
		void outputfile() const; 
		void bugout( const int period, const int iteration ) const;
		void sdcurves( const int period, const int iteration ) const;
		void resetToPriceMarket( const string& goodName, const string& regionName );
		
		Market::marketType getType( const string goodname, const string regionName, const int period ) const;
		string getRegionName( const string& goodName, const string& regionName) const;
        void setMarketToSolve ( const string& goodName, const string& regionName, const int period = -1 );
		void prices_to_bugout( const int period ) const;
		void supply_to_bugout( const int period ) const;
		void demand_to_bugout( const int period ) const;
		bool checkMarketSolution( const double solTolerance,  const double excessDemandSolutionFloor, const int period ) const;
};

#endif