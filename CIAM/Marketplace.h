#ifndef _MARKETPLACE_H_
#define _MARKETPLACE_H_

#if defined(_MSC_VER)
#pragma once
#pragma warning( disable: 4275 )
#endif

/*! 
* \file Marketplace.h
* \ingroup CIAM
* \brief The Marketplace class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <vector>
#include <map>
#include <mtl/matrix.h>

using namespace std;
using namespace mtl;

typedef matrix<double, rectangle<>, dense<>, row_major>::type Matrix;

/*! 
* \ingroup CIAM
* \brief A class which describes the single global marketplace.
* \author Sonny Kim
* \date $ Date $
* \version $ Revision $
*/

class Marketplace
{	
	
private:
	//! Structure which contains the solution information for a single market.
	struct solinfo {
		double X;		//!< unknown, prices
		double ED;		//!< excess demand for X
		double dX;		//!< change in excess demand
		double XL;		//!< left bracket
		double XR;		//!< right bracket
		double EDL;		//!< excess demand for left bracket
		double EDR;		//!< excess demand for right bracket
		double XL_org;	//!< original left bracket
		double XR_org;	//!< original right bracket
		double EDL_org;	//!< original excess demand for left bracket
		double EDR_org;	//!< original excess demand for right bracket
		int bracketed;	//!< 1 or 0 for bracketed or unbrackted. bool?
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
		vector< vector<Market> > mrk; //!< no of market objects by period
		vector<int> mrk_isol; //!< index look up for markets that require solving
		vector<int> mrk_isol_NR; //!< index look up for markets for Newton-Rhapson
		map<string,int> marketMap; //!< map of unique market id from good and market-region names
		map<string,int> region_marketMap; //!< map of market lookup from good and region names
		
		// Private Functions
		int NR_Ron( const double Tol,vector<solinfo>& sol, Matrix& JF, int& n, const int per );
		int NewtRap( const double Tol, vector<solinfo>& sol, Matrix& JF, int& n, const int per );
		void Derivatives( vector<double> prices, Matrix& JFDM, Matrix& JFSM, int& n, const int per );
		void JFunction( vector<double> prices, Matrix& JFDM, int& n, int const per );
		int Secant_all( const double Tol,vector<solinfo>& sol,int& n, const int per );
		int FalsePos_all( const double Tol,vector<solinfo>& sol,int& n, const int per );
		int Bisection_i( const int i, const double Tol, vector<solinfo>& sol,int& n, const int per );
		int Bisection_all( const double Tol, const int IterLimit, vector<solinfo>& sol, int& n, const int per );
		void CheckBracket( const double Tol, vector<solinfo>& sol, bool& allbracketed );
		int Bracket( const double Tol, vector<solinfo>& sol, bool& allbracketed, bool& firsttime,int& n, const int per );
		int getMarketNumber( const string& goodName, const string& regionName ) const; // get the market number
		double marketDemand( const int mktNumber, const int period);
		double checkSupply( const string& goodName, const string& regionName, const int period ) const;
		double checkSupply( const int marketNumber, const int period ) const;
		string getName( const int marketNumber ) const;
		string getGoodName( const int marketNumber) const;
		double getRawSupply( const int marketNumber, const int period ) const;
		double getRawDemand( const int marketNumber, const int period ) const;
		double getRawPrice( const int marketNumber, const int period ) const;
		Market::marketType getType( const int marketNumber, const int period) const;
		const vector<double> jacobian( const int marketNumber, const int period ) const; // calculate the derivative or Jacobian
		const vector<double> dem_elas( const int marketNumber, const int period ) const; // calculate the demand elasticity
		const vector<double> dem_elas_NR( const int marketNumber, const int period ) const; // calculate the demand elasticity
		const vector<double> sup_elas( const int marketNumber, const int period ) const; // calculate the supply elasticity
		const vector<double> sup_elas_NR( const int marketNumber, const int period ) const; // calculate the supply elasticity
		int showmrk_sol( const int id ) const; // returns market index that requires solving
		const vector<double> showPRC( const int period ) const; // returns vector of market prices
		const vector<double> showPRC_NR( const int period ) const; // returns vector of market prices
		const vector<double> showED( const int period ) const; // returns vector of market excess demands
		const vector<double> showED_NR( const int period ) const; // returns vector of market excess demands
		const vector<double> showlogED( const int period ) const; // returns vector of log of market excess demands
		const vector<double> showlogED_NR( const int period ) const; // returns vector of log of market excess demands
		const vector<double> showlogDem( const int period ) const; // returns vector of log of demand
		const vector<double> showlogDem_NR( const int period ) const; // returns vector of log of demand
		const vector<double> showlogSup( const int period ) const; // returns vector of log of supply
		const vector<double> showlogSup_NR( const int period ) const; // returns vector of log of supply
		void setPRC( const vector<double>& prices, const int period ); // sets solution prices for all markets
		void setPRC_NR( const vector<double>& prices, const int period ); // sets solution prices for all markets
		static void invertMatrix( Matrix& A );
	public:
		Marketplace();
		void solve( const int per );
		void toDebugXML( const int period, ostream& out ) const;
		bool setMarket( const string& regionName, const string& marketName, const string& goodName, const Market::marketType type ); // sets number of markets
		void initXMLPrices();
		void nullprc( const int period ); // initialize all market prices to 0
		void nulldem( const int period ); // initialize all market demands to 0
		void nullsup( const int period ); // initialize all market supplies to 0
		void nulldem_imrk( const string& goodName, const string& regionName, const int period ); // set one market demand to 0
		void nullsup_imrk( const string& goodName, const string& regionName, const int period ); // set one market supply to 0
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
		
		bool checkprod( const int period ) const; // checks if supply or demand = 0 for all markets
		int worstED( const int period ) const; // returns index of market that has the largest excess demand
		double maxED( const int period ) const; // returns the largest excess demand
		
		void init_to_last( const int period ); // initialize current market info to last period
		void storeto_last( const int period ); // initialize current market t info to last period
		void storeinfo( const int period ); // stores original market information
		void restoreinfo( const int period ); // restores original market information
		void restoreprc( const int period );
		void restoreprc_NR( const int period );
		void MCoutput() const; 
		void outputfile() const; 
		void bugout( const int period, const int iteration ) const;
		void sdcurves( const int period, const int iteration ) const;
		void resetToPriceMarket( const string& goodName, const string& regionName );
		
		Market::marketType getType( const string goodname, const string regionName, const int period ) const;
		string getName( const string& goodName, const string& regionName) const;
		void setMarketToSolve ( const string& goodName, const string& regionName, const int period );
		void setMarketToSolve ( const string& goodName, const string& regionName );
		void prices_to_bugout( const int period ) const;
		void supply_to_bugout( const int period ) const;
		void demand_to_bugout( const int period ) const;
		bool checkMarketSolution( const double solTolerance, const int period ) const;
};

#endif