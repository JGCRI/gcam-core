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
#include <iostream>
#include <fstream>
#include <mtl/matrix.h>

using namespace std;
using namespace mtl;

typedef matrix<double, rectangle<>, dense<>, row_major>::type Matrix;

class Market;
class Solver;
class SolverLibrary;

/*! 
* \ingroup CIAM
* \brief A class which describes the single global marketplace.
* \author Sonny Kim
*/

class Marketplace
{	
   friend SolverLibrary;
private:
   
   int uniqueNo; //!< number for creating markets
   int numMarkets;  //!< number of markets
   double SMALL_NUM; //!< constant small number to replace for null
   double VERY_SMALL_NUM; //!< constant small number to replace for null
   vector< vector<Market*> > markets; //!< no of market objects by period
   map<string,int> marketMap; //!< map of unique market id from good and market-region names
   map<string,int> regionToMarketMap; //!< map of market lookup from good and region names
   mutable ofstream newSDCurvesStream; //!< Filestream to print debugging supply and demand curves to.
   Solver* solver; //!< Pointer to a solution mechanism.
   
   // Private Functions
   // Utility functions.
   static string createMarketKey( const string& regionName, const string& goodName );
   int getMarketNumberFromNameAndGood( const string& marketName, const string& goodName ) const;
   vector< pair< string, string > > getMarketsToSolve( const int period, const bool isNR ) const;
   bool isPriceOrDemandMarket( const string& marketName, const string& goodName, const int period ) const;
   int getMarketNumber( const string& goodName, const string& regionName ) const;

   double getRawDemand( const string& marketName, const string& goodName, const int period ) const;
   double getChangeInRawDemand( const string& marketName, const string& goodName, const int period ) const;
   double getLogChangeInRawDemand( const string& marketName, const string& goodName, const int period ) const;
   double getExcessDemand( const string& marketName, const string& goodName, const int period ) const;
   
   double getRawSupply( const string& marketName, const string& goodName, const int period ) const;
   double getChangeInRawSupply( const string& marketName, const string& goodName, const int period ) const;
   double getLogChangeInRawSupply( const string& marketName, const string& goodName, const int period ) const;
   
   void setRawPrice( const string& marketName, const string& goodName, const double priceIn, const int period );
   double getRawPrice( const string& marketName, const string& goodName, const int period ) const;
   double getChangeInRawPrice( const string& marketName, const string& goodName, const int period ) const;
   double getLogChangeInRawPrice( const string& marketName, const string& goodName, const int period ) const;
   
   // Used for derivative calculation.
   void removeFromRawDemands( const vector<double>& rawDemands, const int period );
   void removeFromRawSupplies( const vector<double>& rawSupplies, const int period );
   const vector<string> getContainedRegions( const string& marketName, const string& goodName, const int period ) const;
   
   double checkSupply( const string& goodName, const string& regionName, const int period ) const;
   double checkSupply( const int marketNumber, const int period ) const;
   const vector<double> getSupplies( const int per ) const;
   const vector<double> getDemands( const int per ) const;
  
   void findAndPrintSD( vector<Market*>& unsolved, const int period );
   void excessdemand( const int period ); // calculates excess demand for all markets
   void logED( const int period ); // calculates log of excess demand for all markets
   void logDem( const int period ); // calculates log of demand for all markets
   void logSup( const int period ); // calculates log of supply for all markets

   void storeinfo( const int period ); // stores original market information
   void restoreinfo( const int period ); // restores original market information

public:
   enum NewMarketType { //!< Types of new markets which can be instantiated from other parts of the model.
      NORMAL, //!< Normal Market
      CALIBRATION, //!< Calibration Market
      GHG, //!< Greenhouse Gas Market
      DEMAND //!< Demand Market
   };

   Marketplace();
   ~Marketplace();
   void solve( const int per );
   void toDebugXML( const int period, ostream& out ) const;
   bool setMarket( const string& regionName, const string& marketName, const string& goodName, const NewMarketType typeIn );
   void initXMLPrices();
   void nullprc( const int period ); // initialize all market prices to 0
   void nulldem( const int period ); // initialize all market demands to 0
   void nullsup( const int period ); // initialize all market supplies to 0
   
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
   
   void init_to_last( const int period ); // initialize current market info to last period
   void storeto_last( const int period ); // initialize current market t info to last period

   void MCoutput() const; 
   void outputfile() const; 
   void resetToPriceMarket( const string& goodName, const string& regionName );
   void setMarketToSolve ( const string& goodName, const string& regionName, const int period = -1 );
   bool checkMarketSolution( const double soltoleranceerance,  const double excessDemandSolutionFloor, const int period );
};

#endif