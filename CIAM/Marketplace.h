#ifndef _MARKETPLACE_H_
#define _MARKETPLACE_H_
#if defined(_MSC_VER)
#pragma once
#endif
/*! 
* \file Marketplace.h
* \ingroup CIAM
* \brief The Marketplace class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <map>
#include <iosfwd>

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
   friend class SolverLibrary;

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
   void toDebugXML( const int period, std::ostream& out ) const;
   bool createMarket( const std::string& regionName, const std::string& marketName, const std::string& goodName, const NewMarketType typeIn );
   void initPrices();
   void nullPrices( const int period );
   void nullDemands( const int period );
   void nullSupplies( const int period );
   void setPrice( const std::string& goodName, const std::string& regionName, const double value , const int period );
   void setPriceVector( const std::string& goodName, const std::string& regionName, const std::vector<double>& prices );
   void addToSupply( const std::string& goodName, const std::string& regionName, const double value, const int period );
   void addToDemand( const std::string& goodName, const std::string& regionName, const double value, const int period );
   double getPrice( const std::string& goodName, const std::string& regionName, const int period ) const;
   double getSupply( const std::string& goodName, const std::string& regionName, const int period ) const;
   double getDemand( const std::string& goodName, const std::string& regionName, const int period ) const;
   void init_to_last( const int period );
   void storeto_last( const int period );
   void MCoutput() const; 
   void outputfile() const; 
   void resetToPriceMarket( const std::string& goodName, const std::string& regionName );
   void setMarketToSolve ( const std::string& goodName, const std::string& regionName, const int period = -1 );
   bool checkMarketSolution( const double soltoleranceerance,  const double excessDemandSolutionFloor, const int period );
   void storeinfo( const int period );
   void restoreinfo( const int period );

private:
   
   int uniqueNo; //!< number for creating markets
   int numMarkets;  //!< number of markets
   std::vector< std::vector<Market*> > markets; //!< no of market objects by period
   std::map<std::string,int> marketMap; //!< map of unique market id from good and market-region names
   std::map<std::string,int> regionToMarketMap; //!< map of market lookup from good and region names
   Solver* solver; //!< Pointer to a solution mechanism.
   
   static std::string createMarketKey( const std::string& marketName, const std::string& goodName );
   int getMarketNumberFromNameAndGood( const std::string& marketName, const std::string& goodName ) const;
   std::vector< std::pair< std::string, std::string > > getMarketsToSolve( const int period, const bool isNR ) const;
   bool isPriceOrDemandMarket( const std::string& marketName, const std::string& goodName, const int period ) const;
   int getMarketNumber( const std::string& goodName, const std::string& regionName ) const;

   double getRawDemand( const std::string& marketName, const std::string& goodName, const int period ) const;
   double getStoredRawDemand( const std::string& marketName, const std::string& goodName, const int period ) const;
   
   double getRawSupply( const std::string& marketName, const std::string& goodName, const int period ) const;
   double getStoredRawSupply( const std::string& marketName, const std::string& goodName, const int period ) const;
   
   void setRawPrice( const std::string& marketName, const std::string& goodName, const double priceIn, const int period );
   double getRawPrice( const std::string& marketName, const std::string& goodName, const int period ) const;
   double getStoredRawPrice( const std::string& marketName, const std::string& goodName, const int period ) const;
   
   void removeFromRawDemands( const std::vector<double>& rawDemands, const int period );
   void removeFromRawSupplies( const std::vector<double>& rawSupplies, const int period );
   const std::vector<std::string> getContainedRegions( const std::string& marketName, const std::string& goodName, const int period ) const;
   
   double checkSupply( const std::string& goodName, const std::string& regionName, const int period ) const;
   double checkSupply( const int marketNumber, const int period ) const;
   const std::vector<double> getSupplies( const int per ) const;
   const std::vector<double> getDemands( const int per ) const;
  
   void findAndPrintSD( std::vector<Market*>& unsolved, const int period );
};

#endif
