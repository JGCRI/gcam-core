#if defined(_MSC_VER)
#pragma warning( disable: 4275 )
#pragma warning( disable: 4786 )
#endif

/*! 
* \file Marketplace.cpp
* \ingroup CIAM
* \brief Marketplace class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <mtl/matrix.h>
#include <mtl/mtl.h>
#include <mtl/utils.h>
#include <mtl/lu.h>

#include "Definitions.h"

#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
#include <limits>
#include <vector>

#include "scenario.h"
#include "Market.h"
#include "Marketplace.h"
#include "modeltime.h"
#include "XMLHelper.h"
#include "Configuration.h"
#include "World.h"
#include "LoggerFactory.h"
#include "Logger.h"

using namespace std;
using namespace mtl;

extern ofstream bugoutfile, sdcurvefile, logfile;
extern Scenario* scenario;

//! Constructor.
Marketplace::Marketplace() {
   uniqueNo = 0;
   numMarkets = 0;
   numMarketsToSolve = 0;
   numMarketsToSolveNR= 0;
   SMALL_NUM = 1e-6;
   VERY_SMALL_NUM = 1e-8;
   bugTracking = false;
   bugMinimal = false;
   trackED = false;
   totIter = 0;
}

//! Destructor
Marketplace::~Marketplace() {
   for ( vector< vector< Market* > >::iterator outerIter = markets.begin(); outerIter != markets.end(); outerIter++ ) {
      for( vector< Market* >::iterator innerIter = outerIter->begin(); innerIter != outerIter->end(); innerIter++ ) {
         delete *innerIter;
      }
   }
}

//! Write out XML for debugging purposes.
void Marketplace::toDebugXML( const int period, ostream& out ) const {
   
   // write the beginning tag.
   Tabs::writeTabs( out );
   out << "<Marketplace>" << endl;
   
   // increase the indent.
   Tabs::increaseIndent();
   
   // write the xml for the class members.
   XMLWriteElement( numMarkets, "numberOfMarkets", out );
   XMLWriteElement( numMarketsToSolve, "numberOfMarketsRequireSolving", out );
   XMLWriteElement( numMarketsToSolveNR, "numberOfMarketsForNR", out );
   
   // First write out the individual markets
   for( int i = 0; i < static_cast<int>( markets.size() ); i++ ){
      markets[ i ][ period ]->toDebugXML( period, out );
   }
   
   // finished writing xml for the class members.
   
   // decrease the indent.
   Tabs::decreaseIndent();
   
   // write the closing tag.
   Tabs::writeTabs( out );
   out << "</Marketplace>" << endl;
}

//! Function which adds a regional market into the 
/*! Returns true if a new market was added. False if the market already exists.
There is an important distinction here between the region name vs the market name. 
The key to the market is the goodName + market name.
The regionToMarketMap map, uses goodName + regionName to find that market.
*/
bool Marketplace::setMarket( const string& regionName, const string& marketsname, const string& goodName, const NewMarketType typeIn ) {
   const Modeltime* modeltime = scenario->getModeltime();
   int marketsNo;
   bool retValue;
   
   // create unique markets from distinct good and market region names
   string key = goodName + marketsname;
   
   if ( marketMap.find( key ) != marketMap.end() ) { // market exists, no unique number
      retValue = false;
      marketsNo = marketMap[ key ];
      
      // add the additional region to the contained region names.
      for( int i = 0; i < static_cast<int> ( markets[ marketsNo ].size() ); i++ ) {
         markets[ marketsNo ][ i ]->addRegion( regionName );
      }
   } 
   else { // market does not exist, give unique number
      retValue = true;
      marketMap[ key ] = uniqueNo;
      
      // create a vector of market objects, one for each period
      vector<Market*> tempVector( modeltime->getmaxper() );
      
      for( int i = 0; i < static_cast<int>( tempVector.size() ); i++ ){
         if ( typeIn == NORMAL ){
            tempVector[ i ] = new Market( goodName, marketsname, i );
         }
         else if ( typeIn == GHG ) {
            tempVector[ i ] = new GHGMarket( goodName, marketsname, i );
         }
         else if ( typeIn == CALIBRATION ) {
            tempVector[ i ] = new CalibrationMarket( goodName, marketsname, i );
         }
         else if ( typeIn == DEMAND ) {
            tempVector[ i ] = new DemandMarket( goodName, marketsname, i );
         }
         else {
            cerr << "Invalid market type: " << typeIn << endl;
            assert( false );
         }
         
         tempVector[ i ]->addRegion( regionName );
      }
      
      markets.push_back( tempVector ); // create individual markets
      //markets is 2 dimentional
      marketsNo = uniqueNo;
      uniqueNo++;
      
      // increment market sizes
      numMarkets++;
   }
   // market lookup from good and region names
   // marketsNo may not be unique
   regionToMarketMap[ goodName + regionName ] = marketsNo;
   return retValue;
}

//! Returns the market number of a market given a goodName and regionName.
int Marketplace::getMarketNumber( const string& goodName, const string& regionName ) const {
   map <string, int> :: const_iterator findIter = regionToMarketMap.find( goodName + regionName );
   
   if( findIter == regionToMarketMap.end() ) {
      return -1;
   }
   else {
      return findIter->second;
   }
}

//! Restructures a market to account for simultaneities
/*! Changes the named market to a price market, which suplies a trial price for a secondary good
Also adds a corresponding demand market that provides a trial value for demand.
Routines such as Marketplace::setdemand, getdemand, getprice, etc. are adjusted to act 
differently for PRICE and DEMAND markets so that these changes are transparent to the 
rest of the code.
*/
void Marketplace::resetToPriceMarket( const string& goodName, const string& regionName ) {
   
   Configuration* conf = Configuration::getInstance();
   Market* newPriceMarket = 0;
   Market* newDemandMarket = 0;
   const int marketNumber = getMarketNumber( goodName, regionName );
   int demandMarketNumber = 0;
   string marketName;
   string demandGoodName;
   const bool setNewMarkets = conf->getBool( "simulActive" ); // for debugging, set this to false to turn this off
   
   if( marketNumber == -1 ) {
      cerr << "ERROR: Market "<< goodName << " does not exist"  << endl;
   }
   
   else if( !setNewMarkets ) {
      return;
   }
   
   else if ( markets[ marketNumber ][ 0 ]->getType() == "NormalMarket" ) {
      
      // Setup the coresponding demand markets
      marketName = markets[ marketNumber ][ 0 ]->getRegionName();
      demandGoodName = goodName + "Demand_int";
      setMarket( regionName, marketName, demandGoodName, DEMAND );
      demandMarketNumber = getMarketNumber( demandGoodName, regionName );
      
      // loop through time periods            
      for( int per = 1; per < static_cast<int>( markets[ marketNumber ].size() ); per++ ){
         
         // Get the pointer of the new demand market.
         newDemandMarket = markets[ demandMarketNumber ][ per ];
         assert( newDemandMarket );
         
         // Create a new price market from the old market.
         newPriceMarket = new PriceMarket( *markets[ marketNumber ][ per ] );
         
         // Set the demand market's pointer to the price market.
         newDemandMarket->setCompanionMarketPointer( newPriceMarket );
         
         // Set the price market's pointer to a demand market.
         newPriceMarket->setCompanionMarketPointer( newDemandMarket );
         
         // Delete the old market.
         delete markets[ marketNumber ][ per ];
         
         // Insert the new price market. 
         markets[ marketNumber ][ per ] = newPriceMarket;
         
         // Set both markets to solve.
         setMarketToSolve( goodName, regionName, per );
         setMarketToSolve( demandGoodName, regionName, per );
         // this assumes that all markets have the same number of periods
      }
   }
}

//! Set the prices by period of a market from a vector.
void Marketplace::setPriceVector( const string& goodName, const string& regionName, const vector<double>& prices ){
   
   // determine what market the region and good are in.
   const int marketNumber = regionToMarketMap[ goodName + regionName ];
   
   for( int i = 0; i < static_cast<int>( markets[ marketNumber ].size() ) && i < static_cast<int>( prices.size() ); i++ ){
      markets[ marketNumber ][ i ]->setRawPrice( prices[ i ] );
   }
}

//! Initialize prices
/*! Supply and demand sector prices should always get set somewhere else, except for first period,
although no such guarantee for GHG markets, but initialize prices for all periods 
in all markets just to be safe if not already set. 

  Initialization also occurs for supply and demand markets that have prices read-in via routine:
  setPriceVector (in sector as supply & demand mkts are created).
  This has no effect for future periods as these prices are overwritten by 
  Marketplace::init_to_last
*/

void Marketplace::initXMLPrices(){
   
   // initialize supply and demand sector market prices to 1.
   for ( int i = 0; i < static_cast<int>( markets.size() ); i++ ){               
      for( int j = 0; j < static_cast<int>( markets[ i ].size() ); j++ ){
         markets[ i ][ j ]->initPrice();
      }
   }
}

//! Set the solve flag for this market for the given period, or all periods if per argument is undefined.
void Marketplace::setMarketToSolve ( const string& goodName, const string& regionName, const int per ) {
   
   const int marketNumber = getMarketNumber( goodName, regionName );
   
   // If the market exists.
   if ( marketNumber != -1 ) {
      // If by default we are setting all time periods to solve.
      if( per == -1 ) {
         for( int per = 0; per < static_cast<int>( markets[ marketNumber ].size() ); per++ ){                 
            markets[ marketNumber ][ per ]->setSolveMarket( true );
         }
      }
      // Otherwise set the individual market to solve.
      else {
         markets[ marketNumber ][ per ]->setSolveMarket( true );
      }
   }
   else {
      cout << "Error: Market cannot be set to solve as it does not exist: " << goodName << " " << regionName << endl;
   }
}

//! initialize all market prices for the given period to 0.
void Marketplace::nullprc( const int period ) {
   for ( int i = 0; i < numMarkets; i++ )
      markets[ i ][ period ]->nullPrice();
}

//! initialize all market demands for the given period to 0.
void Marketplace::nulldem( const int period ) {
   for ( int i = 0; i < numMarkets; i++ ) {
      markets[ i ][ period ]->nullDemand();
   }
}

//! Set one market demand to 0.
/*! Not used at present */
void Marketplace::nulldem( const string& goodName, const string& regionName, const int per ){
   
   const int marketNumber = getMarketNumber( goodName, regionName );
   
   if ( marketNumber != -1 ) {
      markets[ marketNumber ][ per ]->nullDemand();
   }
}

//! initialize all market supplies to 0
void Marketplace::nullsup( const int period ) {
   for ( int i = 0; i < numMarkets; i++ ) {
      markets[ i ][ period ]->nullSupply();
   }
}

//! Set one market supply to 0
/*! Not used at present */
void Marketplace::nullsup( const string& goodName, const string& regionName, const int period ){
   
   const int marketNumber = getMarketNumber( goodName, regionName );
   
   if ( marketNumber != -1 ) {
      markets[ marketNumber ][ period ]->nullSupply();
   }
}

//! Print markets to standard out.
void Marketplace::showmrks( const int period ) const {
   
   for ( int i = 0; i < numMarkets; i++ ) {
      markets[ i ][ period ]->print( cout );
   }
}

//! set market price
/*! If this is a price market, then the price passed is used to set supply instead of price. 
The market price is also used to set the demand.
Use this convention for pseudo markets -- model values are supply, trail market values are used
for demand. */
void Marketplace::setprice( const string& goodName, const string& regionName, const double value, const int per ){
   const int marketNumber = getMarketNumber( goodName, regionName );
   
   if ( marketNumber != -1 ) {
      markets[ marketNumber ][ per ]->setPrice( value );
   }
}

//! Add to the supply for this market
void Marketplace::setsupply( const string& goodName, const string& regionName, const double value, const int per ){
   
   const int marketNumber = getMarketNumber( goodName, regionName );
   
   if ( marketNumber != -1 ) {
      markets[ marketNumber ][ per ]->setSupply( value );
   }
}

//! set market demand to used for solution mechanism
void Marketplace::setdemand( const string& goodName, const string& regionName, const double value, const int per ){
   const int marketNumber = getMarketNumber( goodName, regionName );
   string marketName;
   string demandGoodName;
   
   // The following condition used to be here, don't think this is needed any more
   //	if( goodName != "renewable" ){ // just a guess
   
   if (value < 0 && goodName != "CO2" ) {
      cerr << "ERROR in setdemand: Demand value < 0 for market " << goodName << " in region " << regionName << endl;
   }
   
   if ( marketNumber != -1 ) {
      
      markets[ marketNumber ][ per ]->setDemand( value );
   }
}

//! return market price
/*! If the market does not exist, return an extremely large price.
\todo do something else about "renewable"
*/
double Marketplace::showprice( const string& goodName, const string& regionName, const int per ) const {
   
   const int marketNumber = getMarketNumber( goodName, regionName );
   
   if( marketNumber == -1 ) {
      // the "renewable" fuel depends on the returned price being zero
      if ( goodName != "renewable" ) {
         logfile << "Market not found for " << goodName << " in region " << regionName << endl;
         return 1e12;
      } else {
         return 0;
      }
   }
   else {
      return markets[ marketNumber ][ per ]->getPrice();
   }
   
}

//! return market supply used for solution mechanism
double Marketplace::showsupply( const string& goodName, const string& regionName, const int per ) const {
   const int marketNumber = getMarketNumber( goodName, regionName );
   string marketName;
   string demandGoodName;
   
   if ( marketNumber != -1 ) {
      return markets[ marketNumber ][ per ]->getSupply();
   }
   else {
      cerr << "ERROR: Called for supply of non-existant market "<< goodName << " in " << regionName << endl;
      return 0;
   }
}

//! return supply for use in checking solution, including Raw supply for demand market
/*! Not used now, could be used for debugging at some point*/
double Marketplace::checkSupply( const string& goodName, const string& regionName, const int per ) const {
   
   const int marketNumber = getMarketNumber( goodName, regionName );
   
   if ( marketNumber != -1 ) {
      return markets[ marketNumber ][ per ]->getSupplyForChecking();
   }
   else {
      cerr << "ERROR: Called for supply of non-existant market "<< goodName << " in " << regionName << endl;
      return 0;
   }
}

//! return supply for use in checking solution, including Raw supply for demand market
/*! Used to double-check solution. Should not use otherwise. */
double Marketplace::checkSupply( const int marketNumber, const int per ) const {
   
   return markets[ marketNumber ][ per ]->getSupplyForChecking();
}

//! return market demand used for solution mechanism
/*! If this is a price market, then get the demand from the corresponding demand market.
If this is a demand market (through the redirect), then get the demand from the trail value (.price)
-- although this is never Rawly occurs at present. 
*/
double Marketplace::showdemand(  const string& goodName, const string& regionName, const int per ) const {
   const int marketNumber = getMarketNumber( goodName, regionName );
   string marketName;
   string demandGoodName;
   
   if ( marketNumber != -1 ) {
      return markets[ marketNumber ][ per ]->getDemand();
   }
   else {
      cerr << "ERROR: Called for demand of non-existant market "<< goodName << " in " << regionName << endl;
      return 0;
   }
}

//! Return the name of the market.
string Marketplace::getName( const int marketNumber ) const {
   return markets[ marketNumber ][ 0 ]->getName();
}

//! return region group of market.
string Marketplace::getRegionName( const string& goodName, const string& regionName ) const {
   
   const int marketNumber = getMarketNumber( goodName, regionName );
   
   if ( marketNumber != -1 ) {
      return markets[ marketNumber ][ 0 ]->getRegionName();
   }
   else {
      return "";
   }
}

//! return region group of market.
string Marketplace::getRegionName( const int marketNumber ) const {
   return markets[ marketNumber ][ 0 ]->getRegionName();
}

//! return good name of market.
string Marketplace::getGoodName( const int marketNumber ) const {
   return markets[ marketNumber ][ 0 ]->getGoodName();;
}

//! Return market supply used for solution mechanism
/*! \warning Use for debugging only */
double Marketplace::getRawSupply( const int marketNumber, const int per ) const {
   return markets[ marketNumber ][ per ]->getRawSupply();
}

//! Return market demand used for solution mechanism
/*! \warning Use for debugging only */
double Marketplace::getRawDemand( const int marketNumber, const int per ) const {
   return markets[ marketNumber ][ per ]->getRawDemand();
}

//! Return market price used for solution mechanism.
/*! \warning Use for debugging only */
double Marketplace::getRawPrice( const int marketNumber, const int per ) const {
   return markets[ marketNumber ][ per ]->getRawPrice();
}

//! Calculates excess demand for all markets
void Marketplace::excessdemand( const int period ) {
   for ( int i = 0; i < numMarkets; i++ ) {
      markets[ i ][ period ]->calcExcessDemand(); 
   }
}

//! Calculates log of excess demand for all markets
void Marketplace::logED( const int period ) {
   for ( int i = 0; i < numMarkets; i++ ) {
      markets[ i ][ period ]->calcLogExcessDemand( SMALL_NUM );
   }
}

//! Calculates log of demand for all markets
void Marketplace::logDem( const int period ) {
   for ( int i = 0; i < numMarkets; i++ ) {
      markets[ i ][ period ]->calcLogDemand( SMALL_NUM ); 
   }
}

//! Calculates log of supply for all markets
void Marketplace::logSup( const int period ) {
   for ( int i = 0; i < numMarkets; i++ ) {
      markets[ i ][ period ]->calcLogSupply( SMALL_NUM );
   }
}

//! Check to see that all markets Rawly solved.
/*! \todo This code should be in a Solution class, so it may not have access to demand.
*/
bool Marketplace::checkMarketSolution( const double solTolerance, const double excessDemandSolutionFloor, const int period ) const {
   bool solvedOK = true;
   
   for( int i = 0; i < static_cast<int>( markets.size() ); i++ ) {
      if ( !isWithinTolerance( markets[ i ][ period ]->getRawDemand() - markets[ i ][ period ]->getRawSupply(), markets[ i ][ period ]->getRawDemand(), solTolerance, excessDemandSolutionFloor ) ) {
         solvedOK = false;
         cout << "Market "<< i << " ("<< getName( i )<< ") S: "<< markets[ i ][ period ]->getRawSupply() << " D: " << markets[ i ][ period ]->getRawDemand() << endl;
      }
   }
   
   return solvedOK;
}

//! Select markets to solve.
int Marketplace::setMarketsToSolve( const int period ) {
   
   numMarketsToSolve = 0;
   markets_isol.clear();
   
   for( int i = 0; i < static_cast<int>( markets.size() ); i++ ) {
      
      // Add markets to the "normal" solution list
      if ( markets[ i ][ period ]->shouldSolve() ) {
         numMarketsToSolve++;
         markets_isol.push_back( i );
      }
   }
   
   return numMarketsToSolve;
}

//! set markets to solve
int Marketplace::setMarketsToSolveNR( const int period ) {
   numMarketsToSolveNR= 0;
   markets_isol_NR.clear();
   
   for( int i = 0; i < static_cast<int>( markets.size() ); i++ ) {
      
      // Add markets to NR solution list
      if ( markets[ i ][ period ]->shouldSolveNR( SMALL_NUM ) ) {
         numMarketsToSolveNR++;
         markets_isol_NR.push_back( i );
      }
   } // end for
   
   return numMarketsToSolveNR;
}

//! return market with largest excess demand. 
int Marketplace::worstED( const vector<int>& indices, const double excessDemandSolutionFloor, const int per ) const {
   int worstID = 0;
   double largest = 0;
   
   for ( int i = 0; i < static_cast<int>( indices.size() ); i++ ) {
      const int j = indices[ i ];
      const double relativeED = getRelativeED( markets[ j ][ per ]->getExcessDemand(), markets[ j ][ per ]->getRawDemand(), excessDemandSolutionFloor );
      
      if ( ( fabs( markets[ j ][ per ]->getRawPrice() ) > SMALL_NUM ) && ( relativeED > largest ) ) {
         worstID = i;
         largest = relativeED;
      }
   }
   return worstID;
}

//! returns largest excess demand
double Marketplace::maxED( const vector<int>& indices, const double excessDemandSolutionFloor, const int per ) const {
   const int worstMarket = worstED( indices, excessDemandSolutionFloor, per );
   return getRelativeED( markets[ indices[ worstMarket ] ][ per ]->getExcessDemand(), markets[ indices[ worstMarket ] ][ per ]->getRawDemand(), excessDemandSolutionFloor );
}

//! set new solution prices for all markets
void Marketplace::setPrices( const vector<double>& prices, const vector<int>& indices, const int per ) {
   // set prices only for markets in price vector
   for ( int i = 0; i <  static_cast<int>( indices.size() ); i++ ) {
      const int j = indices[ i ];	// look up index
      markets[ j ][ per ]->setRawPrice( prices[ i ] );
   }
}

//! Initialize the marketplace prices 
/*! Use last period demand price as starting point for next period */
void Marketplace::init_to_last( const int period ) { 
   // only after the starting period
   if ( period > 0 ) {
      for ( int i = 0; i < numMarkets; i++ ) {
         markets[ i ][ period ]->setPriceToLast( markets[ i ][ period - 1 ]->getRawPrice() );
      }
   }
}

//! For debugging, print out all prices
/*! \warning This assumes prices_to_bugout will be called first.
*   \todo Indices and bugout should be passed as arguments.
*/

void Marketplace::prices_to_bugout( const int per ) const {
   bugoutfile << "Prices,";
   
   for ( int i = 0; i < numMarketsToSolve; i++ ){ 
      bugoutfile << markets[ markets_isol[ i ] ][ per ]->getName() << ",";
   }
   
   bugoutfile << endl;
   bugoutfile << ",";
   for ( int j = 0; j < numMarketsToSolve; j++ ) {
      bugoutfile << markets[ markets_isol[ j ] ][ per ]->getRawPrice() << ",";
   }
   bugoutfile << endl;
}

//! For debugging, print out all supplies
void Marketplace::supply_to_bugout( const int per ) const {
   bugoutfile << "Supplies:,";
   
   for ( int i = 0; i < numMarketsToSolve; i++ ) {
      bugoutfile << markets[ markets_isol[ i ] ][ per ]->getRawSupply() << ",";
   }
   bugoutfile << endl;
}

//! For debugging, print out all demands
void Marketplace::demand_to_bugout( const int per ) const {
   bugoutfile << "Demands:,";
   
   for ( int i = 0; i < numMarketsToSolve; i++ ) {
      bugoutfile << markets[ markets_isol[ i ] ][ per ]->getRawDemand() << ",";
   }
   bugoutfile << endl;
}

//! This sets the ts of the current period to last periods values.
// Pass the old market.
void Marketplace::storeto_last( const int period ) {
   // only after the starting period
   if ( period > 0 ) {
      for ( int i = 0; i < numMarkets; i++ ) {
         markets[ i ][ period ]->storeInfoFromLast( markets[ i ][ period - 1 ]->getRawDemand(), markets[ i ][ period - 1 ]->getRawSupply(), markets[ i ][ period - 1 ]->getRawPrice() );
      }
   }
}

//! Store original demand, supply and price
/*! Used for calculation of derivative */
void Marketplace::storeinfo( const int period ) {
   for ( int i = 0; i  < numMarkets; i++ ) {
      markets[ i ][ period ]->storeInfo();
   }
}

//! Restore original demand, supply and price
/*! Used for calculation of derivative */
void Marketplace::restoreinfo( const int period) {
   for ( int i = 0; i < numMarkets; i++ ) {
      markets[ i ][ period ]->restoreInfo();
   }
}

//! Restore original demand, supply and price.
/*! Used for calculation of derivative */
void Marketplace::restoreprc( const vector<int>& indices, const int per ) {
   // store only for markets that need solving
   for ( int i = 0; i < static_cast<int>( indices.size() ); i++ ) {
      const int j = indices[ i ];	// look up index
      markets[ j ][ per ]->restorePrice();
   }
}

//! returns vector of market prices
const vector<double> Marketplace::getPrices( const vector<int>& indices, const int per ) const {
   
   vector<double> prices( indices.size() );
   
   for ( int i = 0; i < static_cast<int>( indices.size() ); i++ ) {
      const int j = indices[ i ];	// look up index
      prices[ i ] = markets[ j ][ per ]->getRawPrice();
   }
   
   return prices;
}

//! returns vector of market excess demands
const vector<double> Marketplace::getExcessDemands( const vector<int>& indices, const int per ) const {
   
   vector<double> ED( indices.size() );
   
   for ( int i = 0; i < static_cast<int>( indices.size() ); i++ ) {
      const int j = indices[ i ];	// look up index
      ED[ i ] = markets[ j ][ per ]->getExcessDemand();
   }	
   
   return ED;
}

//! returns vector of log of market excess demands
const vector<double> Marketplace::getLogExcessDemands( const vector<int>& indices, const int per ) const {
   
   vector<double> ED( indices.size() );
   
   for ( int i = 0; i < static_cast<int>( indices.size() ); i++ ) {
      const int j = indices[ i ];	// look up index
      ED[ i ] = markets[ j ][ per ]->getLogExcessDemand();
   }
   
   return ED;
}
//! returns vector of market demands for markets within a specified index.
const vector<double> Marketplace::getDemands( const vector<int>& indices, const int per ) const {
   vector<double> demands( indices.size() );
   
   for ( int i = 0;i < static_cast<int>( indices.size() ); i++ ) {
      const int j = indices[ i ];	// look up index
      demands[ i ] = markets[ j ][ per ]->getRawDemand();
   }
   
   return demands;
}

//! Returns a vector of market demands for all markets.
const vector<double> Marketplace::getDemands( const int per ) const {
   
   vector<double> demands( numMarkets );
   
   for ( int i = 0; i < numMarkets; i++ ) {
      demands[ i ] = markets[ i ][ per ]->getRawDemand();
   }
   
   return demands;
}

//! returns vector of market supplies
const vector<double> Marketplace::getSupplies( const vector<int>& indices, const int per ) const {
   vector<double> supplies( indices.size() );
   
   for ( int i = 0;i < static_cast<int>( indices.size() ); i++ ) {
      const int j = indices[ i ];	// look up index
      supplies[ i ] = markets[ j ][ per ]->getRawSupply();
   }
   
   return supplies;
}

//! Returns a vector of market supplies for all markets.
const vector<double> Marketplace::getSupplies( const int per ) const {
   
   vector<double> supplies( numMarkets );
   
   for ( int i = 0; i < numMarkets; i++ ) {
      supplies[ i ] = markets[ i ][ per ]->getRawSupply();
   }
   
   return supplies;
}

//! returns vector of log of market demands
const vector<double> Marketplace::getLogDemands( const vector<int>& indices, const int per ) const {
   vector<double> demands( indices.size() );
   
   for ( int i = 0; i < static_cast<int>( indices.size() ); i++ ) {
      const int j = indices[ i ];	// look up index
      demands[ i ] = markets[ j ][ per ]->getLogDemand();
   }
   
   return demands;
}

//! returns vector of log of market supplies
const vector<double> Marketplace::getLogSupplies( const vector<int>& indices, const int per ) const {
   vector<double> supplies( indices.size() );
   
   for ( int i = 0; i < static_cast<int>( indices.size() ); i++ ) {
      const int j = indices[ i ];	// look up index
      supplies[ i ] = markets[ j ][ per ]->getLogSupply();
   }
   
   return supplies;
}

//! Calculate the derivatives, elasticities or Jacobian.
const vector<double> Marketplace::jacobian( const vector<int>& indices, const int k, const int per ) const {
   double ddemand;
   double dsupply;
   double dprice;
   vector<double> JFD( indices.size() );
   
   for ( int i = 0; i < static_cast<int>( indices.size() ); i++ ) {
      const int j = indices[ i ];	// look up index
      
      ddemand = markets[ j ][ per ]->getChangeInDemand();
      dsupply = markets[ j ][ per ]->getChangeInSupply();
      dprice = markets[ k ][ per ]->getChangeInPrice();
      
      if( dprice == 0 ){
         dprice = SMALL_NUM;
      }
      
      JFD[ i ] = ( ddemand - dsupply ) / dprice; 
   }
   return JFD;
}

//! Calculate demand elasticities
const vector<double> Marketplace::calcDemandElas( const vector<int>& indices, const int marketSolutionNumber, const int per ) const {
   
   double ddemand;
   double dprice;
   vector<double> JFD( indices.size() );
   const int marketNumber = indices[ marketSolutionNumber ]; // market index 
   
   for ( int i = 0; i < static_cast<int>( indices.size() ); i++ ) {
      const int j = indices[ i ];	// look up index
      
      ddemand = markets[ j ][ per ]->getLogChangeInDemand( SMALL_NUM );
      dprice = markets[ marketNumber ][ per ]->getLogChangeInPrice( SMALL_NUM );
      
      if( dprice == 0 ){
         dprice = SMALL_NUM;
      }
      
      JFD[ i ] = ddemand / dprice;
      assert( isValidNumber( JFD[ i ] ) );
   }
   
   return JFD;
}

//! Calculate supply elasticities
const vector<double> Marketplace::calcSupplyElas( const vector<int>& indices, const int marketSolutionNumber, const int per ) const {
   
   double dsupply;
   double dprice;
   
   vector<double> JFS( indices.size() );
   
   const int marketNumber = indices[ marketSolutionNumber ]; // market index 
   
   for ( int i = 0; i < static_cast<int>( indices.size() ); i++ ) {
      const int j = indices[ i ];	// look up index
      
      dsupply = markets[ j ][ per ]->getLogChangeInSupply( SMALL_NUM );
      dprice = markets[ marketNumber ][ per ]->getLogChangeInPrice( SMALL_NUM );
      
      if( dprice == 0 ){
         dprice = SMALL_NUM;
      }
      
      JFS[ i ] = dsupply / dprice;
      assert( isValidNumber( JFS[ i ] ) );
   }
   return JFS;
}

//! write out market info to database
void Marketplace::MCoutput() const {
   
   const Modeltime* modeltime = scenario->getModeltime();
   
   void dboutput4(string var1name,string var2name,string var3name,string var4name,
      string uname,vector<double> dout);
   
   const int maxPeriod = modeltime->getmaxper();
   vector<double> temp(maxPeriod);
   int j;
   // write market prices, supply and demand
   for (int i=0;i<numMarkets;i++) {
      for (j=0;j<maxPeriod;j++) {
         temp[j] = markets[i][j]->getRawPrice();
      }
      dboutput4(markets[i][0]->getRegionName(),"Market",markets[i][0]->getGoodName(),"1_price","$/GJ",temp);
      for (j=0;j<maxPeriod;j++) {
         temp[j] = markets[i][j]->getRawSupply();
      }
      dboutput4(markets[i][0]->getRegionName(),"Market",markets[i][0]->getGoodName(),"2_supply","EJ",temp);
      for (j=0;j<maxPeriod;j++) {
         temp[j] = markets[i][j]->getRawDemand();
      }
      dboutput4(markets[i][0]->getRegionName(),"Market",markets[i][0]->getGoodName(),"3_demand","EJ",temp);
   }
}

//! write out market info to file
void Marketplace::outputfile() const {
   
   const Modeltime* modeltime = scenario->getModeltime();
   
   // function protocol
   void fileoutput2(string var1name,string var2name,string var3name,
      string var4name,string var5name,vector<double> dout,string uname);
   void fileoutput3(string var1name,string var2name,string var3name,
      string var4name,string var5name,string uname,vector<double> dout);
   
   const int maxPeriod = modeltime->getmaxper();
   vector<double> temp(maxPeriod);
   int j;
   
   // Function arguments are variable name, double array, db name, and
   // table name.
   // The function writes all years.
   
   // write market prices and supply (or demand)
   for (int i=0;i<numMarkets;i++) {
      for (j=0;j<maxPeriod;j++) {
         temp[j] = markets[i][j]->getRawPrice();
      }
      fileoutput3(markets[i][0]->getRegionName(),"market",markets[i][0]->getGoodName()," ","price","$/GJ",temp);
      for (j=0;j<maxPeriod;j++) {
         temp[j] = markets[i][j]->getRawSupply();
      }
      fileoutput3(markets[i][0]->getRegionName(),"market",markets[i][0]->getGoodName()," ","supply","EJ",temp);
      for (j=0;j<maxPeriod;j++) {
         temp[j] = markets[i][j]->getRawDemand();
      }
      fileoutput3(markets[i][0]->getRegionName(),"market",markets[i][0]->getGoodName()," ","demand","EJ",temp);
   }
}

//! write out market info to file for debugging
void Marketplace::bugout( const int per, const int iteration) const {
   // write market prices and supply (or demand)
   bugoutfile<< "Period," << per << ",Nth Iteration:," << iteration << endl;
   for ( int i = 0; i < numMarketsToSolveNR; i++ ) {
      bugoutfile << "market:," << getRegionName( markets_isol_NR[ i ] ) << getGoodName( markets_isol_NR[ i ] ) << ",price:,"<< markets[ markets_isol_NR[ i ] ][ per ]->getRawPrice() << ",$/GJ,";
      bugoutfile << "supply:," << markets[ markets_isol_NR[ i ] ][ per ]->getRawSupply() << ",EJ,demand:," << markets[ markets_isol_NR[ i ] ][ per ]->getRawDemand() << ",EJ, "; 
      // bugoutfile << "SolveMarket: " << markets[ i ][ period ]->solveMarket << endl;
   }
   bugoutfile << endl;
}

//! write out market price supply and demand info to file for debugging
void Marketplace::sdcurves( const int period, const int iteration ) const {
   // write market prices and supply (or demand)
   for ( int i = 0; i < numMarkets; i++ ) {
      sdcurvefile << i << "," << markets[ i ][ 0 ]->getGoodName() << "," << markets[ i ][ period ]->getRawPrice() <<",";
      sdcurvefile << markets[ i ][ period ]->getRawSupply() << "," << markets[ i ][ period ]->getRawDemand()<<",";
   }
   sdcurvefile << endl;
}

//! Bracketing function only
/* Function finds bracket interval for each market and puts this information into sol vector
* \author Sonny Kim, Josh Lurz, Steve Smith
* \param solutionTolerance Target value for maximum relative solution for worst market 
* \param excessDemandSolutionFloor *Absolute value* beneath which market is ignored 
* \param bracketInterval Relative multipliciatve interval by which trail values are moved
* \param sol Vector of market solution information 
*** WHAT IS INDEXING OF THE ABOVE? 
* \param allbracketed Boolean that holds bracketing state 
* \param firsttime Boolean that marks first time bracket is performed 
* \param worldCalcCount Counter for number of worldcalc model calls 
* \param per Model period
*/
int Marketplace::Bracket( const double solutionTolerance, const double excessDemandSolutionFloor, 
                         const double bracketInterval, vector<solinfo>& sol, bool& allbracketed, 
                         bool& firsttime, double& worldCalcCount, const int per ) {
   
   World* world = scenario->getWorld();
   int i;
   const int numCurrMarkets = sol.size(); // number of markets to solve
   int numIterations = 0; // number of iterations
   int code = 2; // code that reports success 1 or failure 0
   vector<double> tempPrices( numCurrMarkets ); // temporary prices
   vector<double> EDtemp( numCurrMarkets ); // temporary excess demand
   bugoutfile << ",,Bracketing function called." << endl;
   logfile << ",,Bracketing function called." << endl;
   
   // Loop is done at least once.
   do {
      
      // Store the prices in tempPrices.
      for ( i = 0; i < numCurrMarkets; i++ ) {
         tempPrices[ i ] = sol[ i ].X;
      }
      
      setPrices( tempPrices, markets_isol, per ); // set new prices
      nulldem( per );	// null demand
      nullsup( per ); // null supply
      
      world->calc( per ); // call world object to recalculate supply and demand
      
      excessdemand( per ); // calculate excess demand
      EDtemp = getExcessDemands( markets_isol, per ); // show excess demand
      
      // Set excess demands.
      for ( i = 0; i < numCurrMarkets; i++ ) {
         sol[ i ].ED = EDtemp[ i ];
      }
      
      // for debugging
      const bool bug = false; // debugging
      if ( bug ) {
         bugoutfile << "In bracketing." << endl;
         
         bugoutfile << endl << "Market,X,XL,XR,ED,EDL,EDR,Tolerance,Bracketed" << endl;
         
         for ( i = 0; i < numCurrMarkets; i++ ) {
            bugoutfile << getName( markets_isol[ i ] ) << "," <<sol[ i ].X << "," << sol[ i ].XL << "," << sol[ i ].XR
               << "," << sol[ i ].ED << "," << sol[ i ].EDL << "," << sol[ i ].EDR << "," << solutionTolerance << "," << sol[ i ].bracketed << endl;
         }
         
         bugout( per, numIterations );
         sdcurves( per, numIterations );
      }
      
      // bracketed array is either 0 or 1
      allbracketed = true;
      
      for ( i = 0; i < numCurrMarkets; i++ ) {
         if ( !sol[ i ].bracketed ) {
            allbracketed = false;
            break;
         }
      }
      
      // Bracketing of prices; done first regardless of choice of solution algorithm.
      if ( !allbracketed ) {
         
         // Iterate through each market.
         for ( i = 0; i < numCurrMarkets; i++ ) {
            
            // If the market is not bracketed.
            if ( !sol[ i ].bracketed ) {
               
               // If ED at X and L are the same sign.
               if ( std::sign( sol[ i ].ED ) == std::sign( sol[ i ].EDL ) ) {
                  
                  // If Supply > Demand at point X. Price needs to decrease.
                  if ( sol[ i ].ED < 0 ) { 
                     
                     // Move Left Bracket to X
                     sol[ i ].XL = sol[ i ].X; 
                     sol[ i ].EDL = sol[ i ].ED;
                     
                     // If L and R do not span solution.
                     if ( std::sign( sol[ i ].EDL ) == std::sign( sol[ i ].EDR ) ) {
                        
                        // Decrease X.
                        
                        // If X is positive.
                        if( sol[ i ].X > SMALL_NUM ) {
                           sol[ i ].X *= 1 - bracketInterval; 
                        }
                        
                        // If X is near 0. 
                        else if ( fabs( sol[ i ].X ) < SMALL_NUM ) {
                           sol[ i ].X = 0;
                        }
                        
                        // X is negative.
                        else {
                           assert( false );
                        }
                        
                     } // if ( std::sign( sol[ i ].EDL ) == std::sign( sol[ i ].EDR ) )
                     
                     // If L and R span solution.
                     else {
                        
                        // This market is bracketed.
                        sol[ i ].bracketed = true;
                     } 
                  } // if( sol[ i ].ED < 0 )
                  
                  // If Supply <= Demand. Price needs to increase.
                  else { // ED >= 0
                     
                     // Move R Bracket to X
                     sol[ i ].XR = sol[ i ].X; 
                     sol[ i ].EDR = sol[ i ].ED;
                     
                     // If L and R do not span solution.
                     if ( std::sign( sol[ i ].EDL ) == std::sign( sol[ i ].EDR ) ) {
                        
                        // Increase X.
                        
                        // If X is positive.
                        if( sol[ i ].X > SMALL_NUM ) {
                           sol[ i ].X *= ( 1 + bracketInterval );
                        }
                        
                        // If X is 0
                        else if ( fabs( sol[ i ].X ) < SMALL_NUM ) {
                           sol[ i ].X = 0.05;
                        }
                        
                        // X is negative.
                        else {
                           assert( false );
                        }
                     }
                     
                     // If L and R span solution.
                     else {
                        
                        // This market is bracketed.
                        sol[ i ].bracketed = true;
                     }
                  }
               }
               
               // ED at X and R are the same sign.
               else { 
                  
                  // If Supply > Demand at X. Price needs to decrease.
                  if ( sol[ i ].ED < 0 ) {
                     
                     // Move the left bracket to X.
                     sol[ i ].XL = sol[ i ].X; 
                     sol[ i ].EDL = sol[ i ].ED;
                     
                     // If L and R do not span solution.
                     if ( std::sign( sol[ i ].EDL ) == std::sign( sol[ i ].EDR ) ) {
                        
                        // Decrease X.
                        
                        // If X is positive.
                        if( sol[ i ].X > SMALL_NUM ) {
                           sol[ i ].X *= 1 - bracketInterval;
                        }
                        
                        // If X is 0
                        else if ( fabs( sol[ i ].X ) < SMALL_NUM ) {
                           sol[ i ].X = 0;
                        }
                        
                        // X is negative.
                        else {
                           assert( false );
                        }
                     }
                     else {
                        
                        // The market is bracketed.
                        sol[ i ].bracketed = true;
                     }
                  }
                  
                  // If Supply <= Demand at X. Prices need to increase.
                  else {
                     // Move the right bracket to X.
                     sol[ i ].XR = sol[ i ].X; 
                     sol[ i ].EDR = sol[ i ].ED; 
                     
                     // If L and R do not span solution.
                     if ( std::sign( sol[ i ].EDL ) == std::sign( sol[ i ].EDR ) ) {
                        
                        // Increase X.
                        
                        // If X is positive.
                        if( sol[ i ].X > SMALL_NUM ) {
                           sol[ i ].X *= ( 1 + bracketInterval );
                        }
                        
                        // If X is 0
                        else if ( fabs( sol[ i ].X ) < SMALL_NUM ) {
                           sol[ i ].X = 0;
                        }
                        
                        // X is negative.
                        else {
                           assert( false );
                        }
                     }
                     else {
                        
                        // The market is bracketed.
                        sol[ i ].bracketed = true;
                     }
                  }
               }
            } // if( !sol[ i ].bracketed )
            
            // Check if the bracket is empty.
            // if ( fabs( sol[ i ].XL - sol[ i ].XR ) < VERY_SMALL_NUM ) {
            //    sol[ i ].bracketed = false;
            //    sol[ i ].XL = sol[ i ].XR = sol[ i ].X;
            //    sol[ i ].EDL = sol[ i ].EDR = sol[ i ].ED;
            // }
            
            // Check if X was exogenously forced outside the bracket.
            if( !( sol[ i ].X <= sol[ i ].XL ) || !( sol[ i ].X >= sol[ i ].XR ) ) {
               // sol[ i ].XR = sol[ i ].XL = sol[ i ].X;
               // sol[ i ].EDR = sol[ i ].EDL = sol[ i ].ED;
               sol[ i ].bracketed = false;
            }
            
            // Check if the ED is below the solution tolerance.
            if( isWithinTolerance( sol[ i ].ED, markets[ markets_isol[ i ] ][ per ]->getRawDemand(), solutionTolerance, excessDemandSolutionFloor ) ) {
               sol[ i ].bracketed = true;
            }

            // Check if the market is unbracketable. This check is needed for GHG markets. 
            if( ( sol[ i ].X < VERY_SMALL_NUM ) && ( sol[ i ].ED < SMALL_NUM ) ) {
               sol[ i ].bracketed = true;
                sol[ i ].XR = sol[ i ].X = 0;
            }
            
         } // for 
         
         allbracketed = true;
         for ( i = 0; i < numCurrMarkets; i++ ) {
            if ( !sol[ i ].bracketed ) {
               allbracketed = false;
               break;
            }
         }
      }
   } while ( ++numIterations < 30 && !allbracketed );	
   code = ( allbracketed ? 1 : 0 );	// Report success, 1 or failure, 0
   
   worldCalcCount += numIterations - 1;
   return code;
}

//! function to check bracketing -- ???(see details)
/*! if not solving and bracketed prices are converging,
???? what does this mean?*/

bool Marketplace::isWithinTolerance( const double excessDemand, const double demand, const double solutionTolerance, const double excessDemandSolutionFloor ) const {   
   return ( getRelativeED( excessDemand, demand, excessDemandSolutionFloor ) < solutionTolerance );
}

double Marketplace::getRelativeED( const double excessDemand, const double demand, const double excessDemandFloor ) const {
   
   double retValue;
   double tempDemand = demand;
   
   if( tempDemand < SMALL_NUM ) {
      tempDemand = SMALL_NUM;
   }
   
   // Check if the ED is below a minimal value. 
   if( fabs( excessDemand ) < excessDemandFloor ) {
      retValue = 0;
   }
   
   // Find the ratio of excess demand to demand.
   else {
      retValue = fabs( excessDemand ) / tempDemand * 100;
   }
   
   return retValue;
}

void Marketplace::CheckBracket( const double solutionTolerance, const double excessDemandSolutionFloor, vector<solinfo>& sol, bool& allbracketed ){
   logfile << ",,Check brackets function called.\n";
   int numCurrMarkets = sol.size(); // number of markets to solve
   // try rebracketing by setting bracketed array to false
   for(int i=0;i<numCurrMarkets;i++) {
      if (fabs(sol[i].dX) < SMALL_NUM) {
         allbracketed = false;
         sol[i].bracketed = false;
         sol[i].XL = sol[i].XR = sol[i].X; 
         sol[i].EDL = sol[i].EDR = sol[i].ED; 
      }
   }
}

//! Bisection Solution Mechanism (all markets)
/*! This solution mechanism bisects all markets at once. 
* Bisection is always performed at least a few times. Bisection stops if the maximum 
* relative ED does not change at a rate larger than BREAK_OUT_THRESHOLD.
* If the maximum relative "ED" is larger than BRACKET_THRESHOLD, then the unsolved markets
* are re-bracketed. Then bisection continues. The bracketing interval is smaller than that
* used initially so as to not perturb trial values too much. If a further re-bracket
* is necessary, the bracketing interval is decreased further. 
*
* Also, the price and demand markets are very prone to move outside their brackets.
* A check for that is performed each time and the brackets are adjusted accordingly.
* This check is critical for solution with simultuantey.
*
* Useful TrackED writeout to screen is turned on by toggle in configuration file.
* \author Sonny Kim, Josh Lurz, Steve Smith
* \warning Unless stated otherwise, ED values are normalized (i.e., that 10 == 10% difference).
* \todo need more general way to reset price and demand market types within bisect
* \todo implement check on price and demand markets within bracket?
* \param solutionTolerance Target value for maximum relative solution for worst market 
* \param excessDemandSolutionFloor *Absolute value* beneath which market is ignored 
* \param sol Vector of market solution information 
*** WHAT IS INDEXING OF THE ABOVE? 
* \param worldCalcCount Counter for number of worldcalc model calls 
* \param per Model period
*/
int Marketplace::Bisection_all( const double solutionTolerance, const double excessDemandSolutionFloor, const int IterLimit, vector<solinfo>& sol, double& worldCalcCount, const int per ) {
   
   World* world = scenario->getWorld();
   int i, j;
   int numIterations = 0; // number of iterations
   int interLimitAdd = 0; // Variable to allow more iterations if re-bisect
   int numbRebrackets = 0; // Variable to allow more iterations if re-bisect
   int code = 2; // code that reports success 1 or failure 0
   const int numCurrMarkets = sol.size(); // number of markets to solve
   double maxSolVal; // maximum equality value
   vector<double> tempPrices(numCurrMarkets); // temporary prices
   vector<double> EDtemp(numCurrMarkets); // temporary excess demand
   bool breakout;	// var to allow various conditions to exit bisection routine
   const int MAX_REBRACKETS = 3;
   const double BREAK_OUT_THRESHOLD = 0.001; // leave bracketing if not improving by at least this much
   const double BRACKET_THRESHOLD = 10;	// if breakout & relative maxED is > this then try re-bracketing
   double bracketInterval = 0.05; // starting value for re-bracketing interval
   double previousEDvalue = -1;
   double previousEDint = 0;
   double maxEDvalue = 0;
   bool reBracketingDone = false;
   const bool bug = false;
   
   if ( bug ) {
      bugoutfile << endl << "Market,X-unknown,L-Brack,R-Brack,Ex Dem,EDL-Brac,EDR-Brac,Supply,Price,Demand,tolerance" << endl;
   }
   
   if ( trackED ) { 
      cout << endl << "Bisection begin" << endl; 
   }
   logfile << ",,Bisection_all function called." << endl;
   // solve all markets
   do {
      breakout = false; //default is not to breakout of bisection routine
      maxEDvalue = 0;
      if (bugMinimal) {
         bugoutfile << " Bisect " << numIterations;
      }
      if (bugTracking) {
         prices_to_bugout(per);
      }
      
      for (i=0; i<numCurrMarkets; ++i) {
         if ( !isWithinTolerance( sol[ i ].ED, markets[ markets_isol[ i ] ][ per ]->getRawDemand(), solutionTolerance, excessDemandSolutionFloor )) { // if haven't solved
            // Move the left bracket in if Supply > Demand
            if (sol[i].ED < 0) {
               sol[i].XL = sol[i].X;
               sol[i].EDL = sol[i].ED;
            }
            // Move the right bracket in if Demand >= Supply
            else {
               sol[i].XR = sol[i].X;
               sol[i].EDR = sol[i].ED;
            }
            // Set new trial value to center
            sol[i].X = (sol[i].XL + sol[i].XR)/2;
            // Set difference in bracketed price
            sol[i].dX = sol[i].XR - sol[i].XL;
         }	
         // price=0 and supply>demand
         // only true for constraint case
         // other markets cannot have supply>demand as price->0
         if (fabs(sol[i].X)<SMALL_NUM && sol[i].ED<0) { 
            sol[i].X = 0; 
            sol[i].dX = 0;
         } 
         tempPrices[i]=sol[i].X; // copy prices to temporary vector
      }
      setPrices( tempPrices, markets_isol, per ); // set new prices
      nulldem(per);	// null demand
      nullsup(per); // null supply
      
      world->calc(per); // call world object to recalculate supply and demand
      
      logED(per); // calculate log of excess demand
      excessdemand(per); // calculate excess demand
      EDtemp = getExcessDemands( markets_isol, per ); // show excess demand
      for (i=0;i<numCurrMarkets;i++) {
         sol[i].ED = EDtemp[i];
         j = markets_isol[ i ]; 
         // The  price and demand markets are very prone to moving beyond their brackets. 
         // So check and adjust if needed. Lines below check if XL < Demand, or XR > Demand, 
         // and move brackets if necessary. A more general  bracket check is below, but this
         // is needed more often and can be done simply.
         if ( ( markets[ j ][ per ]->getType() == "PriceMarket" || markets[ j ][ per ]->getType() == "DemandMarket" ) && sol[i].XL < getRawDemand(j,per) ) {
            sol[i].XL = getRawDemand(j,per) * 1.5;
         }
         if ( ( markets[ j ][ per ]->getType() == "PriceMarket" || markets[ j ][ per ]->getType() == "DemandMarket" ) && sol[i].XR > getRawDemand(j,per) ) {
            sol[i].XR = getRawDemand(j,per) / 1.5;
         }
         
         // debugging code that tracks ED
         if (i< 10 && false) {
            cout << getGoodName(j)<<": "<<sol[i].XL <<"("<<getRawPrice(j,per) <<") "<< sol[i].XR;
            cout <<"  S,D: ["<<getRawSupply(j,per)<<","<< getRawDemand(j,per) <<"]";
            if (sol[i].XL < getRawDemand(j,per) ) {
               cout << "***";
            }
            
            else {
               cout <<"  ED: ["<<getRawDemand(j,per) - getRawSupply(j,per)<<","<< sol[i].ED<<"]";
            }
            cout <<endl;
         }
         
      } // end nmrks loop
      maxSolVal = maxED( markets_isol, excessDemandSolutionFloor, per );
      int maxIntSol = worstED( markets_isol, excessDemandSolutionFloor, per ); // index of solution market with maxED
      
      // If the worst ED is not changing too much then breakout of bisection
      if (numIterations > 5 ) {	// always bisect a few times         
         if ( fabs(maxSolVal-previousEDvalue)/previousEDvalue < BREAK_OUT_THRESHOLD && maxIntSol == previousEDint )  {  
            breakout = true; 
         }
      }
      
      previousEDvalue = maxSolVal;
      previousEDint = maxIntSol;
      
      
      
      if (bug) {
         bugoutfile << endl <<"Market,X-unknown,L-Brack,R-Brack,Ex Dem,EDL-Brac,EDR-Brac,tolerance\n";
         for (i=0; i<numCurrMarkets; ++i) {
            j =  markets_isol[ i ]; 
            double bugP,  bugD, bugS;
            // if (j == 1) {
            bugP = getRawPrice(j,per);
            bugS = getRawSupply(j,per);
            bugD = getRawDemand(j,per);
            //     cout <<numIterations<< " sup:"<<bugS<<", p:" << bugP<<", dem:"<<bugD << endl;
            
            bugoutfile<<i<<","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR<<",";
            bugoutfile<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<bugS<<","<< bugP<<","<<bugD<<","<<solutionTolerance<<","<<numIterations<<","<<j<<"\n"; 
            // }
         }
         //  bugout(per,numIterations);
         //  sdcurves(per,numIterations); 
      }
      if (trackED) { 
         int maxInt = markets_isol[ maxIntSol ]; // transform to market index
         cout << "bisect-maxRelED: " << maxSolVal <<" ("<< markets[ maxInt ][ per ]->getName() << ") -> ";
         cout << "S: " << getRawSupply( maxInt , per ) << " , " << " D: "<< getRawDemand( maxInt , per );
         cout << ",  P: " << getRawPrice( maxInt , per );
         cout << endl; 
         
         // Extra debugging statements to check bracketing behavior. 
         //  cout << "     XL: " << sol[maxIntSol].XL << "  X:" << sol[maxIntSol].X << "  XR: "<< sol[maxIntSol].XR << endl;
         
      }
      
      // If have not solved, then try bracketing again.
      // This helps when some market has fell out of its bracketing range
      // This helps if the maxED is still somewhat large (otherwise, NR should be fine)
      // This needs to be below maxED printout so that inconsistent TrackED printout does not occur
      if ( breakout && !reBracketingDone && ( fabs( maxSolVal ) > BRACKET_THRESHOLD ) ) {
         ++numbRebrackets;
         
         if ( numbRebrackets > MAX_REBRACKETS ) {
            reBracketingDone = true; // only try this once
         }
         
         // reset bracket flag for any markets not solved
         for(int i=0;i<numMarketsToSolve;i++) {
            if (fabs(sol[i].ED) > solutionTolerance) {
               sol[i].bracketed = false;
               sol[i].XL = sol[i].XR = sol[i].X; 
               sol[i].EDL = sol[i].EDR = sol[i].ED; 
            }
            else {
               sol[i].bracketed = true;
            }
         }
         logfile << ",";
         bool tempAllBracketed = false;
         bool firstTime = false;
         Bracket( solutionTolerance, excessDemandSolutionFloor, bracketInterval, 
            sol,tempAllBracketed,firstTime,worldCalcCount,per);
         
         // Reduce bracket interval for next re-bracket so do not perturb prices as much
         bracketInterval = bracketInterval/2; 
         breakout = false;
         interLimitAdd = numIterations;
         logfile << ",,,Bisection_all continuing after re-bracketing." << endl;
         if (trackED) {
            cout << "Bisection continuing after re-bracketing" << endl; 
         }
      } // end re-bracket block                       
      
   } // end do loop		
   while (++numIterations < (IterLimit+interLimitAdd) && maxSolVal >= solutionTolerance && !breakout);
   worldCalcCount+=numIterations-1;
   code = (maxSolVal < solutionTolerance ? 1 : 0); // report success, 1 or failure, 0
   
   if ( numIterations >= (IterLimit+interLimitAdd) ) {
      logfile << ",,,***Exited Bisection_all. Max number of Iterations exceeded." << endl;
   }
   if (trackED) { cout << endl; }
   
   return code;
}

//! Bisection Solution Mechanism (single market)
int Marketplace::Bisection_i( const int worstMarket, const double solutionTolerance, vector<solinfo>& sol, double& worldCalcCount, const int per ){
   World* world = scenario->getWorld();
   int numIterations = 0; // number of iterations
   int code = 2; // code that reports success 1 or failure 0
   double maxExcessDemand; // maximum equality value
   vector<double> tempPrices= getPrices( markets_isol, per ); // temporary prices
   vector<double> EDtemp= getExcessDemands( markets_isol, per ); // temporary excess demand
   const int i = worstMarket;
   
   logfile << ",,Bisection_i function called." << endl;
   // solve only one market
   do {
      if (fabs(sol[i].ED) > solutionTolerance) {
         if (sol[i].ED < 0) {
            sol[i].XL = sol[i].X;
            sol[i].EDL = sol[i].ED; 
         }
         else {
            sol[i].XR = sol[i].X;
            sol[i].EDR = sol[i].ED; 
         }
         sol[i].X = (sol[i].XL + sol[i].XR)/2;
         sol[i].dX = sol[i].XR - sol[i].XL;
      }			
      tempPrices[i]=sol[i].X; // copy prices to temporary vector
      // only current market should have different prices
      setPrices( tempPrices, markets_isol, per ); // set new prices
      nulldem(per);	// null demand
      nullsup(per); // null supply
      
      world->calc(per); // call world object to recalculate supply and demand
      
      logED(per); // calculate log of excess demand
      excessdemand(per); // calculate excess demand
      EDtemp = getExcessDemands( markets_isol, per ); // show excess demand
      for (int j=0;j< static_cast<int>( EDtemp.size() );j++) {
         sol[j].ED = EDtemp[j];
      }
      
      maxExcessDemand = fabs(sol[i].ED); // only solving one market
      
      // for debugging
      bool bug = true; // debugging on(1) or off(0)
      if (bug) {
         bugoutfile<<"\nMarket,X,XL,XR,ED,EDL,EDR,tolerance\n";
         bugoutfile<<i<<","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
            <<","<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<solutionTolerance<<"\n";
         bugout(per,numIterations);
         sdcurves(per,numIterations);
      }
      
   } // end do loop		
   while (++numIterations < 30 && maxExcessDemand >= solutionTolerance);			// report success, 1
   code = (maxExcessDemand < solutionTolerance ? 1 : 0);				// or failure, 0, 
   worldCalcCount+=numIterations-1;
   return code;
}

//! False Position Solution Mechanism (all markets)
int Marketplace::FalsePos_all( const double solutionTolerance,vector<solinfo>& sol,double& worldCalcCount, const int per ) {
   World* world = scenario->getWorld();
   int i;
   int numIterations = 0; // number of iterations
   int code = 2; // code that reports success 1 or failure 0
   double maxExcessDemand; // maximum equality value
   int numCurrMarkets = sol.size(); // number of markets to solve
   vector<double> tempPrices(numCurrMarkets); // temporary prices
   vector<double> EDtemp(numCurrMarkets); // temporary excess demand
   
   logfile << ",,FalsePos_all function called.\n";
   // solve all markets
   do {
      for (i=0; i<numCurrMarkets; ++i) {
         if (fabs(sol[i].ED) > solutionTolerance) {
            if (sol[i].ED < 0) {
               sol[i].XL = sol[i].X;
               sol[i].EDL = sol[i].ED; 
            }
            else {
               sol[i].XR = sol[i].X;
               sol[i].EDR = sol[i].ED; 
            }
            sol[i].dX = sol[i].XR - sol[i].XL;
            sol[i].X = sol[i].XL + (sol[i].dX*sol[i].EDL)/(sol[i].EDL-sol[i].EDR);
         }		
         tempPrices[i] = sol[i].X;
      }
      setPrices( tempPrices, markets_isol, per ); // set new prices
      nulldem(per);	// null demand
      nullsup(per); // null supply
      
      world->calc(per); // call world object to recalculate supply and demand
      
      logED(per); // calculate log of excess demand
      excessdemand(per); // calculate excess demand
      EDtemp =  getExcessDemands( markets_isol, per ); // show excess demand
      for (i=0;i< static_cast<int>( EDtemp.size() );i++)
         sol[i].ED = EDtemp[i];
      
      maxExcessDemand = *max_element(EDtemp.begin(), EDtemp.end());
      
      // for debugging
      int bug = 0; // debugging on(1) or off(0)
      if (bug) {
         bugoutfile<<"\nMarket,X,XL,XR,ED,EDL,EDR,tolerance\n";
         for (i=0; i<numCurrMarkets; ++i) {
            bugoutfile<<i<<","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
               <<","<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<solutionTolerance<<"\n";
         }
         bugout(per,numIterations);
         sdcurves(per,numIterations);
      }
      
   } // end do loop		
   while (++numIterations < 30 && maxExcessDemand >= solutionTolerance);			// report success, 1
   code = (maxExcessDemand < solutionTolerance ? 1 : 0);				// or failure, 0, 
   worldCalcCount+=numIterations-1;
   return code;
}

//! Secant Solution Mechanism (all markets)
int Marketplace::Secant_all( const double solutionTolerance,vector<solinfo>& sol, double& worldCalcCount, const int per ) {
   World* world = scenario->getWorld();
   int i;
   int iSec=0; 
   int numIterations = 0; // number of iterations
   int code = 2; // code that reports success 1 or failure 0
   double maxExcessDemand; // maximum equality value
   int numCurrMarkets = sol.size(); // number of markets to solve
   vector<double> tempPrices(numCurrMarkets); // temporary prices
   vector<double> EDtemp(numCurrMarkets); // temporary excess demand
   
   logfile << ",,Secant_all function called.\n";
   // solve all markets
   do {
      if (iSec == 0) { // initial starting point; done once
         for (i=0; i<numCurrMarkets; ++i) {
            if (fabs(sol[i].ED) < fabs(sol[i].EDR)) {
               sol[i].X = sol[i].XL;
               sol[i].XL = sol[i].XR;
               double swap = sol[i].EDL;
               sol[i].EDL = sol[i].EDR;
               sol[i].ED = swap; 
            }
            else 
               sol[i].X = sol[i].XR;
         }
      }
      for (i=0; i<numCurrMarkets; i++) {
         if (fabs(sol[i].ED) > solutionTolerance) {
            sol[i].dX = (sol[i].XL - sol[i].X)*sol[i].ED/(sol[i].ED-sol[i].EDL);
            sol[i].XL = sol[i].X;
            sol[i].EDL = sol[i].ED;
            if (fabs(sol[i].dX)<fabs(sol[i].X))
               sol[i].X += sol[i].dX;
         }
         tempPrices[i] = sol[i].X;
      }
      iSec++;
      
      setPrices( tempPrices, markets_isol, per ); // set new prices
      nulldem(per);	// null demand
      nullsup(per); // null supply
      
      world->calc(per); // call world object to recalculate supply and demand
      
      logED(per); // calculate log of excess demand
      excessdemand(per); // calculate excess demand
      EDtemp =  getExcessDemands( markets_isol, per ); // show excess demand
      for (i=0;i< static_cast<int>( EDtemp.size() );i++)
         sol[i].ED = EDtemp[i];
      
      maxExcessDemand = *( max_element(EDtemp.begin(), EDtemp.end() )); // Max returns largest sol[i].ED
      
      // for debugging
      int bug = 0; // debugging on(1) or off(0)
      if (bug) {
         bugoutfile<<"\nMarket,X,XL,XR,ED,EDL,EDR,tolerance\n";
         for (i=0; i<numCurrMarkets; ++i) {
            bugoutfile<<i<<","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
               <<","<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<solutionTolerance<<"\n";
         }
         bugout(per,numIterations);
         sdcurves(per,numIterations);
      }
      
   } // end do loop		
   while (++numIterations < 30 && maxExcessDemand >= solutionTolerance);			// report success, 1
   code = (maxExcessDemand < solutionTolerance ? 1 : 0);				// or failure, 0, 
   worldCalcCount += numIterations - 1;
   return code;
}

//! Function to calculate derivative
//void JFunction(valarray<double> prices, Matrix& JFDM, int per)
void Marketplace::JFunction( vector<double> prices, Matrix& JFDM, double& worldCalcCount, int const per ) {
   World* world = scenario->getWorld();
   const int marketsToSolve = prices.size();
   const double DELTAP = 1e-4; // What is the proper value for delta?
   vector<double> tprices = prices; // define and initialize prices for storage
   vector<double> tmpJFD( marketsToSolve );
   
   storeinfo( per ); // store original market info before perturbing price
   
   for ( int j = 0; j < marketsToSolve; j++ ) {	// j is column index
      prices[j] *= ( 1 + DELTAP );	  // add price times deltap
      //prices[j] += deltap;	  // add price times deltap
      setPrices( prices, markets_isol_NR, per );	// set new price for one market
      nulldem( per );	// null demand
      nullsup( per );	// null supply
      world->calc( per ); // call world object to recalculate supply and demand
      int col = j; // function calculates rows for each column
      tmpJFD = jacobian( markets_isol_NR, col, per ); // calculate elasticities or Jacobian
      
      for ( int i = 0; i < marketsToSolve; i++ ) { // copy column vector to Jacobian Matrix
         JFDM[i][j] = tmpJFD[i]; // i is row index
      }
      
      restoreprc( markets_isol_NR, per ); // restore perturbed market price
      prices[j] = tprices[j]; //  restore perturbed market price
      worldCalcCount++;
   }
}

/*! \brief Function to calculate partial derivatives for Newton-Rhaphson method, NR_Ron()
*
* This function calculates matrices of partial derivatives of supplies and demands for all markets which are currently being solved.
* The function uses the fact that changing a regional price while holding all other prices constant can only change markets within that region.
* It first creates matrices of supply and demand which contain the amount of supply and demand added to each market by each region
* using the unchanged prices.
* To do this, the function steps through World::calc(), calling it seperately for each region.
* Once these matrices are completed, when calculating a derivative, supplies and demands at the base price for the regions within the market
* for which derivatives are being calculated are subtracted from the saved global totals. Then the price of the market is perturbed,
* and World::calc() is only called on regions contained in the market, thus adding back in supplies and demands for regions within the market
* using the perturbed market price. Cross-derivatives are then calculated for the market and original prices, supplies and demands reset.
* Here is a summary of the steps. 
* <ol>
*  <li>Create matrices of additional supplies and additional demands added to each market by each region at the base prices.</li>
*  <li>Save the original supplies and demands.</li>
*  <li>Iterate over each market performing the following steps: 
*  <ol>
*        <li>Perturb the market price by multiplying by 1 + DELTA.</li>
*        <li>Iterate over all regions contained by the market and remove supplies and demands added to each market by each region, using the additive matrices.</li>
* .      <li>Call World::calc() on only the regions contained by the market.</li>
*        <li>Calculate cross-derivatives of demand and supply.</li>
*        <li>Reset original prices, supplies and demands.</li>
*        </ol></li>
* <li> Return the partial demand matrices of supply and demand. </li>
* </ol>
*
* \param prices The current market prices.
* \param JFDM A matrix of partial derivatives of demands. This matrix is modified by the function and returned by reference.
* \param JFSM A matrix of partial derivatives of supplies. This matrix is modified by the function and returned by reference.
* \param worldCalcCount The current number of iterations of World::calc. This value is modified by the function and returned by reference.
* \param per The current model period.
* \return void
* \sa NR_Ron
*/
void Marketplace::Derivatives( vector<double> prices, Matrix& JFDM, Matrix& JFSM, double& worldCalcCount, const int per ) {
   
   cout << endl << "Begin derivative calculation..." << endl;
   World* world = scenario->getWorld();
   const int marketsToSolve = prices.size();
   const double DELTAP = 1e-4; // Orginal, What is the proper value for delta?
   //const double DELTAP = 1e-5; // What is the proper value for delta?
   vector<double> tprices = prices; // define and initialize prices for storage
   vector<double> tmpJFD( marketsToSolve );
   vector<double> tmpJFS( marketsToSolve );
   
   // Create additive matrices.
   
   // Get the region names from the world.
   const vector<string> regionNames = world->getRegionVector();
   
   // Save original global supplies and demands for error checking.
   vector<double> originalSupplies = getSupplies( per );
   vector<double> originalDemands = getDemands( per );
   
   // Additive matrices, indexed by region name and market number.
   map< string, vector< double > > additionalSupplies;
   map< string, vector< double > > additionalDemands;
   
   // Supplies and demands saved from previous region during calculation of additive matrices. 
   vector<double> prevSupplies( numMarkets, 0.0 );
   vector<double> prevDemands( numMarkets, 0.0 );
   
   // clear demands and supplies.
   nulldem( per );
   nullsup( per );
   
   // iterate over regions and calculate additional supplies and demands for each region for the current prices. 
   for ( vector<string>::const_iterator regionIter = regionNames.begin(); regionIter != regionNames.end(); regionIter++ ) {
      vector<double> currSupplies;
      vector<double> currDemands;
      vector<double> diffInSupplies( numMarkets, 0.0 );
      vector<double> diffInDemands( numMarkets, 0.0 );
      
      // Call world->calc() for this region only. 
      world->calc( per, vector<string>( 1, *regionIter ) );
      worldCalcCount += ( 1.0 / static_cast<double> ( regionNames.size() ) );
      
      currSupplies = getSupplies( per );
      currDemands = getDemands( per );
      
      // calculate differences between previous supply and supply after calculating supply and demand for this region.
      for( int k = 0; k < numMarkets; k++ ) {
         diffInSupplies[ k ] = currSupplies[ k ] - prevSupplies[ k ];
         diffInDemands[ k ] = currDemands[ k ] - prevDemands[ k ];
      }
      
      // save the current supplies and demands.
      prevSupplies = currSupplies;
      prevDemands = currDemands;
      
      // Insert this regions additional supplies and demands into the additive matrices. 
      additionalSupplies[ *regionIter ] = diffInSupplies;
      additionalDemands[ *regionIter ] = diffInDemands;
   }
   
   storeinfo( per ); // store original market info before perturbing price
   
   // Perform optional error checking.
   // This code will sum up the additive value for each market over all regions.
   // These sums are then checked against the original global supplies and demands.
   if( Configuration::getInstance()->getBool( "debugChecking" ) ) {
      
      // Compute the sum of the regional supplies and demands.
      vector<double> suppliesSum( numMarkets, 0.0 );
      vector<double> demandsSum( numMarkets, 0.0 );
      
      for ( vector<string>::const_iterator checkIter = regionNames.begin(); checkIter != regionNames.end(); checkIter++ ) {
         for ( int currMarkIter = 0; currMarkIter < numMarkets; currMarkIter++ ) {
            suppliesSum[ currMarkIter ] += additionalSupplies[ *checkIter ][ currMarkIter ];
            demandsSum[ currMarkIter ] += additionalDemands[ *checkIter ][ currMarkIter ];
         }
      }
      
      // Check if the sum adds up to the total. 
      for( int marketCheckIter = 0; marketCheckIter < numMarkets; marketCheckIter++ ) {
         if( fabs( originalSupplies[ marketCheckIter ] - suppliesSum[ marketCheckIter ] ) > 1E-5 ){
            cout << "Error in derivative Calculation. Unequal sums. M: " << markets[ marketCheckIter ][ per ]->getName();
            cout << " S Difference: " << originalSupplies[ marketCheckIter ] - suppliesSum[ marketCheckIter ];
         }
         if ( fabs( originalDemands[ marketCheckIter ] - demandsSum[ marketCheckIter ] ) > 1E-5 ) {
            cout << "Error in derivative Calculation. Unequal sums. M: " << markets[ marketCheckIter ][ per ]->getName();
            cout << " D Difference: " << originalDemands[ marketCheckIter ] - demandsSum[ marketCheckIter ];
            cout << endl;
         }
      }
   }
   // Sums checking complete.
   // Done creating additive matrices.
   // Now calculate derivatives for each market.
   for ( int j = 0; j < marketsToSolve; j++ ) {	// j is column index
      
      const int fullMarketNumber = markets_isol_NR[ j ]; // the market number is the complete markets vector.
      
      // Price is near zero.
      if( prices[ j ] < DELTAP ) {
         prices[ j ] = DELTAP;
      }
      
      // Price is positive.
      else {
         prices[ j ] *= ( 1 + DELTAP ); // add price times deltap
      }
      
      setPrices( prices, markets_isol_NR, per );	// set new price for one market
      
      // Now remove additive supplies and demands.
      // Iterate over all regions within the market.
      const vector<string> containedRegions = markets[ fullMarketNumber ][ per ]->getContainedRegions();
      for ( vector<string>::const_iterator regionIter2 = containedRegions.begin(); regionIter2 != containedRegions.end(); regionIter2++ ) {
         // Now iterate over the markets.
         for ( int market = 0; market < numMarkets; market++ ) {
            
            // Remove supply.
            markets[ market ][ per ]->removeFromRawSupply( additionalSupplies[ *regionIter2 ][ market ] );
            // Remove demand
            markets[ market ][ per ]->removeFromRawDemand( additionalDemands[ *regionIter2 ][ market ] );
         }
      }
      
      world->calc( per, containedRegions );
      worldCalcCount += ( static_cast<double>( containedRegions.size() ) / static_cast<double> ( regionNames.size() ) );
      
      tmpJFD =  calcDemandElas( markets_isol_NR, j, per ); // calculate demand elasticities
      tmpJFS =  calcSupplyElas( markets_isol_NR, j, per ); // calculate supply elasticities
      
      for ( int i = 0; i < marketsToSolve; i++ ) {// copy column vector to Jacobian Matrix
         JFDM[ i ][ j ] = tmpJFD[ i ]; // i is row index
         JFSM[ i ][ j ] = tmpJFS[ i ]; // i is row index
      }
      
      restoreinfo( per ); // restore market supplies and demands.
      prices[ j ] = tprices[ j ]; //  restore perturbed market price
   }
}


//! Newton Raphson Solution Mechanism (all markets)
int Marketplace::NewtRap( const double solutionTolerance, const double excessDemandSolutionFloor, vector<solinfo>& sol, double& worldCalcCount, const int per ){
   
   World* world = scenario->getWorld();
   const Modeltime* modeltime = scenario->getModeltime();
   int i;
   int NRn = 0; // calls to calculate elasticities
   int numIterations = 0; // number of iterations
   int code = 2; // code that reports success 1 or failure 0
   double maxExcessDemand; // maximum equality value
   int worstExcessDemand;
   const int marketsToSolve =  setMarketsToSolveNR( per ); // number of markets to solve
   vector<double> DP( marketsToSolve ); // adjustment value
   vector<double> logEDVec =  getLogExcessDemands( markets_isol_NR, per ); // show log of excess demand
   vector<double> tempPrices = getPrices( markets_isol_NR, per ); // temporary prices
   vector<double> EDtemp( marketsToSolve ); // temporary excess demand
   Matrix JF( marketsToSolve, marketsToSolve );
   
   logfile << ",,Newton-Raphson function called." << endl;
   cout << "Newton-Raphson" << endl;
   // solve all markets
   do {
      if ( ( NRn < 5 ) && ( per < modeltime->getmaxper() ) ) { // do only once for each period
         JFunction( tempPrices, JF, worldCalcCount, per ); // recalculate Jacobian matrix, returns JF matrix
         invertMatrix( JF );
      }
      
      NRn++;
      for ( i = 0; i < marketsToSolve; i++ ) {
         
         // Calculate new price based on NR
         DP[i] = 0;
         for ( int j = 0; j < marketsToSolve; j++ )
            DP[i] -= JF[i][j] * logEDVec[j];
         if ( DP[i] < -1 ) {
            DP[i] = -0.9;
         }
         tempPrices[i] *= ( 1 + DP[i] ); // new price
      }
      
      setPrices( tempPrices, markets_isol_NR, per ); // set new prices
      nulldem( per );	// null demand
      nullsup( per ); // null supply
      
      world->calc( per ); // call world object to recalculate supply and demand
      logED( per ); // calculate log of excess demand
      excessdemand( per ); // calculate excess demand
      logEDVec =  getLogExcessDemands( markets_isol_NR, per ); // show log of excess demand
      EDtemp = getExcessDemands( markets_isol_NR, per ); // show excess demand
      maxExcessDemand = maxED( markets_isol_NR, excessDemandSolutionFloor, per ); // Max returns largest ED[i] // temp
      
      // for debugging
      const bool bug = false; // debugging
      if ( bug ) {
         worstExcessDemand = worstED(  markets_isol_NR, excessDemandSolutionFloor, per );
         cout << "NR-maxRelED: " << markets[ markets_isol_NR[ worstExcessDemand ] ][ per ]->getName() << ":" << maxExcessDemand << endl;
         
         bugoutfile << endl << "Market,X,DP,ED,tolerance" << endl;
         for ( i = 0; i < marketsToSolve; i++ ) {
            bugoutfile << i << "," << tempPrices[i] << "," << DP[i] << "," << EDtemp[i] << "," << solutionTolerance << endl;
         }
         
         bugout( per, numIterations );
         sdcurves( per, numIterations ); 
      }
      
      // if solution moves in wrong direction
      if( maxExcessDemand > 500 ) {
         logfile << ",,***Exit Newton-Raphson function maxExcessDemand > 500." << endl;
         return 0;
      }
   } while ( ++numIterations < 30 && maxExcessDemand >= solutionTolerance );			// report success, 1
   
   code = ( maxExcessDemand < solutionTolerance ? 1 : 0 );				// or failure, 0, 
   // resize and reasign all solution prices and ED's
   // need to copy prices and ED to sol
   tempPrices =  getPrices( markets_isol_NR, per );
   EDtemp = getExcessDemands( markets_isol_NR, per );
   
   for ( i = 0; i < static_cast<int>( tempPrices.size() ); i++ ) {
      sol[i].X = tempPrices[i];
      sol[i].ED = EDtemp[i];
   }
   worldCalcCount += numIterations - 1;
   return code;
}

//! Ron's version of the Newton Raphson Solution Mechanism (all markets)
/*! Derivatives are taken once. They are not taken again unless:
a) The Max Relative ED after calculation is greater than MAXED_FOR_DERIV_RECALC
b) or 10 NR iterations have occurred.
As long as Bisection is close, one set of derivatives per period is sufficient.
* Useful TrackED writeout to screen is turned on by toggle in configuration file.
* \author Sonny Kim, Josh Lurz, Steve Smith
* \warning Unless stated otherwise, ED values are normalized (i.e., that 10 == 10% difference).
* \param solutionTolerance Target value for maximum relative solution for worst market 
* \param excessDemandSolutionFloor *Absolute value* beneath which market is ignored 
* \param sol Vector of market solution information 
*** WHAT IS INDEXING OF THE ABOVE? 
* \param worldCalcCount Counter for number of worldcalc model calls 
* \param per Model period
*/
int Marketplace::NR_Ron( const double solutionTolerance, const double excessDemandSolutionFloor, vector<solinfo>& sol, double& worldCalcCount, const int per ) {
   
   World* world = scenario->getWorld();
   const Modeltime* modeltime = scenario->getModeltime();
   int i;
   int numDerivativeCalcs = 0; // count number of times derivatives are calculated
   int iter = 0; // number of iterations through solution algorithm
   int code = 2; // code that reports success 1 or failure 0
   const int MAX_DERIVATIVE_CALC = 1; // count number of times derivatives are normally calculated
   const double MAXED_FOR_DERIV_RECALC = 25; // recalculate derivatives is ED is larger than this 
   double maxSolVal; // maximum equality value 
   const int marketsToSolve =  setMarketsToSolveNR( per ); // number of markets to solve
   vector<double> NP( marketsToSolve ); // adjustment value
   vector<double> KD( marketsToSolve ); // k values demand
   vector<double> KS( marketsToSolve ); // k values supply
   vector<double> KDS( marketsToSolve ); // k values demand - supply
   vector<double> tempPrices = getPrices( markets_isol_NR, per ); // temporary prices
   vector<double> EDtemp; // temporary excess demand
   
   bool breakout = false;	// var to allow various conditions to exit NR routine
   double beforeEDvalue = -1;
   double previousEDvalue = -1;
   double BREAK_OUT_THRESHOLD = 0.001;
   
   if ( trackED ) { 
      cout << "NR_Ron begin "; 
   }
   
   Matrix JF( marketsToSolve, marketsToSolve );
   Matrix JFDM( marketsToSolve, marketsToSolve );
   Matrix JFSM( marketsToSolve, marketsToSolve );
   
   logfile << ",,Ron's version of the Newton-Raphson function called." << endl;
   
   // solve all markets
   do {
      if ( bugTracking ) {
         bugoutfile << endl << "Number of Markets:  "<< marketsToSolve << endl;
         bugoutfile << "Ron_NR " << iter;  
         prices_to_bugout( per );
      }
      
      logDem( per ); // calculate log of demand
      logSup( per ); // calculate log of supply
      
      // control no of times derivatives are calculated
      if ( ( numDerivativeCalcs < MAX_DERIVATIVE_CALC ) && ( per < modeltime->getmaxper() ) ) { 
         if ( trackED ) {
            cout <<" ... "; 
         }
         
         // Recalculate Jacobian matrix, returns JF matrix
         Derivatives( tempPrices, JFDM, JFSM, worldCalcCount, per ); 
         numDerivativeCalcs++; // increment count of derivative calculation
         
         for( i = 0; i < marketsToSolve; i++ ) {
            for( int j = 0; j < marketsToSolve; j++ ) {
               JF[ i ][ j ] = JFSM[ i ][ j ] - JFDM[ i ][ j ];
               assert( isValidNumber( JF[ i ][ j ] ) );
            }
         }
         
         invertMatrix( JF );
         logfile << ",,,Derivatives calculated" << endl;
         
         if ( trackED ) {
            cout <<" End Derivatives " <<endl;
         }
      }
      
      // initialize KD and KS as logs of original demand and supply
      KD =  getLogDemands( markets_isol_NR, per ); // return log of demand
      KS =  getLogSupplies( markets_isol_NR, per ); // return log of supply
      
      for ( i = 0; i < marketsToSolve; i++ ) {
         for ( int j = 0; j < marketsToSolve; j++ ) {
            double tempValue = log( max( tempPrices[ j ], SMALL_NUM ) );
            KD[ i ] -= tempValue * JFDM[ i ][ j ];
            KS[ i ] -= tempValue * JFSM[ i ][ j ];
            assert( isValidNumber( KD[ i ] ) );
            assert( isValidNumber( KS[ i ] ) );
         }
         
         KDS[ i ] = KD[ i ] - KS[ i ];
         assert( isValidNumber( KDS[ i ] ) );
      }
      
      // Calculate new log price based on NR
      for ( i = 0; i < marketsToSolve; i++ ) {
         
         NP[ i ] = 0;
         
         for ( int j = 0; j < marketsToSolve; j++ ) {
            assert( isValidNumber( JF[ i ][ j ] ) );
            NP[ i ] += JF[ i ][ j ] * KDS[ j ];
            assert( isValidNumber( NP[ i ] ) );
         }
         
         tempPrices[ i ] = exp( NP[ i ] ); // new price
         
         // Check the validity of the price.
         assert( isValidNumber( tempPrices[ i ] ) );
      }
      
      setPrices( tempPrices, markets_isol_NR, per ); // set new prices
      nulldem( per ); // null demand
      nullsup( per ); // null supply
      
      if ( bugTracking ) {
         bugoutfile << endl << "--before .calc "<< iter;  
         prices_to_bugout( per );
         supply_to_bugout( per );  
         demand_to_bugout( per );
      }
      
      world->calc( per ); // call world object to recalculate supply and demand
      
      if ( bugTracking ) {
         bugoutfile << endl << "--after .calc "<< iter;  
         prices_to_bugout( per );
         supply_to_bugout( per );  
         demand_to_bugout( per );
      }
      
      excessdemand( per ); // calculate excess demand
      EDtemp = getExcessDemands( markets_isol_NR, per ); // show excess demand
      
      maxSolVal = maxED( markets_isol_NR, excessDemandSolutionFloor, per ); // Max returns largest ED[i]
      
      // for debugging
      const bool bug = false; // debugging on or off
      
      if ( bug ) {
         
         bugoutfile << endl << "Market,X,DP,ED,tolerance" << endl;
         
         for ( i = 0; i < marketsToSolve; i++ ) {
            bugoutfile << i << "," << tempPrices[ i ] << "," << NP[ i ] << "," << EDtemp[ i ] << "," << solutionTolerance << endl;
         }
         
         bugout( per, iter );
         sdcurves( per, iter ); 
      }
      
      if ( trackED ) {
         int maxIntSol = worstED( markets_isol_NR, excessDemandSolutionFloor, per ); // index of solution market with maxED
         int maxInt = markets_isol_NR[ maxIntSol ]; // transform to market index
         cout << "NR-maxRelED: "<< maxSolVal <<" ("<< markets[ maxInt ][ per ]->getName() << ") -> ";
         cout << "S: "<<getRawSupply( maxInt , per ) << " , " << " D: "<< getRawDemand( maxInt , per );
         cout << ",  P: "<<getRawPrice( maxInt , per );
         cout << endl; 
      }
      
      if (iter > 3) {	
         // If the worst ED is not changing too much then breakout
         // double tempVal = abs(maxSolVal-previousEDvalue)/previousEDvalue;
         //  cout << " diff: "<<tempVal<<" ";
         if (abs(maxSolVal-previousEDvalue)/previousEDvalue < BREAK_OUT_THRESHOLD) {  // not converging very fast
            breakout = true;
            if (trackED) {
               cout << "slow convergance" << endl;
            }
         }
         /*  if (abs(maxSolVal-beforeEDvalue)/beforeEDvalue < 0.01 && !breakout) {  // oscillating
         breakout = false; 
         if (trackED) {
         cout << "oscillating" << endl;
         }
         logfile << ",,,Exited NR due to oscillation" << endl;
      } */
      }      
      
      beforeEDvalue = previousEDvalue;
      previousEDvalue = maxSolVal;
      
      // if solution moves in wrong direction
      if( maxSolVal > 1500) {
         logfile << ",,***Exit Newton-Raphson function maxSolVal > 1500. "<< endl;
         //   logfile << ", Due to market " << getRegionName(maxInt)<< "-"<< getGoodName(maxInt) <<"\n";
         if ( trackED && per > 0) {
            cout << "Exit Newton-Raphson function maxSolVal > 1500" << endl;
         }
         for ( i = 0; i < numMarkets; i++ ) {
            if ( fabs( markets[ i ][ per ]->getExcessDemand() ) > maxSolVal / 100 ) {
               if ( trackED ) {
                  cout << "ED: (" << getName( i ) << ") - " << markets[ i ][ per ]->getExcessDemand() << endl;
               }
               logfile << ",,,Due to market " << getName( i ) << " - ED: " << markets[ i ][ per ]->getExcessDemand() << endl;
            }
         }
         
         return 0; 
      }
      
      // If have iterated 10 times in NR without solving, then re-do derivatives
      if ( (iter > 3) && ((iter % 10) == 0) ) {
         numDerivativeCalcs = 0;
      }
      
      // IF ED is too high then re-calculate derivatives
      if ( maxSolVal > MAXED_FOR_DERIV_RECALC ) {
         numDerivativeCalcs = 0;
      }
      
   } // end do loop	
   
   while ( ++iter < 35 && maxSolVal >= solutionTolerance && !breakout );	
   code = ( maxSolVal < solutionTolerance ? 1 : 0 ); // report success 1 or failure 0, 
   
   // resize and reasign all solution prices and ED's
   // need to copy prices and ED to sol
   tempPrices =  getPrices( markets_isol_NR, per );
   EDtemp = getExcessDemands( markets_isol_NR, per );
   
   for ( i = 0; i < static_cast<int>( tempPrices.size() ); i++ ) {
      sol[ i ].X = tempPrices[ i ];
      sol[ i ].ED = EDtemp[ i ];
   }
   
   worldCalcCount += iter - 1;
   logfile << ",Number of Newton-Raphson iterations: n =" << iter << endl;
   
   return code;
}

//! Solution method for all markets for one period
void Marketplace::solve( const int per ) {
   World* world = scenario->getWorld();
   bool allbracketed = false;
   const bool useBisect = false;
   const bool useFP = false;
   const bool useSecant = false;
   const bool useNR = false;
   const bool useNR_Ron = true;
   bool firsttime = true;
   int i = 0; // some index
   double worldCalcCount = 0; // index for solution iteration
   int bn = 0; // counter for bisection routine
   int code = 2; // code that reports success 1 or failure 0
   int solved = 0; // code that reports success 1 or failure 0
   const double solTolerance = 0.001; // tolerance for solution criteria
   const double excessDemandSolutionFloor = 0.01; // minimum value below which solution is assumed to be found.
   // Extra high tolerance to get to solution faster
   //double solTolerance = 0.1; // tolerance for solution criteria
   double maxSolVal; // temporary maximum value of equality condition			 
   vector<double> X,ED,logEDVec; // price, excess demand and log of excess demand vectors
   bool calibrationStatus = world->getCalibrationSetting();
   
   const double BRACKET_INTERVAL = 0.5;
   
   
   Configuration* conf = Configuration::getInstance();
   trackED = conf->getBool( "trackMaxED" ); //!< Get parameter to turn on (or not) solution mechanism tracking (to cout)
   
   excessdemand( per ); // first calculate excess demand for all markets
   const int marketsToSolve =  setMarketsToSolve( per ); // set number of markets to solve
   
   logfile << ",Starting Solution. Solving for " << marketsToSolve << " markets." << endl;
   
   // if marketsToSolve = 0, no markets to solve, break out of solution.
   if ( marketsToSolve == 0 ) {
      cout << "Model solved with last period's prices"; 
      return;
   }
   
   logED( per ); // calculate log of excess demand
   X =  getPrices( markets_isol, per ); // showPRC returns a vector of prices	
   logEDVec = getLogExcessDemands( markets_isol, per ); // showlogED returns a vector of logED
   ED = getExcessDemands( markets_isol, per ); // showED returns a vector of ED
   
   vector<solinfo> sol( marketsToSolve ); // create vector of solution information
   // initialize solution information
   for (i = 0; i < marketsToSolve; i++ ) {
      sol[i].XL = sol[i].XR = sol[i].X = X[i];
      sol[i].EDL = sol[i].EDR = sol[i].ED = ED[i]; 
      sol[i].bracketed = 0;
   }
   
   // for debugging
   if ( bugMinimal ) {
      bugoutfile << endl << "Solution() Begin. Per " << per << endl;
      bugoutfile <<"Number of Markets: "<< marketsToSolve << endl;
      bugoutfile << endl << "Market,X,XL,XR,ED,EDL,EDR,Tolerance" << endl;
      
      for ( i = 0; i < marketsToSolve; i++ ) {
         bugoutfile << getRegionName( markets_isol[ i ] ) << getGoodName( markets_isol[ i ]  ) <<"," << sol[ i ].X << "," << sol[ i ].XL << "," << sol[ i ].XR
            << "," << sol[ i ].ED << "," << sol[ i ].EDL << "," << sol[ i ].EDR << "," << solTolerance << endl;
      }
      bugoutfile << endl;
      
      bugout( per, worldCalcCount );
      sdcurves( per, worldCalcCount );
   }
   
   // Loop is done at least once.
   do {
      if ( !allbracketed ) {
         Bracket( solTolerance, excessDemandSolutionFloor, BRACKET_INTERVAL, 
            sol,allbracketed,firsttime,worldCalcCount,per);
      }
      
      // Bisect method
      if (allbracketed && useBisect) {
         if (bn < 1) {
            const int maxIter = 30;
            solved = Bisection_all( solTolerance, excessDemandSolutionFloor, maxIter,sol,worldCalcCount,per);
            
            if (!solved) {
               for (i=0;i<marketsToSolve;i++)
                  CheckBracket(solTolerance, excessDemandSolutionFloor, sol,allbracketed);
            }
            ++bn;
         }
         else {
            i = worstED( markets_isol, excessDemandSolutionFloor, per );
            solved = Bisection_i(i,solTolerance,sol,worldCalcCount,per);
            if (solved) {
               bn = 2;
            }
            else {
               for (i=0;i<marketsToSolve;i++) {
                  CheckBracket( solTolerance, excessDemandSolutionFloor, sol,allbracketed );
               }
            }
         }
      }
      // False position method
      if (allbracketed && useFP) {
         if (bn < 1) {
            solved = FalsePos_all(solTolerance,sol,worldCalcCount,per);
            if (!solved) {
               for (i=0;i<marketsToSolve;i++) {
                  CheckBracket( solTolerance, excessDemandSolutionFloor, sol,allbracketed);
               }
            }
            ++bn;
         }
         else {
            i =  worstED( markets_isol, excessDemandSolutionFloor, per );
            solved = Bisection_i(i,solTolerance,sol,worldCalcCount,per);
            if (solved) {
               bn = 0;
            }
            else {
               CheckBracket(solTolerance, excessDemandSolutionFloor, sol,allbracketed);
            }
         }
      }
      
      // Secant method
      if (allbracketed && useSecant) {
         if (bn < 1) {
            solved = Secant_all(solTolerance,sol,worldCalcCount,per);
            if (!solved) {
               for (i=0;i<marketsToSolve;i++) {
                  CheckBracket(solTolerance, excessDemandSolutionFloor, sol,allbracketed);
               }
            }
            ++bn;
         }
         else {
            i =  worstED( markets_isol_NR, excessDemandSolutionFloor, per );
            solved = Bisection_i(i,solTolerance,sol,worldCalcCount,per);
            if (solved) {
               bn = 0;
            }
            else {
               CheckBracket(solTolerance, excessDemandSolutionFloor, sol,allbracketed);
            }
         }
      }
      
      // Use Newton-Raphson only if all markets are bracketed
      // and production is not zero.
      if (allbracketed && useNR) {
         const int maxIter = 30;
         //int maxIter = 15;
         solved = Bisection_all(solTolerance, excessDemandSolutionFloor, maxIter,sol,worldCalcCount,per);
         logfile <<",Number of iterations: worldCalcCount = "<<worldCalcCount<<"\n";
         maxSolVal =  maxED( markets_isol_NR, excessDemandSolutionFloor, per ); // Max returns largest ED[i]
         // Bisection returns ED, not log of ED
         if(!solved && maxSolVal< 1500 ) {
            solved = NewtRap( solTolerance, excessDemandSolutionFloor, sol, worldCalcCount, per );
         }
         if (!solved) { 
            CheckBracket( solTolerance, excessDemandSolutionFloor, sol, allbracketed );
         }
      }
      
      // Ron's version of the NR routine
      if ( allbracketed && useNR_Ron ) {
         const int maxIter = 30;
         solved = Bisection_all( solTolerance, excessDemandSolutionFloor, maxIter, sol, worldCalcCount, per );
         
         logfile << ",Number of iterations: worldCalcCount = " << worldCalcCount << endl;
         maxSolVal = maxED( markets_isol, excessDemandSolutionFloor, per ); // Max returns largest ED[i]
         
         if( !solved && maxSolVal < 1500 ) {
            
            // turn end-use calibrations off for NR
            world->turnCalibrationsOff();
            solved = NR_Ron( solTolerance, excessDemandSolutionFloor, sol, worldCalcCount, per );
            if ( calibrationStatus ) { // turn end-use calibrations back on if were on originally
               world->turnCalibrationsOn();
            }  
            
            if ( bugMinimal ) { 
               bugoutfile << "After Ron_NR "<< worldCalcCount;
            }
            if ( bugTracking ) {
               prices_to_bugout( per );
            }
         }
         
         if ( !solved ) {
            CheckBracket( solTolerance, excessDemandSolutionFloor, sol, allbracketed );
            
            if ( bugMinimal || bugTracking ) {
               bugoutfile << "After NR CheckB "<< worldCalcCount;  
               prices_to_bugout( per );
            }
            
         }
      }
      
      //sdfile<<"Iteration: "<<worldCalcCount<<"\worldCalcCount"; //supply & demand info
      
      // make sure that ED, NOT Log of ED, is checked against tolerance
      ED =  getExcessDemands( markets_isol, per );
      maxSolVal =  maxED( markets_isol, excessDemandSolutionFloor, per ); // Max returns largest ED[i]
      
      // for debugging
      if ( bugTracking ) {
         bugoutfile << endl << "Solution() loop. N: " << worldCalcCount << endl;
         bugoutfile << endl << "Market,X,XL,XR,ED,EDL,EDR,Tolerance" << endl;
         
         for (i = 0; i < marketsToSolve; ++i ) {
            bugoutfile << getRegionName( markets_isol[ i ] ) << getGoodName( markets_isol[ i ] ) << "," << sol[ i ].X << "," << sol[ i ].XL << "," << sol[ i ].XR 
               << ","<< sol[ i ].ED << "," << sol[ i ].EDL <<","<<sol[ i ].EDR << "," << solTolerance << endl;
         } 
         
         bugout( per, worldCalcCount );
         sdcurves( per, worldCalcCount );
      }
      
   } // end do loop		
   while ( maxSolVal >= solTolerance && ++worldCalcCount < 1000 );			// report success, 0
   code = ( maxSolVal < solTolerance ? 0 : -1 );				// or failure, -1, 
   
   if ( !checkMarketSolution( solTolerance, excessDemandSolutionFloor, per ) && ( code == 0 ) ) {
      cerr << "ERROR: Supplies and Demands are NOT equal" << endl;
   }
   
   totIter += worldCalcCount;
   
   switch (code) {
   case 0:
      cout << "Model solved normally: worldCalcCount = " << int( worldCalcCount ) << "; Cumulative = "<< int( totIter ) << endl;
      logfile << ",Model solved normally: worldCalcCount = " << int( worldCalcCount ) << "; Cumulative = "<< int( totIter ) << endl;
      break;
   case -1:
      cout << "Model did not solve within set iteration	" << int( worldCalcCount )<< endl;
      logfile << ",***Model did not solve within set iteration " << int( worldCalcCount ) << endl;
      
      logfile <<",Market,X,XL,XR,ED,EDL,EDR,Tolerance" << endl;
      for ( i = 0; i < marketsToSolve; i++ ) {
         logfile << "," << getRegionName( markets_isol[ i ] ) << getGoodName( markets_isol[ i ] ) << ","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
            <<","<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<solTolerance<< endl;
      }
      break;
   case 2:
      cout << "Original code has not been changed" << endl;
      logfile << ",Original code has not been changed" << endl;
      break;
   default:
      cout << "Case for code not found" << endl;
      logfile << ",Case for code not found" << endl;
      break;
   }
}

//! Calculate the inverse of a matrix using an LU factorization.
void Marketplace::invertMatrix( Matrix& A ) {
   
   // create LU decomposition
   Matrix LU( A.nrows(), A.ncols() );
   
   dense1D<int> pvector( A.nrows() );
   
   copy(A, LU);
   lu_factor( LU, pvector );
   
   // solve
   lu_inverse( LU, pvector, A );
}