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

#include "scenario.h"
#include "Market.h"
#include "Marketplace.h"
#include "modeltime.h"
#include "XMLHelper.h"
#include "Configuration.h"
#include "World.h"

using namespace std;
using namespace mtl;

extern ofstream bugoutfile, sdcurvefile, logfile;
extern Scenario* scenario;

//! Default constructor.
Marketplace::Marketplace() {
   uniqueNo = 0;
   nomrks = 0;
   nomrks_t = 0;
   nomrks_t_NR = 0;
   nodrscmrks = 0;
   nossecmrks = 0;
   noDemandMarkets = 0;
   noghgmrks = 0;
   priceMult = 1;
   SMALL_NUM = 1e-6;
   VERY_SMALL_NUM = 1e-8;
   bugTracking = false;
   bugMinimal = false;
   trackED =false;
   TotIter = 0;
}

//! Write out XML for debugging purposes.
void Marketplace::toDebugXML( const int period, ostream& out ) const {
   
   // write the beginning tag.
   Tabs::writeTabs( out );
   out << "<Marketplace>" << endl;
   
   // increase the indent.
   Tabs::increaseIndent();
   
   // write the xml for the class members.
   XMLWriteElement( nomrks, "numberOfMarkets", out );
   XMLWriteElement( nomrks_t, "numberOfMarketsRequireSolving", out );
   XMLWriteElement( nomrks_t_NR, "numberOfMarketsForNR", out );
   
   // First write out the individual markets
   for( int i = 0; i < static_cast<int>( mrk.size() ); i++ ){
      mrk[ i ][ period ].toDebugXML( period, out );
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
The region_marketMap map, uses goodName + regionName to find that market.
*/
bool Marketplace::setMarket( const string& regionName, const string& mrkname, const string& goodName, const Market::marketType typeIn ) {
   const Modeltime* modeltime = scenario->getModeltime();
   int mrkNo;
   bool retValue;
   
   // create unique markets from distinct good and market region names
   string key = goodName + mrkname;
   
   if ( marketMap.find( key ) != marketMap.end() ) { // market exists, no unique number
      retValue = false;
      mrkNo = marketMap[ key ];
      
      // add the additional region to the contained region names.
      for( int i = 0; i < static_cast<int> ( mrk[ mrkNo ].size() ); i++ ) {
            mrk[ mrkNo ][ i ].addRegion( regionName );
         }
   } 
   else { // market does not exist, give unique number
      retValue = true;
      marketMap[ key ] = uniqueNo;
      
      // Need to set market parameters before vector is created.
      Market tempMarket( goodName, mrkname, typeIn );
      tempMarket.addRegion( regionName );
      
      // create a vector of market objects, one for each period
      vector<Market> tempVector( modeltime->getmaxper(), tempMarket );
      
      // set the years for the markets.
      for( int i = 0; i < static_cast<int>( tempVector.size() ); i++ ){
         tempVector[ i ].setPeriod( i );
      }
      
      mrk.push_back( tempVector ); // create individual markets
      //mrk is 2 dimentional
      mrkNo = uniqueNo;
      uniqueNo++;
      
      // increment market sizes
      nomrks++;
   }
   // market lookup from good and region names
   // mrkNo may not be unique
   region_marketMap[ goodName + regionName ] = mrkNo;
   return retValue;
}

//! Returns the market number of a market given a goodName and regionName.
int Marketplace::getMarketNumber( const string& goodName, const string& regionName ) const {
   map <string, int> :: const_iterator findIter = region_marketMap.find( goodName + regionName );
   
   if( findIter == region_marketMap.end() ) {
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
   
   const int marketNumber = getMarketNumber( goodName, regionName );
   bool mktset = true;
   string marketName;
   string demandGoodName;
   const bool setNewMarkets = conf->getBool( "simulActive" ); // for debugging, set this to false to turn this off
   
   const int first_period = 1;   // first period for price markets. Needs to be un-hardcoded.
   // skip first period since this is not really solved. If it's not solved skipping is not saving much cost. 
   
   if( marketNumber == -1 ) {
      cerr << "ERROR: Market "<< goodName << " does not exist"  << endl;
   }
   
   else if ( setNewMarkets && getType( marketNumber, first_period ) != Market::PRICE ) {
      // Setup the cooresponding demand markets
      marketName = getRegionName( goodName, regionName );
      demandGoodName = goodName + "Demand_int";
      mktset = setMarket( regionName, marketName, demandGoodName, Market::DEMAND );
      
      // loop through time periods            
      for( int per = first_period; per < static_cast<int>( mrk[ marketNumber ].size() ); per++ ){                
         mrk[ marketNumber ][ per ].type = Market::PRICE; 
         setMarketToSolve( goodName, regionName, per );
         setMarketToSolve( demandGoodName, regionName, per );
         // this assumes that all markets have the same number of periods
      }
   }
}

//! Set the prices by period of a market from a vector.
void Marketplace::setPriceVector( const string& goodName, const string& regionName, const vector<double>& prices ){
   
   // determine what market the region and good are in.
   const int marketNumber = region_marketMap[ goodName + regionName ];
   
   for( int i = 0; i < static_cast<int>( mrk[ marketNumber ].size() ) && i < static_cast<int>( prices.size() ); i++ ){
      mrk[ marketNumber ][ i ].setActualPrice( prices[ i ] );
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
   for ( int i = 0; i < static_cast<int>( mrk.size() ); i++ ){               
      for( int j = 0; j < static_cast<int>( mrk[ i ].size() ); j++ ){
         mrk[ i ][ j ].initPrice();
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
         for( int per = 0; per < static_cast<int>( mrk[ marketNumber ].size() ); per++ ){                 
            mrk[ marketNumber ][ per ].setSolveMarket( true );
         }
      }
      // Otherwise set the individual market to solve.
      else {
         mrk[ marketNumber ][ per ].setSolveMarket( true );
      }
   }
}

//! initialize all market prices for the given period to 0.
void Marketplace::nullprc( const int per ) {
   for ( int i = 0; i < nomrks; i++ )
      mrk[ i ][ per ].nullPrice();
}

//! initialize all market demands for the given period to 0.
void Marketplace::nulldem( const int per ) {
   for ( int i = 0; i < nomrks; i++ ) {
      mrk[ i ][ per ].nullDemand();
   }
}

//! initialize all market supplies to 0
void Marketplace::nullsup( const int per ) {
   for ( int i = 0; i < nomrks; i++ ) {
      mrk[ i ][ per ].nullSupply();
   }
}

//! Set one market demand to 0.
/*! Not used at present */
void Marketplace::nulldem( const string& goodName, const string& regionName, const int per ){
   
   const int marketNumber = getMarketNumber( goodName, regionName );
   
   if ( marketNumber != -1 ) {
      mrk[ marketNumber ][ per ].nullDemand();
   }
}

//! Set one market supply to 0
/*! Not used at present */
void Marketplace::nullsup( const string& goodName, const string& regionName, const int per ){
   
   const int marketNumber = getMarketNumber( goodName, regionName );
   
   if ( marketNumber != -1 ) {
      mrk[ marketNumber ][ per ].nullSupply();
   }
}

//! Print markets to standard out.
void Marketplace::showmrks( const int per ) const {

   for ( int i = 0; i < nomrks; i++ ) {
      mrk[ i ][ per ].print( cout );
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
      mrk[ marketNumber ][ per ].setPrice( value );
   }
}

//! Add to the supply for this market
void Marketplace::setsupply( const string& goodName, const string& regionName, const double value, const int per ){
   const int marketNumber = getMarketNumber( goodName, regionName );
   string marketName;
   string demandGoodName;
   Market::marketType thisMarketType;
   
   if ( marketNumber != -1 ) {
      thisMarketType = getType( marketNumber, per );
      // If is price market, then set supply instead for the corresponding demand market
      if ( thisMarketType == Market::PRICE ) {
         marketName = getRegionName( goodName, regionName );
         demandGoodName = goodName + "Demand_int";
         setsupply( demandGoodName, regionName, value, per );
      }
      else if ( thisMarketType == Market::DEMAND ) {
         // if its a demand market then this is where the supply was set
         mrk[ marketNumber ][ per ].supply = mrk[ marketNumber ][ per ].price; 
         mrk[ marketNumber ][ per ].demMktSupply += value; // Store actual supply value to check later
      }
      else {
         // Otherwise, set supply as normal
         mrk[ marketNumber ][ per ].supply += value;
      }
   }
}

//! set market demand to used for solution mechanism
void Marketplace::setdemand( const string& goodName, const string& regionName, const double value, const int per ){
   const int marketNumber = getMarketNumber( goodName, regionName );
   string marketName;
   string demandGoodName;
   
   // The following condition used to be here, don't think this is needed any more
   //	if( goodName != "renewable" ){ // just a guess
   
   if (value < 0) {
      cerr << "ERROR: Demand value < 0 for market " << goodName << " in region " << regionName << endl;
   }
   
   if ( marketNumber != -1 ) {
      // If is price market, then set demand instead for the corresponding demand market
      if ( getType( marketNumber , per ) == Market::PRICE) {
         marketName = getRegionName( goodName, regionName );
         demandGoodName = goodName + "Demand_int";
         setdemand ( demandGoodName, regionName, value, per );
      }
      else {
         // Otherwise, set demand as normal
         mrk[ marketNumber ][ per ].demand += value;
      }
   }
}

//! return market price
double Marketplace::showprice( const string& goodName, const string& regionName, const int per ) const {
   
   const int marketNumber = getMarketNumber( goodName, regionName );
   
   if( marketNumber == -1 ) {
      return 0;
   }
   else {
      return mrk[ marketNumber ][ per ].getPrice();
   }
   
}

//! return market supply used for solution mechanism
double Marketplace::showsupply( const string& goodName, const string& regionName, const int per ) const 
{
   const int marketNumber = getMarketNumber( goodName, regionName );
   string marketName;
   string demandGoodName;
   
   if ( marketNumber != -1 ) {
      // If is price market, then get supply instead for the corresponding demand market
      if ( getType( marketNumber , per ) == Market::PRICE) {
         marketName = getRegionName( goodName, regionName );
         demandGoodName = goodName + "Demand_int";
         return showsupply ( demandGoodName, regionName, per );
      }
      else {
         // Otherwise, get supply as normal
         return mrk[ marketNumber ][ per ].supply;
      }
   }
   else {
      cerr << "ERROR: Called for supply of non-existant market "<< goodName << " in " << regionName << endl;
      return 0;
   }
}

//! return supply for use in checking solution, including actual supply for demand market
/*! Not used now, could be used for debugging at some point*/
double Marketplace::checkSupply( const string& goodName, const string& regionName, const int per ) const {
   
   const int marketNumber = getMarketNumber( goodName, regionName );
   
   if ( marketNumber != -1 ) {
         return mrk[ marketNumber ][ per ].getActualSupply();
   }
   else {
      cerr << "ERROR: Called for supply of non-existant market "<< goodName << " in " << regionName << endl;
      return 0;
   }
}

//! return supply for use in checking solution, including actual supply for demand market
/*! Used to double-check solution. Should not use otherwise. */
double Marketplace::checkSupply( const int marketNumber, const int per ) const {
   
   return mrk[ marketNumber ][ per ].getActualSupply();
}

//! return market demand used for solution mechanism
/*! If this is a price market, then get the demand from the corresponding demand market.
If this is a demand market (through the redirect), then get the demand from the trail value (.price)
-- although this is never actually occurs at present. 
*/
double Marketplace::showdemand(  const string& goodName, const string& regionName, const int per ) const {
   const int marketNumber = getMarketNumber( goodName, regionName );
   string marketName;
   string demandGoodName;
   
   if ( marketNumber != -1 ) {
      Market::marketType MType = getType( marketNumber , per );
      // If is price market, then get demand instead for the corresponding demand market
      if ( MType == Market::PRICE) {
         marketName = getRegionName( goodName, regionName );
         demandGoodName = goodName + "Demand_int";
         return showdemand ( demandGoodName, regionName, per );
      }
      else if (MType == Market::DEMAND) {
         // Otherwise, get demand from the trial value
         return mrk[ marketNumber ][ per ].price;
      }
      else {
         // Otherwise, get demand as normal
         return mrk[ marketNumber ][ per ].demand;
      }
   }
   else {
      cerr << "ERROR: Called for demand of non-existant market "<< goodName << " in " << regionName << endl;
      return 0;
   }
}

//! Return the name of the market.
string Marketplace::getName( const int marketNumber ) const {
   return mrk[ marketNumber ][ 0 ].getName();
}

//! return region group of market.
string Marketplace::getRegionName( const string& goodName, const string& regionName ) const {
   
   const int marketNumber = getMarketNumber( goodName, regionName );
   
   if ( marketNumber != -1 ) {
      return mrk[ marketNumber ][ 0 ].getRegionName();
   }
   else {
      return "";
   }
}

//! return region group of market.
string Marketplace::getRegionName( const int marketNumber ) const {
   return mrk[ marketNumber ][ 0 ].getRegionName();
}

//! return good name of market.
string Marketplace::getGoodName( const int marketNumber ) const {
   return mrk[ marketNumber ][ 0 ].getGoodName();;
}

//! Return market type (NORMAL, PRICE, DEMAND, etc.)
Market::marketType Marketplace::getType( const string goodName, const string regionName, const int per ) const {
   
   const int marketNumber = getMarketNumber( goodName, regionName );
   
   if( marketNumber == -1 ) {
      cerr << "ERROR: Market "<< goodName << " does not exist"  << endl;
      return Market::NORMAL;
   }
   else {
      return mrk[ marketNumber ][ per ].getType();
   }
}

//! Return market type
/*! \warning Use for debugging only */
Market::marketType Marketplace::getType( const int marketNumber, const int per ) const {
   return mrk[ marketNumber ][ per ].getType();
}

//! Return market supply used for solution mechanism
/*! \warning Use for debugging only */
double Marketplace::getRawSupply( const int marketNumber, const int per ) const {
   return mrk[ marketNumber ][ per ].getSupply();
}

//! Return market demand used for solution mechanism
/*! \warning Use for debugging only */
double Marketplace::getRawDemand( const int marketNumber, const int per ) const {
   return mrk[ marketNumber ][ per ].getDemand();
}

//! Return market price used for solution mechanism.
/*! \warning Use for debugging only */
double Marketplace::getRawPrice( const int marketNumber, const int per ) const {
   return mrk[ marketNumber ][ per ].getActualPrice();
}

//! Calculates excess demand for all markets
void Marketplace::excessdemand( const int per ) {
   for ( int i = 0; i < nomrks; i++ ) {
      mrk[ i ][ per ].calcExcessDemand(); 
   }
}

//! Calculates log of excess demand for all markets
void Marketplace::logED( const int per ) {
   for ( int i = 0; i < nomrks; i++ ) {
      mrk[ i ][ per ].calcLogExcessDemand( SMALL_NUM );
   }
}

//! Calculates log of demand for all markets
void Marketplace::logDem( const int per ) {
   for ( int i = 0; i < nomrks; i++ ) {
      mrk[ i ][ per ].calcLogDemand( SMALL_NUM ); 
   }
}

//! Calculates log of supply for all markets
void Marketplace::logSup( const int per ) {
   for ( int i = 0; i < nomrks; i++ ) {
      mrk[ i ][ per ].calcLogSupply( SMALL_NUM );
   }
}

//! Check to see that all markets actually solved.
/*! \todo This code should be in a Solution class, so it may not have access to demand.
*/
bool Marketplace::checkMarketSolution( const double solTolerance, const double excessDemandSolutionFloor, const int period ) const {
   bool solvedOK = true;
   
   for( int i = 0; i < static_cast<int>( mrk.size() ); i++ ) {
      if ( !isWithinTolerance( mrk[ i ][ period ].demand - mrk[ i ][ period ].getActualSupply(), mrk[ i ][ period ].demand, solTolerance, excessDemandSolutionFloor ) ) {
         solvedOK = false;
         cout << "Market "<< i << " ("<< getName( i )<< ") S: "<< mrk[ i ][ period ].getActualSupply() << " D: " << mrk[ i ][ period ].getDemand() << endl;
      }
   }
   
   return solvedOK;
}

//! Select markets to solve.
int Marketplace::setMarketsToSolve( const int period ) {
   bool solvemkt = false;
   bool smalltest = false; // put all markets in array so that is large enough for all
   nomrks_t = 0;
   mrk_isol.clear();
   
   for( int i = 0; i < static_cast<int>( mrk.size() ); i++ ) {
      // Check if this market is supposed to be solved & if a significant demand exists
      solvemkt = mrk[ i ][ period ].solveMarket;
      
      // if (smalltest) solvemkt =solvemkt && mrk[i][period].demand > SMALL_NUM;
      
      // But don't solve if its a GHG market and there is no constraint
      if ( solvemkt && mrk[ i ][ period ].type == Market::GHG && mrk[ i ][ period ].supply < 0 ) {
         solvemkt = false; 
      }
      
      // Add markets to the "normal" solution list
      if ( solvemkt ) {
         nomrks_t++;
         mrk_isol.push_back( i );
      }
   }
   
   return nomrks_t;
}

//! set markets to solve
int Marketplace::setMarketsToSolveNR( const int period ) {
   bool solvemkt = false;
   nomrks_t_NR = 0;
   mrk_isol_NR.clear();
   
   for( int i = 0; i < static_cast<int>( mrk.size() ); i++ ) {
      solvemkt = false;
      // Check if this market is supposed to be solved & if a significant demand exists
      if ( mrk[ i ][ period ].solveMarket 
         && mrk[ i ][ period ].demand > SMALL_NUM ) {
         solvemkt = true;
      }
      // But if its a GHG market .... 
      if ( solvemkt && mrk[ i ][ period ].type == Market::GHG ){
         //  don't solve if there is no constraint 
         if ( (mrk[ i ][ period ].supply < 0) ||  	
            // or don't solve if exdmd < 0 & price is really small 
            // if exdmd<0, then constraint>emissions
            (mrk[ i ][ period ].price < SMALL_NUM && mrk[ i ][ period ].exdmd < 0) || 
            // or don't solve if price is zero
            ( mrk[ i ][ period ].price == 0 ) ) { 	
            solvemkt = false; 
         }
      } // end GHG block
      
      // Add markets to NR solution list
      if ( solvemkt ) {
         nomrks_t_NR++;
         mrk_isol_NR.push_back( i );
      }
   } // end for
   
   return nomrks_t_NR;
}

//! return market with largest excess demand. 
int Marketplace::worstED( const vector<int>& indices, const double excessDemandSolutionFloor, const int per ) const {
   int worstID = 0;
   double largest = 0;
   
   for ( int i = 0; i < static_cast<int>( indices.size() ); i++ ) {
      const int j = indices[ i ];
      const double relativeED = getRelativeED( mrk[ j ][ per ].exdmd, mrk[ j ][ per ].demand, excessDemandSolutionFloor );
      
      if ( ( fabs( mrk[ j ][ per ].price ) > SMALL_NUM ) && ( relativeED > largest ) ) {
         worstID = i;
         largest = relativeED;
      }
   }
   return worstID;
}

//! returns largest excess demand
double Marketplace::maxED( const vector<int>& indices, const double excessDemandSolutionFloor, const int per ) const {
   const int worstMarket = worstED( indices, excessDemandSolutionFloor, per );
   return getRelativeED( mrk[ indices[ worstMarket ] ][ per ].exdmd, mrk[ indices[ worstMarket ] ][ per ].demand, excessDemandSolutionFloor );
}

//! set new solution prices for all markets
void Marketplace::setPrices( const vector<double>& prices, const vector<int>& indices, const int per ) {
   // set prices only for markets in price vector
   for ( int i = 0; i < indices.size(); i++ ) {
      const int j = indices[ i ];	// look up index
      mrk[ j ][ per ].setActualPrice( prices[ i ] );
   }
}

//! Initialize the marketplace prices 
/*! Use last period demand price as starting point for next period */
void Marketplace::init_to_last( const int per ) { 
   // only after the starting period
   if ( per > 0 ) {
      for ( int i = 0; i < nomrks; i++ ) {
         mrk[ i ][ per ].setPriceToLast( mrk[ i ][ per - 1 ].price );
      }
   }
}

//! For debugging, print out all prices
/*! \warning This assumes prices_to_bugout will be called first.
*   \todo Indices and bugout should be passed as arguments.
*/

void Marketplace::prices_to_bugout( const int per ) const {
   bugoutfile << "Prices,";

   for ( int i = 0; i < nomrks_t; i++ ){ 
      bugoutfile << mrk[ mrk_isol[ i ] ][ per ].getName() << ",";
   }
   
   bugoutfile << endl;
   bugoutfile << ",";
   for ( int j = 0; j < nomrks_t; j++ ) {
      bugoutfile << mrk[ mrk_isol[ j ] ][ per ].price << ",";
   }
   bugoutfile << endl;
}

//! For debugging, print out all supplies
void Marketplace::supply_to_bugout( const int per ) const {
   bugoutfile << "Supplies:,";
   
   for ( int i = 0; i < nomrks_t; i++ ) {
      bugoutfile << mrk[ mrk_isol[ i ] ][ per ].supply << ",";
   }
   bugoutfile << endl;
}

//! For debugging, print out all demands
void Marketplace::demand_to_bugout( const int per ) const {
   bugoutfile << "Demands:,";
   
   for ( int i = 0; i < nomrks_t; i++ ) {
      bugoutfile << mrk[ mrk_isol[ i ] ][ per ].demand << ",";
   }
   bugoutfile << endl;
}

//! This sets the ts of the current period to last periods values.
void Marketplace::storeto_last( const int per ) {
   // only after the starting period
   if ( per > 0 ) {
      for ( int i = 0; i < nomrks; i++ ) {
         mrk[ i ][ per ].storeInfoFromLast( mrk[ i ][ per - 1 ].demand, mrk[ i ][ per - 1 ].supply, mrk[ i ][ per - 1 ].price );
      }
   }
}

//! Store original demand, supply and price
/*! Used for calculation of derivative */
void Marketplace::storeinfo( const int per ) {
   for ( int i = 0; i  < nomrks; i++ ) {
      mrk[ i ][ per ].storeInfo();
   }
}

//! Restore original demand, supply and price
/*! Used for calculation of derivative */
void Marketplace::restoreinfo( const int per ) {
   for ( int i = 0; i < nomrks; i++ ) {
      mrk[ i ][ per ].restoreInfo();
   }
}

//! Restore original demand, supply and price.
/*! Used for calculation of derivative */
void Marketplace::restoreprc( const vector<int>& indices, const int per ) {
   // store only for markets that need solving
   for ( int i = 0; i < static_cast<int>( indices.size() ); i++ ) {
      const int j = indices[ i ];	// look up index
      mrk[ j ][ per ].restorePrice();
   }
}

//! returns vector of market prices
const vector<double> Marketplace::getPrices( const vector<int>& indices, const int per ) const {
   
   vector<double> prices( indices.size() );
   
   for ( int i = 0; i < static_cast<int>( indices.size() ); i++ ) {
      const int j = indices[ i ];	// look up index
      prices[ i ] = mrk[ j ][ per ].price;
   }
   
   return prices;
}

//! returns vector of market excess demands
const vector<double> Marketplace::getExcessDemands( const vector<int>& indices, const int per ) const {
   
   vector<double> ED( indices.size() );
   
   for ( int i = 0; i < static_cast<int>( indices.size() ); i++ ) {
      const int j = indices[ i ];	// look up index
      ED[ i ] = mrk[ j ][ per ].exdmd;
   }	
   
   return ED;
}

//! returns vector of log of market excess demands
const vector<double> Marketplace::getLogExcessDemands( const vector<int>& indices, const int per ) const {
   
   vector<double> ED( indices.size() );
   
   for ( int i = 0; i < static_cast<int>( indices.size() ); i++ ) {
      const int j = indices[ i ];	// look up index
      ED[ i ] = mrk[ j ][ per ].lexdmd;
   }
   
   return ED;
}

//! returns vector of log of market demands
const vector<double> Marketplace::getLogDemands( const vector<int>& indices, const int per ) const {
   vector<double> demands( indices.size() );
   
   for ( int i = 0;i < static_cast<int>( indices.size() ); i++ ) {
      const int j = indices[ i ];	// look up index
      demands[ i ] = mrk[ j ][ per ].ldem;
   }
   
   return demands;
}

//! returns vector of log of market supplies
const vector<double> Marketplace::getLogSupplies( const vector<int>& indices, const int per ) const {
   vector<double> supplies( indices.size() );
   
   for ( int i = 0; i < static_cast<int>( indices.size() ); i++ ) {
      const int j = indices[ i ];	// look up index
      supplies[ i ] = mrk[ j ][ per ].lsup;
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
      
      ddemand = mrk[ j ][ per ].getChangeInDemand();
      dsupply = mrk[ j ][ per ].getChangeInSupply();
      dprice = mrk[ k ][ per ].getChangeInPrice();
      
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
      
      ddemand = mrk[ j ][ per ].getLogChangeInDemand( SMALL_NUM );
      dprice = mrk[ marketNumber ][ per ].getLogChangeInPrice( SMALL_NUM );
      
      if( dprice == 0 ){
         dprice = SMALL_NUM;
      }
      
      JFD[ i ] = ddemand / dprice; 
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
      
      dsupply = mrk[ j ][ per ].getLogChangeInSupply( SMALL_NUM );
      dprice = mrk[ marketNumber ][ per ].getLogChangeInPrice( SMALL_NUM );
      
      if( dprice == 0 ){
         dprice = SMALL_NUM;
      }
      
      JFS[ i ] = dsupply / dprice; 
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
   for (int i=0;i<nomrks;i++) {
      for (j=0;j<maxPeriod;j++) {
         temp[j] = mrk[i][j].price;
      }
      dboutput4(mrk[i][0].region,"Market",mrk[i][0].name,"1_price","$/GJ",temp);
      for (j=0;j<maxPeriod;j++) {
         temp[j] = mrk[i][j].supply;
      }
      dboutput4(mrk[i][0].region,"Market",mrk[i][0].name,"2_supply","EJ",temp);
      for (j=0;j<maxPeriod;j++) {
         temp[j] = mrk[i][j].demand;
      }
      dboutput4(mrk[i][0].region,"Market",mrk[i][0].name,"3_demand","EJ",temp);
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
   for (int i=0;i<nomrks;i++) {
      for (j=0;j<maxPeriod;j++) {
         temp[j] = mrk[i][j].price;
      }
      fileoutput3(mrk[i][0].region,"market",mrk[i][0].name," ","price","$/GJ",temp);
      for (j=0;j<maxPeriod;j++) {
         temp[j] = mrk[i][j].supply;
      }
      fileoutput3(mrk[i][0].region,"market",mrk[i][0].name," ","supply","EJ",temp);
      for (j=0;j<maxPeriod;j++) {
         temp[j] = mrk[i][j].demand;
      }
      fileoutput3(mrk[i][0].region,"market",mrk[i][0].name," ","demand","EJ",temp);
   }
}

//! write out market info to file for debugging
void Marketplace::bugout( const int per, const int iteration) const {
   // write market prices and supply (or demand)
   bugoutfile<< "Period," << per << ",Nth Iteration:," << iteration << endl;
   for ( int i = 0; i < nomrks_t_NR; i++ ) {
      bugoutfile << "market:," << getRegionName( mrk_isol_NR[ i ] ) << getGoodName( mrk_isol_NR[ i ] ) << ",price:,"<< mrk[ mrk_isol_NR[ i ] ][ per ].price << ",$/GJ,";
      bugoutfile << "supply:," << mrk[ mrk_isol_NR[ i ] ][ per ].supply << ",EJ,demand:," << mrk[ mrk_isol_NR[ i ] ][ per ].demand << ",EJ, "; 
      // bugoutfile << "SolveMarket: " << mrk[ i ][ per ].solveMarket << endl;
   }
   bugoutfile << endl;
}

//! write out market price supply and demand info to file for debugging
void Marketplace::sdcurves( const int per, const int iteration ) const {
   // write market prices and supply (or demand)
   for ( int i = 0; i < nomrks; i++ ) {
      sdcurvefile << i << "," << mrk[ i ][ 0 ].name << "," << mrk[ i ][ per ].price <<",";
      sdcurvefile << mrk[ i ][ per ].supply << "," << mrk[ i ][ per ].demand<<",";
   }
   sdcurvefile << endl;
}

//! Bracketing function only, does not find solution
int Marketplace::Bracket( const double solutionTolerance, const double excessDemandSolutionFloor, vector<solinfo>& sol, bool& allbracketed, bool& firsttime,int& worldCalcCount, const int per ) {
   
   World* world = scenario->getWorld();
   int i;
   const int nmrks = sol.size(); // number of markets to solve
   int numIterations = 0; // number of iterations
   int code = 2; // code that reports success 1 or failure 0
   vector<double> tempPrices( nmrks ); // temporary prices
   vector<double> EDtemp( nmrks ); // temporary excess demand
   
   bugoutfile << ",,Bracketing function called." << endl;
   logfile << ",,Bracketing function called." << endl;
   
   // bugoutfile << endl <<"Market,X-unknown,L-Brack,R-Brack,Ex Dem,EDL-Brac,EDR-Brac,Supply,Price,Demand,Tolerance" << endl;
   // bugoutfile << 5 << "," << sol[ 5 ].X << "," << sol[ 5 ].XL << "," << sol[ 5 ].XR << ",";
   // bugoutfile << sol[ 5 ].ED << "," << sol[ 5 ].EDL << "," << sol[ 5 ].EDR << "," << -1 << "," << -1 << "," << -1 << "," << solutionTolerance << "," << -1 << endl; 
   
   // Loop is done at least once.
   do {
      
      // Store the prices in tempPrices.
      for ( i = 0; i < nmrks; i++ ) {
         tempPrices[ i ] = sol[ i ].X;
      }
      
      setPrices( tempPrices, mrk_isol, per ); // set new prices
      nulldem( per );	// null demand
      nullsup( per ); // null supply
      
      world->calc( per ); // call world object to recalculate supply and demand
      
      excessdemand( per ); // calculate excess demand
      EDtemp = getExcessDemands( mrk_isol, per ); // show excess demand
      
      // Set excess demands.
      for ( i = 0; i < nmrks; i++ ) {
         sol[ i ].ED = EDtemp[ i ];
      }
      
      

      // for debugging
      const bool bug = true; // debugging
      if ( bug ) {
         bugoutfile << "In bracketing." << endl;
         
         bugoutfile << endl << "Market,X,XL,XR,ED,EDL,EDR,Tolerance,Bracketed" << endl;
         
         for ( i = 0; i < nmrks; i++ ) {
            bugoutfile << getName( mrk_isol[ i ] ) << "," <<sol[ i ].X << "," << sol[ i ].XL << "," << sol[ i ].XR
               << "," << sol[ i ].ED << "," << sol[ i ].EDL << "," << sol[ i ].EDR << "," << solutionTolerance << "," << sol[ i ].bracketed << endl;
         }
         
         bugout( per, numIterations );
         sdcurves( per, numIterations );
      }
      
      // bracketed array is either 0 or 1
      allbracketed = true;
      
      for ( i = 0; i < nmrks; i++ ) {
         if ( !sol[ i ].bracketed ) {
            allbracketed = false;
            break;
         }
      }
      
      if( allbracketed ) {
         // store original brackeded information
         if( firsttime ) {
            for ( i  = 0; i < nmrks; i++ ) {
               sol[ i ].XL_org = sol[ i ].XL; 
               sol[ i ].EDL_org = sol[ i ].EDL;
               sol[ i ].XR_org = sol[ i ].XR; 
               sol[ i ].EDR_org = sol[ i ].EDR;
            }
            firsttime = false;
         }
      }
      
      // Bracketing of prices; done first regardless of choice of solution algorithm.
      if ( !allbracketed ) {
         
         // Iterate through each market.
         for ( i = 0; i < nmrks; i++ ) {
            
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
                           sol[ i ].X *= 0.5; 
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
                           sol[ i ].X *= 1.5;
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
                           sol[ i ].X *= 0.5;
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
                           sol[ i ].X *= 1.5;
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
            if( isWithinTolerance( sol[ i ].ED, mrk[ mrk_isol[ i ] ][ per ].demand, solutionTolerance, excessDemandSolutionFloor ) ) {
               sol[ i ].bracketed = true;
            }

         } // for 
         
         allbracketed = true;
         for ( i = 0; i < nmrks; i++ ) {
            if ( !sol[ i ].bracketed ) {
               allbracketed = false;
               break;
            }
         }
         
         if( allbracketed ) {
            
            // store original bracketed information
            if( firsttime ) {
               for ( i = 0; i < nmrks; i++ ) {
                  sol[ i ].XL_org = sol[ i ].XL; 
                  sol[ i ].EDL_org = sol[ i ].EDL;
                  sol[ i ].XR_org = sol[ i ].XR; 
                  sol[ i ].EDR_org = sol[ i ].EDR;
               } // for ( i = 0
               firsttime = false;
            } // if( firsttime )
         } // if( allbracketed )
      }
   } while ( ++numIterations < 30 && !allbracketed );	
   code = ( allbracketed ? 1 : 0 );	// Report sucess, 1 or failure, 0
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
   int nmrks = sol.size(); // number of markets to solve
   // try rebracketing by setting bracketed array to false
   for(int i=0;i<nmrks;i++) {
      if (fabs(sol[i].dX) < SMALL_NUM) {
         allbracketed = false;
         sol[i].bracketed = false;
         sol[i].XL = sol[i].XR = sol[i].X; 
         sol[i].EDL = sol[i].EDR = sol[i].ED; 
      }
   }
}

//! Bisection Solution Mechanism (all markets)
int Marketplace::Bisection_all( const double solutionTolerance, const double excessDemandSolutionFloor, const int IterLimit, vector<solinfo>& sol, int& worldCalcCount, const int per ) {
   
   World* world = scenario->getWorld();
   int i, j;
   int numIterations = 0; // number of iterations
   int code = 2; // code that reports success 1 or failure 0
   const int nmrks = sol.size(); // number of markets to solve
   double maxSolVal; // maximum equality value
   vector<double> tempPrices(nmrks); // temporary prices
   vector<double> EDtemp(nmrks); // temporary excess demand
   bool breakout;	// var to allow various conditions to exit bisection routine
   const double BREAK_OUT_THRESHOLD = 0.001;
   double previousEDvalue = -1;
   double maxEDvalue = 0;
   int maxInt = 0;
   
   const bool bug = true;
   
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
      for (i=0; i<nmrks; ++i) {
         if ( !isWithinTolerance( sol[ i ].ED, mrk[ mrk_isol[ i ] ][ per ].demand, solutionTolerance, excessDemandSolutionFloor )) { // if haven't solved
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
      setPrices( tempPrices, mrk_isol, per ); // set new prices
      nulldem(per);	// null demand
      nullsup(per); // null supply
      
      world->calc(per); // call world object to recalculate supply and demand
      
      logED(per); // calculate log of excess demand
      excessdemand(per); // calculate excess demand
      EDtemp = getExcessDemands( mrk_isol, per ); // show excess demand
      for (i=0;i<nmrks;i++) {
         sol[i].ED = EDtemp[i];
         j = mrk_isol[ i ]; 
         // if price market, then check if XL < Demand, if so move XL
         /// BAD, BAD, BAD
         // This is not the way to do this, need a more general bracket check
         if (getType(j,per) == Market::PRICE && sol[i].XL < getRawDemand(j,per) ) {
            sol[i].XL = getRawDemand(j,per)* 1.5;
            // print out to remind us to re-do this correctly
            //if (per == 8) {
            //	cout << "   Shifted XL for market " << j << " ("<< getRegionName(j)<< "-"
            //		<< getGoodName(j)<< ")" << endl;
            //}
         }
         
         // debugging code that tracks ED
         if (i< 10 && false) {
            cout << getGoodName(j)<<": "<<sol[i].XL <<"("<<getRawPrice(j,per) <<") "<< sol[i].XR;
            if (!(getType(j,per) == Market::NORMAL)) {
               cout <<"  S,D: ["<<getRawSupply(j,per)<<","<< getRawDemand(j,per) <<"]";
               if (sol[i].XL < getRawDemand(j,per) ) {
                  cout << "***";
               } 
            }
            else {
               cout <<"  ED: ["<<getRawDemand(j,per) - getRawSupply(j,per)<<","<< sol[i].ED<<"]";
            }
            cout <<endl;
         }
         
      }
      maxSolVal = maxED( mrk_isol, excessDemandSolutionFloor, per );
      
      // for debugging
      if (bug) {
         bugoutfile << endl <<"Market,X-unknown,L-Brack,R-Brack,Ex Dem,EDL-Brac,EDR-Brac,tolerance\n";
         for (i=0; i<nmrks; ++i) {
            j =  mrk_isol[ i ]; 
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
         maxInt = worstED( mrk_isol, excessDemandSolutionFloor, per );
         cout << "maxED: "<< maxSolVal <<" ("<< mrk[ mrk_isol[ maxInt ] ][ per ].getName() << ")"  << endl; 
      }
      if (numIterations > 5 && false) {	// always bisect a few times
         // If the worst ED is not changing too much then breakout of bisection and let NR try to solve this
         if (abs(maxSolVal-previousEDvalue)/previousEDvalue < BREAK_OUT_THRESHOLD)  {  
            breakout = true; 
         }
      }
      
      previousEDvalue = maxSolVal;
      
   } // end do loop		
   while (++numIterations < IterLimit && maxSolVal >= solutionTolerance && !breakout);
   code = (maxSolVal < solutionTolerance ? 1 : 0); // report sucess, 1 or failure, 0
   worldCalcCount+=numIterations-1;
   
   if (trackED) { cout << endl; }
   
   return code;
}

//! Bisection Solution Mechanism (single market)
int Marketplace::Bisection_i( const int worstMarket, const double solutionTolerance, vector<solinfo>& sol,int& worldCalcCount, const int per ){
   World* world = scenario->getWorld();
   int numIterations = 0; // number of iterations
   int code = 2; // code that reports success 1 or failure 0
   double maxExcessDemand; // maximum equality value
   vector<double> tempPrices= getPrices( mrk_isol, per ); // temporary prices
   vector<double> EDtemp= getExcessDemands( mrk_isol, per ); // temporary excess demand
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
      setPrices( tempPrices, mrk_isol, per ); // set new prices
      nulldem(per);	// null demand
      nullsup(per); // null supply
      
      world->calc(per); // call world object to recalculate supply and demand
      
      logED(per); // calculate log of excess demand
      excessdemand(per); // calculate excess demand
      EDtemp = getExcessDemands( mrk_isol, per ); // show excess demand
      for (int j=0;j<EDtemp.size();j++) {
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
   while (++numIterations < 30 && maxExcessDemand >= solutionTolerance);			// report sucess, 1
   code = (maxExcessDemand < solutionTolerance ? 1 : 0);				// or failure, 0, 
   worldCalcCount+=numIterations-1;
   return code;
}

//! False Position Solution Mechanism (all markets)
int Marketplace::FalsePos_all( const double solutionTolerance,vector<solinfo>& sol,int& worldCalcCount, const int per ) {
   World* world = scenario->getWorld();
   int i;
   int numIterations = 0; // number of iterations
   int code = 2; // code that reports success 1 or failure 0
   double maxExcessDemand; // maximum equality value
   int nmrks = sol.size(); // number of markets to solve
   vector<double> tempPrices(nmrks); // temporary prices
   vector<double> EDtemp(nmrks); // temporary excess demand
   
   logfile << ",,FalsePos_all function called.\n";
   // solve all markets
   do {
      for (i=0; i<nmrks; ++i) {
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
      setPrices( tempPrices, mrk_isol, per ); // set new prices
      nulldem(per);	// null demand
      nullsup(per); // null supply
      
      world->calc(per); // call world object to recalculate supply and demand
      
      logED(per); // calculate log of excess demand
      excessdemand(per); // calculate excess demand
      EDtemp =  getExcessDemands( mrk_isol, per ); // show excess demand
      for (i=0;i<EDtemp.size();i++)
         sol[i].ED = EDtemp[i];
      
      maxExcessDemand = *max_element(EDtemp.begin(), EDtemp.end());
      
      // for debugging
      int bug = 0; // debugging on(1) or off(0)
      if (bug) {
         bugoutfile<<"\nMarket,X,XL,XR,ED,EDL,EDR,tolerance\n";
         for (i=0; i<nmrks; ++i) {
            bugoutfile<<i<<","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
               <<","<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<solutionTolerance<<"\n";
         }
         bugout(per,numIterations);
         sdcurves(per,numIterations);
      }
      
   } // end do loop		
   while (++numIterations < 30 && maxExcessDemand >= solutionTolerance);			// report sucess, 1
   code = (maxExcessDemand < solutionTolerance ? 1 : 0);				// or failure, 0, 
   worldCalcCount+=numIterations-1;
   return code;
}

//! Secant Solution Mechanism (all markets)
int Marketplace::Secant_all( const double solutionTolerance,vector<solinfo>& sol,int& worldCalcCount, const int per ) {
   World* world = scenario->getWorld();
   int i;
   int iSec=0; 
   int numIterations = 0; // number of iterations
   int code = 2; // code that reports success 1 or failure 0
   double maxExcessDemand; // maximum equality value
   int nmrks = sol.size(); // number of markets to solve
   vector<double> tempPrices(nmrks); // temporary prices
   vector<double> EDtemp(nmrks); // temporary excess demand
   
   logfile << ",,Secant_all function called.\n";
   // solve all markets
   do {
      if (iSec == 0) { // initial starting point; done once
         for (i=0; i<nmrks; ++i) {
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
      for (i=0; i<nmrks; i++) {
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
      
      setPrices( tempPrices, mrk_isol, per ); // set new prices
      nulldem(per);	// null demand
      nullsup(per); // null supply
      
      world->calc(per); // call world object to recalculate supply and demand
      
      logED(per); // calculate log of excess demand
      excessdemand(per); // calculate excess demand
      EDtemp =  getExcessDemands( mrk_isol, per ); // show excess demand
      for (i=0;i<EDtemp.size();i++)
         sol[i].ED = EDtemp[i];
      
      maxExcessDemand = *( max_element(EDtemp.begin(), EDtemp.end() )); // Max returns largest sol[i].ED
      
      // for debugging
      int bug = 0; // debugging on(1) or off(0)
      if (bug) {
         bugoutfile<<"\nMarket,X,XL,XR,ED,EDL,EDR,tolerance\n";
         for (i=0; i<nmrks; ++i) {
            bugoutfile<<i<<","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
               <<","<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<solutionTolerance<<"\n";
         }
         bugout(per,numIterations);
         sdcurves(per,numIterations);
      }
      
   } // end do loop		
   while (++numIterations < 30 && maxExcessDemand >= solutionTolerance);			// report sucess, 1
   code = (maxExcessDemand < solutionTolerance ? 1 : 0);				// or failure, 0, 
   worldCalcCount += numIterations - 1;
   return code;
}

//! Function to calculate derivative
//void JFunction(valarray<double> prices, Matrix& JFDM, int per)
void Marketplace::JFunction( vector<double> prices, Matrix& JFDM, int& worldCalcCount, int const per ) {
   World* world = scenario->getWorld();
   const int marketsToSolve = prices.size();
   const double DELTAP = 1e-4; // What is the proper value for delta?
   vector<double> tprices = prices; // define and initialize prices for storage
   vector<double> tmpJFD( marketsToSolve );
   
   storeinfo( per ); // store original market info before perturbing price
   
   for ( int j = 0; j < marketsToSolve; j++ ) {	// j is column index
      prices[j] *= ( 1 + DELTAP );	  // add price times deltap
      //prices[j] += deltap;	  // add price times deltap
      setPrices( prices, mrk_isol_NR, per );	// set new price for one market
      nulldem( per );	// null demand
      nullsup( per );	// null supply
      world->calc( per ); // call world object to recalculate supply and demand
      int col = j; // function calculates rows for each column
      tmpJFD = jacobian( mrk_isol_NR, col, per ); // calculate elasticities or Jacobian
      
      for ( int i = 0; i < marketsToSolve; i++ ) { // copy column vector to Jacobian Matrix
         JFDM[i][j] = tmpJFD[i]; // i is row index
      }
      
      restoreprc( mrk_isol_NR, per ); // restore perturbed market price
      prices[j] = tprices[j]; //  restore perturbed market price
      worldCalcCount++;
   }
}

//! Function to calculate derivative for Ron's version
void Marketplace::Derivatives( vector<double> prices, Matrix& JFDM, Matrix& JFSM, int& worldCalcCount, const int per ) {
   
   World* world = scenario->getWorld();
   const int marketsToSolve = prices.size();
   const double DELTAP = 1e-4; // Orginal, What is the proper value for delta?
   //const double DELTAP = 1e-5; // What is the proper value for delta?
   vector<double> tprices = prices; // define and initialize prices for storage
   vector<double> tmpJFD( marketsToSolve );
   vector<double> tmpJFS( marketsToSolve );
   
   storeinfo( per ); // store original market info before perturbing price
   
   for ( int j = 0; j < marketsToSolve; j++ ) {	// j is column index
      
      const int fullMarketNumber = mrk_isol_NR[ j ]; // the market number is the complete mrk vector.
      
      // Price is near zero.
      if( prices[ j ] < DELTAP ) {
         prices[ j ] = DELTAP;
      }
      
      // Price is positive.
      else {
         prices[ j ] *= ( 1 + DELTAP ); // add price times deltap
      }
      
      setPrices( prices, mrk_isol_NR, per );	// set new price for one market
      nulldem( per ); 
      nullsup( per );
      world->calc( per );
      worldCalcCount++;
      
      tmpJFD =  calcDemandElas( mrk_isol_NR, j, per ); // calculate demand elasticities
      tmpJFS =  calcSupplyElas( mrk_isol_NR, j, per ); // calculate supply elasticities
      
      for ( int i = 0; i < marketsToSolve; i++ ) {// copy column vector to Jacobian Matrix
         JFDM[ i ][ j ] = tmpJFD[ i ]; // i is row index
         JFSM[ i ][ j ] = tmpJFS[ i ]; // i is row index
      }
      
      restoreprc( mrk_isol_NR, per ); // restore perturbed market price
      prices[ j ] = tprices[ j ]; //  restore perturbed market price
   }
}

//! Newton Raphson Solution Mechanism (all markets)
int Marketplace::NewtRap( const double solutionTolerance, const double excessDemandSolutionFloor, vector<solinfo>& sol, int& worldCalcCount, const int per ){
   
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
   vector<double> logEDVec =  getLogExcessDemands( mrk_isol_NR, per ); // show log of excess demand
   vector<double> tempPrices = getPrices( mrk_isol_NR, per ); // temporary prices
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
      
      setPrices( tempPrices, mrk_isol_NR, per ); // set new prices
      nulldem( per );	// null demand
      nullsup( per ); // null supply
      
      world->calc( per ); // call world object to recalculate supply and demand
      logED( per ); // calculate log of excess demand
      excessdemand( per ); // calculate excess demand
      logEDVec =  getLogExcessDemands( mrk_isol_NR, per ); // show log of excess demand
      EDtemp = getExcessDemands( mrk_isol_NR, per ); // show excess demand
      maxExcessDemand = maxED( mrk_isol_NR, excessDemandSolutionFloor, per ); // Max returns largest ED[i] // temp
      
      // for debugging
      const bool bug = false; // debugging
      if ( bug ) {
         worstExcessDemand = worstED(  mrk_isol_NR, excessDemandSolutionFloor, per );
         cout << "maxED: " << mrk[ mrk_isol_NR[ worstExcessDemand ] ][ per ].getName() << ":" << maxExcessDemand << endl;
         
         bugoutfile << endl << "Market,X,DP,ED,tolerance" << endl;
         for ( i = 0; i < marketsToSolve; i++ ) {
            bugoutfile << i << "," << tempPrices[i] << "," << DP[i] << "," << EDtemp[i] << "," << solutionTolerance << endl;
         }
         
         bugout( per, numIterations );
         sdcurves( per, numIterations ); 
      }
      
      // if solution moves in wrong direction
      if( maxExcessDemand > 500 ) {
         logfile << ",,Exit Newton-Raphson function maxExcessDemand > 500." << endl;
         return 0;
      }
   } while ( ++numIterations < 30 && maxExcessDemand >= solutionTolerance );			// report sucess, 1
   
   code = ( maxExcessDemand < solutionTolerance ? 1 : 0 );				// or failure, 0, 
   // resize and reasign all solution prices and ED's
   // need to copy prices and ED to sol
   tempPrices =  getPrices( mrk_isol_NR, per );
   EDtemp = getExcessDemands( mrk_isol_NR, per );
   
   for ( i = 0; i < tempPrices.size(); i++ ) {
      sol[i].X = tempPrices[i];
      sol[i].ED = EDtemp[i];
   }
   worldCalcCount += numIterations - 1;
   return code;
}

//! Ron's version of the Newton Raphson Solution Mechanism (all markets)
int Marketplace::NR_Ron( const double solutionTolerance, const double excessDemandSolutionFloor, vector<solinfo>& sol, int& worldCalcCount, const int per ) {
   
   World* world = scenario->getWorld();
   const Modeltime* modeltime = scenario->getModeltime();
   int i;
   int numDerivativeCalcs = 0; // count number of times derivatives are calculated
   int iter = 0; // number of iterations through solution algorithm
   int code = 2; // code that reports success 1 or failure 0
   double maxSolVal; // maximum equality value 
   const int marketsToSolve =  setMarketsToSolveNR( per ); // number of markets to solve
   vector<double> NP( marketsToSolve ); // adjustment value
   vector<double> KD( marketsToSolve ); // k values demand
   vector<double> KS( marketsToSolve ); // k values supply
   vector<double> KDS( marketsToSolve ); // k values demand - supply
   vector<double> tempPrices = getPrices( mrk_isol_NR, per ); // temporary prices
   vector<double> EDtemp( marketsToSolve ); // temporary excess demand
   
   bool breakout = false;	// var to allow various conditions to exit NR routine
   int maxInt = 0;
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
      
      if ( ( numDerivativeCalcs < 5 ) && ( per < modeltime->getmaxper() ) ) { // control no of times derivatives are calculated
         if ( trackED ) {
            cout <<" ... "; 
         }
         
         Derivatives( tempPrices, JFDM, JFSM, worldCalcCount, per ); // Recalculate Jacobian matrix, returns JF matrix
         numDerivativeCalcs++; // increment count of derivative calculation
         
         for( i = 0; i < marketsToSolve; i++ ) {
            for( int j = 0; j < marketsToSolve; j++ ) {
               JF[ i ][ j ] = JFSM[ i ][ j ] - JFDM[ i ][ j ];
            }
         }
         
         invertMatrix( JF );
         
         if ( trackED ) {
            cout <<" End Derivatives " <<endl;
         }
      }
      
      // initialize KD and KS as logs of original demand and supply
      KD =  getLogDemands( mrk_isol_NR, per ); // return log of demand
      KS =  getLogSupplies( mrk_isol_NR, per ); // return log of supply
      
      for ( i = 0; i < marketsToSolve; i++ ) {
         for ( int j = 0; j < marketsToSolve; j++ ) {
            double tempValue = log( max( tempPrices[ j ], SMALL_NUM ) );
            KD[ i ] -= tempValue * JFDM[ i ][ j ];
            KS[ i ] -= tempValue * JFSM[ i ][ j ];
         }
         
         KDS[ i ] = KD[ i ] - KS[ i ];
      }
      
      // Calculate new log price based on NR
      for ( i = 0; i < marketsToSolve; i++ ) {
         
         NP[ i ] = 0;
         
         for ( int j = 0; j < marketsToSolve; j++ ) {
            NP[ i ] += JF[ i ][ j ] * KDS[ j ];
         }
         
         tempPrices[ i ] = exp( NP[ i ] ); // new price
        
         // Check the validity of the price.
         assert( tempPrices[ i ] == tempPrices[ i ] ); // This checks for NaN since NaN != NaN.
         assert( tempPrices[ i ] != std::numeric_limits<double>::infinity() ); // Checks for infinity. 
      }
      
      setPrices( tempPrices, mrk_isol_NR, per ); // set new prices
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
      EDtemp = getExcessDemands( mrk_isol_NR, per ); // show excess demand
      
      maxSolVal = maxED( mrk_isol_NR, excessDemandSolutionFloor, per ); // Max returns largest ED[i]
      
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
      
      maxInt = worstED( mrk_isol_NR, excessDemandSolutionFloor, per );
      if ( trackED ) {
         cout << "maxED: "<< maxSolVal <<" (" << getRegionName( mrk_isol_NR[ maxInt ] ) << getGoodName( mrk_isol_NR[ maxInt ] ) << ")" << endl;
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
         logfile << ",,Exit Newton-Raphson function maxSolVal > 1500. "<< endl;
         //   logfile << ", Due to market " << getRegionName(maxInt)<< "-"<< getGoodName(maxInt) <<"\n";
         if ( trackED && per > 0) {
            cout << "Exit Newton-Raphson function maxSolVal > 1500" << endl;
         }
         for ( i = 0; i < nomrks; i++ ) {
            if ( fabs( mrk[ i ][ per ].exdmd ) > maxSolVal / 100 ) {
               if ( trackED ) {
                  cout << "ED: (" << getName( i ) << ") - " << mrk[ i ][ per ].exdmd << endl;
               }
               logfile << ",,,Due to market " << getName( i ) << " - ED: " << mrk[ i ][ per ].exdmd << endl;
            }
         }
         
         return 0; 
      }
   } // end do loop	
   
   while ( ++iter < 35 && maxSolVal >= solutionTolerance && !breakout );	
   code = ( maxSolVal < solutionTolerance ? 1 : 0 ); // report sucess 1 or failure 0, 
   
   // resize and reasign all solution prices and ED's
   // need to copy prices and ED to sol
   tempPrices =  getPrices( mrk_isol_NR, per );
   EDtemp = getExcessDemands( mrk_isol_NR, per );
   
   for ( i = 0; i < tempPrices.size(); i++ ) {
      sol[ i ].X = tempPrices[ i ];
      sol[ i ].ED = EDtemp[ i ];
   }
   
   worldCalcCount += iter - 1;
   logfile << ",Number of Newton-Raphson iterations: n =" << iter << endl;
   
   return code;
}


void Marketplace::solve( const int per ) {
   bool allbracketed = false;
   bool prod_not_null = false;
   const bool useBisect = false;
   const bool useFP = false;
   const bool useSecant = false;
   const bool useNR = false;
   const bool useNR_Ron = true;
   bool firsttime = true;
   int i = 0; // some index
   int worldCalcCount = 0; // index for solution iteration
   int bn=0; // counter for bisection routine
   int code = 2; // code that reports success 1 or failure 0
   int solved = 0; // code that reports success 1 or failure 0
   const double solTolerance = 0.001; // tolerance for solution criteria
   const double excessDemandSolutionFloor = 0.001; // minimum value below which solution is assumed to be found.
   // Extra high tolerance to get to solution faster
   //double solTolerance = 0.1; // tolerance for solution criteria
   double maxSolVal; // temporary maximum value of equality condition			 
   vector<double> X,ED,logEDVec; // price, excess demand and log of excess demand vectors
   
   excessdemand( per ); // first calculate excess demand for all markets
   const int marketsToSolve =  setMarketsToSolve( per ); // set number of markets to solve
   
   logfile << ",Starting Solution. Solving for " << marketsToSolve << " markets." << endl;
   
   // if marketsToSolve = 0, no markets to solve, break out of solution.
   if ( marketsToSolve == 0 ) {
      cout << "Model solved with last period's prices"; 
      return;
   }
   
   logED( per ); // calculate log of excess demand
   X =  getPrices( mrk_isol, per ); // showPRC returns a vector of prices	
   logEDVec = getLogExcessDemands( mrk_isol, per ); // showlogED returns a vector of logED
   ED = getExcessDemands( mrk_isol, per ); // showED returns a vector of ED
   
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
         bugoutfile << getRegionName( mrk_isol[ i ] ) << getGoodName( mrk_isol[ i ]  ) <<"," << sol[ i ].X << "," << sol[ i ].XL << "," << sol[ i ].XR
            << "," << sol[ i ].ED << "," << sol[ i ].EDL << "," << sol[ i ].EDR << "," << solTolerance << endl;
      }
      bugoutfile << endl;
      
      bugout( per, worldCalcCount );
      sdcurves( per, worldCalcCount );
   }
   
   // Loop is done at least once.
   do {
      if ( !allbracketed ) {
         int didBracket = Bracket( solTolerance, excessDemandSolutionFloor, sol,allbracketed,firsttime,worldCalcCount,per);
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
            i = worstED( mrk_isol, excessDemandSolutionFloor, per );
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
            i =  worstED( mrk_isol, excessDemandSolutionFloor, per );
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
            i =  worstED( mrk_isol_NR, excessDemandSolutionFloor, per );
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
         maxSolVal =  maxED( mrk_isol_NR, excessDemandSolutionFloor, per ); // Max returns largest ED[i]
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
         maxSolVal = maxED( mrk_isol, excessDemandSolutionFloor, per ); // Max returns largest ED[i]
         
         if( !solved && maxSolVal < 1500 ) {
            solved = NR_Ron( solTolerance, excessDemandSolutionFloor, sol, worldCalcCount, per );
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
      ED =  getExcessDemands( mrk_isol, per );
      maxSolVal =  maxED( mrk_isol, excessDemandSolutionFloor, per ); // Max returns largest ED[i]
      
      // for debugging
      if ( bugTracking ) {
         bugoutfile << endl << "Solution() loop. N: " << worldCalcCount << endl;
         bugoutfile << endl << "Market,X,XL,XR,ED,EDL,EDR,Tolerance" << endl;
         
         for (i = 0; i < marketsToSolve; ++i ) {
            bugoutfile << getRegionName( mrk_isol[ i ] ) << getGoodName( mrk_isol[ i ] ) << "," << sol[ i ].X << "," << sol[ i ].XL << "," << sol[ i ].XR 
               << ","<< sol[ i ].ED << "," << sol[ i ].EDL <<","<<sol[i ].EDR << "," << solTolerance << endl;
         } 
         
         bugout( per, worldCalcCount );
         sdcurves( per, worldCalcCount );
      }
      
   } // end do loop		
   while ( maxSolVal >= solTolerance && ++worldCalcCount < 1000 );			// report sucess, 0
   code = ( maxSolVal < solTolerance ? 0 : -1 );				// or failure, -1, 
   
   if ( !checkMarketSolution( solTolerance, excessDemandSolutionFloor, per ) && ( code == 0 ) ) {
      cerr << "ERROR: Supplies and Demands are NOT equal" << endl;
   }
   
   switch (code) {
   case 0:
      cout << "Model solved normally:	World Calc worldCalcCount = " << worldCalcCount << endl;
      logfile << ",Model solved normally:  World Calc worldCalcCount = " << worldCalcCount << endl;
      break;
   case -1:
      cout << "Model did not solve within set iteration	" << worldCalcCount << endl;
      logfile << ",Model did not solve within set iteration " << worldCalcCount << endl;
      
      logfile <<",Market,X,XL,XR,ED,EDL,EDR,Tolerance" << endl;
      for ( i = 0; i < marketsToSolve; i++ ) {
         logfile << "," << getRegionName( mrk_isol[ i ] ) << getGoodName( mrk_isol[ i ] ) << ","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
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


