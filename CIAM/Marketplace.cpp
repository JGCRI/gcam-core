/*! 
* \file Marketplace.cpp
* \ingroup CIAM
* \brief Marketplace class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"

#include <iostream>
#include <fstream>
#include <vector>
#include <map>

#include "scenario.h"
#include "Market.h"
#include "price_market.h"
#include "demand_market.h"
#include "calibration_market.h"
#include "ghg_market.h"
#include "normal_market.h"
#include "Marketplace.h"
#include "SolverLibrary.h"
#include "modeltime.h"
#include "XMLHelper.h"
#include "Configuration.h"
#include "World.h"
#include "LoggerFactory.h"
#include "Logger.h"
#include "Solver.h"
#include "BisectionNRSolver.h"
#include "SupplyDemandCurve.h"

using namespace std;
using namespace mtl;

extern ofstream logfile;
extern Scenario* scenario;

/*! \brief Default constructor
*
* The default constructor for the Marketplace which initializes several datamembers and
* creates an instance of the selected solver.
*
* \todo Make a static methor which returns a new BisectionNRSolver for further encapsulation.
* \today Marketplace might be better as a singleton.
*/
Marketplace::Marketplace() {
   uniqueNo = 0;
   numMarkets = 0;
   solver = new BisectionNRSolver( this );
}

/*! \brief Destructor
*
* Destructor for the marketplace which first deletes all the markets and then
* deletes the solution mechanism.
*/
Marketplace::~Marketplace() {
   
   // Clean up the markets.
   for ( vector< vector< Market* > >::iterator outerIter = markets.begin(); outerIter != markets.end(); outerIter++ ) {
      for( vector< Market* >::iterator innerIter = outerIter->begin(); innerIter != outerIter->end(); innerIter++ ) {
         delete *innerIter;
      }
   }

   // Delete the solution mechanism.
   delete solver;
}

/*! \brief Solve the marketplace using the Solver.
*
* The solve method calls the solve method of the instance of the Solver object 
* that was created in the constructor. This method then clears the market.
*
* \param period Period of the model to solve.
* \todo Error checking return codes for solve.
*/

void Marketplace::solve( const int period ){
   solver->solve( period );
}

/*! \brief Write out XML for debugging purposes.
*
* This method is called hierarchically from the main loop to write out 
* the current state of the model at a given time period. All member variables relevant to the running 
* of the model are written out to assist with debugging. It writes to the output stream
* passed as an argument in an XML format. 
* 
* \warning Currently to limit the size of the file which is written this method is only called for the
* US region.
* \param period The period for which to print the debugging information.
* \param out The output stream to which to print.
* \author Josh Lurz
*/
void Marketplace::toDebugXML( const int period, ostream& out ) const {
   
   // write the beginning tag.
   Tabs::writeTabs( out );
   out << "<Marketplace>" << endl;
   
   // increase the indent.
   Tabs::increaseIndent();
   
   // write the xml for the class members.
   XMLWriteElement( numMarkets, "numberOfMarkets", out );
   
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

/*! \brief Function to create market key from a region name and a good name.
* 
* This convenience function was added to consistently create the name of the market from the region name and
* good name.
*
* \param marketName The market region.
* \param goodName The market good.
*/
string Marketplace::createMarketKey( const string& marketName, const string& goodName ) {
   return ( marketName + goodName );
}

//! Function to find the market number from the market name and good name.
int Marketplace::getMarketNumberFromNameAndGood( const string& marketName, const string& goodName ) const {
   
   string key = createMarketKey( goodName, marketName );
   
   const map<string,int>::const_iterator marketKey = marketMap.find( key );
   
   if ( marketKey != marketMap.end() ) {
      return marketKey->second;
   }
   else {
      cout << "Warning: Market not found: " << key << "! " << endl;
      return -1;
   }
}

//! Function to set the raw price from the marketName and goodName.
/*! \warning MarketName is NOT the same as RegionName.
*/
void Marketplace::setRawPrice( const string& marketName, const string& goodName, const double priceIn, const int period ) {
   
   // Get the market number.
   const int marketNumber = getMarketNumberFromNameAndGood( marketName, goodName );
   
   // Set the market's raw price.
   if ( marketNumber != -1 ) {
      markets[ marketNumber ][ period ]->setRawPrice( priceIn );
   }
}


//! returns a single market's excess demand from the market name and good name.
/*! \warning MarketName is NOT the same as RegionName.
*//*
*/
/*
double Marketplace::getExcessDemand( const string& marketName, const string& goodName, const int period ) const {
   
   // Get the market number.
   const int marketNumber = getMarketNumberFromNameAndGood( marketName, goodName );
   
   // Get the excess demand.
   if ( marketNumber != -1 ) {
      return markets[ marketNumber ][ period ]->getDemand() - markets[ marketNumber ][ period ]->getSupply();
   }
   else {
      cout << "Error market not found!" << endl;
      return 0;
   }
}
/*
//! returns a single market's raw demand from the market name and good name.
/*! \warning MarketName is NOT the same as RegionName.
*/

double Marketplace::getRawDemand( const string& marketName, const string& goodName, const int period ) const {
   
   // Get the market number.
   const int marketNumber = getMarketNumberFromNameAndGood( marketName, goodName );
   
   // Get the raw demand.
   if ( marketNumber != -1 ) {
      return markets[ marketNumber ][ period ]->getRawDemand();
   }
   else {
      cout << "Error market not found!" << endl;
      return 0;
   }
}

//! returns a single market's stored raw demand from the market name and good name.
/*! \warning MarketName is NOT the same as RegionName.
*/
double Marketplace::getStoredRawDemand( const string& marketName, const string& goodName, const int period ) const {
   
   // Get the market number.
   const int marketNumber = getMarketNumberFromNameAndGood( marketName, goodName );
   
   // Get the raw demand.
   if ( marketNumber != -1 ) {
      return markets[ marketNumber ][ period ]->getStoredRawDemand();
   }
   else {
      cout << "Error market not found!" << endl;
      return 0;
   }
}

//! returns if a market is a price or demand market.
/*! \warning MarketName is NOT the same as RegionName.
* \note This still isn't very good OO programming. 
*/
bool Marketplace::isPriceOrDemandMarket( const string& marketName, const string& goodName, const int period ) const {
   
   bool retValue = false;
   // Get the market number.
   const int marketNumber = getMarketNumberFromNameAndGood( marketName, goodName );
   
   // Get the raw demand.
   if ( marketNumber != -1 ) {
      string marketType = markets[ marketNumber ][ period ]->getType();
      if( marketType == "PriceMarket" || marketType == "DemandMarket" ) {
         retValue = true;
      }
   }
   return retValue;
}

//! returns a single market's raw supply from the market name and good name.
/*! \warning MarketName is NOT the same as RegionName.
*/
double Marketplace::getRawSupply( const string& marketName, const string& goodName, const int period ) const {
   
   // Get the market number.
   const int marketNumber = getMarketNumberFromNameAndGood( marketName, goodName );
   
   // Get the raw supply.
   if ( marketNumber != -1 ) {
      return markets[ marketNumber ][ period ]->getRawSupply();
   }
   else {
      cout << "Error market not found!" << endl;
      return 0;
   }
}

//! returns a single market's stored raw supply from the market name and good name.
/*! \warning MarketName is NOT the same as RegionName.
*/
double Marketplace::getStoredRawSupply( const string& marketName, const string& goodName, const int period ) const {
   
   // Get the market number.
   const int marketNumber = getMarketNumberFromNameAndGood( marketName, goodName );
   
   // Get the raw supply.
   if ( marketNumber != -1 ) {
      return markets[ marketNumber ][ period ]->getStoredRawSupply();
   }
   else {
      cout << "Error market not found!" << endl;
      return 0;
   }
}

//! returns a single market's raw price from the market name and good name.
/*! \warning MarketName is NOT the same as RegionName.
*/
double Marketplace::getRawPrice( const string& marketName, const string& goodName, const int period ) const {
   
   // Get the market number.
   const int marketNumber = getMarketNumberFromNameAndGood( marketName, goodName );
   
   // Get the raw price.
   if ( marketNumber != -1 ) {
      return markets[ marketNumber ][ period ]->getRawPrice();
   }
   else {
      cout << "Error market not found!" << endl;
      return 0;
   }
}

//! returns a single market's stored raw price from the market name and good name.
/*! \warning MarketName is NOT the same as RegionName.
*/
double Marketplace::getStoredRawPrice( const string& marketName, const string& goodName, const int period ) const {
   
   // Get the market number.
   const int marketNumber = getMarketNumberFromNameAndGood( marketName, goodName );
   
   // Get the raw price.
   if ( marketNumber != -1 ) {
      return markets[ marketNumber ][ period ]->getStoredRawPrice();
   }
   else {
      cout << "Error market not found!" << endl;
      return 0;
   }
}

//! Remove an amount from all markets raw demand.
void Marketplace::removeFromRawDemands( const vector<double>& rawDemands, const int period ) {
   
   // First check vector sizes match.
   assert( rawDemands.size() == markets.size() );
   
   // Iterate through the markets and remove raw demands.
   for ( int i = 0; i < static_cast<int>( markets.size() ); i++ ) {
      markets[ i ][ period ]->removeFromRawDemand( rawDemands[ i ] );
   }
}

//! Remove an amount from all markets raw supply.
void Marketplace::removeFromRawSupplies( const vector<double>& rawSupplies, const int period ) {
   
   // First check vector sizes match.
   assert( rawSupplies.size() == markets.size() );
   
   // Iterate through the markets and remove raw demands.
   for ( int i = 0; i < static_cast<int>( markets.size() ); i++ ) {
      markets[ i ][ period ]->removeFromRawSupply( rawSupplies[ i ] );
   }
}

//! Get the list of contained regions from a market. 
const vector<string> Marketplace::getContainedRegions( const string& marketName, const string& goodName, const int period ) const {
   
   // Get the market number.
   const int marketNumber = getMarketNumberFromNameAndGood( marketName, goodName );
   
   // Get the contained regions.
   if ( marketNumber != -1 ) {
      return markets[ marketNumber ][ period ]->getContainedRegions();
   }
   else {
      return vector<string>();
   }
}

//! Function which adds a regional market into the regionToMarketMap map, uses goodName + regionName to find that market.
/*! Returns true if a new market was added. False if the market already exists.
* \warning There is an important distinction here between the region name vs the market name. The key to the market is the goodName + market name.
*/
bool Marketplace::setMarket( const string& regionName, const string& marketsname, const string& goodName, const NewMarketType typeIn ) {
   const Modeltime* modeltime = scenario->getModeltime();
   int marketsNo;
   bool retValue;
   
   // create unique markets from distinct good and market region names
   string key = createMarketKey( goodName, marketsname );
   
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
            tempVector[ i ] = new NormalMarket( goodName, marketsname, i );
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
* Also adds a corresponding demand market that provides a trial value for demand.
* Routines such as Marketplace::setdemand, getdemand, getprice, etc. are adjusted to act 
* differently for PRICE and DEMAND markets so that these changes are transparent to the 
* rest of the code.
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
* although no such guarantee for GHG markets, but initialize prices for all periods 
* in all markets just to be safe if not already set. 
* Initialization also occurs for supply and demand markets that have prices read-in via routine:
* setPriceVector (in sector as supply & demand mkts are created).
* This has no effect for future periods as these prices are overwritten by 
* Marketplace::init_to_last
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

//! initialize all market supplies to 0
void Marketplace::nullsup( const int period ) {
   for ( int i = 0; i < numMarkets; i++ ) {
      markets[ i ][ period ]->nullSupply();
   }
}

//! set market price
/*! If this is a price market, then the price passed is used to set supply instead of price. 
* The market price is also used to set the demand.
* Use this convention for pseudo markets -- model values are supply, trial market values are used
* for demand. 
*/
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
      markets[ marketNumber ][ per ]->addToSupply( value );
   }
}

//! set market demand to used for solution mechanism
void Marketplace::setdemand( const string& goodName, const string& regionName, const double value, const int per ){
   const int marketNumber = getMarketNumber( goodName, regionName );
   
   if (value < 0 && goodName != "CO2" ) {
      cerr << "ERROR in setdemand: Demand value < 0 for market " << goodName << " in region " << regionName << endl;
   }
   
   if ( marketNumber != -1 ) {
      markets[ marketNumber ][ per ]->addToDemand( value );
   }
}

//! return market price
/*! If the market does not exist, return an extremely large price.
\todo do something else about "renewable"
*/
double Marketplace::getPrice( const string& goodName, const string& regionName, const int per ) const {
   
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
double Marketplace::getSupply( const string& goodName, const string& regionName, const int per ) const {
   const int marketNumber = getMarketNumber( goodName, regionName );
   
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
double Marketplace::getDemand(  const string& goodName, const string& regionName, const int per ) const {
   const int marketNumber = getMarketNumber( goodName, regionName );
   
   if ( marketNumber != -1 ) {
      return markets[ marketNumber ][ per ]->getDemand();
   }
   else {
      cerr << "ERROR: Called for demand of non-existant market "<< goodName << " in " << regionName << endl;
      return 0;
   }
}

//! Check to see that all markets Rawly solved.
bool Marketplace::checkMarketSolution( const double solTolerance, const double excessDemandSolutionFloor, const int period ) {
   
   const Configuration* conf = Configuration::getInstance();
   const bool debugChecking = conf->getBool( "debugChecking" );
   const bool debugFindSD = conf->getBool( "debugFindSD" );
   
   bool solvedOK = true;
   vector<Market*> unsolved;
   
   for( int i = 0; i < static_cast<int>( markets.size() ); i++ ) {
      if ( !SolverLibrary::isWithinTolerance( markets[ i ][ period ]->getRawDemand() - markets[ i ][ period ]->getRawSupply(), markets[ i ][ period ]->getRawDemand(), solTolerance, excessDemandSolutionFloor ) ) {
         unsolved.push_back( markets[ i ][ period ] );
      }
   }
   
   if( !unsolved.empty() ) {
      solvedOK = false;
      cout << "Unsolved markets: " << endl;
   }
   
   for ( vector<Market*>::const_iterator iter = unsolved.begin(); iter != unsolved.end(); iter++ ) {
      cout << "Market (" << ( *iter )->getName() << ") S: "<< ( *iter )->getRawSupply() << " D: " << ( *iter )->getRawDemand() << endl;
   }
   
   if ( debugFindSD && !unsolved.empty() ) {
      string logName = Configuration::getInstance()->getFile( "supplyDemandOutputFileName", "SDCurves.csv" );
      Logger* sdLog = LoggerFactory::getLogger( logName );
      LOG( sdLog, Logger::WARNING_LEVEL ) << "Supply and demand curves for markets that did not solve in period: " << period << endl;
      findAndPrintSD( unsolved, period );
   }

   return solvedOK;
}

//! Find Supply and Demand curves for bad markets and print them.
void Marketplace::findAndPrintSD( vector<Market*>& unsolved, const int period ) {

   const Configuration* conf = Configuration::getInstance();
   const int numMarketsToFindSD = conf->getInt( "numMarketsToFindSD", 5 );
   const int numPointsForSD = conf->getInt( "numPointsForSD", 5 );
   string logName = Configuration::getInstance()->getFile( "supplyDemandOutputFileName", "SDCurves.csv" );
   Logger* sdLog = LoggerFactory::getLogger( logName );
   World* world = scenario->getWorld();
   
   // Remove any unsolved markets.
   for( vector<Market*>::iterator removeIter = unsolved.begin(); removeIter != unsolved.end(); ) {
      if ( !( *removeIter )->shouldSolve() ) {
         removeIter = unsolved.erase( removeIter );
      }
      else {
         removeIter++;
      }
   }

   // If there aren't any markets left return.
   if( unsolved.empty() ) {
      return;
   }

   // Sort the vector so the worst markets are first and remove the rest.
   sort( unsolved.begin(), unsolved.end(), Market::greaterRelativeExcessDemand() );

   // Truncate vector if neccessary.
   if( static_cast<int>( unsolved.size() ) > numMarketsToFindSD ) { 
      unsolved.resize( numMarketsToFindSD );
   }
      
   // Now determine supply and demand curves for each.
   for ( vector<Market*>::iterator iter = unsolved.begin(); iter != unsolved.end(); iter++ ) {
      SupplyDemandCurve sdCurve( *iter );
      sdCurve.calculatePoints( numPointsForSD, world, period );
      sdCurve.print( sdLog );
   }
   
}

//! Select markets to solve.
vector< pair< string, string > > Marketplace::getMarketsToSolve( const int period, const bool isNR ) const {
   
   vector< pair< string, string > > marketsToSolve;
   
   for( int i = 0; i < static_cast<int>( markets.size() ); i++ ) {
      
      if ( !isNR ) {
         if ( markets[ i ][ period ]->shouldSolve() ) {
            marketsToSolve.push_back( pair<string,string>( markets[ i ][ period ]->getRegionName(), markets[ i ][ period ]->getGoodName() ) );
         }
      }
      else {
         // Add markets to NR solution list
         if ( markets[ i ][ period ]->shouldSolveNR() ) {
            marketsToSolve.push_back( pair<string,string>( markets[ i ][ period ]->getRegionName(), markets[ i ][ period ]->getGoodName() ) );
         }
      }
   }
   return marketsToSolve;
}

//! Initialize the marketplace prices 
/*! Use last period demand price as starting point for next period */
void Marketplace::init_to_last( const int period ) { 
   // only after the starting period
   if ( period > 0 ) {
      for ( int i = 0; i < numMarkets; i++ ) {
         markets[ i ][ period ]->setPriceFromLast( markets[ i ][ period - 1 ]->getRawPrice() );
      }
   }
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

//! Returns a vector of market demands for all markets.
const vector<double> Marketplace::getDemands( const int per ) const {
   
   vector<double> demands( numMarkets );
   
   for ( int i = 0; i < numMarkets; i++ ) {
      demands[ i ] = markets[ i ][ per ]->getRawDemand();
   }
   
   return demands;
}

//! Returns a vector of market supplies for all markets.
const vector<double> Marketplace::getSupplies( const int per ) const {
   
   vector<double> supplies( numMarkets );
   
   for ( int i = 0; i < numMarkets; i++ ) {
      supplies[ i ] = markets[ i ][ per ]->getRawSupply();
   }
   
   return supplies;
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
