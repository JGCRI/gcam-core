/*! 
* \file AgSector.cpp
* \ingroup CIAM
* \brief AgSector class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include <cassert>
#include <iostream>
#include <fstream>
#include "AgSector.h"
#include "xmlHelper.h"
#include "Marketplace.h"
#include "modeltime.h"
#include "scenario.h"

// Fortran calls.
extern "C" { void _stdcall SETGNP( int&, double[] ); };
extern "C" { double _stdcall GETGNP( int&, int& ); };
extern "C" { void _stdcall SETPOP( int&, double[] ); };
extern "C" { double _stdcall GETPOP( int&, int& ); };
extern "C" { void _stdcall SETBIOMASSPRICE( double[] ); };
extern "C" { double _stdcall GETBIOMASSPRICE( ); };
extern "C" { void _stdcall AG2RUN( double[], int&, int&, double[], double[] ); };
extern "C" { double _stdcall AG2CO2EMISSIONS( int&, int& ); };
extern "C" { void _stdcall AG2LINKOUT( void ); };

extern Scenario* scenario;

using namespace std;

int AgSector::regionCount = 0;
const int AgSector::numAgMarkets = 12;
bool AgSector::init = false;
map<string, int> AgSector::nameToIndiceMap;
vector<string> AgSector::marketNameVector;
map<int, string> AgSector::indiceToNameMap;

//! Constructor
AgSector::AgSector() {
   
   regionNumber = regionCount;
   regionCount++;
   biomassPrice = 0;
   
   if( !init ){
      staticInitialize();
   }
}

//! Initialize static data members.
void AgSector::staticInitialize(){
   init = true;
   // Initialize marketNameVector(Really should be static)
   marketNameVector.push_back( "wood" );
   marketNameVector.push_back( "forward wood" );
   marketNameVector.push_back( "food grains" );
   marketNameVector.push_back( "coarse grains" );
   marketNameVector.push_back( "oil crops" );
   marketNameVector.push_back( "misc crops" );
   marketNameVector.push_back( "pasture" );
   
   // Initialize nameToIndiceMap
   nameToIndiceMap[ "crude oil" ] = 0;
   nameToIndiceMap[ "natural gas" ] = 1;
   nameToIndiceMap[ "coal" ] = 2;
   nameToIndiceMap[ "biomass" ] = 3;
   nameToIndiceMap[ "carbon" ] = 4;
   nameToIndiceMap[ "wood" ] = 5;
   nameToIndiceMap[ "forward wood" ] = 6;
   nameToIndiceMap[ "food grains" ] = 7;
   nameToIndiceMap[ "coarse grains" ] = 8;
   nameToIndiceMap[ "oil crops" ] = 9;
   nameToIndiceMap[ "misc crops" ] = 10;
   nameToIndiceMap[ "pasture" ] = 11;
   
   indiceToNameMap[ 0 ] = "crude oil";
   indiceToNameMap[ 1 ] = "natural gas";
   indiceToNameMap[ 2 ] = "coal";
   indiceToNameMap[ 3 ] = "biomass";
   indiceToNameMap[ 4 ] = "carbon";
   indiceToNameMap[ 5 ] = "wood";
   indiceToNameMap[ 6 ] = "forward wood";
   indiceToNameMap[ 7 ] = "food grains";
   indiceToNameMap[ 8 ] = "coarse grains";
   indiceToNameMap[ 9 ] = "oil crops";
   indiceToNameMap[ 10 ] = "misc crops";
   indiceToNameMap[ 11 ] = "pasture";
   
}

//! Return the number of markets the AgLU model uses.
int AgSector::getNumAgMarkets() {
   return numAgMarkets;
}

//! Clear all data members.
void AgSector::clear() {
   regionNumber = 0;
   name = "";
   gnp.clear();
   population.clear();
   CO2Emissions.clear();
   prices.clear();
   supplies.clear();
   demands.clear();
}

//! Initialize the object with XML data.
void AgSector::XMLParse( const DOMNode* node ) {
   const Modeltime* modeltime = scenario->getModeltime();
   
   CO2Emissions.resize( modeltime->getmaxper() );
   prices.resize( modeltime->getmaxper() );
   supplies.resize( modeltime->getmaxper() );
   demands.resize( modeltime->getmaxper() );
   
   for( int i = 0; i < modeltime->getmaxper(); i++ ) {
      prices[ i ].resize( numAgMarkets );
      supplies[ i ].resize( numAgMarkets );
      demands[ i ].resize( numAgMarkets );
   }
}

//! Output the results in XML format.
void AgSector::toXML( ostream& out ) const {
   const Modeltime* modeltime = scenario->getModeltime();
   int iter = 0;
   
   // write the beginning tag.
   Tabs::writeTabs( out );
   out << "<agsector name=\"" << name << "\">"<< endl;
   
   // increase the indent.
   Tabs::increaseIndent();
   
   // write the xml for the class members.
   // write out the market string.
   XMLWriteElement( regionNumber, "regionNumber", out );
   XMLWriteElement( numAgMarkets, "numAgMarkets", out );
   
   for( iter = 0; iter < static_cast<int>( gnp.size() ); iter++ ){
      XMLWriteElement( gnp[ iter ], "gnp", out, modeltime->getper_to_yr( iter ) );
   }
   
   for( iter= 0; iter < static_cast<int>( population.size() ); iter++ ) {
      XMLWriteElement( population[ iter ], "population", out, modeltime->getper_to_yr( iter ) );
   }
   
   XMLWriteElement( biomassPrice, "biomassprice", out );
   
   // finished writing xml for the class members.
   
   // decrease the indent.
   Tabs::decreaseIndent();
   
   // write the closing tag.
   Tabs::writeTabs( out );
   out << "</agsector>" << endl;
}

//! Print the internal variables to XML output.
void AgSector::toDebugXML( const int period, ostream& out ) const {
   int iter = 0;
   int tempRegion = regionNumber; // Needed b/c function is constant.
   const Modeltime* modeltime = scenario->getModeltime();
   
   // write the beginning tag.
   Tabs::writeTabs( out );
   out << "<agsector name=\"" << name << "\">"<< endl;
   
   // increase the indent.
   Tabs::increaseIndent();
   
   // write the xml for the class members.
   // write out the market string.
   XMLWriteElement( regionNumber, "regionNumber", out );
   XMLWriteElement( numAgMarkets, "numAgMarkets", out );
   
   for( iter = 0; iter < static_cast<int>( gnp.size() ); iter++ ){
      XMLWriteElement( gnp[ iter ], "gnp", out, modeltime->getper_to_yr( iter ) );
   }
   
   for ( iter = 0; iter < modeltime->getmaxper(); iter++ ){
      XMLWriteElement( GETGNP( tempRegion, iter ), "gnpFromFortran", out, modeltime->getper_to_yr( iter ) );
   }
   
   for( iter = 0; iter < static_cast<int>( population.size() ); iter++ ) {
      XMLWriteElement( population[ iter ], "population", out, modeltime->getPopPeriodToYear( iter ) );
   }
   
   for ( iter = 1; iter < modeltime->getmaxpopdata(); iter++ ){
      XMLWriteElement( GETPOP( tempRegion, iter ), "popFromFortran", out, modeltime->getPopPeriodToYear( iter ) );
   }
   
   XMLWriteElement( biomassPrice, "biomassprice", out );
   
   XMLWriteElement( GETBIOMASSPRICE(), "biomasspriceFromFortran", out );
   
   for( iter = 0; iter < static_cast<int>( prices[ period ].size() ); iter++ ) {
      XMLWriteElement( prices[ period ][ iter ], "prices", out, modeltime->getper_to_yr( period ) );
   }
   
   for( iter = 0; iter < static_cast<int>( supplies[ period ].size() ); iter++ ) {
      XMLWriteElement( supplies[ period ][ iter ], "supplies", out, modeltime->getper_to_yr( period ) );
   }	
   for( iter = 0; iter < static_cast<int>( demands[ period ].size() ); iter++ ) {
      XMLWriteElement( demands[ period ][ iter ], "demands", out, modeltime->getper_to_yr( period ) );
   }
   // finished writing xml for the class members.
   
   // decrease the indent.
   Tabs::decreaseIndent();
   
   // write the closing tag.
   Tabs::writeTabs( out );
   out << "</agsector>" << endl;
}

//! Set the AgLU gnps from the regional gnp data.
void AgSector::setGNP( const vector<double>& gnpsToFortran ) {
   gnp = gnpsToFortran;
   
   double* toFortran = new double[ gnpsToFortran.size() ];
   
   for ( int i = 0; i < static_cast<int>( gnpsToFortran.size() ); i++ ) {
      toFortran[ i ] = gnpsToFortran[ i ];
      // toFortran[ i ] = readGDPS[ regionNumber ][ i ];
   }
   
   SETGNP( regionNumber, toFortran );
   delete[] toFortran;
}

//! Set the AgLU population data from the regional population data.
void AgSector::setPop( const vector<double>& popsToFortran ) {
   population = popsToFortran;
   
   double* toFortran = new double[ popsToFortran.size() ];
   
   for ( int i = 0; i < static_cast<int>( popsToFortran.size() ); i++ ) {
      toFortran[ i ] = popsToFortran[ i ];
   }
   
   SETPOP( regionNumber, toFortran );
   delete[] toFortran;
}

//! Set the AgLU biomass price from the market biomass price.
void AgSector::setBiomassPrice( const double bioPriceIn ) {
   biomassPrice = bioPriceIn;
   
   double* biomassPriceArray = new double[ 1 ];
   biomassPriceArray[ 0 ] = bioPriceIn;
   
   // SETBIOMASSPRICE( biomassPriceArray ); // new
}

//! Run the underlying AgLU model.
void AgSector::runModel( const int period, const string& regionName ) {
   Marketplace* marketplace = scenario->getMarketplace();
   
   double* priceArray = new double[ numAgMarkets ];
   double* demandArray = new double[ numAgMarkets ];
   double* supplyArray = new double[ numAgMarkets ];
   int tempRegionNumber = regionNumber;
   int tempPeriod = period;
   
   for( int l = 0; l < numAgMarkets; l++ ){
      prices[ period ][ l ] = marketplace->showprice( indiceToNameMap[ l ], regionName, period );
   }
   
   for( int i = 0; i < numAgMarkets; i++ ) {
      priceArray[ i ] = prices[ period ][ i ];
   }
   
   AG2RUN( priceArray, tempRegionNumber, tempPeriod, demandArray, supplyArray );
   
   for( int j = 0; j < numAgMarkets; j++ ) {
      demands[ period ][ j ] = demandArray[ j ];
      supplies[ period ][ j ] = supplyArray[ j ];
   }
   
   // set the market supplies and demands.
   for ( vector<string>::iterator k = marketNameVector.begin(); k != marketNameVector.end(); k++ ) {
      marketplace->setdemand( *k, regionName, demands[ period ][ nameToIndiceMap[ *k ] ], period );
      marketplace->setsupply( *k, regionName, supplies[ period ][ nameToIndiceMap[ *k ] ], period );
   }
   
   // set biomass supply
   marketplace->setsupply( "biomass", regionName, supplies[ period ][ nameToIndiceMap[ "biomass" ] ], period );
   
   delete priceArray;
   delete demandArray;
   delete supplyArray;
}

//! Use the underlying model to calculate the amount of CO2 emitted.
void AgSector::carbLand( const int period, const string& regionName ) {
   
   int tempRegionNumber = regionNumber;
   int tempPeriod = period;
   
   CO2Emissions[ period ] = AG2CO2EMISSIONS( tempPeriod, tempRegionNumber );
}

//! Create a market for the sector.
void AgSector::setMarket( const string& regionName ) {
   
   Marketplace* marketplace = scenario->getMarketplace();
   
   // Add all global markets.
   for( vector<string>::iterator i = marketNameVector.begin(); i != marketNameVector.end() - 1; i++ ) {
      marketplace->setMarket( regionName, "global", *i, Marketplace::NORMAL );
      marketplace->setMarketToSolve ( *i, regionName );
   }
   
   // Add the regional markets.
   marketplace->setMarket( regionName, regionName, marketNameVector[ 6 ], Marketplace::NORMAL );
   marketplace->setMarketToSolve ( marketNameVector[ 6 ], regionName );
   // Initialize prices at a later point.
}

//! Call the Ag modules internal output subroutine
void AgSector::internalOutput() {
   AG2LINKOUT();
}

//! Initialize the market prices for agricultural goods. 
void AgSector::initMarketPrices( const string& regionName, const vector<double>& pricesIn ) {
   
   Marketplace* marketplace = scenario->getMarketplace();
   
   // Initialize prices.
   for( vector<string>::iterator i = marketNameVector.begin(); i != marketNameVector.end(); i++ ) {
      marketplace->setprice( *i, regionName, pricesIn[ nameToIndiceMap[ *i ] ], 0 );
   }
}