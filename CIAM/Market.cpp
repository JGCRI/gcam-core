/*! 
* \file Market.cpp
* \ingroup CIAM
* \brief Market class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <cassert>

#include "modeltime.h" 
#include "XMLHelper.h"
#include "Market.h"

using namespace std;

//! Default constructor.
Market::Market( const string& goodNameIn, const string& regionNameIn, const marketType typeIn ){
   
   name = goodNameIn;
   region = regionNameIn;
   type = typeIn;
   period = 0;
   priceMult = 1;
   price = 0;
   tprice = 0;
   demand = 0;
   tdemand = 0;
   supply = 0;
   demMktSupply = 0;
   tsupply = 0;
   exdmd = 0;
   lexdmd = 0;
   texdmd = 0;
   dexdmd = 0;
   ldem = 0;
   lsup = 0;
   solveMarket = false;
}

//! Write out XML for debugging purposes.
void Market::toDebugXML( const int period, ostream& out ) const {
   
   
   // write the beginning tag.
   Tabs::writeTabs( out );
   out << "<Market name=\""<< name << "\">" << endl;
   
   // increase the indent.
   Tabs::increaseIndent();
   
   XMLWriteElement( region, "region", out );
   XMLWriteElement( type, "type", out );
   XMLWriteElement( period, "period", out );
   XMLWriteElement( price, "price", out );
   XMLWriteElement( tprice, "tprice", out );
   XMLWriteElement( demand, "demand", out );
   XMLWriteElement( tdemand, "tdemand", out );
   XMLWriteElement( supply, "supply", out );
   XMLWriteElement( tsupply, "tsupply", out );
   XMLWriteElement( exdmd, "exdmd", out );
   XMLWriteElement( lexdmd, "lexdmd", out );
   XMLWriteElement( texdmd, "texdmd", out );
   XMLWriteElement( dexdmd, "dexdmd", out );
   XMLWriteElement( ldem, "ldem", out );
   XMLWriteElement( lsup, "lsup", out );
   
   for( vector<string>::const_iterator i = containedRegionNames.begin(); i != containedRegionNames.end(); i++ ) {
      XMLWriteElement( *i, "ContainedRegion", out );
   }
   // maybe add more to write out.
   
   // finished writing xml for the class members.
   
   // decrease the indent.
   Tabs::decreaseIndent();
   
   // write the closing tag.
   Tabs::writeTabs( out );
   out << "</Market>" << endl;
}

//! Add a region to the contained regions.
void Market::addRegion( const string& regionNameIn ) {
   if ( std::find( containedRegionNames.begin(), containedRegionNames.end(), regionNameIn ) == containedRegionNames.end()	) {
      containedRegionNames.push_back( regionNameIn );
   }
}

//! Set an initial price for the market.
void Market::initPrice() {
   switch( type ) {
   case GHG:
      price = 0;
      break;
   default:
      // Generally want a non-zero value as starting value for other markets
      if ( price == 0 ) {	// don't set if already set by read-in
         price = 1;	
      }
      break;
   }
}

//! Null the price.
void Market::nullPrice() {
   price = 0;
}

//! Set the price without checking the type.
void Market::setActualPrice( const double priceIn ) {
   price = priceIn;
}

//! Set the price in a method depending on the type.
void Market::setPrice( const double priceIn ) {
   
   // If is price market, then set "supply" to passed-in price
   if ( type == PRICE ) {
      // Note, can't use function setsupply here since that would be re-directed
      // reversed (the other order does not work)
      demand = priceIn * priceMult;
      supply = price * priceMult;
   }
   else {	
      // Otherwise, set the price as normal
      price = priceIn;
   }
}

//! Set the price using the price from the last period.
void Market::setPriceToLast( const double lastPrice ) {
   if( type != CALIBRATION ) {
      price = lastPrice;
   }
}

//! Get the price.
double Market::getPrice() const {
   
   double retValue;

   if( name == "crude oil" ){ 
      retValue = 0 * 1;
   }

   if( name == "renewable" ){ // total guess
      retValue = 0;
   }
   else {
         if ( type == PRICE ) {
            retValue = price / priceMult ; 
         }
         else {
            retValue = price; 
         }
      }
   return retValue;
}

//! Get the actual price.
double Market::getActualPrice() const {
   return price;
}

//! Restore the previous price.
void Market::restorePrice() {
   price = tprice;
}

//! Get the change in price relative to the previous price.
double Market::getChangeInPrice() const {
   double change;
   
   if( tprice == 0  ){ 
      change = 0;
   }
   else {
      change = ( price - tprice ) / tprice;
   }
   return change;
}

//! Get the log of the change in price relative to the previous price.
double Market::getLogChangeInPrice( const double SMALL_NUM ) const {
   
   double change;
   // Case 1: Price or Previous price is 0.
   if( ( tprice == 0 ) || ( price == 0 ) ) {
      change = SMALL_NUM;
   }
   
   // Case 2: Price and Previous Price are both positive. 
   else if( ( tprice > 0 ) && ( price > 0 ) ) {
      change = log( price ) - log( tprice );
   }
   
   // Case 3: Price or Previous Price are negative. This should not occur.
   else {
      assert( false );
   }
   return change;
}

//! Null the demand.
void Market::nullDemand() {
   demand = 0;
}

//! Set the demand.
void Market::setDemand( const double value ) {
   demand = value;
}

//! Increment the demand.
void Market::incrementDemand( const double value ) {
   demand += value;
}

//! Get the demand
double Market::getDemand() const {
   return demand;
}

//! Calculate the natural log of the demand.
void Market::calcLogDemand( const double SMALL_NUM ) {
   ldem = log( max( demand, SMALL_NUM ) );
}

//! Return the natural log of the demand.
double Market::getLogDemand() const {
   return ldem;
}

//! Return the change in demand relative to the previous demand.
double Market::getChangeInDemand() const {
   
   double change;
   
   if( tdemand == 0 ){ 
      change = 0;
   }
   else { 
      change = ( demand - tdemand ) / tdemand;
   }
   return change;
}

//! Get the log of the change in demand relative to the previous demand.
double Market::getLogChangeInDemand( const double SMALL_NUM ) const {
   
   double change;
   // Case 1: Demand or Previous Demand is zero.
   if( tdemand == 0 || demand == 0 ) {
      change = SMALL_NUM;
   }
   
   // Case 2: Demand and Previous Demand are both positive.
   else if( ( demand > 0 ) && ( tdemand > 0 ) ) {
      change = log( demand ) - log( tdemand );
   }
   
   // Case 3: Demand or Previous Demand is negative. This should not occur.
   else {
      assert( false );
   }
   
   return change;
}

//! Null the supply.
void Market::nullSupply() {
   supply = 0;
   demMktSupply = 0;
}

//! Get the supply.
double Market::getSupply() const {
   return supply;
}

//! Get the supply for use in checking solution, including actual supply for demand market.
double Market::getActualSupply() const {
   
   // If is price market, then get supply instead for the corresponding demand market
   if ( type == DEMAND ) {
      return demMktSupply;
   }
   // Otherwise, get supply as normal
   else {
      return supply;
   }
}

//! Calculate the natural log of the supply.
void Market::calcLogSupply( const double SMALL_NUM ) {
   lsup = log( max( supply, SMALL_NUM ) );
}

//! Get the natural log of the supply.
double Market::getLogSupply() const {
   return lsup;
}

//! Get the change in the supply relative to the previous supply.
double Market::getChangeInSupply() const {
   
   double change;
   
   if( tsupply == 0 ){ 
      change = 0;
   }
   else {
      change = ( supply - tsupply ) / tsupply;
   }
   return change;
}

//! Get the log of the change in supply relative to the previous supply.
double Market::getLogChangeInSupply( const double SMALL_NUM ) const {
   
   double change;
   // Case 1: Supply or Previous Supply is zero.
   if( tsupply == 0 || supply == 0 ) {
      change = SMALL_NUM;
   }
   
   // Case 2: Supply and Previous Supply are both positive.
   else if( ( supply > 0 ) && ( tsupply > 0 ) ) {
      change = log( supply ) - log( tsupply );
   }
   
   // Case 3: Supply or Previous Supply is negative. This should not occur.
   else {
      assert( false );
   }
   
   return change;
}

//! Calculate the excess demand, setting the exdmd variable.
void Market::calcExcessDemand() {
   exdmd = demand - supply;
}

//! Return the excess demand.
double Market::getExcessDemand() const {
   return exdmd;
}

//! Calculate the log base 10 of the excess demand, setting the lexdmd variable.
void Market::calcLogExcessDemand( const double SMALL_NUM ) {
   if ( demand < SMALL_NUM && supply < SMALL_NUM ) {
      lexdmd = 0; // logs may not be calculated exactly
   }
   else {
      lexdmd = log10( max( demand, SMALL_NUM ) ) - log10( max( supply, SMALL_NUM ) ); 
   }
}

//! Return the log base 10 of the excess demand.
double Market::getLogExcessDemand() const {
   return lexdmd;
}

//! Set the period
void Market::setPeriod( const int periodIn ) {
   period = periodIn;
}

//! Return the market name.
string Market::getName() const {
   return region + name;
}

//! Return the market good name.
string Market::getGoodName() const {
   return name;
}

//! Return the market region.
string Market::getRegionName() const {
   return region;
}

//! Return the market type.
Market::marketType Market::getType() const {
   return type;
}

//! Store info from last period.
void Market::storeInfoFromLast( const double lastDemand, const double lastSupply, const double lastPrice ) {
   tdemand = lastDemand;
   tsupply = lastSupply;
   tprice = lastPrice;
}

//! Store the current demand, supply, and price.
void Market::storeInfo() {
   tdemand = demand;
   tsupply = supply;
   tprice = price;
}

//! Restore the previous demand, supply, and price.
void Market::restoreInfo() {
   demand = tdemand;
   supply = tsupply;
   price = tprice;
}

//! Set the solveMarket flag.
void Market::setSolveMarket( const bool doSolve ) {
   solveMarket = doSolve;
}

//! Print the market to the given output stream.
void Market::print( ostream& out ) const {
   
   out << "Market Period: " << period << endl;
   out << "Market Name: " << name << endl;
   out << "Market Region: " << region << endl;
   out << "Market Price: " << price << endl;
   out << "Market Supply: " << supply << endl;
   out << "Market Demand: " << demand << endl << endl;
}