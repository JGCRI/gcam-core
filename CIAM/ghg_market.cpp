/*! 
* \file ghg_market.cpp
* \ingroup CIAM
* \brief GHGMarket class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "definitions.h"
#include <string>
#include "ghg_market.h"
#include "util.h"

using namespace std;

///! Constructor
GHGMarket::GHGMarket( const string& goodNameIn, const string& regionNameIn, const int periodIn ) :
Market( goodNameIn, regionNameIn, periodIn ) {
}

void GHGMarket::derivedToDebugXML( ostream& out ) const {
}

string GHGMarket::getType() const {
   return "GHGMarket";
}

void GHGMarket::initPrice() {
   price = 1;
}

void GHGMarket::setPrice( const double priceIn ) {
    Market::setPrice( priceIn );
}

void GHGMarket::setPriceFromLast( const double lastPrice ) {
   Market::setPriceFromLast( lastPrice );
}

double GHGMarket::getPrice() const {
    return Market::getPrice();
}

void GHGMarket::addToDemand( const double demandIn ) {
    Market::addToDemand( demandIn );
}

double GHGMarket::getDemand() const {
    return Market::getDemand();
}

void GHGMarket::nullSupply() {
   Market::nullSupply();
}

double GHGMarket::getSupply() const {
    return Market::getSupply();
}

double GHGMarket::getSupplyForChecking() const {
   return Market::getSupplyForChecking();
}

void GHGMarket::addToSupply( const double supplyIn ) {
    Market::addToSupply( supplyIn );
}

bool GHGMarket::shouldSolve() const {

   // Don't solve if  there is no constraint
   return ( solveMarket && supply > 1E-6 );
}

bool GHGMarket::shouldSolveNR() const {
   bool doSolveMarket = false;
   // Check if this market is supposed to be solved & if a significant demand exists
   if ( solveMarket && demand > util::getSmallNumber() ) {
      doSolveMarket = true;
   }
   
   if ( ( supply < util::getSmallNumber() ) ||  ( price < util::getSmallNumber() && ( demand - supply ) < util::getSmallNumber() ) || ( price < util::getSmallNumber() ) ) {
      doSolveMarket = false; 
   }

   return doSolveMarket;
}