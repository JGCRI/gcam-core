/*! 
* \file normal_market.cpp
* \ingroup CIAM
* \brief NormalMarket class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include "marketplace/include/normal_market.h"

using namespace std;

//! Constructor
NormalMarket::NormalMarket( const string& goodNameIn, const string& regionNameIn, const int periodIn ) :
Market( goodNameIn, regionNameIn, periodIn ) {
}

void NormalMarket::derivedToDebugXML( ostream& out, Tabs* tabs ) const {
}

string NormalMarket::getType() const {
   return "NormalMarket";
}

void NormalMarket::initPrice() {
   Market::initPrice();
}

void NormalMarket::setPrice( const double priceIn ) {
    Market::setPrice( priceIn );
}

void NormalMarket::setPriceFromLast( const double lastPrice ) {
   Market::setPriceFromLast( lastPrice );
}

double NormalMarket::getPrice() const {
    return Market::getPrice();
}

void NormalMarket::addToDemand( const double demandIn ) {
    Market::addToDemand( demandIn );
}

double NormalMarket::getDemand() const {
    return Market::getDemand();
}

void NormalMarket::nullSupply() {
   Market::nullSupply();
}

double NormalMarket::getSupply() const {
    return Market::getSupply();
}

double NormalMarket::getSupplyForChecking() const {
   return Market::getSupplyForChecking();
}

void NormalMarket::addToSupply( const double supplyIn ) {
    Market::addToSupply( supplyIn );
}

bool NormalMarket::meetsSpecialSolutionCriteria() const {
    return Market::meetsSpecialSolutionCriteria();
}

bool NormalMarket::shouldSolve() const {
   return Market::shouldSolve();
}

bool NormalMarket::shouldSolveNR() const {
   return Market::shouldSolveNR();
}
