/*! 
* \file price_market.cpp
* \ingroup CIAM
* \brief PriceMarket class source file.
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/

#include "definitions.h"
#include <string>
#include "price_market.h"
#include "XMLHelper.h"

using namespace std;

//! Constructor
PriceMarket::PriceMarket( const string& goodNameIn, const string& regionNameIn, const int periodIn ) :
Market( goodNameIn, regionNameIn, periodIn ) {
   priceMultiplier = 1;
   demandMarketPointer = 0;
}

//! Copy Constructor.
PriceMarket::PriceMarket( const Market& marketIn ) : Market( marketIn ) {
   priceMultiplier = 1;
   demandMarketPointer = 0;
}

void PriceMarket::derivedToDebugXML( ostream& out ) const {
   XMLWriteElement( priceMultiplier, "PriceMultiplier", out );
   
   if ( demandMarketPointer != 0 ) {
      XMLWriteElement( demandMarketPointer->getName(), "LinkedDemandMarket", out );
   }
}

string PriceMarket::getType() const {
   return "PriceMarket";
}

void PriceMarket::setCompanionMarketPointer( Market* pointerIn ) {
   assert( pointerIn );
   demandMarketPointer = pointerIn;
}

void PriceMarket::initPrice() {
   Market::initPrice();
}

void PriceMarket::setPrice( const double priceIn ) {
   demand = priceIn * priceMultiplier;
   supply = price * priceMultiplier;
}

void PriceMarket::setPriceFromLast( const double lastPrice ) {
   Market::setPriceFromLast( lastPrice );
}

double PriceMarket::getPrice() const {
   return price / priceMultiplier; 
}

void PriceMarket::addToDemand( const double demandIn ) {
   assert( demandMarketPointer );
   demandMarketPointer->addToDemand( demandIn );
}

double PriceMarket::getDemand() const {
   assert( demandMarketPointer );
   return demandMarketPointer->getDemand();
}

void PriceMarket::nullSupply() {
   Market::nullSupply();
}

double PriceMarket::getSupply() const {
   assert( demandMarketPointer );
   return demandMarketPointer->getSupply();
}

double PriceMarket::getSupplyForChecking() const {
   return Market::getSupplyForChecking();
}

void PriceMarket::addToSupply( const double supplyIn ) {
   // Pass the supply to the underlying supply market.
   assert( demandMarketPointer );
   demandMarketPointer->addToSupply( supplyIn );
}

bool PriceMarket::shouldSolve() const {
   return Market::shouldSolve();
}

bool PriceMarket::shouldSolveNR() const {
   return Market::shouldSolveNR();
}