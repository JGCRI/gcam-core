/*! 
* \file price_market.cpp
* \ingroup CIAM
* \brief PriceMarket class source file.
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include "marketplace/include/price_market.h"
#include "util/base/include/xml_helper.h"

using namespace std;

//! Constructor
PriceMarket::PriceMarket( const string& goodNameIn, const string& regionNameIn, const int periodIn, Market* demandMarketIn ) :
Market( goodNameIn, regionNameIn, periodIn ) {
    assert( demandMarketIn );
    priceMultiplier = 1;
    demandMarketPointer = demandMarketIn;
}

//! Copy Constructor.
PriceMarket::PriceMarket( const Market& marketIn, Market* demandMarketIn ) : Market( marketIn ) {
    assert( demandMarketIn );
    priceMultiplier = 1;
    demandMarketPointer = demandMarketIn;
}

void PriceMarket::derivedToDebugXML( ostream& out, Tabs* tabs ) const {
    XMLWriteElement( priceMultiplier, "PriceMultiplier", out, tabs );
    XMLWriteElement( demandMarketPointer->getName(), "LinkedDemandMarket", out, tabs );
}

string PriceMarket::getType() const {
    return "PriceMarket";
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
    demandMarketPointer->addToDemand( demandIn );
}

double PriceMarket::getDemand() const {
    return demandMarketPointer->getDemand();
}

void PriceMarket::nullSupply() {
    Market::nullSupply();
}

double PriceMarket::getSupply() const {
    return demandMarketPointer->getSupply();
}

double PriceMarket::getSupplyForChecking() const {
    return Market::getSupplyForChecking();
}

void PriceMarket::addToSupply( const double supplyIn ) {
    demandMarketPointer->addToSupply( supplyIn );
}

bool PriceMarket::shouldSolve() const {
    return Market::shouldSolve();
}

bool PriceMarket::shouldSolveNR() const {
    return Market::shouldSolveNR();
}