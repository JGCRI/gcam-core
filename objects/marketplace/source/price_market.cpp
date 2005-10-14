/*! 
* \file price_market.cpp
* \ingroup Objects
* \brief PriceMarket class source file.
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include "marketplace/include/market.h"
#include "marketplace/include/price_market.h"
#include "util/base/include/xml_helper.h"

using namespace std;

//! Constructor
PriceMarket::PriceMarket( const string& goodNameIn, const string& regionNameIn, const int periodIn, Market* demandMarketIn ) :
Market( goodNameIn, regionNameIn, periodIn ) {
    assert( demandMarketIn );
    demandMarketPointer = demandMarketIn;
}

//! Copy Constructor.
PriceMarket::PriceMarket( const Market& marketIn, Market* demandMarketIn ) : Market( marketIn ) {
    assert( demandMarketIn );
    demandMarketPointer = demandMarketIn;
}

void PriceMarket::toDebugXMLDerived( ostream& out, Tabs* tabs ) const {
    XMLWriteElement( demandMarketPointer->getName(), "LinkedDemandMarket", out, tabs );
}

string PriceMarket::getType() const {
    return "PriceMarket";
}

void PriceMarket::initPrice() {
    Market::initPrice();
}

/*! \brief Set the price of the market based on the type.
*
* This method is used throughout the model to set a new price into a market. 
* But this is not used by the solution mechanism.
* For the price markets the price set directly to this market (priceIn) is the 
* actual price calculated by a supply sector. This is set to the demand side.
* The supply side is set here to equal the price variable (set directly by the solution mechanism)
* This sets up the solution mechanism to always give a trial value for this price.
*
* \author Josh Lurz
* \param priceIn The new price to set the market price to.
* \sa setRawPrice
* \sa setPriceToLast
*/
void PriceMarket::setPrice( const double priceIn ) {
    demand = priceIn;
    supply = price;
}

void PriceMarket::setPriceFromLast( const double lastPrice ) {
    Market::setPriceFromLast( lastPrice );
}

double PriceMarket::getPrice() const {
    return price; 
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

bool PriceMarket::meetsSpecialSolutionCriteria() const {
    return Market::meetsSpecialSolutionCriteria();
}

bool PriceMarket::shouldSolve() const {
    return Market::shouldSolve();
}

bool PriceMarket::shouldSolveNR() const {
    return Market::shouldSolveNR();
}
