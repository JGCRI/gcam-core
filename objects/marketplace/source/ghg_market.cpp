/*! 
* \file ghg_market.cpp
* \ingroup CIAM
* \brief GHGMarket class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include "marketplace/include/ghg_market.h"
#include "util/base/include/util.h"

using namespace std;

///! Constructor
GHGMarket::GHGMarket( const string& goodNameIn, const string& regionNameIn, const int periodIn ) :
Market( goodNameIn, regionNameIn, periodIn ) {
}

void GHGMarket::derivedToDebugXML( ostream& out, Tabs* tabs ) const {
}

string GHGMarket::getType() const {
   return "GHGMarket";
}

/* \brief Initialize the GHGMarket price.
* \details This method checks first if the price has already been initialized.
* If it has been initialized, the price is left unchanged. Otherwise the method checks the value of the constraint,
* or supply, to determine how to initialize the price. If supply is 0, price is set to 0. If the supply 
* is greater than 0, the price is initialized to 1.
* \author Josh Lurz
*/
void GHGMarket::initPrice() {
    // If price is near zero it needs to be initialized.
    if( price < util::getSmallNumber() ){
        // If supply is 0 price should be 0.
        if( supply < util::getSmallNumber() ){
            price = 0;
        }
        else {
            price = 1;
        }
    }
}

void GHGMarket::setPrice( const double priceIn ) {
    Market::setPrice( priceIn );
}

/* \brief Initialize the GHGMarket price from last period's price.
* \details This method first checks if the lastPrice was 0. This would mean that last period's constraint was 
* 0. If it is, then it checks if the constraint in the current period is greater than 0. In this case price is 
* set to 1 as this is the initial constrained period. Otherwise price is set to the previous period's price as
* is done in the normal market.
* \param lastPrice Previous period's price. 
* \author Josh Lurz
*/
void GHGMarket::setPriceFromLast( const double lastPrice ) {
    // If the price is zero and a constraint is set, we should initialize the price.
    if( price < util::getSmallNumber() && supply > util::getSmallNumber() ){
        // If the last price is 0, we should set the price to 0.
        if( lastPrice < util::getSmallNumber() ){
            price = 1;
        }
        // Otherwise set the price to the previous period's price.
        else {
            price = lastPrice;
        }
    }
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

/* \brief This method determines whether to solve a GHGMarket with the solution mechanism.
* \details This method only returns that the solution mechanism should attempt to solve the market
* if the supply is positive. This prevents the solution mechanism from trying to solve unconstrained 
* time periods.
* \return Whether to solve the market.
* \author Sonny Kim
*/
bool GHGMarket::shouldSolve() const {

   // Don't solve if  there is no constraint
   return ( solveMarket && supply > 1E-6 );
}

/* \brief This method determines whether to solve a GHGMarket with the NR solution mechanism.
* \details This method only returns that the NR solution mechanism should attempt to solve the market
* if the supply and demand is positive, and the excess demand is not miniscule. 
* \todo This needs a more detailed explanation, but the logic is convoluded.
* \return Whether to solve the market in NR.
* \author Sonny Kim
*/
bool GHGMarket::shouldSolveNR() const {
   bool doSolveMarket = false;
   // Check if this market is supposed to be solved & if a significant demand exists
   if ( solveMarket && demand > util::getSmallNumber() ) {
      doSolveMarket = true;
   }
   
   if ( ( supply < util::getSmallNumber() ) ||  ( price < util::getSmallNumber() && ( demand - supply ) < util::getSmallNumber() ) ) {
      doSolveMarket = false; 
   }

   return doSolveMarket;
}
