/*! 
* \file ghg_market.cpp
* \ingroup Objects
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
* \todo Some of these changes for the carbon market might be beneficial to the general market, or end up being the same.
*/
void GHGMarket::initPrice() {
    // If price is near zero it needs to be initialized.
    if( price < util::getSmallNumber() ){
        // If this market should be solved price should be set to 1.
        if( solveMarket ){
            price = 1;
        }
        // The market will not be solved so it should be zero'd out. 
        else {
            price = 0;
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
* \param lastPrice Previous period's price. This should have already been set in store to last!!
* \author Josh Lurz
*/
void GHGMarket::setPriceFromLast( const double lastPrice ) {
    // If the price is zero and the solve flag is set so a constraint exists. 
    if( price < util::getSmallNumber() && solveMarket ){
        // If the last price is 0, we should set the price to 1.
        if( lastPrice < util::getSmallNumber() ){
            price = 1;
        }
        // Otherwise set the price to the previous period's price.
        else {
            price = lastPrice;
        }
    }
    // There is no else here becuase we do not want to override prices in the case of a fixed tax.
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

//! The supply in GHGMarket is the constraint, it should not be removed by calls to nullSupply
void GHGMarket::nullSupply() {
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
    // I think this is the default.
   return ( solveMarket );
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
    
   // New
   if( price < util::getSmallNumber() ){
       doSolveMarket = false;
   }

   return doSolveMarket;
}

//! Check whether the market meets market specific solution criteris.
bool GHGMarket::meetsSpecialSolutionCriteria() const {
    // If there is no constraint, this market is solved.
    if( !solveMarket ){
        return true;
    }

    // If price is zero demand cannot be driven any higher.
    // The constraint is still higher than the demand, so this market is solved.
    if( ( price < util::getSmallNumber() ) && ( supply > demand ) ){
        return true;
    }
    return false;
}
