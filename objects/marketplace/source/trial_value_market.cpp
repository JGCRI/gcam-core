/*! 
* \file trial_value_market.cpp
* \ingroup CIAM
* \brief The trial_value_market class header file.
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include "marketplace/include/trial_value_market.h"

using namespace std;

//! Constructor
TrialValueMarket::TrialValueMarket( const string& goodNameIn, const string& regionNameIn, const int periodIn ) :
Market( goodNameIn, regionNameIn, periodIn ) {
}

void TrialValueMarket::toDebugXMLDerived( ostream& out, Tabs* tabs ) const {
}

string TrialValueMarket::getType() const {
   return "TrialValueMarket";
}

void TrialValueMarket::initPrice() {
   Market::initPrice();
}

void TrialValueMarket::setPrice( const double priceIn ) {
    Market::setPrice( priceIn );
}

void TrialValueMarket::setPriceFromLast( const double lastPrice ) {
   Market::setPriceFromLast( lastPrice );
}

double TrialValueMarket::getPrice() const {
    return Market::getPrice();
}

/*! \brief Add to the the Market an amount of demand in a method based on the Market's type.
* This is the only method that is different for the trial market type. 
* Here is where the price variable is copied to supply, thereby setting up the solution mechanism
* to solve for the trial value of this quantity.
*
* \author Steve Smith
*
* \param demandIn The new demand to add to the current demand.
* \sa setRawDemand
*/
void TrialValueMarket::addToDemand( const double demandIn ) {
    Market::addToDemand( demandIn );
    supply = price;
}

double TrialValueMarket::getDemand() const {
    return Market::getDemand();
}

void TrialValueMarket::nullSupply() {
   Market::nullSupply();
}

double TrialValueMarket::getSupply() const {
    return Market::getSupply();
}

double TrialValueMarket::getSupplyForChecking() const {
   return Market::getSupplyForChecking();
}

void TrialValueMarket::addToSupply( const double supplyIn ) {
    Market::addToSupply( supplyIn );
}

bool TrialValueMarket::meetsSpecialSolutionCriteria() const {
    // Trial value markets must be solved in all periods including the base
    // period.
    return false;
}

bool TrialValueMarket::shouldSolve() const {
   return Market::shouldSolve();
}

bool TrialValueMarket::shouldSolveNR() const {
   return Market::shouldSolveNR();
}
