/*! 
* \file demand_market.cpp
* \ingroup CIAM
* \brief DemandMarket class source file.
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include "marketplace/include/demand_market.h"
#include "util/base/include/xml_helper.h"

using namespace std;

//! Constructor
DemandMarket::DemandMarket( const string& goodNameIn, const string& regionNameIn, const int periodIn ) :
Market( goodNameIn, regionNameIn, periodIn ) {
   demMktSupply = 0;
}

void DemandMarket::derivedToDebugXML( ostream& out, Tabs* tabs ) const {
   XMLWriteElement( demMktSupply, "DemandMarketSupply", out, tabs );
}

string DemandMarket::getType() const {
   return "DemandMarket";
}

void DemandMarket::initPrice() {
    Market::initPrice();
}

void DemandMarket::setPrice( const double priceIn ) {
    Market::setPrice( priceIn );
}

void DemandMarket::setPriceFromLast( const double lastPrice ) {
    Market::setPriceFromLast( lastPrice );
}

double DemandMarket::getPrice() const {
    return Market::getPrice();
}

void DemandMarket::addToDemand( const double demandIn ) {
    Market::addToDemand( demandIn );
}

double DemandMarket::getDemand() const {
   return price;
}

void DemandMarket::nullSupply() {
   supply = 0;
   demMktSupply = 0;
}

double DemandMarket::getSupply() const {
    return Market::getSupply();
}

double DemandMarket::getSupplyForChecking() const {
   return demMktSupply;
}

void DemandMarket::addToSupply( const double supplyIn ) {
   supply = price; 
   demMktSupply += supplyIn; // Store Raw supply value to check later
}

bool DemandMarket::meetsSpecialSolutionCriteria() const {
    return Market::meetsSpecialSolutionCriteria();
}

bool DemandMarket::shouldSolve() const {
    return Market::shouldSolve();
}

bool DemandMarket::shouldSolveNR() const {
    return Market::shouldSolveNR();
}
