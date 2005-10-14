/*! 
* \file calibration_market.cpp
* \ingroup Objects
* \brief CalibrationMarket class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include "marketplace/include/calibration_market.h"
#include "util/base/include/util.h"

using namespace std;

//! Constructor
CalibrationMarket::CalibrationMarket( const string& goodNameIn, const string& regionNameIn, const int periodIn ) :
Market( goodNameIn, regionNameIn, periodIn ) {
}

//! Destructor
CalibrationMarket::~CalibrationMarket() {
}

void CalibrationMarket::toDebugXMLDerived( ostream& out, Tabs* tabs ) const {
}

string CalibrationMarket::getType() const {
   return "CalibrationMarket";
}

void CalibrationMarket::initPrice() {
    Market::initPrice();
}

void CalibrationMarket::setPrice( const double priceIn ) {
    Market::setPrice( priceIn );
}

void CalibrationMarket::setPriceFromLast( const double lastPrice ) {
   // Do nothing, as in a calibration market the initial values for each period are set from the XML.
}

double CalibrationMarket::getPrice() const {
    return Market::getPrice();
}

void CalibrationMarket::addToDemand( const double demandIn ) {
    Market::addToDemand( demandIn );
}

double CalibrationMarket::getDemand() const {
    return Market::getDemand();
}

// Do nothing, as constraints should not be cleared.
void CalibrationMarket::nullDemand() {
}

void CalibrationMarket::nullSupply() {
    Market::nullSupply();
}

double CalibrationMarket::getSupply() const {
    return Market::getSupply();
}

double CalibrationMarket::getSupplyForChecking() const {
    return Market::getSupplyForChecking();
}

void CalibrationMarket::addToSupply( const double supplyIn ) {
    Market::addToSupply( supplyIn );
}

bool CalibrationMarket::shouldSolve() const {
    return Market::shouldSolve();
}

bool CalibrationMarket::shouldSolveNR() const {
    return Market::shouldSolveNR();
}

bool CalibrationMarket::meetsSpecialSolutionCriteria() const {
    // Check if the market was set to solve without a constraint.
    return ( demand < util::getSmallNumber() );
}

