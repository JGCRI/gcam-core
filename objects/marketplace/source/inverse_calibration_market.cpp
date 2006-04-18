/*! 
* \file inverse_calibration_market.cpp
* \ingroup Objects
* \brief InverseCalibrationMarket class source file.
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <string>
#include "marketplace/include/inverse_calibration_market.h"
#include "util/base/include/util.h"

using namespace std;

//! Constructor
InverseCalibrationMarket::InverseCalibrationMarket( const string& goodNameIn, const string& regionNameIn, const int periodIn ) :
Market( goodNameIn, regionNameIn, periodIn ) {
}

//! Destructor
InverseCalibrationMarket::~InverseCalibrationMarket() {
}

void InverseCalibrationMarket::toDebugXMLDerived( ostream& out, Tabs* tabs ) const {
}

IMarketType::Type InverseCalibrationMarket::getType() const {
    return IMarketType::INVERSE_CALIBRATION;
}

void InverseCalibrationMarket::initPrice() {
    Market::initPrice();
}

void InverseCalibrationMarket::setPrice( const double priceIn ) {
    Market::setPrice( priceIn );
}

void InverseCalibrationMarket::setPriceFromLast( const double lastPrice ) {
   // Do nothing, as in a calibration market the initial values for each period
   // are set from the XML.
}

double InverseCalibrationMarket::getPrice() const {
    return Market::getPrice();
}

void InverseCalibrationMarket::addToDemand( const double demandIn ) {
    Market::addToDemand( demandIn );
}

double InverseCalibrationMarket::getDemand() const {
    return Market::getDemand();
}

void InverseCalibrationMarket::nullDemand() {
    // Differs from the CalibrationMarket because demand should be cleared each
    // iteration. In the CalibrationMarket, the constraint is stored in the demand
    // variable and so should not be cleared.
    Market::nullDemand();
}

void InverseCalibrationMarket::nullSupply() {
    // Differs from the CalibrationMarket because supply should not be cleared
    // each iteration. In the CalibrationMarket, the constraint is stored in the
    // demand variable and so supply is cleared each iteration.
}

double InverseCalibrationMarket::getSupply() const {
    return Market::getSupply();
}

double InverseCalibrationMarket::getSupplyForChecking() const {
    return Market::getSupplyForChecking();
}

void InverseCalibrationMarket::addToSupply( const double supplyIn ) {
    Market::addToSupply( supplyIn );
}

bool InverseCalibrationMarket::shouldSolve() const {
    return Market::shouldSolve();
}

bool InverseCalibrationMarket::shouldSolveNR() const {
    return Market::shouldSolveNR();
}

bool InverseCalibrationMarket::meetsSpecialSolutionCriteria() const {
    // Check if the market was set to solve without a constraint. Differs from
    // the CalibrationMarket because the supply variable is checked instead of
    // the demand variable because it is the constraint.
    return ( supply < util::getSmallNumber() );
}
