/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


/*! 
* \file market_RES.cpp
* \ingroup Objects
* \brief MarketRES class source file. Derived from GHGMarket.
* \author MW  
*/

#include "util/base/include/definitions.h"
#include "marketplace/include/market_RES.h"
#include "util/base/include/util.h"
#include "containers/include/iinfo.h"

using namespace std;

///! Constructor
MarketRES::MarketRES( const MarketContainer* aContainer ) :
  Market( aContainer ) {
}

void MarketRES::toDebugXMLDerived( ostream& out, Tabs* tabs ) const {
}

IMarketType::Type MarketRES::getType() const {
    return IMarketType::RES;
}

/*!
 * \brief Get the default price to use when a constraint is set and no other
 *        price information is available.
 * \return The appropriate default price to use for this market.
 */
double MarketRES::getDefaultPrice() const {
    const double DEFAULT_PRICE = 1;
    return DEFAULT_PRICE;
}

/* \brief Initialize the MarketRES price.
* \details This method initializes the price of the tax market to 1
* or null if not a solved market.
* \author Josh Lurz, Sonny Kim
*/
void MarketRES::initPrice() {
    // If price is near zero it needs to be initialized.
    if( !mPrice.isInited() ){
        if( mSolveMarket ){
            mPrice = getDefaultPrice();
        }
        // The market will not be solved so it is set to null. 
        else {
            mPrice = 0;
        }
    }
    // get the minimum price from the market info
    const string LOWER_BOUND_KEY = "lower-bound-supply-price";
    mMinPrice = mMarketInfo->getDouble( LOWER_BOUND_KEY, 0.0 );
}

void MarketRES::setPrice( const double priceIn ) {
    Market::setPrice( priceIn );
}

/* \brief Initialize the MarketRES price from last period's price.
* \details This method checks if the lastPrice was 0. This would mean that last period's constraint was 
* 0. If it is, price is set to 1 as this is the initial constrained period.
* Otherwise price is set to the previous period's price as is done in the normal market.
* \param lastPrice Previous period's price. This should have already been set in store to last!!
* \author Josh Lurz, Sonny Kim
*/
void MarketRES::set_price_to_last_if_default( const double lastPrice ) {
    // If the price is zero and the solve flag is set so a constraint exists. 
    if( mPrice == getDefaultPrice() && mSolveMarket ){
        if( lastPrice < util::getSmallNumber() ){
            mPrice = getDefaultPrice();
        }
        // Otherwise set the price to the previous period's price.
        else {
            mPrice = lastPrice;
        }
    }
    // There is no else here because we do not want to override prices in the case of a fixed tax.
}

/* \brief Initialize the MarketRES price from last period's price.
* \details This method checks if the lastPrice was 0. This would mean that last period's constraint was 
* 0. If it is, price is set to 1 as this is the initial constrained period.
* Otherwise price is set to the previous period's price as is done in the normal market.
* \param lastPrice Previous period's price. This should have already been set in store to last!!
* \author Josh Lurz, Sonny Kim
*/
void MarketRES::set_price_to_last( const double lastPrice ) {
    // If the price is zero and the solve flag is set so a constraint exists. 
    if( mSolveMarket ){
        if( lastPrice < util::getSmallNumber() ){
            mPrice = getDefaultPrice();
        }
        // Otherwise set the price to the previous period's price.
        else {
            mPrice = lastPrice;
        }
    }
    // There is no else here because we do not want to override prices in the case of a fixed tax.
}

double MarketRES::getPrice() const {
    // Constraint type markets may be non-binding even at a price of mMinPrice.
    // In this case the solver will generate a "correction" by exploring potentially
    // negative prices.  The model calculations should not see these negative
    // prices so we cap it at mMinPrice.
    return std::max( Market::getPrice(), mMinPrice );
}

void MarketRES::addToDemand( const double demandIn ) {
    Market::addToDemand( demandIn );
}

double MarketRES::getDemand() const {
    return Market::getDemand();
}

void MarketRES::nullDemand() {
    Market::nullDemand();
}

//! The supply in MarketRES is the constraint, but unlike a fixed constraint
// like a CO2 target it is updated each iteration and should call nullSupply
void MarketRES::nullSupply() {
	Market::nullSupply();
}

double MarketRES::getSupply() const {
    return Market::getSupply();
}

double MarketRES::getSolverSupply() const {
    return Market::getSolverSupply();
}

void MarketRES::addToSupply( const double supplyIn ) {
    Market::addToSupply( supplyIn );
}

/* \brief This method determines whether to solve a MarketRES with the solution mechanism.
* \details This method returns false for solving the tax market if constraints are not binding
* when the price is essentially null. Otherwise, it returns the default solveMarket boolean
* that is set to true for a valid market by the marketplace object.  The check for binding 
* constraints prevents wasted efforts in the solution mechanism, such as in finding bracket 
* intervals and running bisection.
* \return Whether to solve the market.
* \author Sonny Kim
*/
bool MarketRES::shouldSolve() const {
    return mSolveMarket;
}

/* \brief This method determines whether to solve a MarketRES with the NR solution mechanism.
* \details This method only returns that the NR solution mechanism should attempt to solve the market
* if a constraint exists and is not binding with a null price
* \return Whether to solve the market in NR.
* \author Sonny Kim
*/
bool MarketRES::shouldSolveNR() const {
    return shouldSolve();
}

/* \brief Check whether the market meets market specific solution criteris.
* \details This method returns true if market is solved or if price is null 
* and constraint is not binding.
* \return Whether special solution criteria is met.
* \author Sonny Kim
*/
bool MarketRES::meetsSpecialSolutionCriteria() const {
    // If there is no constraint, this market is solved.
    if( !mSolveMarket ){
        return true;
    }

    // If price is below the min-price, demand cannot be driven any higher.
    // The constraint is not binding (greater than the demand), so this market is solved.
    if( ( mPrice <= mMinPrice ) && ( getRawSupply() >= getRawDemand() ) ){
		return true;
    }
    return false;
}
