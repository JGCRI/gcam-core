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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*! 
 * \file linked_market.cpp
 * \ingroup Objects
 * \brief LinkedMarket class source file.
 * \author Pralit Patel
 */

#include "util/base/include/definitions.h"
#include "marketplace/include/linked_market.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"

using namespace std;

///! Constructor
LinkedMarket::LinkedMarket( Market* aLinkedMarket, const MarketContainer* aContainer ):
Market( aContainer ),
mLinkedMarket( aLinkedMarket )
{
    mPriceMult = 1.0;
    mQuantityMult = 1.0;
}

void LinkedMarket::toDebugXMLDerived( ostream& out, Tabs* tabs ) const {
}

IMarketType::Type LinkedMarket::getType() const {
    return IMarketType::LINKED;
}


void LinkedMarket::initPrice() {
    mPriceMult = mMarketInfo->getDouble( "price-adjust", 1.0 );
    mQuantityMult = mMarketInfo->getDouble( "demand-adjust", 1.0 );
}

void LinkedMarket::setPrice( const double aPrice ) {
    // TODO: possibly could allow setting the linked market price.
}

void LinkedMarket::set_price_to_last_if_default( const double aLastPrice ) {
    // Linked markets do not use a price of their own.
}

void LinkedMarket::set_price_to_last( const double aLastPrice ) {
    // Linked markets do not use a price of their own.
}

double LinkedMarket::getPrice() const {
    return mLinkedMarket ? mLinkedMarket->getPrice() * mPriceMult
        : Marketplace::NO_MARKET_PRICE;
}

void LinkedMarket::nullDemand() {
    Market::nullDemand();
}

double LinkedMarket::getDemand() const {
    return Market::getDemand();
}

void LinkedMarket::addToDemand( const double aDemand ) {
    Market::addToDemand( aDemand );
    if( mLinkedMarket ) {
        mLinkedMarket->addToDemand( aDemand * mQuantityMult );
    }
}

void LinkedMarket::nullSupply() {
    Market::nullSupply();
}

double LinkedMarket::getSupply() const {
    return Market::getSupply();
}

void LinkedMarket::addToSupply( const double aSupply ) {
    Market::addToSupply( aSupply );
    if( mLinkedMarket ) {
        mLinkedMarket->addToSupply( aSupply * mQuantityMult );
    }
}

bool LinkedMarket::shouldSolve() const {
    // Linked markets should never be solved directly.
    return false;
}

bool LinkedMarket::shouldSolveNR() const {
    // Linked markets should never be solved directly.
    return false;
}

bool LinkedMarket::meetsSpecialSolutionCriteria() const {
    // Linked markets meet the special solution criteria that they are never
    // solved so allow the supply != demand
    return true;
}
