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
* \file trial_value_market.cpp
* \ingroup CIAM
* \brief The TrialValueMarket class header file.
* \author Steve Smith
*/

#include "util/base/include/definitions.h"
#include "util/base/include/util.h"
#include <string>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include "marketplace/include/trial_value_market.h"

using namespace std;

//! Constructor
TrialValueMarket::TrialValueMarket( const string& goodNameIn, const string& regionNameIn, const int periodIn ) :
Market( goodNameIn, regionNameIn, periodIn ) {
}

void TrialValueMarket::toDebugXMLDerived( ostream& out, Tabs* tabs ) const {
}

IMarketType::Type TrialValueMarket::getType() const {
    return IMarketType::TRIAL_VALUE;
}

void TrialValueMarket::initPrice() {
    const double MIN_PRICE = 0.01;
    if ( price < util::getSmallNumber() ) {
        // Initialize price to a random number between MIN_PRICE 
        // and (1 + MIN_PRICE)
        srand( (unsigned)time( NULL ) );
        price = ((double) rand() / (double) RAND_MAX) + MIN_PRICE;
    }
}

void TrialValueMarket::setPriceFromLast( const double lastPrice ) {
   Market::setPriceFromLast( lastPrice );
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
