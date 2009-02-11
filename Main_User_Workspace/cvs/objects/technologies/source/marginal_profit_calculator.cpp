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
 * \file vintage_production_state.cpp
 * \ingroup Objects
 * \brief VintageProductionState class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <cassert>
#include "technologies/include/marginal_profit_calculator.h"
#include "technologies/include/technology.h"
#include "util/base/include/util.h"

using namespace std;

/*!
 * \brief Constructor.
 * \param aTechnology Technology for which to calculate marginal profits.
 */
MarginalProfitCalculator::MarginalProfitCalculator( const Technology* aTechnology )
: mTechnology( aTechnology )
{}

/*!
 * \brief Calculate the short term marginal profit as a proportion of non-energy
 *        costs.
 * \param aRegionName Region name.
 * \param aPeriod Model period.
 * \return Short term marginal profit as a proportion of non-energy costs.
 * \todo This calculation will have to be improved when a Technology has
 *       multiple and correctly differentiated fixed and variable costs. The
 *       code currently assumes that all non-energy costs are fixed, which would
 *       not be true for O&M for example.
 */
double MarginalProfitCalculator::calcShortTermMarginalProfit( const string& aRegionName,
                                                              const string& aSectorName,
                                                              const int aPeriod ) const
{
    double marginalRevenue = mTechnology->getMarginalRevenue( aRegionName,
                                                              aSectorName,
                                                              aPeriod );

    double variableCosts = mTechnology->getEnergyCost( aRegionName,
                                                       aSectorName,
                                                       aPeriod );
    
    // Variable costs are positive. Renewable technologies could
    // have zero variable costs.
    assert( variableCosts >= 0 );

    double nonEnergyCost = mTechnology->getCost( aPeriod ) - variableCosts;

    // Marginal profit is defined here as the percentage that price exceeds
    // variable costs
    double marginalProfit = (marginalRevenue - variableCosts) / variableCosts;
    return marginalProfit;
}
