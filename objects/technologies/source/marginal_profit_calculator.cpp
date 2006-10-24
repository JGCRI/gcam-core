/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
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

    double variableCosts = mTechnology->getFuelCost( aRegionName,
                                                     aSectorName,
                                                     aPeriod );
    
    // Variable costs are positive. Renewable technologies could
    // have zero variable costs.
    assert( variableCosts >= 0 );

    double nonEnergyCost = mTechnology->getNonEnergyCost( aPeriod );

    // If the non-energy cost is zero, return the absolute value. This should
    // not occur in practice because a technology with no capital cannot
    // be meaningfully vintaged.
    double marginalProfit = marginalRevenue - variableCosts;
    if( nonEnergyCost > util::getSmallNumber() ){
       marginalProfit /= nonEnergyCost;
    }
    return marginalProfit;
}
