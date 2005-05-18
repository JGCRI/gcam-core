/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Labratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responisbility for the 
	use of this software.
*/

/*! 
* \file variable_cost_shutdown_decider.cpp
* \ingroup Objects
* \brief VariableCostShutdownDecider class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include "technologies/include/variable_cost_shutdown_decider.h"
#include "functions/include/function_utils.h"
#include "functions/include/ifunction.h"
#include "containers/include/scenario.h" // for marketplace.
#include "marketplace/include/marketplace.h"

using namespace std;

extern Scenario* scenario;

//! Constructor
VariableCostShutdownDecider::VariableCostShutdownDecider(){
}

/*! \brief Function which creates a duplicate of this object.
* \details Cloning is neccessary so that this object can be copied without knowing its type.
* \return A copy of this object.
* \warning The caller is responsible for memory allocated by this function.
* \author Josh Lurz
*/
VariableCostShutdownDecider* VariableCostShutdownDecider::clone() const {
    return new VariableCostShutdownDecider( *this );
}

/*! \brief Calculate the coefficient which will scale and eventually shutdown
*          unprofitable production.
* \details FIX THIS COMMENT! This function returns a scaling factor which is
*          used to decrease and eventually shutdown profit and output of a
*          vintage as it is nearing zero profit. A smoothing is calculated near
*          the zero profit point to prevent a vintage from switching on and off
*          abruptly. The MIN_PROFIT_RATE variable determines the initial profit
*          point where smoothing begins. At the point of zero profits the
*          vintage is shutdown fully.
* \param aFuncInfo A structure containing the neccessary data items to call the
*        production technology's shutdown decision.
* \param aRegionName Name of the region in which the shutdown decision is
*        occurring.
* \param aSectorName Name of the sector in which the shutdown decision is
*        occurring.
* \param aPeriod Period in which to determine the scale factor.
* \return The scale factor used to reduce profits and output, zero if the
*         vintage is unprofitable.
* \note Unit variable costs must be used instead of the total as the calcCosts
*       function relies on demand currencies being known, which cannot be true
*       before the shutdown decision is made.
* \author Josh Lurz
*/
double VariableCostShutdownDecider::calcShutdownCoef( const ProductionFunctionInfo& aFuncInfo,
                                                const string& aRegionName,
                                                const string& aSectorName,
                                                const int aPeriod ) const 
{
    // There is no shutdown decision in the base period.
    // Default scale factor not to scale.
    double scaleFactor = 1;
    if( aPeriod > 0 ){
        // Calculate the variable cost. 
        double variableCost = aFuncInfo.mProductionFunction->calcVariableCost( aFuncInfo.mInputs,
                                                                               aRegionName,
                                                                               aSectorName,
                                                                               aPeriod,
                                                                               aFuncInfo.mAlphaZeroScaler,
                                                                               aFuncInfo.mSigma );
        assert( variableCost >= 0 );
        // Get the price received for the sector from the marketplace.
        double priceReceived = scenario->getMarketplace()->getMarketInfo( aSectorName,
                                                                          aRegionName,
                                                                          aPeriod,
                                                                          "priceReceived",
                                                                          true );
        assert( priceReceived > 0 );
        // Calculate the profit rate as the price received minus the variable cost.
        // This is not the true profit rate.
        double profitRate = priceReceived - variableCost;

        // Determine if the profit rate is lower than the minimum for scaling. This
        // minimum is higher than scaling based on profit as that is a profit
        // capital ratio.
        const static double MIN_PROFIT_RATE = 0.05;
        // If the profit rate is negative, fully shut the vintage down.
        if( profitRate <= 0 ){
            scaleFactor = 0;
        }
        else if( profitRate < MIN_PROFIT_RATE ){
            double ratio = profitRate / MIN_PROFIT_RATE;
            // I think this is only right with a 5 year timestep.
            scaleFactor = max( 3 * pow( ratio, 2 ) - 2 * pow( ratio, 3 ), 0.0 );
        }
    }
    /*! \post Scale factor is between 0 and 1. */
    assert( scaleFactor >= 0 && scaleFactor <= 1 );
    return scaleFactor;
}
