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
* \file profit_shutdown_decider.cpp
* \ingroup Objects
* \brief ProfitShutdownDecider class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include "technologies/include/profit_shutdown_decider.h"
#include "functions/include/function_utils.h"
#include "functions/include/ifunction.h"
using namespace std;

//! Constructor
ProfitShutdownDecider::ProfitShutdownDecider(){
}

/*! \brief Calculate the coefficient which will scale and eventually shutdown
*          unprofitable production.
* \details This function returns a scaling factor which is used to decrease and
*          eventually shutdown profit and output of a vintage as it is nearing
*          zero profit. A smoothing is calculated near the zero profit point to
*          prevent a vintage from switching on and off abruptly. The
*          MIN_PROFIT_RATE variable determines the initial profit point where
*          smoothing begins. At the point of zero profits the vintage is
*          shutdown fully.
* \param aFuncInfo A structure containing the neccessary data items to
*        call the production technology's shutdown decision.
* \param aRegionName Name of the region in which the shutdown decision is occurring.
* \param aSectorName Name of the sector in which the shutdown decision is occurring.
* \param aPeriod Period in which to determine the scale factor.
* \return The scale factor used to reduce profits and output, zero if the
*         vintage is unprofitable.
* \author Josh Lurz
*/
double ProfitShutdownDecider::calcShutdownCoef( const ProductionFunctionInfo& aFuncInfo,
                                                const string& aRegionName,
                                                const string& aSectorName,
                                                const int aPeriod ) const 
{
    // Default scale factor not to scale.
    double scaleFactor = 1;
    // There is no shutdown decision in the base period.
    if( aPeriod > 0 ){
        // Calculate the unscaled profits.
        double profits = aFuncInfo.mProductionFunction->calcUnscaledProfits( aFuncInfo.mInputs,
                                                                             aRegionName,
                                                                             aSectorName,
                                                                             aPeriod,
                                                                             aFuncInfo.mCapitalStock, 
                                                                             aFuncInfo.mAlphaZeroScaler,
                                                                             aFuncInfo.mSigma );
        assert( profits >= 0 );
        assert( aFuncInfo.mCapitalStock > 0 );
        // Determine the profit per unit of capital.
        double profitRate = profits / aFuncInfo.mCapitalStock;

        // Determine if the profit rate is lower than the minimum for scaling.
        const static double MIN_PROFIT_RATE = 0.01;
        if( profitRate < MIN_PROFIT_RATE  ){
            double ratio = profitRate / MIN_PROFIT_RATE;
            // I think this is only right with a 5 year timestep.
            scaleFactor = 3 * pow( ratio, 2 ) - 2 * pow( ratio, 3 );
        }
    }

    /*! \post Scale factor is between 0 and 1. */
    assert( scaleFactor >= 0 && scaleFactor <= 1 );
    return scaleFactor;
}
