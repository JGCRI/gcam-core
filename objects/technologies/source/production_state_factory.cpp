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
 * \file production_state_factory.cpp
 * \ingroup Objects
 * \brief ProductionStateFactory source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <string>
#include "technologies/include/production_state_factory.h"
#include "util/base/include/model_time.h"
#include "containers/include/scenario.h"

// Add new types here.
#include "technologies/include/fixed_production_state.h"
#include "technologies/include/variable_production_state.h"
#include "technologies/include/vintage_production_state.h"
#include "technologies/include/retired_production_state.h"

extern Scenario* scenario;

using namespace std;

/*
* \brief Create the production state for a Technology in a given period.
* \details This function is called at the beginning of each period to create an
*          object that determines how a Technology will determine its output
*          level for the period. All Technologies begin in a retired state, this
*          represents that they have not been built yet. In the initial
*          production period of a Technology, it will set its production state
*          to either fixed or variable. The fixed state is used when a level of
*          fixed investment is specified by the data, the variable state is used
*          otherwise. In the period after the Technology's initial production
*          period, the Technology will change it's production state to either
*          vintaged or retired. Vintaged Technologies will continue in the same
*          state until their lifetime is exceeded at which point they will enter
*          the retired state. Retired Technologies do not produce any output.
* \param aInvestYear The year in which the Technology was new investment.
* \param aLifetimeYears The maximum number of years the Technology may operate.
* \param aFixedOutput The fixed output level, or
*        IProductionState::fixedOutputDefault if there is none.
* \param aInitialOutput The output in the initial operating period of the
*        technology, or IProductionState::fixedOutputDefault if it cannot be
*        calculated.
* \param aPeriod Model period.
*/
auto_ptr<IProductionState> ProductionStateFactory::create( const int aInvestYear,
                                                           const int aLifetimeYears,
                                                           const double aFixedOutput,
                                                           const double aInitialOutput,
                                                           const int aPeriod )
{
    // Initialize the production state.
    auto_ptr<IProductionState> newState;
    int currYear = scenario->getModeltime()->getper_to_yr( aPeriod );

    if( aInvestYear == currYear ){
        // If the new vintage has fixed output use a fixed output production
        // state.
        if( aFixedOutput != IProductionState::fixedOutputDefault() ){
            newState.reset( new FixedProductionState );
            newState->setBaseOutput( aFixedOutput, aInvestYear );
            
        }
        // Otherwise use a variable production state.
        else {
            newState.reset( new VariableProductionState );
        }
    }
    // Check if it is a still operating vintage.
    else if( ( currYear > aInvestYear ) &&
        ( aInvestYear + aLifetimeYears > currYear ) ){
        assert( aPeriod > 0 );
        newState.reset( new VintageProductionState );
        // Set the base level of output to the output in the initial investment
        // year.
        newState->setBaseOutput( aInitialOutput, aInvestYear );
    }
    // Otherwise it is retired. This may occur if the technology has not been
    // created yet as well.
    else {
        newState.reset( new RetiredProductionState );
    }
    return newState;
}
