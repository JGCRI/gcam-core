/*! 
* \file scenario_runner.cpp
* \ingroup CIAM
* \brief ScenarioRunner class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include "containers/include/scenario_runner.h"
#include "containers/include/scenario.h"

using namespace std;

/*! \brief Constructor
* \param scenarioIn A pointer to the scenario which will be run.
*/
ScenarioRunner::ScenarioRunner( Scenario* scenarioIn ){
    
    // Make sure the scenario is instantiated.
    assert( scenarioIn );

    // Initialize the pointer to the scenario.
    scenario = scenarioIn;
}

//! Destructor
ScenarioRunner::~ScenarioRunner(){
}
