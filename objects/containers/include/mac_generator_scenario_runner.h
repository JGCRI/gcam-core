#ifndef _MAC_GENERATOR_SCENARIO_RUNNER_H_
#define _MAC_GENERATOR_SCENARIO_RUNNER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file mac_generator_scenario_runner.h
* \ingroup CIAM
* \brief The MACGeneratorScenarioRunner class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <iostream>
#include "containers/include/scenario_runner.h"

class Timer;
class Scenario;

/*! 
* \ingroup CIAM
* \brief A ScenarioRunner that runs a scenario multiple times in order to generate a MAC curve for each time period.
* \detailed This class runs a scenario multiple times while varying a fixed carbon price,
* to determine the MAC curve for the scenario.
* \author Josh Lurz
*/
class MACGeneratorScenarioRunner: public ScenarioRunner {
public:
    MACGeneratorScenarioRunner( Scenario* scenarioIn );
    virtual ~MACGeneratorScenarioRunner();
    virtual void runScenario( Timer& timer );
private:
    void printOutput();
    void calculateAbatementCostCurve();
    };
#endif // _MAC_GENERATOR_SCENARIO_RUNNER_H_