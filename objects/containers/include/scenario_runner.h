#ifndef _SCENARIO_RUNNER_H_
#define _SCENARIO_RUNNER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file scenario_runner.h
* \ingroup CIAM
* \brief The ScenarioRunner class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <iostream>

class Scenario;
class Timer;

/*! 
* \ingroup CIAM
* \brief A class which is responsible for running a scenario or set of scenarios.
* \detailed This is an abstract base class from which all scenario runner's are derived.
* A ScenarioRunner is defined as a method of running a scenario several times, varying a variable
* or set of variables. A scenario runner must inherit the run function. 
* \author Josh Lurz
*/
class ScenarioRunner {
public:
    ScenarioRunner( Scenario* scenarioIn );
    virtual ~ScenarioRunner();
    virtual void runScenario( Timer& timer ) = 0;
protected:
    Scenario* scenario;
    };
#endif // _SCENARIO_RUNNER_H_