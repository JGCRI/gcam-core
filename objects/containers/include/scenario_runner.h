#ifndef _SCENARIO_RUNNER_H_
#define _SCENARIO_RUNNER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file scenario_runner.h
* \ingroup Objects
* \brief The ScenarioRunner class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
#include <list>
#include <string>
class Timer;

/*! 
* \ingroup CIAM
* \brief A class which is responsible for running a scenario or set of scenarios.
* \details This is an abstract base class from which all scenario runner's are derived.
* A ScenarioRunner is defined as a method of running a scenario several times, varying a variable
* or set of variables. A scenario runner must inherit the run function. 
* \author Josh Lurz
*/
class ScenarioRunner {
public:
    ScenarioRunner(){};
    virtual ~ScenarioRunner(){};
    virtual bool setupScenario( Timer& timer, const std::string aName = std::string(), const std::list<std::string> aScenComponents = std::list<std::string>() ) = 0;
    virtual bool runScenario( Timer& timer ) = 0;
    virtual void printOutput( Timer& timer, const bool aCloseDB = true ) const = 0;
protected:
    };
#endif // _SCENARIO_RUNNER_H_

