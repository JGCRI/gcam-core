#ifndef _SCENARIO_RUNNER_H_
#define _SCENARIO_RUNNER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file scenario_runner.h
* \ingroup Objects
* \brief The ScenarioRunner abstract base class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
#include <list>
#include <string>
class Timer;

/*! 
* \ingroup Objects
* \brief A class which is responsible for running a scenario or set of scenarios.
* \details This is an abstract base class from which specific types of scenario_runner must be derived.
* A ScenarioRunner is an object that defines a method of running a scenario several times, varying a variable
* or set of variables.  
* Each specific derived scenario_runner must define its own run function (runScenario) since it is not 
* defined in this ScenarioRunner abstract base class.
* \author Josh Lurz
*/
class ScenarioRunner {
public:
    ScenarioRunner(){};
    virtual ~ScenarioRunner(){};
    virtual bool setupScenario( Timer& aTimer, const std::string aName = std::string(), const std::list<std::string> aScenComponents = std::list<std::string>() ) = 0;
    virtual bool runScenario( Timer& timer ) = 0;
    virtual void printOutput( Timer& timer, const bool aCloseDB = true ) const = 0;
protected:
    };
#endif // _SCENARIO_RUNNER_H_

