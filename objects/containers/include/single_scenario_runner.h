#ifndef _SINGLE_SCENARIO_RUNNER_H_
#define _SINGLE_SCENARIO_RUNNER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file scenario_runner.h
* \ingroup Objects
* \brief The SingleScenarioRunner class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <memory>
#include <list>
#include "containers/include/scenario_runner.h"

class Timer;
class Scenario;

/*! 
* \ingroup Object
* \brief A class which is responsible for running a scenario or set of scenarios.
* \details This is an abstract base class from which all scenario runner's are derived.
* A SingleScenarioRunner is defined as a method of running a scenario several times, varying a variable
* or set of variables. A scenario runner must inherit the run function. 
* \author Josh Lurz
*/
class SingleScenarioRunner: public ScenarioRunner {
public:
    SingleScenarioRunner();
    virtual ~SingleScenarioRunner();
    virtual bool setupScenario( Timer& timer, const std::string aName = "", const std::list<std::string> aScenComponents = std::list<std::string>() );
    virtual bool runScenario( Timer& timer );
    virtual void printOutput( Timer& timer, const bool aCloseDB = true ) const;
protected:
    std::auto_ptr<Scenario> mScenario; //!< Autopointer to the main scenario.
    };
#endif // _SINGLE_SCENARIO_RUNNER_H_
