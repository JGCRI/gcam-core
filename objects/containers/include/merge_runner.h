#ifndef _MERGE_RUNNER_H_
#define _MERGE_RUNNER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file merge_runner.h
* \ingroup CIAM
* \brief The ScenarioRunner class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <iostream>
#include <memory>
#include <string>
#include <list>
#include "containers/include/scenario_runner.h"

class Scenario;
class Timer;

/*! 
* \ingroup CIAM
* \brief A class which runs the scenario only to get the merged output.
* \author Josh Lurz
*/
class MergeRunner: public ScenarioRunner {
public:
    MergeRunner();
    ~MergeRunner();
    bool setupScenario( Timer& timer, const std::string aName, const std::list<std::string> aScenComponents = std::list<std::string>() );
    void runScenario( Timer& timer );
    void printOutput( Timer& timer, const bool aCloseDB = true ) const;
protected:
    std::auto_ptr<Scenario> mScenario;
    };
#endif // _MERGE_RUNNER_H_
