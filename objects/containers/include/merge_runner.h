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
*/

#include <memory>
#include <string>
#include <list>
#include "containers/include/iscenario_runner.h"

class Scenario;
class Timer;

/*! 
* \ingroup CIAM
* \brief A class which runs the scenario only to get the merged output.
* \author Josh Lurz
*/
class MergeRunner: public IScenarioRunner {
	friend class ScenarioRunnerFactory;
public:
    virtual ~MergeRunner();

    virtual const std::string& getName() const;

    // IParsable interface
    virtual bool XMLParse( const xercesc::DOMNode* aRoot );

    virtual bool setupScenarios( Timer& timer,
                                 const std::string aName,
                                 const std::list<std::string> aScenComponents =
                                   std::list<std::string>() );
    
    virtual bool runScenarios( const int aSinglePeriod,
                               const bool aPrintDebugging,
                               Timer& aTimer );
    
    void printOutput( Timer& timer, const bool aCloseDB = true ) const;
	Scenario* getInternalScenario();
	virtual const Scenario* getInternalScenario() const;

protected:
    MergeRunner();
	static const std::string& getXMLNameStatic();
    
	//! The internal scenario.
	std::auto_ptr<Scenario> mScenario;
};
#endif // _MERGE_RUNNER_H_
