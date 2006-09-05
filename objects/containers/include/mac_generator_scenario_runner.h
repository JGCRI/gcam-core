#ifndef _MAC_GENERATOR_SCENARIO_RUNNER_H_
#define _MAC_GENERATOR_SCENARIO_RUNNER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file mac_generator_scenario_runner.h
* \ingroup Objects
* \brief The MACGeneratorScenarioRunner class header file.
* \author Josh Lurz
*/

#include <memory>
#include "containers/include/iscenario_runner.h"

class Timer;
class TotalPolicyCostCalculator;

/*! 
* \ingroup Objects
* \brief A derived ScenarioRunner class that runs a scenario multiple times in
*        order to generate a marginal abatement cost (MAC) curve for each time
*        period.
* \details This class runs a scenario multiple times while varying a fixed
*          carbon price, to determine the MAC curve and total cost for the
*          scenario.
* \author Josh Lurz
*/
class MACGeneratorScenarioRunner: public IScenarioRunner {
    friend class ScenarioRunnerFactory;
public:
    virtual ~MACGeneratorScenarioRunner();

    // IParsable interface
    virtual bool XMLParse( const xercesc::DOMNode* aRoot );

    virtual bool setupScenarios( Timer& timer,
        const std::string aName = "",
        const std::list<std::string> aScenComponents = std::list<std::string>() );
    
    virtual bool runScenarios( const int aSinglePeriod,
                              Timer& timer );

    virtual void printOutput( Timer& timer,
        const bool aCloseDB = true ) const;

    virtual Scenario* getInternalScenario();
    virtual const Scenario* getInternalScenario() const;
private:
    //! The scenario runner which controls running the initial scenario, and all
    //! fixed taxed scenarios after.
    std::auto_ptr<IScenarioRunner> mSingleScenario;

    //! The delegate object which calculates total costs.
    std::auto_ptr<TotalPolicyCostCalculator> mPolicyCostCalculator;

    MACGeneratorScenarioRunner();
    static const std::string& getXMLNameStatic();
};
#endif // _MAC_GENERATOR_SCENARIO_RUNNER_H_
