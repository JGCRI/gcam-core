#ifndef _ISCENARIO_RUNNER_H_
#define _ISCENARIO_RUNNER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file scenario_runner.h
* \ingroup Objects
* \brief The IScenarioRunner interface header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
#include <list>
#include <string>

class Timer;
class Scenario;

/*! 
* \ingroup Objects
* \brief An interface which is responsible for running a scenario or set of
*        scenarios.
* \details
* \author Josh Lurz
*/
class IScenarioRunner {
public:
	/*! \brief Virtual destructor so that derived instances can be deleted
    *          through the base class pointer.
    */
    virtual inline ~IScenarioRunner();

	/*! \brief Setup the ScenarioRunner before running a single or series of
    *          scenarios. 
	* \details This method must be called to setup the scenario runner before
    *          the runScenario method is called. This performs initialization
    *          for the scenario runner.
	* \param aTimer A reference to the global timer.
	* \param aName Name of the scenario or set of scenarios. Defaults to the
    *        empty string.
	* \param aScenComponents A list of locations of scenario components.
    *        Defaults to an empty list.
	* \return Whether the ScenarioRunner was setup successfully.
    */
    virtual bool setupScenario( Timer& aTimer,
		                        const std::string aName = std::string(),
								const std::list<std::string> aScenComponents = std::list<std::string>() ) = 0;
    
	/*! \brief Run the scenario or set of scenarios.
	* \details Run the scenarios in a manner defined by the type of the scenario
    *          runner.
    * \param aSinglePeriod The single period to run or Scenario::RUN_ALL_PERIODS
    *        to run all periods.
	* \param aTimer Reference to the global timer.
	* \return Whether the scenario or set of scenarios ran successfully.
    */
	virtual bool runScenario( const int aSinglePeriod, Timer& aTimer ) = 0;
    
	/*! \brief Print the output from the set of scenarios run.
    * \details Print the output of the scenario runs.
	* \param aTimer Reference to the global timer.
	* \param aCloseDB Whether to close the database. Defaults to true.
	* \todo Rename the aCloseDB parameter to something more general.
    */
	virtual void printOutput( Timer& aTimer,
		                      const bool aCloseDB = true ) const = 0;

	/*! \brief Get the a mutable reference to the internal scenario object.
	* \details At any given time there is only one Scenario object within the
    *          model that is performing runs. This function returns a pointer to
    *          that internal scenario. Since scenario runners may contain other
    *          scenario runners, this often requires searching down through
    *          several levels.
	* \return The internal scenario.
	*/
	virtual Scenario* getInternalScenario() = 0;


	/*! \brief Get a constant reference to the internal scenario object.
	* \details At any given time there is only one Scenario object within the
    *          model that is performing runs. This function returns a pointer to
    *          that internal scenario. Since scenario runners may contain other
    *          scenario runners, this often requires searching down through
    *          several levels.
	* \return Constant pointer to the internal scenario.
    */
	virtual const Scenario* getInternalScenario() const = 0;
};

// Inline destructor to avoid compiler problems.
IScenarioRunner::~IScenarioRunner(){}

#endif // _SCENARIO_RUNNER_H_

