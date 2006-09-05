#ifndef _ISCENARIO_RUNNER_H_
#define _ISCENARIO_RUNNER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Labratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

/*! 
 * \file iscenario_runner.h
 * \ingroup Objects
 * \brief The IScenarioRunner interface header file.
 * \author Josh Lurz
 */
#include <list>
#include <string>
#include "util/base/include/iparsable.h"

class Timer;
class Scenario;

/*! 
 * \ingroup Objects
 * \brief An interface to a class which is responsible for running a scenario or
 *        set of scenarios.
 * \details An object implementing this interface defines a series of methods
 *          for setting up, running, and printing results for a set of
 *          scenarios. The method for generating the scenarios is implemented by
 *          the object, and not defined by the interface.
 * \author Josh Lurz
 */
class IScenarioRunner: public IParsable {
public:
    /*!
     * \brief Virtual destructor so that derived instances can be deleted
     *        through the base class pointer.
     */
    virtual ~IScenarioRunner();

    /*!
     * \brief Setup the ScenarioRunner before running a single or series of
     *        scenarios.
     * \details This method must be called to setup the scenario runner before
     *          the runScenarios method is called. This performs initialization
     *          for the scenario runner.
     * \param aTimer A reference to the global timer.
     * \param aName Name of the scenario or set of scenarios. Defaults to the
     *        empty string.
     * \param aScenComponents A list of locations of scenario components.
     *        Defaults to an empty list.
     * \return Whether the ScenarioRunner was setup successfully.
     */
    virtual bool setupScenarios( Timer& aTimer,
                                 const std::string aName = std::string(),
                                 const std::list<std::string> aScenComponents = std::list<std::string>() ) = 0;
    
    /*!
     * \brief Run the scenario or set of scenarios.
     * \details Run the scenarios in a manner defined by the type of the
     *          scenario runner.
     * \param aSinglePeriod The single period to run or
     *        Scenario::RUN_ALL_PERIODS to run all periods.
     * \param aTimer Reference to the global timer.
     * \return Whether the scenario or set of scenarios ran successfully.
     */
    virtual bool runScenarios( const int aSinglePeriod,
                               Timer& aTimer ) = 0;
    
    /*!
     * \brief Print the output from the set of scenarios run.
     * \details Print the output of the scenario runs.
     * \param aTimer Reference to the global timer.
     * \param aCloseDB Whether to close the database. Defaults to true.
     * \todo Rename the aCloseDB parameter to something more general.
     */
    virtual void printOutput( Timer& aTimer,
                              const bool aCloseDB = true ) const = 0;

    /*!
     * \brief Get the a mutable reference to the internal scenario object.
     * \details At any given time there is only one Scenario object within the
     *          model that is performing runs. This function returns a pointer
     *          to that internal scenario. Since scenario runners may contain
     *          other scenario runners, this often requires searching down
     *          through several levels.
     * \return The internal scenario.
     */
    virtual Scenario* getInternalScenario() = 0;


    /*!
     * \brief Get a constant reference to the internal scenario object.
     * \details At any given time there is only one Scenario object within the
     *          model that is performing runs. This function returns a pointer
     *          to that internal scenario. Since scenario runners may contain
     *          other scenario runners, this often requires searching down
     *          through several levels.
     * \return Constant pointer to the internal scenario.
     */
    virtual const Scenario* getInternalScenario() const = 0;
};

// Inline destructor.
inline IScenarioRunner::~IScenarioRunner(){}

#endif // _SCENARIO_RUNNER_H_
