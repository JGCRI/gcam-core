#ifndef _SCENARIO_RUNNER_FACTORY_H_
#define _SCENARIO_RUNNER_FACTORY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
* This software, which is provided in confidence, was prepared by employees of
* Pacific Northwest National Labratory operated by Battelle Memorial Institute.
* Battelle has certain unperfected rights in the software which should not be
* copied or otherwise disseminated outside your organization without the express
* written authorization from Battelle. All rights to the software are reserved
* by Battelle. Battelle makes no warranty, express or implied, and assumes no
* liability or responsibility for the use of this software.
*/

/*! 
* \file scenario_runner_factory.h
* \ingroup Objects
* \brief ScenarioRunnerFactory header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <memory>

class IScenarioRunner;

/*! 
* \ingroup Objects
* \brief A factory which is used to create the various types of ScenarioRunners.
* \details The factory encapsulates the creation of various types of
*          ScenarioRunners. This simplifies adding new types and also minimizes
*          recompilation.
* \author Josh Lurz
*/
class ScenarioRunnerFactory { 
public:
	static bool isOfType( const std::string& aType );
	static std::auto_ptr<IScenarioRunner> create( const std::string& aType );
};

#endif // _SCENARIO_RUNNER_FACTORY_H_

