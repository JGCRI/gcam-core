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
* \file scenario_runner_factory.cpp
* \ingroup Objects
* \brief ScenarioRunnerFactory source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include "containers/include/scenario_runner_factory.h"
#include "util/logger/include/ilogger.h"

// Add new types here.
#include "containers/include/merge_runner.h"
#include "containers/include/single_scenario_runner.h"
#include "containers/include/batch_runner.h"
#include "containers/include/mac_generator_scenario_runner.h"
#include "target_finder/include/policy_target_runner.h"
#include "target_finder/include/simple_policy_target_runner.h"

using namespace std;

/*! \brief Returns whether the requested type is a type the factory knows how to
*          create.
* \param aType Type to determine if the factory can create.
* \return Whether the factory can create the type.
*/
bool ScenarioRunnerFactory::isOfType( const string& aType ) {
    // Search the list of known types.
    return ( ( aType == MergeRunner::getXMLNameStatic() )
        || ( aType == SingleScenarioRunner::getXMLNameStatic() )
        || ( aType == MACGeneratorScenarioRunner::getXMLNameStatic() )
        || ( aType == BatchRunner::getXMLNameStatic() )
        || ( aType == PolicyTargetRunner::getXMLNameStatic() )
        || ( aType == SimplePolicyTargetRunner::getXMLNameStatic() ) );
}

/*! \brief Return a new instance of a component of the requested type.
* \param aType Type of IScenarioRunner to return.
* \return A newly created IScenarioRunner wrapped in an auto_ptr. The pointer
*         is null if the type is unknown.
*/
auto_ptr<IScenarioRunner> ScenarioRunnerFactory::create( const string& aType ) {
    // Search the list of known types.
    if( aType == MergeRunner::getXMLNameStatic() ) {
        return auto_ptr<IScenarioRunner>( new MergeRunner );
    }
    if( aType == SingleScenarioRunner::getXMLNameStatic() ){
        return auto_ptr<IScenarioRunner>( new SingleScenarioRunner );
    }
    if( aType == MACGeneratorScenarioRunner::getXMLNameStatic() ){
        return auto_ptr<IScenarioRunner>( new MACGeneratorScenarioRunner );
    }
    if( aType == BatchRunner::getXMLNameStatic() ){
        return auto_ptr<IScenarioRunner>( new BatchRunner );
    }
    if( aType == PolicyTargetRunner::getXMLNameStatic() ){
        return auto_ptr<IScenarioRunner>( new PolicyTargetRunner );
    }
    if( aType == SimplePolicyTargetRunner::getXMLNameStatic() ){
        return auto_ptr<IScenarioRunner>( new SimplePolicyTargetRunner );
    }

    // Unknown type.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::ERROR );
    mainLog << "Could not create Scenario Runner of type " << aType << "." << endl;
    return auto_ptr<IScenarioRunner>();
}
