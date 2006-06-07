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
* \file target_factory.cpp
* \ingroup Objects
* \brief TargetFactory source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include "target_finder/include/target_factory.h"
#include "util/logger/include/ilogger.h"

// Add new types here.
#include "target_finder/include/concentration_target.h"
#include "target_finder/include/forcing_target.h"
#include "target_finder/include/temperature_target.h"
#include "util/base/include/configuration.h"

using namespace std;

/*! \brief Returns whether the requested type is a type the factory knows how to
*          create.
* \param aType Type to determine if the factory can create.
* \return Whether the factory can create the type.
*/
bool TargetFactory::isOfType( const string& aType ) {
    // Search the list of known types.
    return ( ( aType == ConcentrationTarget::getXMLNameStatic() )
        || ( aType == ForcingTarget::getXMLNameStatic() )
        || ( aType == TemperatureTarget::getXMLNameStatic() ) );
}

/*!
 * \brief Return a new instance of a component of the requested type.
 * \param aType Type of ITarget to return.
 * \param aClimateModel Scenario's climate model.
 * \param aTargetValue The target value.
 * \return A newly created ITarget wrapped in an auto_ptr. The pointer
 *         is null if the type is unknown.
 */
auto_ptr<ITarget> TargetFactory::create( const string& aType,
                                         const IClimateModel* aClimateModel,
                                         double aTargetValue )
{

    if( aType == ConcentrationTarget::getXMLNameStatic() ) {
        return auto_ptr<ITarget>( new ConcentrationTarget( aClimateModel,
                                                           aTargetValue ) );
    }
    if( aType == ForcingTarget::getXMLNameStatic() ){
        return auto_ptr<ITarget>( new ForcingTarget( aClimateModel,
                                                     aTargetValue ) );
    }
    if( aType == TemperatureTarget::getXMLNameStatic() ){
        return auto_ptr<ITarget>( new TemperatureTarget( aClimateModel,
                                                         aTargetValue ) );
    }

    // Unknown type.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::ERROR );
    mainLog << "Could not create Target of type " << aType << "." << endl;
    return auto_ptr<ITarget>();
}
