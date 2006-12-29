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
 * \file capture_component_factory.cpp
 * \ingroup Objects
 * \brief CaptureComponentFactory source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <string>
#include "technologies/include/capture_component_factory.h"
#include "util/logger/include/ilogger.h"

// Add new types here.
#include "technologies/include/standard_capture_component.h"
#include "technologies/include/non_energy_use_capture_component.h"
#include "technologies/include/power_plant_capture_component.h"

using namespace std;

/*!
 * \brief Returns whether the requested type is a type the factory knows how to
 *          create.
 * \param aType Type to determine if the factory can create.
 * \return Whether the factory can create the type.
 */
bool CaptureComponentFactory::isOfType( const string& aType ) {
    // Search the list of known types.
    return ( ( aType == StandardCaptureComponent::getXMLNameStatic() )
        || ( aType == NonEnergyUseCaptureComponent::getXMLNameStatic() ) 
        || ( aType == PowerPlantCaptureComponent::getXMLNameStatic() ) );
}

/*!
 * \brief Return a new instance of a component of the requested type.
 * \param aType Type of capture component to return.
 * \return A newly created capture component wrapped in an auto_ptr. The pointer
 *         is null if the type is unknown.
 */
auto_ptr<ICaptureComponent> CaptureComponentFactory::create( const string& aType ) {
    // Search the list of known types.
    if( aType == StandardCaptureComponent::getXMLNameStatic() ) {
        return auto_ptr<ICaptureComponent>( new StandardCaptureComponent );
    }
    if( aType == NonEnergyUseCaptureComponent::getXMLNameStatic() ){
        return auto_ptr<ICaptureComponent>( new NonEnergyUseCaptureComponent );
    }
    if( aType == PowerPlantCaptureComponent::getXMLNameStatic() ){
        return auto_ptr<ICaptureComponent>( new PowerPlantCaptureComponent );
    }

    // Unknown type.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::ERROR );
    mainLog << "Could not create capture component of type " << aType << "." << endl;
    return auto_ptr<ICaptureComponent>();
}
