/*
* This software, which is provided in confidence, was prepared by employees of
* Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
* Battelle has certain unperfected rights in the software which should not be
* copied or otherwise disseminated outside your organization without the express
* written authorization from Battelle. All rights to the software are reserved
* by Battelle. Battelle makes no warranty, express or implied, and assumes no
* liability or responsibility for the use of this software.
*/

/*! 
* \file emissions_driver_factory.cpp
* \ingroup Objects
* \brief EmissionsDriverFactory source file.
* \author Jim Naslund
*/

#include "util/base/include/definitions.h"

#include <string>

#include "emissions/include/emissions_driver_factory.h"
#include "emissions/include/input_output_driver.h"
#include "emissions/include/input_driver.h"
#include "emissions/include/output_driver.h"

using namespace std;

/*!
 * \brief Return a new instance of a component of the requested type.
 * \return A newly created EmissionsDriver wrapped in an auto_ptr. The pointer
 *         is null if the type is unknown.
 */
auto_ptr<AEmissionsDriver> EmissionsDriverFactory::create( const string& aType ){
    if( aType == InputDriver::getXMLNameStatic() ){
        return auto_ptr<AEmissionsDriver>( new InputDriver );
    }
    if( aType == OutputDriver::getXMLNameStatic() ){
        return auto_ptr<AEmissionsDriver>( new OutputDriver );
    }
    if( aType == InputOutputDriver::getXMLNameStatic() ){
        return auto_ptr<AEmissionsDriver>( new InputOutputDriver );
    }
    return auto_ptr<AEmissionsDriver>();
}

const bool EmissionsDriverFactory::isEmissionsDriverNode( const string& aNodeName ){
    return aNodeName == InputDriver::getXMLNameStatic() 
           || aNodeName == OutputDriver::getXMLNameStatic()
           || aNodeName == InputOutputDriver::getXMLNameStatic();
}

