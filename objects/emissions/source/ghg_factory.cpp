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
* \file ghg_factory.cpp
* \ingroup Objects
* \brief GHGFactory source file.
* \author Jim Naslund
*/

#include "util/base/include/definitions.h"

#include <string>

#include "emissions/include/ghg_factory.h"
#include "emissions/include/co2_emissions.h"
#include "emissions/include/generic_emissions.h"
#include "emissions/include/so2_emissions.h"
#include "emissions/include/input_output_driver.h"
#include "emissions/include/input_driver.h"

using namespace std;

/*!
 * \brief Return a new instance of a component of the requested type.
 * \details This class returns a GHG of the requested type.  It is 
 *          hard-coded to set the emissions drivers for CO2 and SO2.
 *          It reads the emissions driver for a generic GHG from
 *          the input XML.
 * \warning This allows the user to create a GHG object that does
 *          not have a driver which will result in a fatal error
 *          when the model is run.
 * \return A newly created GHG wrapped in an auto_ptr. The pointer
 *         is null if the type is unknown.
 */
auto_ptr<AGHG> GHGFactory::create( const string& aType ){
    if( aType == CO2Emissions::getXMLNameStatic() ){
        AGHG* toReturn = new CO2Emissions();
        auto_ptr<AEmissionsDriver> newDriver( new InputOutputDriver );
        toReturn->setEmissionsDriver( newDriver );
        return auto_ptr<AGHG>( toReturn );
    }
    if( aType == SO2Emissions::getXMLNameStatic() ){
        AGHG* toReturn = new SO2Emissions();
        auto_ptr<AEmissionsDriver> newDriver( new InputOutputDriver );
        toReturn->setEmissionsDriver( newDriver );
        return auto_ptr<AGHG>( toReturn );
    }
    if( aType == GenericEmissions::getXMLNameStatic() ){
        // An emissions driver will be set from input.
        return auto_ptr<AGHG>( new GenericEmissions );
    }
    return auto_ptr<AGHG>();
}

/*!
 * \brief Returns whether the type is a valid GHG type.
 * \details Returns whether the type is a valid GHG type.  This is useful because
 *          when additional GHG types are created they only need to be added here.
 * \param aType type
 * \return boolean indicating whether the type of node is a valid GHG type
 */
const bool GHGFactory::isGHGNode( const string& aType ){
    return aType == CO2Emissions::getXMLNameStatic() 
           || aType == SO2Emissions::getXMLNameStatic()
           || aType == GenericEmissions::getXMLNameStatic();
}

