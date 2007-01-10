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
* \file temperature_target.cpp
* \ingroup Objects
* \brief TemperatureTarget class source file.
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include <string>
#include <cmath>
#include "climate/include/iclimate_model.h"
#include "target_finder/include/temperature_target.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/util.h"

using namespace std;

/*!
 * \brief Constructor
 * \param aClimateModel The climate model.
 * \param aTargetValue The target value.
 */
TemperatureTarget::TemperatureTarget( const IClimateModel* aClimateModel,
                                      const double aTargetValue ):
mClimateModel( aClimateModel ),
mTargetValue( aTargetValue ){
}

/*!
 * \brief Get the status of the last trial with respect to the target.
 * \param aTolerance Solution tolerance.
 * \param aYear Year in which to get the status.
 * \return Status of the last trial.
 */
ITarget::TrialStatus TemperatureTarget::getStatus( const double aTolerance,
                                                   const double aYear ) const
{
    // Check if we are above or below the target.
    // TODO: Avoid loss of precision.
    const double currTemperature =
        mClimateModel->getTemperature( util::round( aYear ) );

    // Determine how how far away from the target the current estimate is.
    double percentOff = ( currTemperature - mTargetValue ) / mTargetValue * 100;
    
    // Print an information message.
    ILogger& targetLog = ILogger::getLogger( "target_finder_log" );
    targetLog.setLevel( ILogger::NOTICE );
    targetLog << "Currently " << percentOff << " percent away from the temperature target." << endl
              << "Current: " << currTemperature << " Target: " << mTargetValue << endl;

    TrialStatus status = UNKNOWN;
    // Check if the target is solved.
    if( fabs( percentOff ) < aTolerance ){
        status = SOLVED;
    }
    else if( percentOff > 0 ){
        status = HIGH;
    }
    else {
        status = LOW;
    }

    return status;
}

/*! \brief Return the static name of the object.
* \return The static name of the object.
*/
const string& TemperatureTarget::getXMLNameStatic(){
    static const string XML_NAME = "temperature-target";
    return XML_NAME;
}
