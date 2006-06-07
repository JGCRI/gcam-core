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
* \file forcing_target.cpp
* \ingroup Objects
* \brief ForcingTarget class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include <string>
#include <cmath>
#include "util/base/include/model_time.h"
#include "climate/include/iclimate_model.h"
#include "target_finder/include/forcing_target.h"
#include "util/logger/include/ilogger.h"

using namespace std;

/*!
 * \brief Constructor
 * \param aClimateModel The climate model.
 * \param aTargetValue The target value.
 */
ForcingTarget::ForcingTarget( const IClimateModel* aClimateModel,
                              const double aTargetValue ):
mClimateModel( aClimateModel ),
mTargetValue( aTargetValue ){
    /*! \pre Final period should be after initial period. */
    assert( aFinalPeriod >= aInitialPeriod );
}

/*!
 * \brief Get the status of the last trial with respect to the target.
 * \param aTolerance Solution tolerance.
 * \param aYear Year in which to get the status.
 * \return Status of the last trial.
 */
ITarget::TrialStatus ForcingTarget::getStatus( const double aTolerance,
                                               const unsigned int aYear ) const
{
    // Check if we are above or below the target.
    const double currForcing = mClimateModel->getTotalForcing( aYear );

    // Determine how how far away from the target the current estimate is.
    double percentOff = ( currForcing - mTargetValue ) / mTargetValue * 100;
    
    // Print an information message.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Currently " << percentOff << " percent away from the forcing target." << endl;
    mainLog << "Current: " << currForcing << " Target: " << mTargetValue << endl;

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

/*! \brief Get the name of the target gas.
* \return The name of the current gas.
* \todo What to return?
*/
const string& ForcingTarget::getTaxName() const {
    const static string TARGET_GASES = "all";
    return TARGET_GASES;
}

/*! \brief Return the static name of the object.
* \return The static name of the object.
*/
const string& ForcingTarget::getXMLNameStatic(){
    static const string XML_NAME = "forcing-target";
    return XML_NAME;
}
