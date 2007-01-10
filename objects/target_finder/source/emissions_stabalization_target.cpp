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
 * \file emissions_stabalization_target.cpp
 * \ingroup Objects
 * \brief EmissionsStabalizationTarget class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <cassert>
#include <string>
#include <cmath>
#include "climate/include/iclimate_model.h"
#include "target_finder/include/emissions_stabalization_target.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "util/base/include/util.h"
#include "util/base/include/configuration.h"

extern Scenario* scenario;

using namespace std;

/*!
 * \brief Constructor
 * \param aClimateModel The climate model.
 * \param aTargetValue The value of the target.
 */
EmissionsStabalizationTarget::EmissionsStabalizationTarget(
    const IClimateModel* aClimateModel,
    const double aTargetValue ):
mClimateModel( aClimateModel )
{
    // Store configuration variables.
    const Configuration* conf = Configuration::getInstance();
    mTargetGas = conf->getString( "concentration-target-gas", "CO2" );
}

ITarget::TrialStatus
EmissionsStabalizationTarget::getStatus( const double aTolerance,
                                         const double aYear ) const
{
    // Determine the total system emissions. NOTE: Net terrestrial uptake
    // includes land use emissions as a negative, so they are not added here as
    // that would double account.
    
    // NOTE: Assumes that net terrestrial uptake is not removed from industrial
    // emissions.
    int prevYear = static_cast<int>( floor( aYear ) );
    int nextYear = static_cast<int>( ceil( aYear ) );

    double totalEmissions =
        objects::linearInterpolateY( aYear, prevYear, nextYear,
        mClimateModel->getEmissions( "CO2", prevYear ),
        mClimateModel->getEmissions( "CO2", nextYear ) );

    // Year can be between two years
    double netOceanUp = objects::linearInterpolateY( aYear, prevYear, nextYear,
        mClimateModel->getNetOceanUptake( prevYear ),
        mClimateModel->getNetOceanUptake( nextYear ) );

    double netTerrUp = objects::linearInterpolateY( aYear, prevYear, nextYear,
        mClimateModel->getNetTerrestrialUptake( prevYear ),
        mClimateModel->getNetTerrestrialUptake( nextYear ) );

    double totalNetUptake = netOceanUp + netTerrUp;

    // Determine how how far away from the target the current estimate is.
    double percentOff = objects::percentDiff( totalNetUptake, totalEmissions );
    
    // Print an information message.
    ILogger& targetLog = ILogger::getLogger( "target_finder_log" );
    targetLog.setLevel( ILogger::NOTICE );
    targetLog << "Currently " << percentOff
              << " percent away from the stabilization target. "
              << "Emissions: " << totalEmissions << " Total net uptake: "
              << totalNetUptake << endl;


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

/*!
 * \brief Return the static name of the object.
 * \return The static name of the object.
 */
const string& EmissionsStabalizationTarget::getXMLNameStatic(){
	static const string XML_NAME = "stabalization-target";
	return XML_NAME;
}
