/*
 * LEGAL NOTICE
 * This computer software was prepared by Battelle Memorial Institute,
 * hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
 * with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
 * CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
 * sentence must appear on any copies of this computer software.
 * 
 * EXPORT CONTROL
 * User agrees that the Software will not be shipped, transferred or
 * exported into any country or used in any manner prohibited by the
 * United States Export Administration Act or any other applicable
 * export laws, restrictions or regulations (collectively the "Export Laws").
 * Export of the Software may require some form of license or other
 * authority from the U.S. Government, and failure to obtain such
 * export control license may result in criminal liability under
 * U.S. laws. In addition, if the Software is identified as export controlled
 * items under the Export Laws, User represents and warrants that User
 * is not a citizen, or otherwise located within, an embargoed nation
 * (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
 *     and that User is not otherwise prohibited
 * under the Export Laws from receiving the Software.
 * 
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
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
	static const string XML_NAME = "stabilization-target";
	return XML_NAME;
}
