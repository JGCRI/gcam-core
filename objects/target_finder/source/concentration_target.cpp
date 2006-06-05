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
* \file concentration_target.cpp
* \ingroup Objects
* \brief ConcentrationTarget class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include <string>
#include <cmath>
#include "climate/include/iclimate_model.h"
#include "target_finder/include/concentration_target.h"
#include "util/base/include/configuration.h"
#include "util/logger/include/ilogger.h"

#include "containers/include/scenario.h" // TODO: Remove, for modeltime only.
#include "util/base/include/model_time.h"

extern Scenario* scenario; // TODO: Remove, for modeltime only.

using namespace std;

/*! \brief Constructor
* \param aClimateModel The climate model.
* \param aInitialPeriod The initial period of the target.
* \param aFinalPeriod The final period of the target.
*/
ConcentrationTarget::ConcentrationTarget( const IClimateModel* aClimateModel,
                                          const unsigned int aInitialPeriod,
                                          const unsigned int aFinalPeriod,
                                          const double aTargetValue ):
mClimateModel( aClimateModel ),
mInitialPeriod( aInitialPeriod ),
mFinalPeriod( aFinalPeriod ),
mTargetValue( aTargetValue ){
    /*! \pre Final period should be after initial period. */
    assert( aFinalPeriod >= aInitialPeriod );
    // Store configuration variables.
    const Configuration* conf = Configuration::getInstance();
    mInitialReduction = conf->getDouble( "initial-target-reduction", 1 );
    mTargetGas = conf->getString( "concentration-target-gas", "CO2" );
}

/*! \brief Get the status of the last trial with respect to the target.
* \param aTolerance Solution tolerance.
* \param aPeriod Period in which to get the status.
* \return Status of the last trial.
*/
ITarget::TrialStatus ConcentrationTarget::getStatus( const double aTolerance,
                                                     const unsigned int aPeriod ) const
{
    // Determine the current level of the target.
    const double currTarget = calcTarget( aPeriod );

    // Check if we are above or below the target.
    const Modeltime* modeltime = scenario->getModeltime();
    const double currConcentration = mClimateModel->getConcentration( mTargetGas,
                                                                      modeltime->getper_to_yr( aPeriod ) );

    // Determine how how far away from the target the current estimate is.
    double percentOff = ( currConcentration - currTarget ) / currTarget * 100;
    
    // Print an information message.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Currently " << percentOff << " percent away from the concentration target." << endl;
    mainLog << "Current: " << currConcentration << " Target: " << currTarget << endl;

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
*/
const string& ConcentrationTarget::getTaxName() const {
    return mTargetGas;
}

/*! \brief Return the static name of the object.
* \return The static name of the object.
*/
const string& ConcentrationTarget::getXMLNameStatic(){
	static const string XML_NAME = "concentration-target";
	return XML_NAME;
}

/*! \brief Calculate the target for a given period.
* \details Determines the appropriate target given the final target level and
*          the relationship between the current period and initial and final
*          target periods.
* \param aPeriod Model period.
* \return The target for the given period.
*/
double ConcentrationTarget::calcTarget( const unsigned int aPeriod ) const {
    // If there is not a target in the given year, set the target to a large number.
    if( aPeriod < mInitialPeriod ){
        return 10000;
    }

    // Adjust the target value by the soft
    // landing percentage such that the inital target value is adjusted by the
    // full amount, the final period is not adjusted and the values inbetween
    // are interpolated.
    double target = mTargetValue * calcReductionFactor( aPeriod );
    
    /*! \post Target must be positive. */
    assert( target > 0 );
    return target;
}

/*! \brief Calculate the reduction factor for a given period by interpolating between the first and last periods of the target.
* \param aPeriod Period in which to calculate a reduction factor.
* \return Reduction factor.
*/
double ConcentrationTarget::calcReductionFactor( const unsigned int aPeriod ) const {
    // If this is a one-period target, the reduction factor is 1.
    if( mInitialPeriod == mFinalPeriod ){
        return 1;
    }
    
    // Perform a linear interpolation.
    // TODO: Make this latex so it's readable.
    // f(x) = y(a) * (x(a)-x)/(x(b) - x(a)) + y(b) * (x(a)-x)/(x(b)-x(a)).
    double firstTerm = mInitialReduction * static_cast<double>( ( mFinalPeriod - aPeriod ) )
                       / static_cast<double>( ( mFinalPeriod - mInitialPeriod ) );

    double secondTerm = static_cast<double>( ( aPeriod - mInitialPeriod ) )
                        / static_cast<double>( ( mFinalPeriod - mInitialPeriod ) );
    return firstTerm + secondTerm;
}
