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
 * \file cumulative_emissions_target.cpp
 * \ingroup Objects
 * \brief CumulativeEmissionsTarget class source file.
 * \author Pralit Patel
 */

#include "util/base/include/definitions.h"
#include <cassert>
#include <string>
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "target_finder/include/cumulative_emissions_target.h"
#include "util/base/include/configuration.h"

using namespace std;

extern Scenario* scenario;

/*!
 * \brief Constructor
 * \param aClimateModel The climate model.
 * \param aTargetValue The target value.
 * \param aFirstTaxYear The first tax year.
 */
CumulativeEmissionsTarget::CumulativeEmissionsTarget( const IClimateModel* aClimateModel,
                                                      const double aTargetValue,
                                                      const int aFirstTaxYear ):
mTargetValue( aTargetValue ),
mFirstTaxYear( aFirstTaxYear )
{
    // Store configuration variables.
    const Configuration* conf = Configuration::getInstance();
    mTargetGas = conf->getString( "cumulative-target-gas", "CO2" );
}

/*! \brief Return the static name of the object.
 * \return The static name of the object.
 */
const string& CumulativeEmissionsTarget::getXMLNameStatic(){
	static const string XML_NAME = "cumulative-emissions-target";
	return XML_NAME;
}

/*!
 * \brief Get the status of the last trial with respect to the target.
 * \param aTolerance Solution tolerance.
 * \param aYear Year in which to get the status.
 * \return Status of the last trial.
 */
double CumulativeEmissionsTarget::getStatus( const int aYear ) const
{
    // Make sure we are using the correct year.
    const int year = aYear == ITarget::getUseMaxTargetYearFlag() ? getYearOfMaxTargetValue()
        : aYear;
    /*!
     * \pre year must be greater than mFirstTaxYear otherwise we will have no
     *      ability to change the status in that year.
     */
    assert( year > mFirstTaxYear );
    
    // Calculate the cumulative emissions using the trapezoidal rule for interpolation.
    const Modeltime* modeltime = scenario->getModeltime();
    const Marketplace* marketplace = scenario->getMarketplace();
    const string gasRegion = "USA";
    int period = modeltime->getyr_to_per( mFirstTaxYear );
    int prevYear = mFirstTaxYear;
    int currYear = mFirstTaxYear;
    double prevEmissions = 0;
    double currEmissions = marketplace->getDemand( mTargetGas, gasRegion, period );
    double cumulativeEmissions = 0;
    for( ++period; period < modeltime->getmaxper() && period <= modeltime->getyr_to_per( year ); ++period ) {
        prevYear = currYear;
        currYear = modeltime->getper_to_yr( period );
        prevEmissions = currEmissions;
        currEmissions = marketplace->getDemand( mTargetGas, gasRegion, period );
        double cumulativeEmissionsForTimestep = .5 * ( currYear - prevYear ) * ( currEmissions + prevEmissions );
        
        // Adjust the cumulative emissions if year was actually in the middle of this time-step.
        if( currYear <= year ) {
            cumulativeEmissions += cumulativeEmissionsForTimestep;
        }
        else {
            cumulativeEmissions += cumulativeEmissionsForTimestep * ( year - prevYear ) / ( currYear - prevYear );
        }
    }
    
    // TODO: allow extrapolation if year is beyond the final model year?
    
    // Determine how how far away from the target the current estimate is.
    double percentOff = ( cumulativeEmissions - mTargetValue ) / mTargetValue * 100;
    
    return percentOff;
}

int CumulativeEmissionsTarget::getYearOfMaxTargetValue() const {
    // Since we are doing cumulative emissions the max target value will be when
    // the annual emissions switches to negative.
    const Modeltime* modeltime = scenario->getModeltime();
    const Marketplace* marketplace = scenario->getMarketplace();
    const string gasRegion = "USA";
    int period = modeltime->getyr_to_per( mFirstTaxYear );
    double prevEmissions = marketplace->getDemand( mTargetGas, gasRegion, period );
    double currEmissions = prevEmissions;
    for( ++period; period < modeltime->getmaxper() && currEmissions > 0; ++period ) {
        prevEmissions = currEmissions;
        currEmissions = marketplace->getDemand( mTargetGas, gasRegion, period );
    }
    if( period == modeltime->getmaxper() ) {
        return modeltime->getEndYear();
    }
    else {
        // We must calculate by linear interpolation the year at which the emissions
        // were zero.
        double yearOffest = -1 * prevEmissions * modeltime->gettimestep( period ) / ( currEmissions - prevEmissions );
        // Note we are implicitly rounding down.
        return modeltime->getper_to_yr( period - 1 ) + yearOffest;
    }
}
