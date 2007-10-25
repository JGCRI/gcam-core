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
 * \file input_emissions_coef.h
 * \ingroup Objects
 * \brief InputEmissionsCoef header file.
 * \author Jim Naslund
 */

#include "util/base/include/definitions.h"

#include "emissions/include/input_emissions_coef.h"
#include "util/base/include/util.h"

using namespace std;

//! Constructor that initializes input emissions.
InputEmissionsCoef::InputEmissionsCoef( const double aInputEmissions ):
AEmissionsCoef(),
mInputEmissions( aInputEmissions ){
    // Cannot use member init. list because mOverridesFuturePeriods is a member of AEmissionsCoef
    mOverridesFuturePeriods = true;
}

//! Clone operator.
InputEmissionsCoef* InputEmissionsCoef::clone() const {
    return new InputEmissionsCoef( *this );
}

void InputEmissionsCoef::updateCoef( const double adjEmissDriver ){
    // Updates the emissions coefficient to be proportional to emissions divided by driver.
    // Check for divide by zero.
    mEmissionsCoef = adjEmissDriver > util::getSmallNumber() ? mInputEmissions / adjEmissDriver : 0;
}

void InputEmissionsCoef::initCalc( const IInfo* aSubsectorInfo, const string& aName, const int aPeriod ){
}

double InputEmissionsCoef::getInputEmissions() const {
    return mInputEmissions;
}

double InputEmissionsCoef::calcMaxCntrl( const double aFinalEmissCoef, const double aB,
                                         const double aMultiplier ) const {
    // Guard against divide by 0
    return aMultiplier > util::getSmallNumber() ? 100 * ( 1 - ( aFinalEmissCoef * ( ( aB - 1 ) )
                                                          / ( ( ( aB * mInputEmissions ) / aMultiplier )
                                                          - aFinalEmissCoef ) ) ) : 0;
}

bool InputEmissionsCoef::needsCalcForAdjustment() const {
    return true;
}

const string& InputEmissionsCoef::getXMLName() const{
    static const string XML_NAME = "inputEmissions";
    return XML_NAME;
}

double InputEmissionsCoef::getXMLValue() const{
    return mInputEmissions;
}
