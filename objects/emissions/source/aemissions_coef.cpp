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
 * \file aemissions_coef.cpp
 * \ingroup Objects
 * \brief AEmissionsCoef source file.
 * \author Jim Naslund
 */

#include "util/base/include/definitions.h"

#include "emissions/include/aemissions_coef.h"
#include "util/base/include/util.h"
#include "util/base/include/xml_helper.h"
#include "util/logger/include/ilogger.h"

using namespace std;

//! Constructor
AEmissionsCoef::AEmissionsCoef( const double aEmissionsCoef ):
mEmissionsCoef( aEmissionsCoef ),
mOverridesFuturePeriods( false )
{
}

//! Returns the emissions coefficient.
double AEmissionsCoef::getCoef() const {
    return mEmissionsCoef;
}

/*
 * \brief Sets the emissions coefficient.
 * \param aEmissionsCoef The value to set the emissions coefficient to.
 */
void AEmissionsCoef::setCoef( const double aEmissionsCoef ){
}

/*
 * \brief Updates the emissions coefficient.
 * \param output The emissions output used to calculate the emissions coefficient.
 */
void AEmissionsCoef::updateCoef( const double aOutput ){
}

/*
 * \brief Returns the emissions based on a given output value.
 * \param aOutput The emissions output used to calculate the emissions.
 * \return The emissions.
 */
double AEmissionsCoef::getEmissions( const double aOutput ) const {
    return mEmissionsCoef * aOutput;
}

/*
 * \brief Returns the input emissions value.
 * \todo This doesn't really belong in this abstract class...
 *       Is there any way to get it out of here without using reflection somewhere else?
 * \return The input emissions value.
 */
double AEmissionsCoef::getInputEmissions() const {
    return -1.0;
}

/*
 * \brief Calculates the maxCntrl value.
 * \details Method for calculating an maxCntrl when using emissions coefficients that require maxCntrl in their
 *          calculation.  The formula is derived by setting up equations where maxCntrl can be eliminated through 
 *          substitution, the emissions coefficient can be solved for, and that expression can be substituted back in 
 *          into the expression for maxCntrl. See formula for emission in AComplexGHG::calcEmission().
 * \return The maxCntrl value.
 */
double AEmissionsCoef::calcMaxCntrl( const double aFinalEmissCoef, const double aB,
                                     const double aMultiplier ) const {
    // Guard against divide by 0
    double maxCntrl = 0;
    if( mEmissionsCoef > util::getSmallNumber() ){
        maxCntrl = 100 * ( 1 - ( aFinalEmissCoef / ( mEmissionsCoef ) ) );
    }
    else{
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << " emissCoef = 0, control function set to 0"<< endl;
    }
    return maxCntrl;

}

/*
 * \brief Returns whether or not the calcMaxCntrl function needs the B and multiplier variables.
 * \return Whether or not the calcMaxCntrl function needs the B and multiplier variables.
 */
bool AEmissionsCoef::needsCalcForAdjustment() const {
    return false;
}

bool AEmissionsCoef::getOverride() const{
    return mOverridesFuturePeriods;
}

void AEmissionsCoef::overrideCoef( const double aEmissionsCoef ){
}

void AEmissionsCoef::toInputXML( ostream& out, Tabs* tabs ) const{
    XMLWriteElement( getXMLValue(), getXMLName(), out, tabs );
}

void AEmissionsCoef::toDebugXML( ostream& out, Tabs* tabs ) const{
    toInputXML( out, tabs );
}

double AEmissionsCoef::getXMLValue() const{
    return mEmissionsCoef;
}

