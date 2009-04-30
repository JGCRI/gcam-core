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
* \file generic_technology_info.cpp
* \ingroup Objects
* \brief GenericTechnologyInfo source file.
* \author Pralit Patel
*/
// Standard Library headers
#include "util/base/include/definitions.h"
#include <string>
#include <cassert>

#include "technologies/include/generic_technology_info.h"
#include "util/base/include/xml_helper.h"

using namespace std;
using namespace xercesc;


GenericTechnologyInfo::GenericTechnologyInfo( const string &aName )
: name( aName ), mBaseEfficiency( 1 ), 
mBaseNonEnergyCost( 0 ), fMultiplier( 1 ), 
fuelPrefElasticity( 0 ) {
}

ITechnologyInfo* GenericTechnologyInfo::clone() {
    return new GenericTechnologyInfo( *this );
}

void GenericTechnologyInfo::completeInit() {
    // Check for nonsensical efficiency.
    if( mBaseEfficiency <= 0 ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Resetting invalid efficiency for Technology " << name << endl;
        mBaseEfficiency =  1;
    }
}

//! write object to xml output stream
void GenericTechnologyInfo::toInputXML( ostream &out, Tabs *tabs ) const {
    // don't write open/close tags for xml because that is the way it was read.

    XMLWriteElement( fuelname, "fuelname", out, tabs );
    XMLWriteElementCheckDefault( mBaseEfficiency, "efficiency", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( mBaseNonEnergyCost, "nonenergycost", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( fuelPrefElasticity, "fuelprefElasticity", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( fMultiplier, "fMultiplier", out, tabs, 1.0 );

}

//! write object to xml debugging output stream
void GenericTechnologyInfo::toDebugXML( int period, ostream &out, Tabs *tabs ) const {
    // don't write open/close tags for xml because that is the way it was read.

    XMLWriteElement( fuelname, "fuelname", out, tabs );
    XMLWriteElementCheckDefault( mBaseEfficiency, "efficiency", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( mBaseNonEnergyCost, "nonenergycost", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( fuelPrefElasticity, "fuelprefElasticity", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( fMultiplier, "fMultiplier", out, tabs, 1.0 );

}

const string& GenericTechnologyInfo::getName() const {
    return name;
}

const string& GenericTechnologyInfo::getFuelName() const {
    return fuelname;
}

double GenericTechnologyInfo::getEfficiency() const {
    return mBaseEfficiency;
}

double GenericTechnologyInfo::getNonEnergyCost() const {
    return mBaseNonEnergyCost;
}

double GenericTechnologyInfo::getFMultiplier() const {
    return fMultiplier;
}

double GenericTechnologyInfo::getFuelPrefElasticity() const {
    return fuelPrefElasticity;
}

void GenericTechnologyInfo::setFuelName( const string& aFuelName ) {
    fuelname = aFuelName;
}

void GenericTechnologyInfo::setEfficiency( const double aEfficiency ) {
    mBaseEfficiency = aEfficiency;
}

void GenericTechnologyInfo::setNonEnergyCost( const double aNonEnergyCost ) {
    mBaseNonEnergyCost = aNonEnergyCost;
}

void GenericTechnologyInfo::setFMultiplier( const double aFMultiplier ) {
    fMultiplier = aFMultiplier;
}

void GenericTechnologyInfo::setFuelPrefElasticity( const double aFuelPrefElasticity ) {
    fuelPrefElasticity = aFuelPrefElasticity;
}
