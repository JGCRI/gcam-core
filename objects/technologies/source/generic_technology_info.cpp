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
: name( aName ), mBaseEfficiency( 1 ), effPenalty( 0 ), 
mBaseNonEnergyCost( 0 ), neCostPenalty( 0 ), fMultiplier( 1 ), 
fuelPrefElasticity( 0 ) {
}

ITechnologyInfo* GenericTechnologyInfo::clone() {
    return new GenericTechnologyInfo( *this );
}

void GenericTechnologyInfo::completeInit() {
    // Check for non-sensical efficiency.
    if( mBaseEfficiency <= 0 ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Resetting invalid effiency for Technology " << name << endl;
        mBaseEfficiency =  1;
    }
}

//! write object to xml output stream
void GenericTechnologyInfo::toInputXML( ostream &out, Tabs *tabs ) const {
    // don't write open/close tags for xml becaues that is the way it was read.

    XMLWriteElement( fuelname, "fuelname", out, tabs );
    XMLWriteElementCheckDefault( mBaseEfficiency, "efficiency", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( effPenalty, "efficiencyPenalty", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( mBaseNonEnergyCost, "nonenergycost", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( neCostPenalty, "neCostPenalty", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( fuelPrefElasticity, "fuelprefElasticity", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( fMultiplier, "fMultiplier", out, tabs, 1.0 );

}

//! write object to xml debugging output stream
void GenericTechnologyInfo::toDebugXML( int period, ostream &out, Tabs *tabs ) const {
    // don't write open/close tags for xml becaues that is the way it was read.

    XMLWriteElement( fuelname, "fuelname", out, tabs );
    XMLWriteElementCheckDefault( mBaseEfficiency, "efficiency", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( effPenalty, "efficiencyPenalty", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( mBaseNonEnergyCost, "nonenergycost", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( neCostPenalty, "neCostPenalty", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( fuelPrefElasticity, "fuelprefElasticity", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( fMultiplier, "fMultiplier", out, tabs, 1.0 );

}

const string& GenericTechnologyInfo::getName() const {
    return name;
}

const string& GenericTechnologyInfo::getFuelName() const {
    return fuelname;
}

const double GenericTechnologyInfo::getEfficiency() const {
    return mBaseEfficiency;
}

const double GenericTechnologyInfo::getNonEnergyCost() const {
    return mBaseNonEnergyCost;
}

const double GenericTechnologyInfo::getEffPenalty() const {
    return effPenalty;
}

const double GenericTechnologyInfo::getNECostPenalty() const {
    return neCostPenalty;
}

const double GenericTechnologyInfo::getFMultiplier() const {
    return fMultiplier;
}

const double GenericTechnologyInfo::getFuelPrefElasticity() const {
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

void GenericTechnologyInfo::setEffPenalty( const double aEffPenalty ) {
    effPenalty = aEffPenalty;
}

void GenericTechnologyInfo::setNECostPenalty( const double aNECostPenalty ) {
    neCostPenalty = aNECostPenalty;
}

void GenericTechnologyInfo::setFMultiplier( const double aFMultiplier ) {
    fMultiplier = aFMultiplier;
}

void GenericTechnologyInfo::setFuelPrefElasticity( const double aFuelPrefElasticity ) {
    fuelPrefElasticity = aFuelPrefElasticity;
}
