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
 * \file fixed_production_state.cpp
 * \ingroup Objects
 * \brief FixedProductionState class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <cassert>
#include "technologies/include/fixed_production_state.h"
#include "util/base/include/xml_helper.h"

using namespace std;

FixedProductionState::FixedProductionState(){
}

FixedProductionState* FixedProductionState::clone() const {
    return new FixedProductionState( *this );
}

bool FixedProductionState::isSameType( const string& aType ) const {
    return aType == getXMLNameStatic();
}

const string& FixedProductionState::getName() const {
    return getXMLNameStatic();
}

void FixedProductionState::toDebugXML( const int aPeriod,
                                       ostream& aOut,
                                       Tabs* aTabs ) const
{
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElement( mFixedOutput, "base-output", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

const string& FixedProductionState::getXMLNameStatic() {
    const static string XML_NAME = "fixed-production-state";
    return XML_NAME;
}

double FixedProductionState::calcProduction( const string& aRegionName,
                                             const string& aSectorName,
                                             const double aVariableOutput,
                                             const MarginalProfitCalculator* aMarginalProfitCalc,
                                             const double aFixedOutputScaleFactor,
                                             const vector<IShutdownDecider*>& aShutdownDeciders,
                                             const int aPeriod ) const
{
    // Fixed output must be initialized.
    assert( mFixedOutput.isInited() );

    // Production is the fixed output multiplied by the scale factor.
    return mFixedOutput * aFixedOutputScaleFactor;
}

void FixedProductionState::setBaseOutput( const double aBaseOutput, const int aBaseYear ){
    assert( aBaseOutput >= 0 );
    mFixedOutput.set( aBaseOutput );
}

bool FixedProductionState::isNewInvestment() const {
    // Fixed output is converted to a vintage production state if it has a
    // lifetime longer than one timestep, so this production state always is new
    // investment.
    return true;
}

bool FixedProductionState::isOperating() const {
    return true;
}
