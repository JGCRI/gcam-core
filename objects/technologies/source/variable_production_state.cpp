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
 * \file variable_production_state.cpp
 * \ingroup Objects
 * \brief VariableProductionState class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <cassert>
#include "technologies/include/variable_production_state.h"
#include "util/base/include/xml_helper.h"

using namespace std;

VariableProductionState::VariableProductionState(){
}

VariableProductionState* VariableProductionState::clone() const {
    return new VariableProductionState( *this );
}

bool VariableProductionState::isSameType( const std::string& aType ) const {
    return aType == getXMLNameStatic();
}

const string& VariableProductionState::getName() const {
    // Use the XML name as the name.
    return getXMLNameStatic();
}

void VariableProductionState::toDebugXML( const int aPeriod,
                                          ostream& aOut,
                                          Tabs* aTabs ) const
{
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

const string& VariableProductionState::getXMLNameStatic() {
    const static string XML_NAME = "variable-production-state";
    return XML_NAME;
}

double VariableProductionState::calcProduction( const string& aRegionName,
                                                const string& aSectorName,
                                                const double aVariableOutput,
                                                const MarginalProfitCalculator* aMarginalProfitCalc,
                                                const double aFixedOutputScaleFactor,
                                                const vector<IShutdownDecider*>& aShutdownDeciders,
                                                const int aPeriod ) const
{
    assert( aVariableOutput >= 0 );
    // Variable production states produce exactly what the subsector requested.
    return aVariableOutput;
}

void VariableProductionState::setBaseOutput( const double aBaseOutput,
                                             const int aPeriod )
{
    // This is ignored, variable production has no base level output.
}

bool VariableProductionState::isNewInvestment() const {
    return true;
}

bool VariableProductionState::isOperating() const {
    return true;
}
