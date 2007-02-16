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
 * \file retired_production_state.cpp
 * \ingroup Objects
 * \brief RetiredProductionState class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <cassert>
#include "technologies/include/retired_production_state.h"
#include "util/base/include/xml_helper.h"

using namespace std;

RetiredProductionState::RetiredProductionState(){
}

RetiredProductionState* RetiredProductionState::clone() const {
	return new RetiredProductionState( *this );
}

bool RetiredProductionState::isSameType( const string& aType ) const {
	return aType == getXMLNameStatic();
}

const string& RetiredProductionState::getName() const {
	return getXMLNameStatic();
}


void RetiredProductionState::toDebugXML( const int aPeriod,
                                         std::ostream& aOut,
                                         Tabs* aTabs ) const
{
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

const string& RetiredProductionState::getXMLNameStatic() {
	const static string XML_NAME = "retired-production-state";
	return XML_NAME;
}

double RetiredProductionState::calcProduction( const string& aRegionName,
                                               const string& aSectorName,
                                               const double aVariableOutput,
                                               const MarginalProfitCalculator* aMarginalProfitCalc,
                                               const double aFixedOutputScaleFactor,
                                               const vector<IShutdownDecider*>& aShutdownDeciders,
                                               const int aPeriod ) const
{
    // Retired technologies have zero output.
    return 0;
}

void RetiredProductionState::setBaseOutput( const double aBaseOutput,
                                            const int aPeriod )
{
    // No base level of output.
}

bool RetiredProductionState::isNewInvestment() const {
    return false;
}

bool RetiredProductionState::isOperating() const {
    return false;
}
