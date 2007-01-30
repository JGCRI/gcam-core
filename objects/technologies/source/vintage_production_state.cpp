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
 * \file vintage_production_state.cpp
 * \ingroup Objects
 * \brief VintageProductionState class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <cassert>
#include "technologies/include/vintage_production_state.h"
#include "technologies/include/ishutdown_decider.h"
#include "util/base/include/xml_helper.h"
#include "technologies/include/marginal_profit_calculator.h"

using namespace std;

VintageProductionState::VintageProductionState():
mInitialYear( -1 ){
}

VintageProductionState* VintageProductionState::clone() const {
    return new VintageProductionState( *this );
}

bool VintageProductionState::isSameType( const string& aType ) const {
    return aType == getXMLNameStatic();
}

const string& VintageProductionState::getName() const {
    return getXMLNameStatic();
}

void VintageProductionState::toDebugXML( const int aPeriod,
                                         ostream& aOut,
                                         Tabs* aTabs ) const
{
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElement( mBaseOutput, "base-output", aOut, aTabs, mInitialYear );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

const string& VintageProductionState::getXMLNameStatic() {
    const static string XML_NAME = "vintage-production-state";
    return XML_NAME;
}

double VintageProductionState::calcProduction( const string& aRegionName,
                                               const string& aSectorName,
                                               const double aVariableOutput,
                                               const MarginalProfitCalculator* aMarginalProfitCalc,
                                               const double aFixedOutputScaleFactor,
                                               const vector<IShutdownDecider*>& aShutdownDeciders,
                                               const int aPeriod ) const
{
    // Check that setBaseOutput has been called to initialize the base output
    // and initial output year. 
    assert( mBaseOutput.isInited() && mInitialYear != 1 );

    double shutdownCoef = calcShutdownCoefficient( aRegionName, aSectorName,
                                                   aShutdownDeciders,
                                                   aMarginalProfitCalc,
                                                   aPeriod );
    // Get the amount of output from the Technology in a period after the initial
    // investment period. This is calculated based on the aggregate shutdown
    // coefficient and the amount of output in the initial investment period.
    return mBaseOutput * shutdownCoef * aFixedOutputScaleFactor;
}

/*!
 * \brief Return the aggregate shutdown coefficient which shuts down Technology
 *          production.
 * \details The aggregate shutdown coefficient is multiplied by the output in
 *          any operating year to determine the Technology output. The aggregate
 *          coefficient is the product of all the Technology's shutdown
 *          decisions.
 * \param aRegionName Region name.
 * \param aSectorName Sector name.
 * \param aShutdownDeciders Set of shutdown decision makers.
 * \param aMarginalProfitCalc Calculator of the marginal profit rate for the
 *        Technology.
 * \param aPeriod Model period.
 * \return The aggregate shutdown coefficient.
 */
double VintageProductionState::calcShutdownCoefficient( const string& aRegionName,
										                const string& aSectorName,
                                                        const vector<IShutdownDecider*>& aShutdownDeciders,
                                                        const MarginalProfitCalculator* aMarginalProfitCalc,
									                    const int aPeriod ) const
{
	// Loop through the shutdown decision makers and calculate the product of
    // their coefficients.
	double shutdownCoef = 1;

    // Avoid expensive marginal profit calculation if there are no shutdown deciders.
    if( aShutdownDeciders.empty() ){
        return shutdownCoef;
    }

    double marginalProfit = aMarginalProfitCalc->calcShortTermMarginalProfit( aRegionName,
                                                                              aSectorName,
                                                                              aPeriod );

	for( unsigned int i = 0; i < aShutdownDeciders.size(); ++i ){
		shutdownCoef *= aShutdownDeciders[ i ]->calcShutdownCoef( 0, marginalProfit, aRegionName,
			                                                      aSectorName, mInitialYear, aPeriod );
	}
	return shutdownCoef;
}

void VintageProductionState::setBaseOutput( const double aBaseOutput,
                                            const int aBaseYear )
{
    assert( aBaseOutput >= 0 );
    
    // Store the base level of output and the period in which it was produced.
    mBaseOutput.set( aBaseOutput );
    mInitialYear = aBaseYear;
}

bool VintageProductionState::isNewInvestment() const {
    return false;
}

bool VintageProductionState::isOperating() const {
    return true;
}
