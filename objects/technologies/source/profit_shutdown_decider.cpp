/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial
 * Institute. Battelle has certain unperfected rights in the software which
 * should not be copied or otherwise disseminated outside your organization
 * without the express written authorization from Battelle. All rights to the
 * software are reserved by Battelle. Battelle makes no warranty, express or
 * implied, and assumes no liability or responsibility for the use of this
 * software.
 */

/*! 
 * \file profit_shutdown_decider.cpp
 * \ingroup Objects
 * \brief ProfitShutdownDecider class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <cassert>
#include "technologies/include/profit_shutdown_decider.h"
#include "functions/include/function_utils.h"
#include "functions/include/ifunction.h"
#include "util/base/include/xml_helper.h"

using namespace std;

//! Constructor
ProfitShutdownDecider::ProfitShutdownDecider(){
}

ProfitShutdownDecider* ProfitShutdownDecider::clone() const {
    return new ProfitShutdownDecider( *this );
}

bool ProfitShutdownDecider::isSameType( const std::string& aType ) const {
    return aType == getXMLNameStatic();
}

const string& ProfitShutdownDecider::getName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
* \details This public function accesses the private constant string, XML_NAME.
*          This way the tag is always consistent for both read-in and output and
*          can be easily changed. The "==" operator that is used when parsing,
*          required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& ProfitShutdownDecider::getXMLNameStatic() {
    const static string XML_NAME = "profit-shutdown-decider";
    return XML_NAME;
}

bool ProfitShutdownDecider::XMLParse( const xercesc::DOMNode* node ){
    // TODO: Handle success and failure better.
    return true;
}

void ProfitShutdownDecider::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void ProfitShutdownDecider::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

double ProfitShutdownDecider::calcShutdownCoef( const ProductionFunctionInfo* aFuncInfo,
                                                const double aCalculatedProfitRate,
                                                const string& aRegionName,
                                                const string& aSectorName,
                                                const int aInitialTechYear,
                                                const int aPeriod ) const 
{
    // Default scale factor not to scale.
    double scaleFactor = 1;
    // There is no shutdown decision in the base period.
    if( aPeriod > 0 ){
        double profitRate;
        // Calculate the profit rate dynamically.
        if( aCalculatedProfitRate == getUncalculatedProfitRateConstant() ){
            // Calculate the unscaled profits.
            assert( aFuncInfo );
            double profits = aFuncInfo->mProductionFunction->calcUnscaledProfits( aFuncInfo->mInputs,
                                                                                  aRegionName,
                                                                                  aSectorName,
                                                                                  aPeriod,
                                                                                  aFuncInfo->mCapitalStock, 
                                                                                  aFuncInfo->mAlphaZeroScaler,
                                                                                  aFuncInfo->mSigma );
            assert( profits >= 0 );
            assert( aFuncInfo->mCapitalStock > 0 );
            // Determine the profit per unit of capital.
            profitRate = profits / aFuncInfo->mCapitalStock;
        }
        else {
            // Use the passed in profit rate.
            profitRate = aCalculatedProfitRate;
        }

        // Determine if the profit rate is lower than the minimum for scaling.
        const static double MIN_PROFIT_RATE = 0.01;
        if( profitRate < MIN_PROFIT_RATE  ){
            double ratio = max( profitRate, 0.0 ) / MIN_PROFIT_RATE;
            scaleFactor = 3 * pow( ratio, 2 ) - 2 * pow( ratio, 3 );
        }
    }

    // Scale factor is between 0 and 1.
    assert( scaleFactor >= 0 && scaleFactor <= 1 );
    return scaleFactor;
}
