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
 * \file profit_shutdown_decider.cpp
 * \ingroup Objects
 * \brief ProfitShutdownDecider class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <cassert>
#include "technologies/include/profit_shutdown_decider.h"
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "functions/include/function_utils.h"
#include "functions/include/ifunction.h"
#include "util/base/include/xml_helper.h"

using namespace std;
using namespace xercesc;

//! Constructor
ProfitShutdownDecider::ProfitShutdownDecider():
    mMaxShutdown(1.0),
    mSteepness(5.0),
    mMedianShutdownPoint(0.0)
{
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
    
    // Assume we have a valid node.
    assert( node );

    const xercesc::DOMNodeList* nodeList = node->getChildNodes();
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ) {
        const xercesc::DOMNode* curr = nodeList->item( i );
        if( curr->getNodeType() != xercesc::DOMNode::ELEMENT_NODE ){
            continue;
        }
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
        if( nodeName == "max-shutdown" ){
            mMaxShutdown = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "steepness" ) {
            mSteepness = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "median-shutdown-point" ) {
            mMedianShutdownPoint = XMLHelper<double>::getValue( curr );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Unknown tag " << nodeName << " encountered while processing "
                    << getXMLNameStatic() << endl;
        }
    }
    
    return true;
}

void ProfitShutdownDecider::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElementCheckDefault( mMaxShutdown, "max-shutdown", aOut, aTabs, 1.0 );
    XMLWriteElementCheckDefault( mSteepness, "steepness", aOut, aTabs, 5.0 );
    XMLWriteElementCheckDefault( mMedianShutdownPoint, "median-shutdown-point", aOut, aTabs, 0.0 );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void ProfitShutdownDecider::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElement( mMaxShutdown, "max-shutdown", aOut, aTabs, 0.0 );
    XMLWriteElement( mSteepness, "steepness", aOut, aTabs, 0.0 );
    XMLWriteElement( mMedianShutdownPoint, "median-shutdown-point", aOut, aTabs, 0.0 );
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
       
        // Compute Shutdown factor using exponential S-curve.  ScaleFactor that is returned
        // is actually the fraction not shut down, so it is 1.0 - the shutdown fraction.

        scaleFactor = 1.0 - mMaxShutdown / 
                      ( 1.0 + exp(mSteepness * (profitRate - mMedianShutdownPoint) ) );
    }

    // Scale factor is between 0 and 1.
    assert( scaleFactor >= 0 && scaleFactor <= 1 );
    return scaleFactor;
}
