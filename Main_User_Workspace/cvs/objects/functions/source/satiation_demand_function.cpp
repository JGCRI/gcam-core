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
* \file satiation_demand_function.cpp
* \ingroup Objects
* \brief The SatiationDemandFunction class source file.
* \author Pralit Patel
* \author Jiyong Eom
*/

#include "util/base/include/definitions.h"
#include <cmath>
#include <cassert>

#include "functions/include/satiation_demand_function.h"
#include "util/base/include/xml_helper.h"

using namespace std;
using namespace xercesc;

SatiationDemandFunction::SatiationDemandFunction():
mSatiationAdder( 0 )
{
}

const string& SatiationDemandFunction::getXMLNameStatic() {
    const static string XML_NAME = "satiation-demand-function";
    return XML_NAME;
}

const string& SatiationDemandFunction::getName() const {
    return getXMLNameStatic();
}

bool SatiationDemandFunction::XMLParse( const DOMNode* aNode ) {
    /*! \pre Make sure we were passed a valid node. */
    assert( aNode );

    // get all child nodes.
    DOMNodeList* nodeList = aNode->getChildNodes();
    
    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
        
        if( nodeName == XMLHelper<void>::text() ) {
            continue;
        }
        else if( nodeName == "satiation-level" ) {
            mParsedSatiationLevel.set( XMLHelper<double>::getValue( curr ) );
        }
        else if( nodeName == "satiation-adder" ) {
            mSatiationAdder.set( XMLHelper<double>::getValue( curr ) );
        }
        else if( nodeName == "satiation-base-year-increase" ) {
            mBaseYearSatiationMultiplier.set( XMLHelper<double>::getValue( curr ) );
            
            /*!
             * \pre The increase from base year demand to set as the satiation
             *      level must be greater than 1.
             */
            assert( mBaseYearSatiationMultiplier > 1.0 );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Unknown element " << nodeName << " encountered while parsing " << getXMLNameStatic() << endl;
        }
    }
    
    return true;
}

void SatiationDemandFunction::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );

    if( mParsedSatiationLevel.isInited() ) {
        XMLWriteElement( mParsedSatiationLevel, "satiation-level", aOut, aTabs );
    }
    if( mBaseYearSatiationMultiplier.isInited() ) {
        XMLWriteElement( mBaseYearSatiationMultiplier, "satiation-base-year-increase", aOut, aTabs );
    }
    XMLWriteElementCheckDefault( mSatiationAdder, "satiation-adder", aOut, aTabs, Value( 0.0 ) );
    
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

/*!
 * \brief Evaluate the satiation function at the given driver level.
 * \param aDemandDriver The value at which to calculate the function.
 * \return The value of the function at the given demand driver.
 */
double SatiationDemandFunction::calcDemand( const double aDemandDriver ) const {
    /*!
     * \pre The satiation level must have been set.
     */
    assert( mSatiationLevel.isInited() );

    /*!
     * \pre The satiation impedance is calibrated.
     */
    assert( mSatiationImpedance.isInited() );

    const double log2 = log( 2.0 );
    return ( mSatiationLevel - mSatiationAdder )
        * ( 1 - exp( -log2 / mSatiationImpedance * aDemandDriver ) ) + mSatiationAdder;
}

/*!
 * \brief Calibrate the satiation impedance given the data point (aDemand, aDemandDriver).
 * \details With the given data point, satiation adder (subsistence level), and satiation level
 *          (asymptote) then the last shape parameter satiation impedance can be determined.
 *          If the user specified the satiation level as a base year demand increase
 *          that value can also be determined now.
 * \param aDemand The calibrated output of this function.
 * \param aDemandDriver The driver for the calibrated demand level.
 */
void SatiationDemandFunction::calibrateSatiationImpedance( const double aDemand, const double aDemandDriver ) {
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::ERROR );
    // Figure out the appropraite satiation level the user wanted to use.
    if( mBaseYearSatiationMultiplier.isInited() && mParsedSatiationLevel.isInited() ) {
        mainLog << "Both satiation-level: " << mParsedSatiationLevel << " and satiation-base-year-increase "
                << mBaseYearSatiationMultiplier << " were parsed, only one can be used." << endl;
        exit( 1 );
    }
    else if( mParsedSatiationLevel.isInited() ) {
        mSatiationLevel = mParsedSatiationLevel;
    }
    else if( mBaseYearSatiationMultiplier.isInited() ) {
        mSatiationLevel = aDemand * mBaseYearSatiationMultiplier;
    }
    
    // Do some errors checking
    if( aDemand >= mSatiationLevel ) {
        mainLog << "Base year demand: " << aDemand << " is greater than satiation level: " << mSatiationLevel << endl;
        exit( 1 );
    }
    else if( mSatiationLevel <= mSatiationAdder ) {
        mainLog << "Satiation level: " << mSatiationLevel << " is less than satiation adder: " << mSatiationAdder << endl;
        exit( 1 );
    }
    else if( aDemand <= mSatiationAdder ) {
        mainLog << "Base year demand: " << aDemand << " is less than satiation adder: " << mSatiationAdder << endl;
        exit( 1 );
    }
    
    // calibrate the satiation impedance
    const double log2 = log( 2.0 );
    mSatiationImpedance = ( log2 * aDemandDriver ) /
        log( ( mSatiationLevel - mSatiationAdder ) / ( mSatiationLevel - aDemand ) );
}
