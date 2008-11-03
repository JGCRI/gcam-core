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
* \file production_carbon_calc.cpp
* \ingroup Objects
* \brief ProductionCarbonCalc class source file.
* \author James Blackwood
*/

#include "util/base/include/definitions.h"
#include "util/base/include/xml_helper.h"
#include <xercesc/dom/DOMNodeList.hpp>
#include "emissions/include/production_carbon_calc.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "ccarbon_model/include/carbon_model_utils.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief Constructor.
* \author James Blackwood
*/
ProductionCarbonCalc::ProductionCarbonCalc()
{
}

//! Default destructor
ProductionCarbonCalc::~ProductionCarbonCalc() {
}

ProductionCarbonCalc* ProductionCarbonCalc::clone() const{
    return 0;
}

/*! \brief Parses XML for the class.
* \author James Blackwood
* \param aCurr pointer to the current node in the XML input tree
*/
bool ProductionCarbonCalc::XMLParse( const DOMNode* aCurr ) {
    // Assume we are passed a valid node.
    assert( aCurr );
 
    // get all the children.
    DOMNodeList* nodeList = aCurr->getChildNodes();
    
    for( unsigned int i = 0;  i < nodeList->getLength(); ++i ){
        const DOMNode* curr = nodeList->item( i );
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string " << nodeName << " found while parsing " << getXMLNameStatic() << "." << endl;
        }
    }
    // TODO: Improve error handling.
    return true;
}

void ProductionCarbonCalc::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElement( mPotentialAboveGroundCarbon[ aPeriod ], "potential-above-ground-carbon", aOut, aTabs );
    XMLWriteElement( mPotentialBelowGroundCarbon[ aPeriod ], "potential-below-ground-carbon", aOut, aTabs );
    XMLWriteElement( mLandUse[ aPeriod ], "land-use", aOut, aTabs );
    int year = scenario->getModeltime()->getper_to_yr( aPeriod );
    XMLWriteElement( mTotalEmissions[ year ], "total-emissions", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void ProductionCarbonCalc::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    // Carbon production objects are created dynamically.
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& ProductionCarbonCalc::getXMLNameStatic() {
    const static string XML_NAME = "production-carbon-calc";
    return XML_NAME;
}

void ProductionCarbonCalc::completeInit( int aKey ) {

}

void ProductionCarbonCalc::setUnitAboveGroundCarbon( const double aAboveGroundCarbon,
                                                     const int aPeriod )
{
    mPotentialAboveGroundCarbon[ aPeriod ] = aAboveGroundCarbon;
}

void ProductionCarbonCalc::setUnitBelowGroundCarbon( const double aBelowGroundCarbon,
                                                     const int aPeriod )
{
    mPotentialBelowGroundCarbon[ aPeriod ] = aBelowGroundCarbon;
}

void ProductionCarbonCalc::setActualAboveGroundCarbonDensity( const double aAboveGroundCarbonDensity,
                                                     const int aPeriod )
{
    mActualAboveGroundCarbon[ aPeriod ] = aAboveGroundCarbonDensity;
}

void ProductionCarbonCalc::setActualBelowGroundCarbonDensity( const double aBelowGroundCarbonDensity,
                                                     const int aPeriod )
{
    mActualBelowGroundCarbon[ aPeriod ] = aBelowGroundCarbonDensity;
}

void ProductionCarbonCalc::setMatureAge( const int aMatureAge )
{
    mMatureAge = aMatureAge;
}

double ProductionCarbonCalc::getPotentialAboveGroundCarbon( const int aYear ) const {
    return CarbonModelUtils::interpYearHelper( mPotentialAboveGroundCarbon, aYear );
}

double ProductionCarbonCalc::getPotentialBelowGroundCarbon( const int aYear ) const {
    // Use an exponential decay function to determine emissions.
    return CarbonModelUtils::interpYearHelper( mPotentialBelowGroundCarbon, aYear );
}

double ProductionCarbonCalc::getActualAboveGroundCarbonDensity( const int aYear ) const {
    return CarbonModelUtils::interpYearHelper( mActualAboveGroundCarbon, aYear );
}

double ProductionCarbonCalc::getActualBelowGroundCarbonDensity( const int aYear ) const {
    // Use an exponential decay function to determine emissions.
    return CarbonModelUtils::interpYearHelper( mActualBelowGroundCarbon, aYear );
    
}
