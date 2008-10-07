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
* \file unmanaged_carbon_calc.cpp
* \ingroup Objects
* \brief UnmanagedCarbonCalc class source file.
* \author James Blackwood
*/

#include "util/base/include/definitions.h"
#include "util/base/include/xml_helper.h"
#include <xercesc/dom/DOMNodeList.hpp>
#include "emissions/include/unmanaged_carbon_calc.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "ccarbon_model/include/carbon_model_utils.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief Constructor.
* \author James Blackwood
*/
UnmanagedCarbonCalc::UnmanagedCarbonCalc():
mAboveGroundCarbon( -1.0 ),
mBelowGroundCarbon( -1.0 ),
mAvgAboveGroundCarbon( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear(), -1.0 ),
mAvgBelowGroundCarbon( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear(), -1.0 )
{
}

//! Default destructor
UnmanagedCarbonCalc::~UnmanagedCarbonCalc() {
}

UnmanagedCarbonCalc* UnmanagedCarbonCalc::clone() const{
    return 0;
}

/*! \brief Parses all XML data for the class.
* \author James Blackwood
* \param aCurr Pointer to the current node in the XML input tree
*/
bool UnmanagedCarbonCalc::XMLParse( const DOMNode* aCurr ) {
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
        else if( nodeName == "above-ground-carbon" ) {
            XMLHelper<double>::insertValueIntoVector( curr, mAvgAboveGroundCarbon );
        }
        else if( nodeName == "below-ground-carbon" ) {
            XMLHelper<double>::insertValueIntoVector( curr, mAvgBelowGroundCarbon);
        }
        else if( nodeName == "mature-age" ) {        
            mMatureAge = XMLHelper<int>::getValue( curr );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string " << nodeName
                    << " found while parsing " << getXMLNameStatic() << "." << endl;
        }
    }

    // TODO: Improve error handling.
    return true;
}

void UnmanagedCarbonCalc::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    const Modeltime* modeltime = scenario->getModeltime();
    const int year = modeltime->getper_to_yr( aPeriod );
    XMLWriteElement( mAvgAboveGroundCarbon[ year ], "potential-above-ground-carbon", aOut, aTabs );
    XMLWriteElement( mAvgBelowGroundCarbon[ year ], "potential-below-ground-carbon", aOut, aTabs );
    XMLWriteElement( mLandUse[ aPeriod ], "land-use", aOut, aTabs );
    XMLWriteElement( mTotalEmissions[ year ], "total-emissions", aOut, aTabs );
    XMLWriteElement( mMatureAge, "mature-age", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void UnmanagedCarbonCalc::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteVector( mAvgAboveGroundCarbon, "above-ground-carbon", aOut, aTabs, CarbonModelUtils::getStartYear(), 0.0 );
    XMLWriteVector( mAvgBelowGroundCarbon, "below-ground-carbon", aOut, aTabs, CarbonModelUtils::getStartYear(), 0.0 );
    XMLWriteElement( mMatureAge, "mature-age", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
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
const string& UnmanagedCarbonCalc::getXMLNameStatic() {
    const static string XML_NAME = "unmanaged-carbon-calc";
    return XML_NAME;
}

/*! 
* \brief Perform initializations that only need to be done once.
* \author James Blackwood
*/
void UnmanagedCarbonCalc::completeInit( int aKey ) {
    for ( int i = 0; i < scenario->getModeltime()->getmaxper(); ++i ){
        mAboveGroundCarbon[ i ] = mAvgAboveGroundCarbon[ scenario->getModeltime()->getper_to_yr( i ) ];
        mBelowGroundCarbon[ i ] = mAvgBelowGroundCarbon[ scenario->getModeltime()->getper_to_yr( i ) ];
    }
}

void UnmanagedCarbonCalc::setUnitAboveGroundCarbon( const double aAboveGroundCarbon,
                                                    const int aPeriod )
{
    // Carbon content of unmanaged land is read-in.
    //assert( false );
}

void UnmanagedCarbonCalc::setUnitBelowGroundCarbon( const double aBelowGroundCarbon,
                                                    const int aPeriod )
{
    // Carbon content of unmanaged land is read-in.
    //assert( false );
}

void UnmanagedCarbonCalc::setActualAboveGroundCarbon( const double aAboveGroundCarbon,
                                                     const int aPeriod )
{
    mAboveGroundCarbon[ aPeriod ] = aAboveGroundCarbon;
}

void UnmanagedCarbonCalc::setActualBelowGroundCarbon( const double aBelowGroundCarbon,
                                                     const int aPeriod )
{
    mBelowGroundCarbon[ aPeriod ] = aBelowGroundCarbon;
}

void UnmanagedCarbonCalc::setMatureAge( const int aMatureAge )    
{
    mMatureAge = aMatureAge;
}

double UnmanagedCarbonCalc::getPotentialAboveGroundCarbon( const int aYear ) const {
    return CarbonModelUtils::interpYearHelper( mAvgAboveGroundCarbon, CarbonModelUtils::getStartYear(),
        CarbonModelUtils::getEndYear(), aYear );
}

double UnmanagedCarbonCalc::getPotentialBelowGroundCarbon( const int aYear ) const {
    return CarbonModelUtils::interpYearHelper( mAvgBelowGroundCarbon, CarbonModelUtils::getStartYear(),
        CarbonModelUtils::getEndYear(), aYear );
}

double UnmanagedCarbonCalc::getActualAboveGroundCarbon( const int aYear ) const {
    return CarbonModelUtils::interpYearHelper( mAboveGroundCarbon, aYear );
}

double UnmanagedCarbonCalc::getActualBelowGroundCarbon( const int aYear ) const {
    return CarbonModelUtils::interpYearHelper( mBelowGroundCarbon, aYear );
}