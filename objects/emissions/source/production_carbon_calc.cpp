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

void ProductionCarbonCalc::completeInit() {

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

double ProductionCarbonCalc::getPotentialAboveGroundCarbon( const int aYear ) const {
    return interpYearHelper( mPotentialAboveGroundCarbon, aYear );
}

double ProductionCarbonCalc::getPotentialBelowGroundCarbon( const int aYear ) const {
    // Use an exponential decay function to determine emissions.
    return interpYearHelper( mPotentialBelowGroundCarbon, aYear );
}
