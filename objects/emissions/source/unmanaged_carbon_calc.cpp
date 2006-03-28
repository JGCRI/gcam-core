/*! 
* \file unmanaged_carbon_calc.cpp
* \ingroup Objects
* \brief UnmanagedCarbonCalc class source file.
* \author James Blackwood
*/

#include "util/base/include/xml_helper.h"
#include <xercesc/dom/DOMNodeList.hpp>
#include "emissions/include/unmanaged_carbon_calc.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief Constructor.
* \author James Blackwood
*/
UnmanagedCarbonCalc::UnmanagedCarbonCalc():
mAboveGroundCarbon( getStartYear(), getEndYear() ),
mBelowGroundCarbon( getStartYear(), getEndYear() )
{
}

//! Default destructor
UnmanagedCarbonCalc::~UnmanagedCarbonCalc() {
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
            XMLHelper<double>::insertValueIntoVector( curr, mAboveGroundCarbon );
        }
        else if( nodeName == "below-ground-carbon" ) {
            XMLHelper<double>::insertValueIntoVector( curr, mBelowGroundCarbon );
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
    XMLWriteElement( mAboveGroundCarbon[ year ], "potential-above-ground-carbon", aOut, aTabs );
    XMLWriteElement( mBelowGroundCarbon[ year ], "potential-below-ground-carbon", aOut, aTabs );
    XMLWriteElement( mLandUse[ aPeriod ], "land-use", aOut, aTabs );
    XMLWriteElement( mTotalEmissions[ year ], "total-emissions", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void UnmanagedCarbonCalc::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteVector( mAboveGroundCarbon, "above-ground-carbon", aOut, aTabs, getStartYear(), 0.0 );
    XMLWriteVector( mBelowGroundCarbon, "below-ground-carbon", aOut, aTabs, getStartYear(), 0.0 );
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
* \param aSubsectorIInfo Parent information container.
* \author James Blackwood
*/
void UnmanagedCarbonCalc::completeInit() {

}

void UnmanagedCarbonCalc::setUnitAboveGroundCarbon( const double aAboveGroundCarbon,
                                                    const int aPeriod )
{
    // Carbon content of unmanaged land is read-in.
    assert( false );
}

void UnmanagedCarbonCalc::setUnitBelowGroundCarbon( const double aBelowGroundCarbon,
                                                    const int aPeriod )
{
    // Carbon content of unmanaged land is read-in.
    assert( false );
}

double UnmanagedCarbonCalc::getPotentialAboveGroundCarbonPerLandArea( const int aYear ) const {
    return mAboveGroundCarbon[ aYear ];
}

double UnmanagedCarbonCalc::getPotentialBelowGroundCarbonPerLandArea( const int aYear ) const {
    return mBelowGroundCarbon[ aYear ];
}
