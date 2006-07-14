/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Labratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

/*! 
 * \file cal_data_input.cpp
 * \ingroup Objects
 * \brief CalDataInput class source file.
 * \author James Blackwood
 */
#include "util/base/include/definitions.h"
#include <string>
#include <xercesc/dom/DOMNodeList.hpp>
#include "util/base/include/xml_helper.h"
#include "technologies/include/cal_data_input.h"

using namespace std;
using namespace xercesc;

/*! \brief Default constructor.
* \author James Blackwood
*/
CalDataInput::CalDataInput() {
    mCalInputValue = 0;
}

/*! \brief Clone the current object.
* \return A clone of the object.
*/
CalDataInput* CalDataInput::clone() const {
    return new CalDataInput( *this );
}

/*! \brief Parses XML for the object.
* \author James Blackwood
* \param aNode pointer to the current node in the XML input tree
*/
void CalDataInput::XMLParse( const DOMNode* aNode ){
	// assume we are passed a valid node.
	assert( aNode );

	// get all the children.
	DOMNodeList* nodeList = aNode->getChildNodes();

	for( unsigned int i = 0;  i <  nodeList->getLength(); ++i ){
		const DOMNode* curr = nodeList->item( i );
		const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

		if( nodeName == "#text" ) {
			continue;
		}
        else if( nodeName == "calInputValue" ) {
            mCalInputValue = XMLHelper<double>::getValue( curr );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
	        mainLog << "Unrecognized text string: " << nodeName << " found while parsing "
                    << getXMLNameStatic() << "." << endl;
		}
	}
}

//! write object to xml output stream
void CalDataInput::toInputXML( std::ostream& out, Tabs* tabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), out, tabs );
    XMLWriteElement( mCalInputValue, "calInputValue", out, tabs );
    XMLWriteClosingTag( getXMLNameStatic(), out, tabs );
}

//! Write object to debugging xml output stream.
void CalDataInput::toDebugXML( std::ostream& out, Tabs* tabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), out, tabs );
    XMLWriteElement( mCalInputValue, "calInputValue", out, tabs );
    XMLWriteClosingTag( getXMLNameStatic(), out, tabs );
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const std::string& CalDataInput::getXMLNameStatic() {
    const static string XML_NAME = "CalDataInput";
    return XML_NAME;
}

void CalDataInput::initCalc( const Demographic* aDemographics, const int aPeriod ) {
}

double CalDataInput::getCalInput( double aEfficiency ) {
    /*! \pre Efficiency is greater than zero. */
    assert( aEfficiency > 0 );

    return mCalInputValue;
}

double CalDataInput::getCalOutput( double aEfficiency ) {
    /*! \pre Efficiency is greater than zero. */
    assert( aEfficiency > 0 );

    return mCalInputValue * aEfficiency;
}

void CalDataInput::scaleValue( const double aScaleFactor ) {
    mCalInputValue *= aScaleFactor;
}
