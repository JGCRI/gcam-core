/*! 
* \file cal_data_output_percap.cpp
* \ingroup Objects
* \brief CalDataOutputPercap class source file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/


#include "util/base/include/definitions.h"
#include <string>
#include <xercesc/dom/DOMNodeList.hpp>
#include "technologies/include/cal_data_output_percap.h"

#include "util/base/include/xml_helper.h"
#include "demographics/include/demographic.h"

using namespace std;
using namespace xercesc;

/*! \brief Constructor.
* \author James Blackwood
*/
CalDataOutputPercap::CalDataOutputPercap() {
    mPopulation = -1;
    mCalOutputPercapValue = 0;
}

/*! \brief Clone the current object.
* \return A clone of the object.
*/
CalDataOutputPercap* CalDataOutputPercap::clone() const {
    return new CalDataOutputPercap( *this );
}

/*! \brief Parses XML for the object.
* \author James Blackwood
* \param aNode pointer to the current node in the XML input tree
*/
void CalDataOutputPercap::XMLParse( const DOMNode* aNode ){
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
        else if( nodeName == "calOutputPercapValue" ) {
            mCalOutputPercapValue = XMLHelper<double>::getValue( curr );
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
void CalDataOutputPercap::toInputXML( std::ostream& out, Tabs* tabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), out, tabs );
    XMLWriteElement( mCalOutputPercapValue, "calOutputPercapValue", out, tabs );
    XMLWriteClosingTag( getXMLNameStatic(), out, tabs );
}

//! Write object to debugging xml output stream.
void CalDataOutputPercap::toDebugXML( std::ostream& out, Tabs* tabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), out, tabs );
    XMLWriteElement( mCalOutputPercapValue, "calOutputPercapValue", out, tabs );
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
const std::string& CalDataOutputPercap::getXMLNameStatic() {
    const static string XML_NAME = "CalDataOutputPercap";
    return XML_NAME;
}

void CalDataOutputPercap::initCalc( const Demographic* aDemographics, const int aPeriod ) {
    /*! \pre A demographics object was passed in. */
    assert( aDemographics );

    // Cache the population for the period.
    // TODO: This is not ideal because the population cannot change.
    mPopulation = aDemographics->getTotal( aPeriod );
}

double CalDataOutputPercap::getCalInput( const double aEfficiency ) {
    /*! \pre Efficiency is greater than zero. */
    assert( aEfficiency > 0 );

    return mCalOutputPercapValue * mPopulation / 1000 / aEfficiency;
}

double CalDataOutputPercap::getCalOutput( const double aEfficiency ) {
    /*! \pre Efficiency is greater than zero. */
    assert( aEfficiency > 0 );

    return mCalOutputPercapValue * mPopulation / aEfficiency;
}

void CalDataOutputPercap::scaleValue( const double aScaleValue ) {
    mCalOutputPercapValue *= aScaleValue;
}
