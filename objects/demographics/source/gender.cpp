/*! 
* \file gender.cpp
* \ingroup Objects-SGM
* \brief world class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "demographics/include/gender.h"
#include "util/base/include/xml_helper.h"

using namespace std;
using namespace xercesc;
// static initializion
const string Gender::XML_NAME = "gender";

//! Default constructor
Gender::Gender(){
	mPopulation = -1;
	mSurvivingPop = 0;
	mSurvivalRate = 0;
}

//! parse SOME xml data for Male or Female objects
void Gender::XMLParse( const DOMNode* node ) {
    /*! \pre make sure we were passed a valid node. */
    assert( node );

    // get all child nodes.
    DOMNodeList* nodeList = node->getChildNodes();

    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
		else if ( nodeName == "population" ) {
			mPopulation = XMLHelper<double>::getValue( curr );
		}
		else if (nodeName == "survivalRate" ) {
			mSurvivalRate = XMLHelper<double>::getValue( curr );
		}
        else if( !XMLDerivedClassParse( nodeName, curr ) ){
			ILogger& mainLog = ILogger::getLogger( "main_log" );
			mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string " << nodeName << " encountered while parsing " << getXMLName() << endl;
		}
	}
}

//! Output to XML data
void Gender::toInputXML( ostream& out, Tabs* tabs ) const {
	XMLWriteOpeningTag( getXMLNameStatic(), out, tabs, "", 0, getXMLName() );

	XMLWriteElementCheckDefault( mPopulation, "population", out, tabs );
	XMLWriteElementCheckDefault( mSurvivingPop, "survivingPop", out, tabs );
	XMLWriteElementCheckDefault( mSurvivalRate, "survivalRate", out, tabs );

	// write out variables for derived classes
    toInputXMLDerived( out, tabs );

	// finished writing xml for the class members.
	XMLWriteClosingTag( getXMLNameStatic(), out, tabs );
}

//! Output debug info to XML data
void Gender::toDebugXML( ostream& out, Tabs* tabs ) const {
	XMLWriteOpeningTag ( getXMLName(), out, tabs );

	XMLWriteElement( mPopulation, "population", out, tabs );
	XMLWriteElement( mSurvivingPop, "survivingPop", out, tabs );
	XMLWriteElement( mSurvivalRate, "survivalRate", out, tabs );

	// write out variables for derived classes
    toDebugXMLDerived( out, tabs );

	// finished writing xml for the class members.
	XMLWriteClosingTag( getXMLName(), out, tabs );
}

// calculate the suviving population which is 
// passed on to the next age group and time
double Gender::calcSurvivingPop() {
    assert( mPopulation != -1 );
    mSurvivingPop = mPopulation * mSurvivalRate;
    return mSurvivingPop;
}

//! Returns the population
double Gender::getPopulation(){
    assert( mPopulation != -1 );
	return mPopulation;
}

//! Sets the population to the param passed in
void Gender::setPopulation( double aPopulation ){
	mPopulation = aPopulation;
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
const std::string& Gender::getXMLNameStatic(){
	return XML_NAME;
}

