/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Laboratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responsibility for the 
	use of this software.
*/

/*! 
* \file output_meta_data.cpp
* \ingroup Objects
* \brief The OutputMetaData class source file
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "containers/include/output_meta_data.h"
#include "util/base/include/ivisitor.h"
#include "util/base/include/xml_helper.h"
#include "util/logger/include/ilogger.h"

using namespace std;
using namespace xercesc;

//! Constructor
OutputMetaData::OutputMetaData() {
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
const std::string& OutputMetaData::getXMLNameStatic() {
    const static string XML_NAME = "output-meta-data";
    return XML_NAME;
}

/*! \brief Write out XML data for input.
* \param aOut Output stream.
* \param aTabs Tabs object.
*/
void OutputMetaData::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag ( getXMLNameStatic(), aOut, aTabs );
    typedef list<string>::const_iterator CListIterator;
    map<string, string> attrs;
    for( CListIterator iter = mPrimaryFuels.begin(); iter != mPrimaryFuels.end(); ++iter ) {
        attrs[ "var" ] = *iter;
        XMLWriteElementWithAttributes( "", "primary-fuel", aOut, aTabs, attrs );
    }
    for( CListIterator iter = mSummableVariables.begin(); iter != mSummableVariables.end(); ++iter ) {
        attrs[ "var" ] = *iter;
        XMLWriteElementWithAttributes( "", "summable", aOut, aTabs, attrs );
    }
    XMLWriteElement( mScenarioSummary, "summary", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

/*! \brief Parse the meta-data from XML.
* \details The model does not use this meta-data internally but reads it from
*          the input XML file here and passes that information along to both the
*          database output xml and the derived xml input file.
* \param aNode Root node of the object's DOM subtree.
*/
void OutputMetaData::XMLParse( const DOMNode* aNode ) {
    /*! \pre make sure we were passed a valid node. */
    assert( aNode );

    // get all child nodes.
    const DOMNodeList* nodeList = aNode->getChildNodes();

    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        const DOMNode* curr = nodeList->item( i );
        if( curr->getNodeType() != DOMNode::ELEMENT_NODE ){
            continue;
        }
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
        if( nodeName == "primary-fuel" ){
            mPrimaryFuels.push_back( XMLHelper<string>::getAttr( curr, "var" ) );
        }
        else if( nodeName == "summable" ){
            mSummableVariables.push_back( XMLHelper<string>::getAttr( curr, "var" ) );
        }
        else if ( nodeName == "summary" ){
            mScenarioSummary = XMLHelper<string>::getValue( curr );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing " << getXMLNameStatic() << "." << endl;
        }
    }
}

void OutputMetaData::accept( IVisitor* aVisitor, const int aPeriod ) const{
	aVisitor->startVisitOutputMetaData( this, aPeriod );
	aVisitor->endVisitOutputMetaData( this, aPeriod );
}

//! Get the primary fuel list. Remove this function once the output database is removed.
const list<string>& OutputMetaData::getPrimaryFuelList() const {
    return mPrimaryFuels;
}
