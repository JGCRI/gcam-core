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
* \file global_technology_datbase.cpp
* \ingroup Objects
* \brief GlobalTechnologyDatabase class source file.
* \author Pralit Patel
*/              

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

// User headers
#include "technologies/include/global_technology_database.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

typedef std::vector<boost::shared_ptr<GlobalTechnology> >::iterator GlobalTechListIterator;
typedef std::vector<boost::shared_ptr<GlobalTechnology> >::const_iterator CGlobalTechListIterator;

//! Default constructor
GlobalTechnologyDatabase::GlobalTechnologyDatabase() {
}

/*! \brief Get GlobalTechnology for the given name and year.
*
* This public function returns a GlobalTechnology with the corresponding name and year.
* If a GlobalTechnology could not be found with the
* passed in technology name and year NULL is returned.
* \author Pralit Patel
* \param aTechnologyName The technology name to look for.
* \param aYear The year of the technology requested.
* \return A new GlobalTechnology, or NULL if there is no GlobalTechnology for the tech name and year.
*/
const boost::shared_ptr<GlobalTechnology>& GlobalTechnologyDatabase::getTechnology( const string& aTechnologyName, const int aYear ) const {
    for( CGlobalTechListIterator it = mTechnologyList.begin(); it != mTechnologyList.end(); ++it ) {
        if( (*it)->getName() == aTechnologyName && (*it)->getYear() == aYear ) {
            return *it;
        }
    }
    // did not find..
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::ERROR );
    mainLog << "Couldn't find " << aTechnologyName << " for year " << aYear 
        << " in GlobalTechnologyDatabase." << endl;
    static const boost::shared_ptr<GlobalTechnology> nullPtr;
    return nullPtr;
}

//! parses GlobalTechnologyDatabase xml object
void GlobalTechnologyDatabase::XMLParse( const DOMNode* aNode ){
    // assume we are passed a valid node.
    assert( aNode );

    // get all the children.
    DOMNodeList* nodeList = aNode->getChildNodes();

    const Modeltime* modeltime = scenario->getModeltime();

    for( unsigned int i = 0;  i < nodeList->getLength(); ++i ){
        DOMNode* curr = nodeList->item( i );
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        else if( nodeName == GlobalTechnology::getXMLNameStatic() ) {
            boost::shared_ptr<GlobalTechnology> tmpTech ( new GlobalTechnology() );
            parseContainerNode( curr, mTechnologyList, tmpTech, &GlobalTechnology::parseIdentifier );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Unknown element " << nodeName << " encountered while parsing " << getXMLNameStatic() << endl;
        }
    }
}

//! Write out datamembers to XML output stream.
void GlobalTechnologyDatabase::toInputXML( ostream& aOut, Tabs* aTabs ) const {

    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );

    for( CGlobalTechListIterator i = mTechnologyList.begin(); i != mTechnologyList.end(); ++i ){
        ( *i )->toInputXML( aOut, aTabs );
    }

    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );

}

//! Write out XML for debugging purposes.
void GlobalTechnologyDatabase::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {

    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );

    for( CGlobalTechListIterator i = mTechnologyList.begin(); i != mTechnologyList.end(); ++i ) { 
        ( *i )->toDebugXML( aPeriod, aOut, aTabs );
    }

    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
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
const std::string& GlobalTechnologyDatabase::getXMLNameStatic() {
    const static string XML_NAME = "global-technology-database";
    return XML_NAME;
}
