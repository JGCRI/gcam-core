/*! 
* \file population.cpp
* \ingroup CIAM
* \brief Population class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <string>
#include <map>
#include <cassert>
#include <vector>
#include <cmath>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "containers/include/scenario.h"
#include "demographics/include/population.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/configuration.h"
#include "util/logger/include/ilogger.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string Population::XML_NAME = "demographics";

//! Default constructor
Population::Population(){
    // Resize all vectors to the max population period. 
    const int popmaxper = scenario->getModeltime()->getmaxpopdata();
    malepop.resize( popmaxper ); 
    femalepop.resize( popmaxper );
    totalpop.resize( popmaxper );
}

//! Clear data members.
void Population::clear(){
    malepop.clear();
    femalepop.clear();
    totalpop.clear();
}

//! parses Population xml object
void Population::XMLParse( const DOMNode* node ){

    const Modeltime* modeltime = scenario->getModeltime();

    // make sure we were passed a valid node.
    assert( node );

    DOMNodeList* nodeList = node->getChildNodes();

    for( unsigned int i = 0; i < nodeList->getLength(); ++i ){
        DOMNode* curr = nodeList->item( i );

        // get the name of the node.
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        // total population 
        else if( nodeName == "population" ){
            XMLHelper<double>::insertValueIntoVector( curr, totalpop, modeltime, true );
        } 
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing Population." << endl;
        }
    }
}

//! Writes datamembers to datastream in XML format.
void Population::toInputXML( ostream& out, Tabs* tabs ) const {

    const Modeltime* modeltime = scenario->getModeltime();

	XMLWriteOpeningTag( getXMLName(), out, tabs );

    // write the xml for the class members.
    for(unsigned int iter = 0; iter < totalpop.size(); iter++ ){
        XMLWriteElement( totalpop[ iter ], "population", out, tabs, modeltime->getPopPeriodToYear( iter ) );
    }

	XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Writes datamembers to debugging datastream in XML format.
void Population::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    const Modeltime* modeltime = scenario->getModeltime();

    // one additional period (base - 1) is read in for demographics data
    // call modeltime for proper offset
    int popPeriod = modeltime->getmod_to_pop( period );

    XMLWriteOpeningTag( getXMLName(), out, tabs );
    XMLWriteElement( malepop[ popPeriod ], "malepop", out, tabs );
    XMLWriteElement( femalepop[ popPeriod ], "femalepop", out, tabs );
    XMLWriteElement( totalpop[ popPeriod ], "totalpop", out, tabs );
	XMLWriteClosingTag( getXMLName(), out, tabs );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& Population::getXMLName() const {
	return XML_NAME;
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
const std::string& Population::getXMLNameStatic() {
	return XML_NAME;
}

//! return total population vector
const vector<double>& Population::getTotalPopVec() const {
    return totalpop;
}

//! return total population
// Todo: Remove the second parameter and always pass in the model period.
double Population::getTotal( const int per, const bool isPopPeriod ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    
    int period;
    if( isPopPeriod ){
        period = per;
    }
    else {
        period = modeltime->getmod_to_pop( per );
    }
    return totalpop[ period ];
}

//! outputing population info to file
void Population::csvOutputFile( const string& regionName ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxPeriod = modeltime->getmaxper();
    vector<double> temp( maxPeriod );

    // function protocol
    void fileoutput3( string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);

    // write population to temporary array since not all will be sent to output
    for ( int i = 0; i < maxPeriod; i++ ){
        temp[ i ] = totalpop[ modeltime->getmod_to_pop( i ) ];
    }

    // function arguments are variable name, double array, db name, table name
    // the function writes all years
    fileoutput3( regionName," "," "," ","population","1000s",temp);
}

//! MiniCAM output to file
void Population::dbOutput( const string& regionName ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxPeriod = modeltime->getmaxper();
    vector<double> temp( maxPeriod );

    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);

    // write population to temporary array since not all will be sent to output
    for ( int i = 0; i < maxPeriod; i++ ){
        temp[ i ] = totalpop[ modeltime->getmod_to_pop( i ) ];
    }
    // function arguments are variable name, double array, db name, table name
    // the function writes all years
    dboutput4( regionName, "General", "Population", "Total", "thous", temp );
}
