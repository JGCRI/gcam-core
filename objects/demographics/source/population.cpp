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

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

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

    DOMNode* curr = 0;
    DOMNodeList* nodeList;
    string nodeName;

    // make sure we were passed a valid node.
    assert( node );

    nodeList = node->getChildNodes();

    for( int i = 0; i < static_cast<int>( nodeList->getLength() ); i++ ){
        curr = nodeList->item( i );

        // get the name of the node.
        nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        // total population 
        else if( nodeName == "population" ){
            XMLHelper<double>::insertValueIntoVector( curr, totalpop, modeltime, true );
        } 
        else {
            cout << "Unrecognized text string: " << nodeName << " found while parsing Population." << endl;
        }
    }
}

//! Writes datamembers to datastream in XML format.
void Population::toXML( ostream& out, Tabs* tabs ) const {

    const Modeltime* modeltime = scenario->getModeltime();
    int iter;

    // write the beginning tag.
    tabs->writeTabs( out );
    out << "<demographics>" << endl;

    // increase the indent.
    tabs->increaseIndent();

    // write the xml for the class members.
    for( iter = 0; iter < static_cast<int>( totalpop.size() ); iter++ ){
        XMLWriteElement( totalpop[ iter ], "population", out, tabs, modeltime->getPopPeriodToYear( iter ) );
    }

    // decrease the indent.
    tabs->decreaseIndent();

    // write the closing tag.
    tabs->writeTabs( out );
    out << "</demographics>" << endl;
}

//! Writes datamembers to debugging datastream in XML format.
void Population::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    const Modeltime* modeltime = scenario->getModeltime();

    // one additional period (base - 1) is read in for demographics data
    // call modeltime for proper offset
    int pop_period = modeltime->getmod_to_pop(period);

    // write the beginning tag.
    tabs->writeTabs( out );
    out << "<demographics>" << endl;

    // increase the indent.
    tabs->increaseIndent();

    // Write the xml for the class members.
    XMLWriteElement( malepop[ pop_period ], "malepop", out, tabs );

    XMLWriteElement( femalepop[ pop_period ], "femalepop", out, tabs );

    XMLWriteElement( totalpop[ pop_period ], "totalpop", out, tabs );
    // Done writing XML for the class members.

    // decrease the indent.
    tabs->decreaseIndent();

    // write the closing tag.
    tabs->writeTabs( out );
    out << "</demographics>" << endl;
}

//! return total population vector
const vector<double>& Population::getTotalPopVec() const {
    return totalpop;
}

//! return total population
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
void Population::outputfile( const string& regionName ) const {
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
void Population::MCoutput( const string& regionName ) const {
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