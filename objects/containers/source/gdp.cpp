/*! 
* \file gdp.cpp
* \ingroup CIAM
* \brief The GDP class source file.
* \author Josh Lurz, Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <cmath>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "containers/include/gdp.h"
#include "demographics/include/population.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/xml_helper.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//! Default Constructor
GDP::GDP() {
    // Resize all vectors to the max population period. 
    const int popMaxPeriod = scenario->getModeltime()->getmaxpopdata();
    laborProdGrowthRate.resize( popMaxPeriod );
    laborForceParticipationPercent.resize( popMaxPeriod );
    laborForce.resize( popMaxPeriod );
}

//! Destructor
GDP::~GDP(){
}

//! parses Population xml object
void GDP::XMLParse( const DOMNode* node ){

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
        else if ( nodeName == "laborproductivity" ){
            XMLHelper<double>::insertValueIntoVector( curr, laborProdGrowthRate, modeltime, true );
        }
        // labor force participation rate
        else if( nodeName == "laborforce" ){
            XMLHelper<double>::insertValueIntoVector( curr, laborForceParticipationPercent, modeltime, true );
        } else {
            cout << "Unrecognized text string: " << nodeName << " found while parsing GDP." << endl;
        }
    }
}

//! Writes datamembers to datastream in XML format.
void GDP::toXML( ostream& out, Tabs* tabs ) const {

    const Modeltime* modeltime = scenario->getModeltime();
    int iter;

    // write the beginning tag.
    tabs->writeTabs( out );
    out << "<GDP>" << endl;

    // increase the indent.
    tabs->increaseIndent();

    for( iter = 0; iter < static_cast<int>( laborProdGrowthRate.size() ); iter++ ){
        XMLWriteElement( laborProdGrowthRate[ iter ], "laborproductivity", out, tabs, modeltime->getPopPeriodToYear( iter ) );
    }

    for( iter = 0; iter < static_cast<int>( laborForceParticipationPercent.size() ); iter++ ){
        XMLWriteElement( laborForceParticipationPercent[ iter ], "laborforce", out, tabs, modeltime->getPopPeriodToYear( iter ) );
    }

    // decrease the indent.
    tabs->decreaseIndent();

    // write the closing tag.
    tabs->writeTabs( out );
    out << "</GDP>" << endl;
}

//! Writes datamembers to debugging datastream in XML format.
void GDP::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    const Modeltime* modeltime = scenario->getModeltime();
    int popPeriod = modeltime->getmod_to_pop( period );

    // write the beginning tag.
    tabs->writeTabs( out );
    out << "<GDP>" << endl;

    // increase the indent.
    tabs->increaseIndent();

    XMLWriteElement( laborProdGrowthRate[ popPeriod ], "laborprod", out, tabs );

    XMLWriteElement( laborForceParticipationPercent[ popPeriod ], "laborforce_p", out, tabs );

    XMLWriteElement( laborForce[ popPeriod ], "laborforce", out, tabs );
    // Done writing XML for the class members.

    // decrease the indent.
    tabs->decreaseIndent();

    // write the closing tag.
    tabs->writeTabs( out );
    out << "</GDP>" << endl;
}

//! Initialize the labor force.
void GDP::initData( const Population* regionalPop ){
    const Modeltime* modeltime = scenario->getModeltime();
    const int popmaxper = modeltime->getmaxpopdata();

    for ( int i = 0; i < popmaxper; i++ ) {
        laborForce[ i ] = regionalPop->getTotal(  i, true ) * laborForceParticipationPercent[ i ];
    }
}

//! Create calibration markets
void GDP::setupCalibrationMarkets( const string& regionName ) {

    const string goodName = "GDP";
    const Modeltime* modeltime = scenario->getModeltime();
    Marketplace* marketplace = scenario->getMarketplace();

    if ( marketplace->createMarket( regionName, regionName, goodName, Marketplace::CALIBRATION ) ) {
        vector<double> tempLFPs( modeltime->getmaxper() );
        for( int i = 0; i < modeltime->getmaxper(); i++ ){
            tempLFPs[ i ] = pow( 1 + laborProdGrowthRate[ modeltime->getmod_to_pop( i ) ], modeltime->gettimestep( i ) );
        }
        marketplace->setPriceVector( goodName, regionName, tempLFPs );
    }
}

//! Write back the calibrated values from the marketplace to the member variables.
void GDP::writeBackCalibratedValues( const string& regionName, const int period ) {

    const Marketplace* marketplace = scenario->getMarketplace();
    const Modeltime* modeltime = scenario->getModeltime();
    const string goodName = "GDP";

    // Only need to write back calibrated values for the current period.
    double totalLaborProd = marketplace->getPrice( goodName, regionName, period );

    laborProdGrowthRate[ modeltime->getmod_to_pop( period ) ] = pow( totalLaborProd, double( 1 ) / double( modeltime->gettimestep( period ) ) ) - 1;
}

//! return labor productivity
double GDP::getLaborProdGR( const int per ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    return laborProdGrowthRate[modeltime->getmod_to_pop(per)];
}

//! Return the  total labor force productivity. 
double GDP::getTotalLaborProductivity( const int period ) const {

    const Modeltime* modeltime = scenario->getModeltime();
    return pow( 1 + laborProdGrowthRate[ modeltime->getmod_to_pop( period ) ], modeltime->gettimestep( period ) );
}

//! return the labor force (actual working)
double GDP::getLaborForce( const int per ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    return laborForce[ modeltime->getmod_to_pop( per ) ];
}

//! outputing GDP info to file
void GDP::outputfile( const string& regionName ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxPeriod = modeltime->getmaxper();
    vector<double> temp( maxPeriod );

    // function protocol
    void fileoutput3( string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);

    // write gdp to temporary array since not all will be sent to output
    for ( int i = 0; i < maxPeriod; i++ ) {
        temp[ i ] = laborProdGrowthRate[ modeltime->getmod_to_pop( i ) ];
    }
    fileoutput3( regionName," "," "," ", "labor prod", "%/yr", temp );	
}

//! MiniCAM output to file
void GDP::MCoutput( const string& regionName ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxPeriod = modeltime->getmaxper();
    vector<double> temp( maxPeriod );

    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);

    // labor productivity
    for( int i = 0; i < maxPeriod; i++ ){
        temp[ i ] = laborProdGrowthRate[ modeltime->getmod_to_pop( i ) ];
    }
    dboutput4( regionName, "General", "Labor Prod", "GrowthRate", "per yr", temp );	
}