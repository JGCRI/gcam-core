/*! 
* \file demographic.cpp
* \ingroup CIAM
* \brief demographic class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <cassert>
#include <vector>

// xml headers
#include <xercesc/util/XMLString.hpp>
#include <xercesc/dom/DOM.hpp>

#include "Scenario.h"
#include "demographic.h"
#include "modeltime.h"
#include "xmlHelper.h"


using namespace std;
extern Scenario scenario;

//! Default constructor
demographic::demographic(){

}

//! Clear data members.
void demographic::clear(){
	malepop.clear();
	femalepop.clear();
	totalpop.clear();
	laborprod.clear();
	laborforce_p.clear();
	laborforce.clear();
}

//! parses demographic xml object
void demographic::XMLParse( const DOMNode* node ){
	
	const Modeltime* modeltime = scenario.getModeltime();

	DOMNode* curr = 0;
	DOMNodeList* nodeList;
	string nodeName;
	
	// make sure we were passed a valid node.
	assert( node );
	
	nodeList = node->getChildNodes();
	
	for( int i = 0; i < nodeList->getLength(); i++ ){
		curr = nodeList->item( i );
		
		// get the name of the node.
		nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
		// total population 
		if( nodeName == "population" ){
			totalpop.push_back( XMLHelper<double>::getValue( curr ) );
		}
		// labor productivity growth rate
		else if ( nodeName == "laborproductivity" ){
			laborprod.push_back( XMLHelper<double>::getValue( curr ) );
		}
		// labor force participation rate
		else if( nodeName == "laborforce" ){
			laborforce_p.push_back( XMLHelper<double>::getValue( curr ) );
		}
	}
	
	initData();
	// not read in so not sized by data
	// pop has one more historical period
	int popmaxper = modeltime->getmaxper() + modeltime->getdataoffset( 0 ); 
	malepop.resize( popmaxper ); 
	femalepop.resize( popmaxper );
}

//! Writes datamembers to datastream in XML format.
void demographic::toXML( ostream& out ) const {
	
	const Modeltime* modeltime = scenario.getModeltime();
	int iter;

	// write the beginning tag.
	Tabs::writeTabs( out );
	out << "<demographics>" << endl;
	
	// increase the indent.
	Tabs::increaseIndent();
	
	// write the xml for the class members.
	for( iter = 0; iter < static_cast<int>( totalpop.size() ); iter++ ){
		XMLWriteElement( totalpop[ iter ], "population", out, modeltime->getPopPeriodToYear( iter ) );
	}

	for( iter = 0; iter < static_cast<int>( laborprod.size() ); iter++ ){
		XMLWriteElement( laborprod[ iter ], "laborproductivity", out, modeltime->getPopPeriodToYear( iter ) );
	}

	for( iter = 0; iter < static_cast<int>( laborforce_p.size() ); iter++ ){
		XMLWriteElement( laborforce_p[ iter ], "laborforce", out, modeltime->getPopPeriodToYear( iter ) );
	}
	
	// decrease the indent.
	Tabs::decreaseIndent();
	
	// write the closing tag.
	Tabs::writeTabs( out );
	out << "</demographics>" << endl;
}

//! Writes datamembers to debugging datastream in XML format.
void demographic::toDebugXML( const int period, ostream& out ) const {
	
	const Modeltime* modeltime = scenario.getModeltime();

	// one additional period (base - 1) is read in for demographics data
	// call modeltime for proper offset
	int pop_period = modeltime->getmod_to_pop(period);

	// write the beginning tag.
	Tabs::writeTabs( out );
	out << "<demographics>" << endl;
	
	// increase the indent.
	Tabs::increaseIndent();
	
	// Write the xml for the class members.
	XMLWriteElement( malepop[ pop_period ], "malepop", out );
	
	XMLWriteElement( femalepop[ pop_period ], "femalepop", out );

	XMLWriteElement( totalpop[ pop_period ], "totalpop", out );

	XMLWriteElement( laborprod[ pop_period ], "laborprod", out );
	
	XMLWriteElement( laborforce_p[ pop_period ], "laborforce_p", out );

	XMLWriteElement( laborforce[ pop_period ], "laborforce", out );
	// Done writing XML for the class members.

	// decrease the indent.
	Tabs::decreaseIndent();
	
	// write the closing tag.
	Tabs::writeTabs( out );
	out << "</demographics>" << endl;
}

void demographic::initData(){
	
	laborforce.resize( totalpop.size() );

	for ( int i = 0; i < totalpop.size(); i++ ) {
		laborforce[ i ] = totalpop[ i ] * laborforce_p[ i ];
	}
}

//! return labor productivity
double demographic::labor( const int per ) const {
	const Modeltime* modeltime = scenario.getModeltime();
	return laborprod[modeltime->getmod_to_pop(per)];
}

//! return total population vector
const vector<double>& demographic::getTotalPopVec() const {
	return totalpop;
}

//! return total population
double demographic::total( const int per ) const {
	const Modeltime* modeltime = scenario.getModeltime();
	return totalpop[ modeltime->getmod_to_pop( per ) ];
}

//! return labor force (actual working)
double demographic::getlaborforce( const int per ) const {
	const Modeltime* modeltime = scenario.getModeltime();
	return laborforce[ modeltime->getmod_to_pop( per ) ];
}

//! show demographic information to screen
void demographic::show(int per) {
	const Modeltime* modeltime = scenario.getModeltime();
	int m = modeltime->getmod_to_pop(per);
	cout << "Male Population: " << malepop[m] << "\n";
	cout << "Female Population: " << femalepop[m] << "\n";
	cout << "Total Population: " << totalpop[m] << "\n";
}

//! outputing population info to file
void demographic::outputfile( const string& regname ) {
	int i=0;
	const Modeltime* modeltime = scenario.getModeltime();
	int maxper = modeltime->getmaxper();
	vector<double> temp(maxper);

	// function protocol
	void fileoutput3( string var1name,string var2name,string var3name,
				  string var4name,string var5name,string uname,vector<double> dout);
	
	// write population to temporary array since not all will be sent to output
	for (i=0;i<maxper;i++)
		temp[i] = totalpop[modeltime->getmod_to_pop(i)];
	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	fileoutput3( regname," "," "," ","population","1000s",temp);

	// labor productivity
	for (i=0;i<maxper;i++)
		temp[i] = laborprod[modeltime->getmod_to_pop(i)];
	fileoutput3( regname," "," "," ","labor prod","%/yr",temp);	
}

//! MiniCAM output to file
void demographic::MCoutput( const string& regname ) {
	int i=0;
	const Modeltime* modeltime = scenario.getModeltime();
	int maxper = modeltime->getmaxper();
	vector<double> temp(maxper);

	// function protocol
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout);
	
	// write population to temporary array since not all will be sent to output
	for (i=0;i<maxper;i++)
		temp[i] = totalpop[modeltime->getmod_to_pop(i)];
	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	dboutput4(regname,"General","Population","Total","thous",temp);

	// labor productivity
	for (i=0;i<maxper;i++)
		temp[i] = laborprod[modeltime->getmod_to_pop(i)];
	dboutput4(regname,"General","Labor Prod","GrowthRate","per yr",temp);	
}

//! Create calibration markets
void demographic::setCalibrationMarkets( const string& regionName ) {
	
	const string goodName = "GDP";
	const Modeltime* modeltime = scenario.getModeltime();
	Marketplace* marketplace = scenario.getMarketplace();

	if ( marketplace->setMarket( regionName, regionName, goodName, Market::CALIBRATION ) ) {
		vector<double> tempLFPs( modeltime->getmaxper() );
		for( int i = 0; i < modeltime->getmaxper(); i++ ){
			int popPeriod = modeltime->getmod_to_pop( i );
			tempLFPs[ i ] = pow( 1 + laborprod[ popPeriod ], modeltime->gettimestep( i ) );
		}
		marketplace->setPriceVector( goodName, regionName, tempLFPs );
	}
}