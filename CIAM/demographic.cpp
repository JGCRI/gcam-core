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

#include "scenario.h"
#include "demographic.h"
#include "modeltime.h"
#include "xmlHelper.h"
#include "Configuration.h"
#include "Marketplace.h"

using namespace std;
extern Scenario* scenario;

//! Default constructor
demographic::demographic(){
   const Modeltime* modeltime = scenario->getModeltime();
   
   // Resize all vectors to the max population period. 
   const int popmaxper = modeltime->getmaxpopdata();
	malepop.resize( popmaxper ); 
	femalepop.resize( popmaxper );
   laborforce.resize( popmaxper );
   totalpop.resize( popmaxper );
   laborprod.resize( popmaxper );
   laborforce_p.resize( popmaxper );
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
		// total population 
		if( nodeName == "population" ){
         XMLHelper<double>::insertValueIntoVector( curr, totalpop, modeltime, true );
		}
		// labor productivity growth rate
		else if ( nodeName == "laborproductivity" ){
         XMLHelper<double>::insertValueIntoVector( curr, laborprod, modeltime, true );
		}
		// labor force participation rate
		else if( nodeName == "laborforce" ){
		   XMLHelper<double>::insertValueIntoVector( curr, laborforce_p, modeltime, true );
		}
	}
	
	initData();

}

//! Writes datamembers to datastream in XML format.
void demographic::toXML( ostream& out ) const {
	
	const Modeltime* modeltime = scenario->getModeltime();
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
	
	const Modeltime* modeltime = scenario->getModeltime();

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
	
   const Modeltime* modeltime = scenario->getModeltime();
   const int popmaxper = modeltime->getmaxpopdata();

	for ( int i = 0; i < popmaxper; i++ ) {
		laborforce[ i ] = totalpop[ i ] * laborforce_p[ i ];
	}
}

//! return labor productivity
double demographic::labor( const int per ) const {
	const Modeltime* modeltime = scenario->getModeltime();
	return laborprod[modeltime->getmod_to_pop(per)];
}

//! return total population vector
const vector<double>& demographic::getTotalPopVec() const {
	return totalpop;
}

//! return total population
double demographic::total( const int per ) const {
	const Modeltime* modeltime = scenario->getModeltime();
	return totalpop[ modeltime->getmod_to_pop( per ) ];
}

//! return labor force (actual working)
double demographic::getlaborforce( const int per ) const {
	const Modeltime* modeltime = scenario->getModeltime();
	return laborforce[ modeltime->getmod_to_pop( per ) ];
}

//! show demographic information to screen
void demographic::show(int per) {
	const Modeltime* modeltime = scenario->getModeltime();
	const int m = modeltime->getmod_to_pop(per);
	cout << "Male Population: " << malepop[m] << endl;
	cout << "Female Population: " << femalepop[m] << endl;
	cout << "Total Population: " << totalpop[m] << endl;
}

//! outputing population info to file
void demographic::outputfile( const string& regname ) {
	int i=0;
	const Modeltime* modeltime = scenario->getModeltime();
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
	const Modeltime* modeltime = scenario->getModeltime();
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

//! Write back the calibrated values from the marketplace to the member variables.
void demographic::writeBackCalibratedValues( const string& regionName, const int period ) {
   
   const Marketplace* marketplace = scenario->getMarketplace();
   const Modeltime* modeltime = scenario->getModeltime();
   const string goodName = "GDP";

   // Only need to write back calibrated values for the current period.
   double totalLaborProd = marketplace->showprice( goodName, regionName, period );
   
   laborprod[ modeltime->getmod_to_pop( period ) ] = pow( totalLaborProd, double( 1 ) / double( modeltime->gettimestep( period ) ) ) - 1;
}

//! Create calibration markets
void demographic::setupCalibrationMarkets( const string& regionName ) {
	
	const string goodName = "GDP";
	const Modeltime* modeltime = scenario->getModeltime();
	Marketplace* marketplace = scenario->getMarketplace();

	if ( marketplace->setMarket( regionName, regionName, goodName, Marketplace::CALIBRATION ) ) {
		vector<double> tempLFPs( modeltime->getmaxper() );
		for( int i = 0; i < modeltime->getmaxper(); i++ ){
			tempLFPs[ i ] = pow( 1 + laborprod[ modeltime->getmod_to_pop( i ) ], modeltime->gettimestep( i ) );
		}
		marketplace->setPriceVector( goodName, regionName, tempLFPs );
	}
}

//! Return the correct total labor force productivity. 
double demographic::getTotalLaborProductivity( const int period ) const {

      const Modeltime* modeltime = scenario->getModeltime();
      return pow( 1 + laborprod[ modeltime->getmod_to_pop( period ) ], modeltime->gettimestep( period ) );
}
