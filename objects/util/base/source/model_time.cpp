/*! 
* \file model_time.cpp
* \ingroup CIAM
* \brief modeltime class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <fstream>

#include <vector>
#include <map>
#include <string>
#include <cassert>

// xml headers
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "util/base/include/xml_helper.h"
#include "util/base/include/model_time.h"

using namespace std;
using namespace xercesc;

// static initialize.
const string Modeltime::XML_NAME = "modeltime";

//! Default constructor.
Modeltime::Modeltime(){
	initElementalMembers();
}

//! Clear all datamembers.
void Modeltime::clear(){

	// First clear all the integer members.
	initElementalMembers();
	
	// Now initialize all the maps and vectors.
	periodToTimeStep.clear();
	dataPeriodToModelPeriod.clear();
	modelPeriodToPopPeriod.clear();
	popDataToVariable.clear();
	dataOffset.clear();
	modelPeriodToYear.clear();
	yearToModelPeriod.clear();
}

//! Initialize elemental type datamembers.
void Modeltime::initElementalMembers(){
	startYear = 0;
	interYear1 = 0;
	interYear2 = 0;
	endYear = 0;
	startReportYear = 0;
	popStartYear = 0;
	dataEndYear = 0;
	maxPeriod = 0;
	maxDataPeriod = 0;
	dataTimeStep = 0;
	timeStep1 = 0;
	timeStep2 = 0;
	timeStep3 = 0;
	numberOfPeriods1 = 0;
	numberOfPeriods1a = 0;
	numberOfPeriods2 = 0;
	numberOfPeriods2a = 0;
	numberOfPeriods3 = 0;
	numberOfPeriods3a = 0;
}

//! Set the data members from the XML input.
void Modeltime::XMLParse( const DOMNode* node ) {
	DOMNode* curr = 0;
	DOMNodeList* nodeList; 
	string nodeName;
	
	// assume node is valid.
	assert( node );

	// get all children of the node.
	nodeList = node->getChildNodes();
	
	// loop through the children
	for ( int i = 0; i < static_cast<int>( nodeList->getLength() ); i++ ){
		curr = nodeList->item( i );
		nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
		
		// select the type of node.
      if( nodeName == "#text" ) {
         continue;
      }

		else if ( nodeName == "startyear" ){
			startYear = XMLHelper<int>::getValue( curr );
		} 
		else if ( nodeName == "interyear1" ){
			interYear1 = XMLHelper<int>::getValue( curr );
		} 
		else if ( nodeName == "interyear2" ){
			interYear2 = XMLHelper<int>::getValue( curr );
		} 
		else if ( nodeName == "endyear" ){
			endYear = XMLHelper<int>::getValue( curr );
		} 
		else if ( nodeName == "startReportYear" ){
			startReportYear = XMLHelper<int>::getValue( curr );
		} 
		else if ( nodeName == "timestep1" ){
			timeStep1 = XMLHelper<int>::getValue( curr );
		} 
		else if ( nodeName == "timestep2" ){
			timeStep2 = XMLHelper<int>::getValue( curr );
		} 
		else if ( nodeName == "timestep3" ){
			timeStep3 = XMLHelper<int>::getValue( curr );
		} 
		else if ( nodeName == "popbegin" ){
			popStartYear = XMLHelper<int>::getValue( curr );
		} 
		else if ( nodeName == "dataend" ){
			dataEndYear = XMLHelper<int>::getValue( curr );
		} 
		else if ( nodeName == "datatimestep" ){
			dataTimeStep = XMLHelper<int>::getValue( curr );
		}
      else {
         cout << "Unrecognized text string: " << nodeName << " found while parsing modeltime." << endl;
      }
	}
}

//! Write data members to datastream in XML format.
void Modeltime::toInputXML( ostream& out, Tabs* tabs ) const {
	
	XMLWriteOpeningTag( getXMLName(), out, tabs );

	XMLWriteElement( startYear, "startyear", out, tabs );
	XMLWriteElement( interYear1, "interyear1", out, tabs );
	XMLWriteElement( interYear2, "interyear2", out, tabs );
	XMLWriteElement( endYear, "endyear", out, tabs );
    XMLWriteElement( startReportYear, "startReportYear", out, tabs );
	XMLWriteElement( timeStep1, "timestep1", out, tabs );
	XMLWriteElement( timeStep2, "timestep2", out, tabs );
	XMLWriteElement( timeStep3, "timestep3", out, tabs );
	XMLWriteElement( popStartYear, "popbegin", out, tabs );
	XMLWriteElement( dataEndYear, "dataend", out, tabs );
	XMLWriteElement( dataTimeStep, "datatimestep", out, tabs );
	
	XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Write out object to output stream for debugging.
void Modeltime::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
	
	XMLWriteOpeningTag( getXMLName(), out, tabs );

	XMLWriteElement( startYear, "startyear", out, tabs );
	XMLWriteElement( interYear1, "interyear1", out, tabs );
	XMLWriteElement( interYear2, "interyear2", out, tabs );
	XMLWriteElement( endYear, "endyear", out, tabs );
    XMLWriteElement( startReportYear, "startReportYear", out, tabs );
	XMLWriteElement( timeStep1, "timestep1", out, tabs );
	XMLWriteElement( timeStep2, "timestep2", out, tabs );
	XMLWriteElement( timeStep3, "timestep3", out, tabs );
	XMLWriteElement( popStartYear, "popbegin", out, tabs );
	XMLWriteElement( dataEndYear, "dataend", out, tabs );
	XMLWriteElement( dataTimeStep, "datatimestep", out, tabs );
	XMLWriteElement( periodToTimeStep[ period ], "periodToTimeStep", out, tabs );

	// Write out the three vectors associated with the model period.
	XMLWriteElement( dataOffset[ period ], "dataOffset", out, tabs );
	XMLWriteElement( modelPeriodToYear[ period ], "modelPeriodToYear", out, tabs );
	XMLWriteElement( modelPeriodToPopPeriod[ period ], "modelPeriodToPopPeriod", out, tabs );

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
const std::string& Modeltime::getXMLName() const {
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
const std::string& Modeltime::getXMLNameStatic() {
	return XML_NAME;
}

void Modeltime::set() {
	
	numberOfPeriods1 = (interYear1 - startYear)/timeStep1 + 1; // +1 for first year
	numberOfPeriods2 = (interYear2 - interYear1)/timeStep2; 
	numberOfPeriods3 = (endYear - interYear2)/timeStep3;
	
	numberOfPeriods1a = numberOfPeriods1; // initialize
	numberOfPeriods2a = numberOfPeriods2;
	numberOfPeriods3a = numberOfPeriods3;
	// write message if time intervals are not divisible by their
	// relative time steps, model will still run okay
	int rem1 = (interYear1 - startYear)%timeStep1;
	int rem2 = (interYear2 - interYear1)%timeStep2;
	int rem3 = (endYear - interYear2)%timeStep3;
	
	if(rem1 != 0) {
		numberOfPeriods1a++; // one more for remainder year
		cout<<"first time interval not divisible timeStep1\n"; 
	}
	if(rem2 != 0) {
		numberOfPeriods2a++; // one more for remainder year
		cout<<"second time interval not divisible timeStep2\n"; 
	}
	if(rem3 != 0) {
		numberOfPeriods3a++; // one more for remainder year
		cout<<"third time interval not divisible timeStep3\n"; 
	}
	maxPeriod = numberOfPeriods1a + numberOfPeriods2a + numberOfPeriods3a; // calculate total number of periods
	// number of periods for general data
	maxDataPeriod = (dataEndYear - startYear)/dataTimeStep+1; // +1 for first year
	// number of periods for population data (one more than general data)
	maxPopData = (dataEndYear - popStartYear)/dataTimeStep+1; // +1 for first year
	// initialize per_timeStep vector
	// retrieve timeStep for each modeling period
	periodToTimeStep.resize(maxPeriod);

    int i;	//Need to define "i" outside loops in order to work with strict c++ definitions of gcc

	for(i=0;i<numberOfPeriods1;i++){
		periodToTimeStep[i]=timeStep1;
	}
	for(i=numberOfPeriods1;i<numberOfPeriods1a;i++){
		periodToTimeStep[i]=rem1;
	}
	for(i=numberOfPeriods1a;i<(numberOfPeriods1a+numberOfPeriods2);i++) { 
		periodToTimeStep[i]=timeStep2;
	}
	for(i=(numberOfPeriods1a+numberOfPeriods2);i<(numberOfPeriods1a+numberOfPeriods2a);i++) { 
		periodToTimeStep[i]=rem2;
	}
	for(i=(numberOfPeriods1a+numberOfPeriods2a);i<(numberOfPeriods1a+numberOfPeriods2a+numberOfPeriods3);i++){
		periodToTimeStep[i]=timeStep3;
	}
	for(i=(numberOfPeriods1a+numberOfPeriods2a+numberOfPeriods3);i<(numberOfPeriods1a+numberOfPeriods2a+numberOfPeriods3a);i++){
		periodToTimeStep[i]=rem3;
	}
	
	// initialize map object
	// retrieve model period from year
	int baseyr = startYear;
	yearToModelPeriod[baseyr] = 0; // map object, no resize required
	modelPeriodToYear.resize(maxPeriod);
		
	modelPeriodToYear[0] = baseyr;
	
	for (i=1;i<maxPeriod;i++) {
		yearToModelPeriod[baseyr + periodToTimeStep[i]] = i;
		modelPeriodToYear[i] = baseyr + periodToTimeStep[i];
		// set years between two time periods to correspond to the
		// second time period
		if(periodToTimeStep[i]>1) {
			for(int y=1;y<periodToTimeStep[i];y++)
				yearToModelPeriod[baseyr + y] = i;
		} 
		baseyr += periodToTimeStep[i];
	}
	
	popPeriodToYear.resize( maxPopData );
	int currYear = popStartYear;
	for( i = 0; i < maxPopData; i++ ){
      yearToPopPeriod[ currYear ] = i;
		popPeriodToYear[ i ] = currYear;
		currYear += dataTimeStep;
	}

	// number of model periods to reach each data period
	dataOffset.resize(maxDataPeriod);
	// retrieve model period from data period
	dataPeriodToModelPeriod.resize(maxDataPeriod);
	
	for (i=0;i<maxDataPeriod;i++) {
		int m = yearToModelPeriod[startYear + i*dataTimeStep];
		dataOffset[i] = dataTimeStep/periodToTimeStep[m];
		dataPeriodToModelPeriod[i] = m;
	}

	// retrieve population period from model period
	modelPeriodToPopPeriod.resize(maxPeriod);
	for (i=0;i<maxPeriod;i++) {
		int m = i+dataOffset[0]; //offset by first timeStep only
		modelPeriodToPopPeriod[i] = m;
	}
	// retrieve pop variable index from pop data period
	popDataToVariable.resize(maxPopData);
	popDataToVariable[0] = 0; 
	for (i=0;i<maxDataPeriod;i++) {
		popDataToVariable[i+1] = dataPeriodToModelPeriod[i]+dataOffset[0];
	}
}