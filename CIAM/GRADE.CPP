/*! 
* \file GRADE.CPP
* \ingroup CIAM
* \brief grade class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include <string>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <cmath>
#include <cassert>
#include "scenario.h"
#include "grade.h"
#include "modeltime.h"
#include "xmlHelper.h"

using namespace std;

extern Scenario scenario;

//! Default constructor
grade::grade() {
	initElementalMembers();
}

//! Clear member variables
void grade::clear() {
	initElementalMembers();
	name = "";
}

//! Initialize elemental members
void grade::initElementalMembers() {
	year = 0;
	available = 0;
	extractCost = 0;
	totalCost = 0;
}

//! Initialize data members from XML.
void grade::XMLParse( const DOMNode* tempNode ) {
	DOMNodeList* tempNodeLst;
	DOMNode* tNode = 0;
	string tNodeName;

	//! \pre assume we are passed a valid node.
	assert( tempNode );
	
	tempNodeLst = tempNode->getChildNodes();

	for( int i = 0; i < tempNodeLst->getLength(); i++ ) {
		tNode = tempNodeLst->item( i );
		tNodeName = XMLHelper<string>::safeTranscode( tNode->getNodeName() );
		
		if( tNodeName == "name" ){
			name = XMLHelper<string>::getValueString( tNode );
			#if ( _DEBUG )
				// cout << "\t\t\tGrade name set as " << name << endl;
			#endif
		}
		else if( tNodeName == "year" ){
			year = XMLHelper<int>::getValue( tNode );
		}
		else if( tNodeName == "available" ){
			available = XMLHelper<double>::getValue( tNode );
		}
		else if( tNodeName == "extractioncost" ){
			extractCost = XMLHelper<double>::getValue( tNode );
		}
	/*	else if( tNodeName == "environmentalcost" ){
			environCost = XMLHelper<double>::getValue( tNode );
		}
		else if( tNodeName == "severencetax" ){
			tax = XMLHelper<double>::getValue( tNode );
		}
		else if( tNodeName == "techchange" ){
			techChange = XMLHelper<double>::getValue( tNode );
		} */
	}
}

//! Write datamembers to datastream in XML format.
void grade::toXML( ostream& out ) const {
	
	Tabs::writeTabs( out );
	out << "<period>" << endl;
	
	Tabs::increaseIndent();

	XMLWriteElement( name, "name", out );
	XMLWriteElement( year, "year", out );
	XMLWriteElement( available, "available", out );
	XMLWriteElement( extractCost, "extractioncost", out );
//	XMLWriteElement( environCost, "environmentalcost", out );
//	XMLWriteElement( tax, "severencetax", out );
//	XMLWriteElement( techChange, "techchange", out );
	// XMLWriteElement( totalCost, "totalcost", out );
	
	Tabs::decreaseIndent();

	Tabs::writeTabs( out );
	out << "</period>" << endl;
}

//! Write datamembers to debugging datastream in XML format.
void grade::toDebugXML( const int period, ostream& out ) const {
	
	Tabs::writeTabs( out );
	out << "<grade name=\"" << name << "\" year=\"" << year << "\">" << endl;
	Tabs::increaseIndent();

	XMLWriteElement( available, "available", out );
	XMLWriteElement( extractCost, "extractioncost", out );
//	XMLWriteElement( environCost, "environmentalcost", out );
//	XMLWriteElement( techChange, "techchange", out );
//	XMLWriteElement( tax, "severencetax", out );
	XMLWriteElement( totalCost, "totalcost", out );
	
	Tabs::decreaseIndent();
	Tabs::writeTabs( out );
	out << "</grade>" << endl;
}

//! Print technology info.
void grade::printGrade() const
{
	// write to file or database later
	cout << name << endl;
	cout << "Year: " << year << endl;
	cout << "Available: " << available << endl;
	cout << "Extraction Cost: " << extractCost << endl;
}

//! Calculate cummulative technical change for each grade.
void grade::calcTechChangeCumm( const double techChange, const int per ) {
	const Modeltime* modeltime = scenario.getModeltime();
        if (per > 0) {
            techChangeCumm[per] = techChangeCumm[ per-1 ] * 
                                  pow( ( 1.0 + techChange ), modeltime->gettimestep(per) );
        }
}

//! Total cost of each grade.
void grade::calcCost(  const double tax, const double cumTechChange, const double environCost, const int per ) {
	totalCost = ( extractCost + environCost ) / cumTechChange + tax;
}

//! Return available amount in each grade.
double grade::getAvail() const {
	return available;
}

//! Return the total cost.
double grade::getCost() const {
	return totalCost;
}

//! Return the extraction cost.
double grade::getExtCost() const {
	return extractCost;
}

//! Set the extraction cost.
void grade::setExtCost( const double eCost ) {
	extractCost = eCost;
}

//! Get the name.
string grade::getName() const {
	return name;
}


