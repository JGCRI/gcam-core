/*! 
* \file Grade.CPP
* \ingroup CIAM
* \brief Grade class source file.
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
#include "Grade.h"
#include "modeltime.h"
#include "xmlHelper.h"

using namespace std;

extern Scenario* scenario;

//! Default constructor
Grade::Grade() {
	initElementalMembers();
}

//! Clear member variables
void Grade::clear() {
	initElementalMembers();
	name = "";
}

//! Initialize elemental members
void Grade::initElementalMembers() {
	year = 0;
	available = 0;
	extractCost = 0;
	totalCost = 0;
}

//! Initialize data members from XML.
void Grade::XMLParse( const DOMNode* tempNode ) {
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
	}
}

//! Write datamembers to datastream in XML format.
void Grade::toXML( ostream& out ) const {
	
	Tabs::writeTabs( out );
	out << "<period>" << endl;
	
	Tabs::increaseIndent();

	XMLWriteElement( name, "name", out );
	XMLWriteElement( year, "year", out );
	XMLWriteElement( available, "available", out );
	XMLWriteElement( extractCost, "extractioncost", out );
	XMLWriteElement( totalCost, "totalcost", out );
	
	Tabs::decreaseIndent();

	Tabs::writeTabs( out );
	out << "</period>" << endl;
}

//! Write datamembers to debugging datastream in XML format.
void Grade::toDebugXML( const int period, ostream& out ) const {
	
	Tabs::writeTabs( out );
	out << "<Grade name=\"" << name << "\" year=\"" << year << "\">" << endl;
	Tabs::increaseIndent();

	XMLWriteElement( available, "available", out );
	XMLWriteElement( extractCost, "extractioncost", out );
	XMLWriteElement( totalCost, "totalcost", out );
	
	Tabs::decreaseIndent();
	Tabs::writeTabs( out );
	out << "</Grade>" << endl;
}

//! Print technology info.
void Grade::printGrade() const
{
	// write to file or database later
	cout << name << endl;
	cout << "Year: " << year << endl;
	cout << "Available: " << available << endl;
	cout << "Extraction Cost: " << extractCost << endl;
}

//! Total cost of each grade.
void Grade::calcCost(  const double tax, const double cumTechChange, const double environCost, const int per ) {
	totalCost = ( extractCost + environCost ) / cumTechChange + tax;
}

//! Return available amount in each Grade.
double Grade::getAvail() const {
	return available;
}

//! Return available amount in each Grade.
void Grade::setAvail( double availableVal ) {
   available = availableVal;
}

//! Return the total cost.
double Grade::getCost() const {
	return totalCost;
}

//! Return the extraction cost.
double Grade::getExtCost() const {
	return extractCost;
}

//! Set the extraction cost.
void Grade::setExtCost( const double eCost ) {
	extractCost = eCost;
}

//! Get the name.
string Grade::getName() const {
	return name;
}


