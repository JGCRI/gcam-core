/* grade.cpp												*
 * Method definition for grade class						*
 * Coded by Sonny Kim 9/13/00								*/

#include "Definitions.h"
#include <string>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <cmath>
#include <cassert>
#include "grade.h"
#include "modeltime.h"
#include "xmlHelper.h"

using namespace std; // enables elimination of std::

extern Modeltime modeltime;

//! Default constructor
grade::grade() 
{
	initElementalMembers();
}

//! Clear member variables
void grade::clear(){
	initElementalMembers();
	name = "";
}

//! Initialize elemental members
void grade::initElementalMembers(){
	year = 0;
	available = 0;
	extCost = 0;
	envCost = 0;
	techCh = 0;
	tax = 0;
	tCost = 0;
}

//! Initialize data members from XML.
void grade::XMLParse( const DOMNode* tempNode )
{	
	DOMNodeList* tempNodeLst;
	DOMNode* tNode = 0;
	string tNodeName;

	//! \pre assume we are passed a valid node.
	assert( tempNode );
	
	tempNodeLst = tempNode->getChildNodes();

	for( int i = 0; i < tempNodeLst->getLength(); i++ ) {
		tNode = tempNodeLst->item( i );
		tNodeName = XMLString::transcode( tNode->getNodeName() );
		
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
			extCost = XMLHelper<double>::getValue( tNode );
		}
		else if( tNodeName == "environmentalcost" ){
			envCost = XMLHelper<double>::getValue( tNode );
		}
		else if( tNodeName == "severencetax" ){
			tax = XMLHelper<double>::getValue( tNode );
		}
		else if( tNodeName == "techchange" ){
			techCh = XMLHelper<double>::getValue( tNode );
		}
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
	XMLWriteElement( extCost, "extractioncost", out );
	XMLWriteElement( envCost, "environmentalcost", out );
	XMLWriteElement( tax, "severencetax", out );
	XMLWriteElement( techCh, "techchange", out );
	// XMLWriteElement( tCost, "totalcost", out );
	
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
	XMLWriteElement( extCost, "extractioncost", out );
	XMLWriteElement( envCost, "environmentalcost", out );
	XMLWriteElement( techCh, "techchange", out );
	XMLWriteElement( tax, "severencetax", out );
	XMLWriteElement( tCost, "totalcost", out );
	
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
	cout << "Extraction Cost: " << extCost << endl;
	cout << "Environmental Cost: " << envCost << endl;
	cout << "Technical Change: " << techCh << endl;
	cout << "Severance Tax: "<< tax << endl << endl;
}

//! Total cost of each grade.
void grade::cost( const int per ) 
{
	double tech;

	tech = pow( ( 1.0 + techCh ),modeltime.gettimestep( per ) );
	tCost = ( extCost + envCost ) / tech + tax;
}

//! Return available amount in each grade.
double grade::getAvail() const 
{
	return available;
}

double grade::getCost() const
{
	return tCost;
}

double grade::getExtCost() const
{
	return extCost;
}

double grade::getEnvCost() const
{
	return envCost;
}

void grade::setExtCost( const double eCost ) 
{
	extCost = eCost;
}

string grade::getName() const 
{
	return name;
}

