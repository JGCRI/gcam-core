/*! 
* \file ghg_mac.cpp
* \ingroup objects
* \brief GHGMac source file.
* \author Nick Fernandez
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "emissions/include/ghg_mac.h"
#include "util/base/include/xml_helper.h"
#include "util/curves/include/point_set_curve.h"
#include "util/curves/include/curve.h"
#include "util/curves/include/explicit_point_set.h"
#include "util/curves/include/xy_data_point.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/scenario.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

const string GhgMAC::XML_NAME = "MAC";

//! Default constructor.
GhgMAC::GhgMAC(){
}

//! Destructor. auto_ptr automatically deallocated the curve. 
GhgMAC::~GhgMAC(){
}

//! Copy constructor.
GhgMAC::GhgMAC( const GhgMAC& other ){
    copy( other );
}

//! Assignment operator.
GhgMAC& GhgMAC::operator=( const GhgMAC& other ){
    if( this != &other ){
        // If there was a destructor it would need to be called here.
        copy( other );
    }
    return *this;
}

//! Copy helper function.
void GhgMAC::copy( const GhgMAC& other ){
    macCurve.reset( other.macCurve->clone() );
}

//! Clone function which returns a deep copy of the GhgMAC.
GhgMAC* GhgMAC::clone() const {
    return new GhgMAC( *this );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& GhgMAC::getXMLName() const {
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
const std::string& GhgMAC::getXMLNameStatic() {
	return XML_NAME;
}
/*! \brief Reads in a series of data points and creates a MAC curve from those points
* \details The x value of the data points is the carbon price.  The y- value is the amount of 
* reduction in emissions (0= none, 1= completely reduced) The curve that is created is 
* piecewise linear, so that the reduction for any carbon price between two read-in points
* will be found using linear interpolation.
* \author Nick Fernandez and Josh Lurz
* \param node Current node in the DOM tree. 
*/
void GhgMAC::XMLParse( const xercesc::DOMNode* node ){
	/*! \pre Assume we are passed a valid node. */
	assert( node );

	DOMNodeList* nodeList = node->getChildNodes();
	ExplicitPointSet* currPoints = new ExplicitPointSet();
	for( unsigned int i = 0; i < nodeList->getLength(); i++ ) {
		DOMNode* curr = nodeList->item( i );
		const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );		
	
		if ( nodeName == "Reduction" ){
			double taxVal = XMLHelper<double>::getAttr( curr, "tax" );	
			double reductionVal = XMLHelper<double>::getValue( curr );;
			XYDataPoint* currPoint = new XYDataPoint( taxVal, reductionVal );
			currPoints->addPoint( currPoint );
		}
	}
    // Can't override a MAC curve currently.
	macCurve.reset( new PointSetCurve( currPoints ) );
}

//! Write out the datamembers of the GHGMac in an XML format.
void GhgMAC::toInputXML( ostream& out, Tabs* tabs ) const {
	XMLWriteOpeningTag( getXMLName(), out, tabs );
	
	const vector<pair<double,double> > pairs = macCurve->getSortedPairs();
	typedef vector<pair<double, double> >::const_iterator PairIterator;
	for( PairIterator currPair = pairs.begin(); currPair != pairs.end(); ++currPair ) {
        XMLWriteElementAndAttribute( currPair->second, "Reduction", out, tabs, currPair->first, "tax" );
	}
	XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Write out datamembers for debugging in an XML format.
void GhgMAC::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
	XMLWriteOpeningTag( getXMLName(), out, tabs );
	
	const vector<pair<double,double> > pairs = macCurve->getSortedPairs();
	typedef vector<pair<double, double> >::const_iterator PairIterator;
	for( PairIterator currPair = pairs.begin(); currPair != pairs.end(); ++currPair ) {
		double taxVal= currPair->first;
		double reductionVal= currPair->second;
		XMLWriteElement( taxVal, "taxVal", out, tabs);
		XMLWriteElement( reductionVal, "reductionVal", out, tabs);
	}
	XMLWriteClosingTag( getXMLName(), out, tabs );
}

/*! \brief Finds the reduction using a created MAC curve
* \details The function finds the current carbon price, and using the pre-defined MAC curve,
* it uses that carbon price as the x- value to find the reduction.
* \author  Nick Fernandez
* \param regionName the name of the region 
* \param period the current period
* \warning MAC curves are all keyed off of CO2 price.
* \todo Add capability to adjust for different GWP in GHG vs MAC.
*/
double GhgMAC::findReduction( const string& regionName, const int period ){

	const Marketplace* marketplace = scenario->getMarketplace();
	double carbonPrice = marketplace->getPrice( "CO2", regionName, period );
	// todo: make the good name more general instead of hard coding CO2
	return macCurve->getY( carbonPrice );
}
