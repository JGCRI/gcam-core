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
    const Modeltime* modeltime = scenario->getModeltime();

	phaseIn = 1;
	macCurveOff = 0;
	finalReduction = 0;
	finalReductionYear = modeltime->getEndYear();
	shiftRange = 0;
	noBelowZero = false;
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
* This function may be virtual to be overridden by derived class pointers.
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
* \detailed The x value of the data points is the carbon price.  The y- value is the amount of 
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
		if ( nodeName == "phaseIn" ){
			 phaseIn = XMLHelper<double>::getValue( curr);
		}
		if ( nodeName == "shiftRange" ){
			 shiftRange = XMLHelper<double>::getValue( curr);
		}
		if ( nodeName == "finalReduction"){
			finalReduction = XMLHelper<double>::getValue( curr);
		}
		if ( nodeName == "finalReductionYear"){
			finalReductionYear = XMLHelper<int>::getValue( curr);
 		}
		if ( nodeName == "macCurveOff"){
			macCurveOff = XMLHelper<double>::getValue( curr);
		}
		if ( nodeName == "noBelowZero"){
			noBelowZero = XMLHelper<bool>::getValue( curr);
		}
		if ( nodeName == "curveShiftFuelName"){
			curveShiftFuelName = XMLHelper<bool>::getValue( curr);
		}
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
    const Modeltime* modeltime = scenario->getModeltime();

	XMLWriteOpeningTag(getXMLName(), out, tabs, name );
	
	XMLWriteElementCheckDefault( noBelowZero, "noBelowZero", out, tabs, false);
	XMLWriteElementCheckDefault( macCurveOff, "macCurveOff", out, tabs, 0.0 );
	XMLWriteElementCheckDefault( shiftRange, "shiftRange", out, tabs, 0.0 );
	XMLWriteElementCheckDefault( phaseIn, "phaseIn", out, tabs, 1.0 );
	XMLWriteElementCheckDefault( finalReduction, "finalReduction", out, tabs, 0.0 );
	XMLWriteElementCheckDefault( finalReductionYear, "finalReductionYear", out, tabs, modeltime->getEndYear() );
	const vector<pair<double,double> > pairs = macCurve->getSortedPairs();
	typedef vector<pair<double, double> >::const_iterator PairIterator;
	for( PairIterator currPair = pairs.begin(); currPair != pairs.end(); ++currPair ) {
        XMLWriteElementAndAttribute( currPair->second, "Reduction", out, tabs, currPair->first, "tax" );
	}
	XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Write out datamembers for debugging in an XML format.
void GhgMAC::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
	XMLWriteOpeningTag( getXMLName(), out, tabs, name );

	XMLWriteElement( noBelowZero, "noBelowZero", out, tabs);
	XMLWriteElement( macCurveOff, "macCurveOff", out, tabs);
	XMLWriteElement( shiftRange, "shiftRange", out, tabs );
	XMLWriteElement( phaseIn, "phaseIn", out, tabs);
	XMLWriteElement( finalReduction, "finalReduction", out, tabs);

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
* \detailed The function finds the current carbon price, and using the pre-defined MAC curve,
* it uses that carbon price as the x- value to find the reduction.
* \author  Nick Fernandez
* \param regionName the name of the region 
* \param period the current period
* \warning MAC curves are all keyed off of CO2 price.
* \todo Add capability to adjust for different GWP in GHG vs MAC.
* \todo make the good name more general instead of hard coding CO2?
* \todo Put the CH4 shift into a separate type of MAC class
* \todo Take out hard coded maximum carbon tax --getY(200)-- and get that from macCurve instead. Change other hard coded max/min carbon taxes
*/
double GhgMAC::findReduction( const string& regionName, const int period ){

	double reduction;
	if ( macCurveOff != 1 ){
        const Marketplace* marketplace = scenario->getMarketplace();
        double effectiveCarbonPrice = marketplace->getPrice( "CO2", regionName, period );
	
		if (name == "CH4"){
			effectiveCarbonPrice = shiftNatGas( period, regionName, effectiveCarbonPrice );
		}
	    reduction = macCurve->getY( effectiveCarbonPrice );
		if ( noBelowZero && effectiveCarbonPrice < 0 ){
			reduction = 0;
		}
		
        const Modeltime* modeltime = scenario->getModeltime();
		double maxReduction = macCurve->getY( 200 );
		reduction *= adjustPhaseIn( period );
		int finalReductionPeriod = modeltime->getper_to_yr( finalReductionYear );

		if ( finalReduction > maxReduction && finalReductionPeriod > 1 ){
			reduction *= adjustTechCh( period, finalReductionPeriod, maxReduction );
		}
	}
	else {
		reduction = 0;
	}
	return reduction;
}
/*! \brief returns a multiplier that phases in the Mac Curve.
*  if for example, the MAC curve was to be phased in over 3 periods, in the base 
*  period, it would return a multiplier of 0, the next period, 1/3, the next period 2/3,
*  and by and after the 3rd period after the base year, it would return a 1.
* \author Nick Fernandez
* \param period the current period
*/
double GhgMAC::adjustPhaseIn( const int period ){
	double mult=1;
	if ((( period - 1 ) < phaseIn) && ( phaseIn >= 1 )){
		 mult = ( period - 1 ) / phaseIn;
	}
	return mult;
}
/*! \brief returns a multiplier that shifts the MAC curve upwards due to technological change.
*  technological change is calculated the same way as for GHG's, with the exception that
*  a direct multiplier is applied to the reductions based on the technological change parameter
* \author Nick Fernandez
* \param period
*/
double GhgMAC::adjustTechCh( const int period, const int finalReductionPeriod, const double maxReduction ){
	double multiplier;
	double change = maxReduction/finalReduction;
	if ( period <= finalReductionPeriod ){
		multiplier = change * ( 1 / ( finalReductionPeriod - 2 ) ) * ( period - 2 );
	}
	else {
        multiplier = change;
	}
    return multiplier;
}


/*! \brief Returns a new effective carbon price that is shifted up or down, based on the Natural Gas price
*  The range of the carbon price shift is set by the read-in parameter, shiftRange.
*  It is a unique parameter that approximates the best fit of each table.
*  It represents the initial range of the shift that occurs between a 50% reduction in the natural gas
*  price and a 200% increase in the natural gas price from the base year (as taken from EPA-EMF results).  
*  This range decreases as the carbon price increases.  This is taken into account by the variable "convergence Factor"
*  NORM_FACTOR is a constant that normalizes the value (1- priceChangeRatio) so that it ranges 
*  from -0.6 at 50% reduction in carbon price to 0.4 at a 200% increase in carbon price 
*  (it is normalized to a range of 1).
* \author Nick Fernandez
* \param period the current period
* \param regionName the region
* \param carbonPrice the current carbon price
* \todo Get the max and minimum carbon prices from the mac curve instead of hard coded here. At present all mac curves are the same but this could change.
*/
double GhgMAC::shiftNatGas( const int period, const string& regionName, const double carbonPrice){
	const Marketplace* marketplace = scenario->getMarketplace();
	double natGasPrice = marketplace->getPrice( curveShiftFuelName, regionName, period );
	double natGasBasePrice = marketplace->getPrice( curveShiftFuelName, regionName, 1 ); // change prices relative to the period 1

	double priceChangeRatio = 1;
	if ( natGasPrice != 0 ) { 
		priceChangeRatio = natGasBasePrice / natGasPrice;
	}
	
	const double NORM_FACTOR = 3 / 5; // This value was adjusted to fit the EPA-EMF data.
	double convergenceFactor = ( 1 / 2 ) + ( 1 / 2 )*( ( 200 - carbonPrice ) / 220 );
    double newCarbonPrice = carbonPrice + ( NORM_FACTOR*( 1 - priceChangeRatio ) * shiftRange * convergenceFactor );

    // Reset carbon price if beyond MAC curve values (is this necessary? sjs)
	const double MIN_CPRICE = -20;	// Minimum carbon price used in this MAC curve
	const double MAX_CPRICE = 200; // Maximum carbon price used in this MAC curve
	newCarbonPrice = max( newCarbonPrice, MIN_CPRICE );
	newCarbonPrice = min( newCarbonPrice, MAX_CPRICE );

	return newCarbonPrice;
}
