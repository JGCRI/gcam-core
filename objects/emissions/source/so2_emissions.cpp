/*! 
* \file so2_emissions.cpp
* \ingroup objects
* \brief SO2 class source file.
* \author Nick Fernandez
* \date $Date$
* \version $Revision$
*/
#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <xercesc/dom/DOMNode.hpp>
#include "emissions/include/so2_emissions.h"
#include "util/base/include/xml_helper.h"

using namespace std;
using namespace xercesc;

const string SO2Emissions::XML_NAME = "SO2";

//! Default Constructor.
SO2Emissions::SO2Emissions(){
	ashRetention = 0;
	percentSulfur = 0;
	gjPerTonne = 1000;
	emissCoef = 1; // Note overriding default emissCoef since coef is included in driver.
}

//! Default destructor.
SO2Emissions::~SO2Emissions(){
}

//! Clone operator.
SO2Emissions* SO2Emissions::clone() const {
    return new SO2Emissions( *this );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& SO2Emissions::getXMLName() const {
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
const std::string& SO2Emissions::getXMLNameStatic() {
	return XML_NAME;
}

/*! \brief Parses any child nodes specific to derived classes.
* \details Method parses any input data from child nodes that are specific to the classes derived from this class.
* \author Nick Fernandez
* \param nodeName name of current node
* \param curr pointer to the current node in the XML input tree
*/
bool SO2Emissions::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ){
	if( nodeName == "ashRetention"){
		ashRetention = XMLHelper<double>::getValue( curr );
	}
	else if( nodeName == "percentSulfur"){
		percentSulfur = XMLHelper<double>::getValue( curr );
	}
	else if( nodeName == "gjTonne"){
		gjPerTonne = XMLHelper<double>::getValue( curr );
	}
    else {
        return false;
    }
    return true;
}

//! Write out XML elements specific to the derived class.
void SO2Emissions::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    XMLWriteElementCheckDefault( ashRetention, "ashRetention", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( percentSulfur, "percentSulfur", out, tabs, 0.0 );
	XMLWriteElementCheckDefault( gjPerTonne, "gjTonne", out, tabs, 1000.0 );
}

//! Write out XML elements specific to the derived class.
void SO2Emissions::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    XMLWriteElement( ashRetention, "ashRetention", out, tabs );
    XMLWriteElement( percentSulfur, "percentSulfur", out, tabs );
	XMLWriteElement( gjPerTonne, "gjTonne", out, tabs );
}

/*! \brief Returns the emissions driver for SO2
\ detailed SO2 emissions are driven by input, which is converted from EJ to Tg by multiplying by 1000 
* and dividing by the ratio of gj per metric ton. This input is then multiplied by the percent
* sulfur content to give the weight of the sulfur, and then multiplied by the amount of that sulfur
* that escapes (is not retained in the form of ash)
* \author Nick Fernandez
* \param inputIn energy input
* \param outputIn energy output
* \return The emissions driver
*/
double SO2Emissions::emissionsDriver( const double inputIn, const double outputIn ) const {
	return inputIn * ( 1000 / gjPerTonne ) * ( percentSulfur / 100 ) * ( 1 - ( ashRetention / 100 ) );
}
