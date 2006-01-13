/*! 
* \file food_demand_subsector.cpp
* \ingroup Objects
* \brief FoodDemandSubsector class source file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNodeList.hpp>

#include "sectors/include/food_demand_subsector.h"
#include "technologies/include/food_demand_technology.h"

using namespace std;
using namespace xercesc;

/*! \brief Constructor
*/
FoodDemandSubsector::FoodDemandSubsector( const string regionName, const string sectorName )
: Subsector( regionName, sectorName ){
}


/*! \brief Returns true if the nodename is a valid child for this class.
*
* Virtual function which specifies the XML name of the possible technology children of this class.
* This function allows all technologies to be properly parsed using the base subsector code.
* \author Steve Smith
* \pre Needs cooresponding createChild() function
* \return True if nodename is a valid child of this class.
*/
bool FoodDemandSubsector::isNameOfChild( const string& nodename ) const {
    return ( nodename == FoodDemandTechnology::getXMLNameStatic1D() );
}

/*! \brief Virtual function to generate a child element or construct the appropriate technology.
*
* \pre Needs cooresponding isNameOfChild() function
* \author Steve Smith
* \return returns a new child object of appropriate type.
*/
technology* FoodDemandSubsector::createChild( const string& nodename ) const {
    return new FoodDemandTechnology();
}

//! Parses any input variables specific to derived classes
bool FoodDemandSubsector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    return false;
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& FoodDemandSubsector::getXMLName() const {
	return getXMLNameStatic();
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
const string& FoodDemandSubsector::getXMLNameStatic() {
	const static string XML_NAME = "FoodDemandSubsector";
	return XML_NAME;
}
