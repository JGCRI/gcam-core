/*! 
* \file ghg_input.cpp
* \ingroup CIAM
* \brief GhgInput class source file.
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include "emissions/include/ghg_input.h"

using namespace std;

const string GhgInput::XML_NAME = "GHG_INPUT"; // All caps is non-standard.

//! Clone function which returns a deep copy of the Ghg.
GhgInput* GhgInput::clone() const {
    return new GhgInput( *this );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& GhgInput::getXMLName() const {
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
const std::string& GhgInput::getXMLNameStatic() {
    return XML_NAME;
}

//! Calculate Ghg emissions based on the input value.
double GhgInput::emissionsDriver( const double inputIn, const double outputIn ) const {
	return inputIn;
}
