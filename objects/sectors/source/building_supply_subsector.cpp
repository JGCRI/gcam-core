/*! 
* \file building_supply_subsector.cpp
* \ingroup CIAM
* \brief The BuildingSupplySubSector class source file.
* \author Steve Smith
*/

#include "util/base/include/definitions.h"
#include <string>
#include <vector>
#include <xercesc/dom/DOMNode.hpp>

#include "sectors/include/building_supply_subsector.h"
#include "technologies/include/building_supply_technology.h"

using namespace std;
using namespace xercesc;

// static initialize.
const string BuildingSupplySubSector::XML_NAME = "buildingsupplysubsector";

/*! \brief Default constructor.
*
* Constructor initializes member variables with default values, sets vector sizes, etc.
*
* \author Sonny Kim, Steve Smith, Josh Lurz
*/

BuildingSupplySubSector::BuildingSupplySubSector( const string regionName, const string sectorName ) : Subsector( regionName, sectorName ){

}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& BuildingSupplySubSector::getXMLName() const {
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
const std::string& BuildingSupplySubSector::getXMLNameStatic() {
    return XML_NAME;
}

/*! \brief Parses any input variables specific to derived classes
*
*/
bool BuildingSupplySubSector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {

    return false;
}

/*! \brief Returns true if the nodename is a valid child for this class.
*
* Virtual function which specifies the XML name of the possible technology children of this class.
* This function allows all technologies to be properly parsed using the base subsector code.
* \author Steve Smith
* \pre Needs cooresponding createChild() function
* \return True if nodename is a valid child of this class.
*/
bool BuildingSupplySubSector::isNameOfChild  ( const string& nodename ) const {
    return nodename == BuildingSupplyTechnology::getXMLNameStatic1D();
}

/*!
 * \brief Derived helper function to generate a child element or construct the
 *        appropriate technology.
 * \param aTechType The name of the XML node, which is the type of the
 *        technology.
 * \param aTechName The name of the new technology.
 * \param aYear The year of the new technology.
 * \pre isNameOfChild returned that the type could be created.
 * \author Steve Smith
 * \return A newly created technology of the specified type.
 */
ITechnology* BuildingSupplySubSector::createChild( const string& aTechType,
                                                  const string& aTechName,
                                                  const int aTechYear ) const
{
    return new BuildingSupplyTechnology( aTechName, aTechYear );
}
