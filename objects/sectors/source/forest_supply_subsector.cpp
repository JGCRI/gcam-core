/*! 
* \file forest_supply_subsector.cpp
* \ingroup Objects
* \brief ForestSupplySubsector class source file.
* \author James Blackwood
*/

#include "util/base/include/definitions.h"
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/forest_supply_subsector.h"
#include "technologies/include/forest_production_technology.h"

using namespace std;
using namespace xercesc;

/*! \brief Constructor.
* \author James Blackwood
*/

ForestSupplySubsector::ForestSupplySubsector( const string& regionName,
                                              const string& sectorName )
: Subsector( regionName, sectorName )
{
}

ForestSupplySubsector::~ForestSupplySubsector() {
}

/*! \brief Returns true if the nodename is a valid child for this class.
*
* Virtual function which specifies the XML name of the possible technology children of this class.
* This function allows all technologies to be properly parsed using the base subsector code.
* \author Steve Smith
* \pre Needs cooresponding createChild() function
* \return True if nodename is a valid child of this class.
*/
bool ForestSupplySubsector::isNameOfChild( const string& nodename ) const {
    return ( nodename == ForestProductionTechnology::getXMLNameStatic1D() );
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
ITechnology* ForestSupplySubsector::createChild( const string& aTechType,
                                                const string& aTechName,
                                                const int aTechYear ) const
{
    return new ForestProductionTechnology( aTechName, aTechYear );
}

//! Parses any input variables specific to derived classes
bool ForestSupplySubsector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
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
const string& ForestSupplySubsector::getXMLName() const {
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
const string& ForestSupplySubsector::getXMLNameStatic() {
    const static string XML_NAME = "ForestSupplySubsector";
    return XML_NAME;
}

void ForestSupplySubsector::calcShare( const int aPeriod, const GDP* aGDP ) {
    // call function to compute technology shares. This is required
    // currently because costs are calculated for the technologies here.
    calcTechShares( aGDP, aPeriod );

    share[ aPeriod ] = 1;
}

void ForestSupplySubsector::adjustForCalibration( double sectorDemand,
                                                  double totalfixedOutput,
                                                  double totalCalOutputs,
                                                  const bool allFixedOutput,
                                                  const int period )
{
    // All calibration occurs on the supply side. Allow technologies to perform
    // any adjustments required. Share is always one, so the technology
    // calibrated output is the same as totalCalOutput.
    for( unsigned int i = 0; i < techs.size(); ++i ){
        techs[ i ][period]->adjustForCalibration( totalCalOutputs, regionName,
                                                  mSubsectorInfo.get(), period );
    }
}

