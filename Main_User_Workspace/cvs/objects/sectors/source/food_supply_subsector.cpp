/*
 * LEGAL NOTICE
 * This computer software was prepared by Battelle Memorial Institute,
 * hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
 * with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
 * CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
 * sentence must appear on any copies of this computer software.
 * 
 * EXPORT CONTROL
 * User agrees that the Software will not be shipped, transferred or
 * exported into any country or used in any manner prohibited by the
 * United States Export Administration Act or any other applicable
 * export laws, restrictions or regulations (collectively the "Export Laws").
 * Export of the Software may require some form of license or other
 * authority from the U.S. Government, and failure to obtain such
 * export control license may result in criminal liability under
 * U.S. laws. In addition, if the Software is identified as export controlled
 * items under the Export Laws, User represents and warrants that User
 * is not a citizen, or otherwise located within, an embargoed nation
 * (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
 *     and that User is not otherwise prohibited
 * under the Export Laws from receiving the Software.
 * 
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*! 
* \file food_supply_subsector.cpp
* \ingroup Objects
* \brief FoodSupplySubsector class source file.
* \author James Blackwood
*/

#include "util/base/include/definitions.h"
#include <string>
#include <xercesc/dom/DOMNode.hpp>

#include "sectors/include/food_supply_subsector.h"
#include "technologies/include/food_production_technology.h"
#include "technologies/include/unmanaged_land_technology.h"
#include "util/base/include/xml_helper.h"

using namespace std;
using namespace xercesc;

/*! \brief Constructor.
*/
FoodSupplySubsector::FoodSupplySubsector( const string& regionName,
                                          const string& sectorName )
                                          : Subsector( regionName, sectorName )
{
    // food subsectors do not utilize share-weights however we must initialize
    // the value to avoid errors from shared code
    for( int period = 0; period < mShareWeights.size(); ++period ) {
        mShareWeights[ period ].set( 1.0 );
    }
}

FoodSupplySubsector::~FoodSupplySubsector() {
}

/*! \brief Returns true if the nodename is a valid child for this class.
*
* Virtual function which specifies the XML name of the possible technology children of this class.
* This function allows all technologies to be properly parsed using the base subsector code.
* \author Steve Smith
* \pre Needs corresponding createChild() function
* \return True if nodename is a valid child of this class.
*/
bool FoodSupplySubsector::isNameOfChild( const string& nodename ) const {
    return ( nodename == FoodProductionTechnology::getXMLNameStatic1D() ) ||
           ( nodename == UnmanagedLandTechnology::getXMLNameStatic1D() );
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
ITechnology* FoodSupplySubsector::createChild( const string& aTechType,
                                               const string& aTechName,
                                               const int aTechYear ) const
{
    if ( aTechType == FoodProductionTechnology::getXMLNameStatic1D() ) {
      return new FoodProductionTechnology( aTechName, aTechYear );
    }
    else if ( aTechType == UnmanagedLandTechnology::getXMLNameStatic1D() ) {
      return new UnmanagedLandTechnology( aTechName, aTechYear );
    }
}

//! Parses any input variables specific to derived classes
bool FoodSupplySubsector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    return false;
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& FoodSupplySubsector::getXMLName() const {
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
const string& FoodSupplySubsector::getXMLNameStatic() {
    const static string XML_NAME = "FoodSupplySubsector";
    return XML_NAME;
}

double FoodSupplySubsector::calcShare( const int aPeriod,
                                       const GDP* aGDP,
                                       const double aLogitExp ) const
{
    return 1;
}
