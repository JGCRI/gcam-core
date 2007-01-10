/*! 
* \file building_supply_technology.cpp
* \ingroup CIAM
* \brief BuildingSupplyTechnology source file
* \author Steve Smith
*/

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>

// User headers
#include "technologies/include/building_supply_technology.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"
#include "sectors/include/building_dmd_subsector.h"
#include "technologies/include/iproduction_state.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string BuildingSupplyTechnology::XML_NAME1D = "buildingsupplytech";

/*! 
 * \brief Constructor.
 * \param aName Technology name.
 * \param aYear Technology year.
 */
BuildingSupplyTechnology::BuildingSupplyTechnology( const string& aName, const int aYear )
:Technology( aName, aYear ){
    internalLoadFraction = 1;
}

//! Clone Function. Returns a deep copy of the current technology.
BuildingSupplyTechnology* BuildingSupplyTechnology::clone() const {
    return new BuildingSupplyTechnology( *this );
}

void BuildingSupplyTechnology::completeInit( const string& aRegionName,
                                             const string& aSectorName,
                                             DependencyFinder* aDepFinder,
                                             const IInfo* aSubsectorInfo,
                                             ILandAllocator* aLandAllocator,
                                             const GlobalTechnologyDatabase* aGlobalTechDB )
{
    Technology::completeInit( aRegionName, aSectorName, aDepFinder,
                              aSubsectorInfo, aLandAllocator, aGlobalTechDB );
}

const std::string& BuildingSupplyTechnology::getXMLName1D() const {
	return XML_NAME1D;
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
const std::string& BuildingSupplyTechnology::getXMLNameStatic1D() {
	return XML_NAME1D;
}

bool BuildingSupplyTechnology::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {

    if( nodeName == "internalLoadFraction" ){
        internalLoadFraction = XMLHelper<double>::getValue( curr );
    }
    else {
      return false;
    }
    return true;
}

void BuildingSupplyTechnology::toInputXMLDerived( ostream& out, Tabs* tabs ) const {  
    XMLWriteElement( internalLoadFraction, "internalLoadFraction", out, tabs);
}

void BuildingSupplyTechnology::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const { 
    XMLWriteElement( internalLoadFraction, "internalLoadFraction", out, tabs );
    XMLWriteElement( mInput[ period ] * internalLoadFraction, "internalGain", out, tabs );
}

void BuildingSupplyTechnology::initCalc( const string& aRegionName,
										 const string& aSectorName,
                                         const IInfo* aSubsectorInfo,
                                         const Demographic* aDemographic,
										 const int aPeriod )
{
	Technology::initCalc( aRegionName, aSectorName, aSubsectorInfo, aDemographic, aPeriod );
}

void BuildingSupplyTechnology::postCalc( const string& aRegionName, const int aPeriod ) {
    Technology::postCalc( aRegionName, aPeriod );
}

void BuildingSupplyTechnology::production( const string& aRegionName,
                                           const string& aSectorName,
                                           double aVariableDemand,
                                           double aFixedOutputScaleFactor, 
										   const GDP* aGDP,
                                           const int aPeriod )
{
    // Call the normal production function
	Technology::production( aRegionName, aSectorName, aVariableDemand,
                            aFixedOutputScaleFactor, aGDP, aPeriod );

    if( !mProductionState[ aPeriod ]->isOperating() ){
        return;
    }

	Marketplace* marketplace = scenario->getMarketplace();
    // Now, if an internal gains market has been set, calculate internal gains
    // and add to that market 
    const string intGainsMarketName =
        marketplace->getMarketInfo( aSectorName, aRegionName, aPeriod, true )->
        getString( BuildingDemandSubSector::getInternalGainsInfoName(), false );

    if ( internalLoadFraction > 0 && !intGainsMarketName.empty() ) {
        double internalGain = mInput[ aPeriod ] * internalLoadFraction;
        marketplace->addToDemand( intGainsMarketName, aRegionName, internalGain,
                                  aPeriod );
    }
}

void BuildingSupplyTechnology::calcCost( const string& aRegionName,
                                         const string& aSectorName,
										 const int aPeriod )
{
    Technology::calcCost( aRegionName, aSectorName, aPeriod );
}

double BuildingSupplyTechnology::getFuelCost( const string& aRegionName,
                                              const string& aSectorName,
							                  const int aPeriod ) const 
{
	return Technology::getFuelCost( aRegionName, aSectorName, aPeriod );
}

double BuildingSupplyTechnology::calcShare( const string& aRegionName,
                                            const string& aSectorName,
                                            const GDP* aGDP,
                                            const int aPeriod ) const
{
    return Technology::calcShare( aRegionName, aSectorName, aGDP, aPeriod );
}

double BuildingSupplyTechnology::getEfficiency( const int aPeriod ) const {
    return Technology::getEfficiency( aPeriod );
}

double BuildingSupplyTechnology::getNonEnergyCost( const int aPeriod ) const {
    return Technology::getNonEnergyCost( aPeriod );
}
