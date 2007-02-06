/*! 
* \file building_supply_sector.cpp
* \ingroup CIAM
* \brief The BuildingSupplySector class source file.
* \author Steve Smith
*/

#include "util/base/include/definitions.h"
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/xml_helper.h"

#include "sectors/include/building_supply_sector.h"
#include "sectors/include/building_supply_subsector.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/scenario.h"
#include "containers/include/iinfo.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string BuildingSupplySector::XML_NAME = "buildingservicesupply";

/*! \brief Default constructor.
*
* Constructor initializes member variables with default values, sets vector sizes, and sets value of debug flag.
*
* \author Sonny Kim, Steve Smith, Josh Lurz
*/
BuildingSupplySector::BuildingSupplySector( const string& aRegionName )
: SupplySector( aRegionName ){
}

//! Default destructor
BuildingSupplySector::~BuildingSupplySector() {
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& BuildingSupplySector::getXMLName() const {
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
const std::string& BuildingSupplySector::getXMLNameStatic() {
	return XML_NAME;
}

/*! \brief Parses any attributes specific to derived classes
*
* Method parses any input data attributes (not child nodes, see XMLDerivedClassParse) that are specific to any classes derived from this class.
*
* \author Josh Lurz, Steve Smith
* \param nodeName The name of the curr node. 
* \param curr pointer to the current node in the XML input tree
* \return returns true if the node was parsed
*/
bool BuildingSupplySector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
            
    if ( SupplySector::XMLDerivedClassParse( nodeName, curr) ) {
    }   // if false, node was not parsed so far so try to parse here
    else if( nodeName == BuildingSupplySubSector::getXMLNameStatic() ){
        parseContainerNode( curr, subsec, subSectorNameMap, new BuildingSupplySubSector( regionName, name ) );
    }
    else {
        return false;
    }
    // If was true somewhere above then node was parsed
    return true;
}

/*! \brief Perform any sector level calibration data consistency checks
*
* For the buildings service supply sectors need to supply total calibrated output
* to the building demand subsector through use of market info mechanism. (The building
* demand subsector needs this information to calibrate demands)
*
* \author Steve Smith
* \param period Model period
*/
void BuildingSupplySector::initCalc( NationalAccount* aNationalAccount,
                                     const Demographic* aDemographic,
                                     const int aPeriod )
{
    SupplySector::initCalc( aNationalAccount, aDemographic, aPeriod );

    // Store calibrated output
    // Note that cannot use existing calSupply market information variable as this is only
    // set if all outputs are calibrated. For calibration of buildings sectors we must have calibrated
    // output, even if entire sector is not calibrated (which could cause problems, as noted in postCalc() warning
    
    // Can't determine calibrated output from the Technology's until initCalc is called.
    double calOutput = getCalOutput( aPeriod  );
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->getMarketInfo( name, regionName, aPeriod, true )->setDouble( "calOutput", calOutput );
    
    if ( aPeriod == 1 && calOutput == 0 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "No calibrated output for sector " << getName() << " in region " << regionName
                << " was not found. This may cause erratic model behavior." << endl;
    }
}

/*! \brief Perform any functions that are needed after the iteration has finished.
*
* \author Steve Smith
* \param aPeriod Period for which to peform any post calcuations
*/
void BuildingSupplySector::postCalc( const int aPeriod )
{
    const double CAL_DATA_RESET = -10;
    
    // Remove calibration information as flag to demand sectors that output is 
    // for next period (if output is calibrated, initCalc will add this information)
    Marketplace* marketplace = scenario->getMarketplace();
    
    double calOutput = marketplace->getMarketInfo( name, regionName, aPeriod, true )->getDouble( "calOutput", true );
    double calSupply = marketplace->getMarketInfo( name, regionName, aPeriod, true )->getDouble( "calSupply", true );

    // While calSupply and calOutput may not be equal (due to calibration adjustments in World::checkCalConsistancy() )
    // The sector should have fully calibrated outputs for a base year, or else something may be wrong so emit
    // a warning.
    
    if ( calSupply < 0 && calOutput > 0 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Output from sector " << getName() << " in region " << regionName
                << " was not 100% fixed or calibrated. This may cause calibration to fail." << endl;
    }
    
    ILogger& calibrationLog = ILogger::getLogger( "calibration_log" );
    calibrationLog.setLevel( ILogger::DEBUG );
    calibrationLog << "calOutput in marketplace for sector " << getName() << " in region " << regionName;
    calibrationLog << " reset to null from value: " 
                   << marketplace->getMarketInfo( name, regionName, aPeriod, true )->getDouble( "calOutput", true )
                   << endl;

    calibrationLog << "calSupply for sector " << getName() << " in region " << regionName;
    calibrationLog << " was : " 
                   << marketplace->getMarketInfo( name, regionName, aPeriod, true )->getDouble( "calSupply", true )
                   << endl;

    marketplace->getMarketInfo( name, regionName, aPeriod, true )->setDouble( "calOutput", CAL_DATA_RESET );

    SupplySector::postCalc( aPeriod );
}

