/*! 
* \file subsector.cpp
* \ingroup CIAM
* \brief Subsector class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <vector>
#include <algorithm>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "sectors/include/building_dmd_subsector.h"
#include "technologies/include/building_dmd_technology.h"

#include "util/base/include/configuration.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/summary.h"
#include "emissions/include/indirect_emiss_coef.h"
#include "containers/include/world.h"
#include "containers/include/gdp.h"
#include "marketplace/include/market_info.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string BuildingSubSector::XML_NAME = "buildingSubSector";

/*! \brief Default constructor.
*
* Constructor initializes member variables with default values, sets vector sizes, etc.
*
* \author Sonny Kim, Steve Smith, Josh Lurz
*/
const double LOGIT_EXP_DEFAULT = -3;

BuildingSubSector::BuildingSubSector( const string regionName, const string sectorName ) : Subsector( regionName, sectorName ){
}

/*! \brief Default destructor.
*
* deletes all technology objects associated  with this sector.
*
* \author Josh Lurz
*/
BuildingSubSector::~BuildingSubSector() {
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& BuildingSubSector::getXMLName() const {
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
const std::string& BuildingSubSector::getXMLNameStatic() {
	return XML_NAME;
}

//! Virtual function which specifies the XML name of the children of this class, the type of technology.
const string& BuildingSubSector::getChildXMLName() const {
    return BuildingDmdTechnology::getXMLNameStatic1D();
}

//! Virtual function to generate a child element or construct the appropriate technology.
technology* BuildingSubSector::createChild() const {
    return new BuildingDmdTechnology();
}

/*! \brief Parses any input variables specific to derived classes
*
*/
bool BuildingSubSector::XMLDerivedClassParse( const string nodeName, const DOMNode* curr ) {
    const Modeltime* modeltime = scenario->getModeltime();

   if( nodeName == "dayLighting" ){
        XMLHelper<double>::insertValueIntoVector( curr, dayLighting, modeltime );
    } 
    else if( nodeName == "aveInsulation" ){
        XMLHelper<double>::insertValueIntoVector( curr, aveInsulation, modeltime );
    } 
    else if( nodeName == "floorToSurfaceArea" ){
        XMLHelper<double>::insertValueIntoVector( curr, floorToSurfaceArea, modeltime );
    } else {
      return false;
    }
    return true;
}

/*! \brief Perform any initializations needed for each period.
*
* Put dayLighting and other subsector variables into subSectorInfo object so that these are available to 
* demand technologies
*
* \author Steve Smith
* \param period Model period
*/
void BuildingSubSector::initCalc( const int period, std::auto_ptr<MarketInfo> mSectorInfo ) {

    // Add items from sectorInfo
    mSubsectorInfo->addItem( "heatingDegreeDays", mSectorInfo->getItemValue( "heatingDegreeDays" ) );
    mSubsectorInfo->addItem( "coolingDegreeDays", mSectorInfo->getItemValue( "coolingDegreeDays" ) );

    // Add subsector data items
    mSubsectorInfo->addItem( "dayLighting", dayLighting[ period ] );
    mSubsectorInfo->addItem( "aveInsulation", aveInsulation[ period ] );
    mSubsectorInfo->addItem( "floorToSurfaceArea", floorToSurfaceArea[ period ] );

    Subsector::initCalc( period, mSectorInfo );
    
}

/*! \brief calculate Subsector unnormalized shares
*
* Uses normal subSector share calculation.
*
* Then calculates internal loads and passes that information to the appropriate technologies.
*
* \author Steve Smith
* \param regionName region name
* \param period model period
* \param gdp gdp object
* \warning technologies can not independently have fixed outputs
* \warning there is no difference between demand and supply technologies. Control behavior with value of parameter fuelPrefElasticity
*/
void BuildingSubSector::calcShare(const int period, const GDP* gdp ) {
Marketplace* marketplace = scenario->getMarketplace();
   
    Subsector::calcShare( period, gdp );

    double internalLoads; // internal loads per square meter
    for ( int i=0; i<notech; i++ ) {
        string loadName = techs[ i ][ period ]->getFuelName( );
        internalLoads += marketplace->getMarketInfo( loadName, regionName, period, "internalLoadPerFS" );
    }
    
    mSubsectorInfo->addItem( "internalLoadPerFS", internalLoads );

   // sjsTODO -- get this information to technologies
}

/*! \brief Calls building technology calibration routine
*  
* For the building demand subSector there is no calibration
* (unless there is more than one building type in the base year)
*
* But each BuildingDmdTechnology must have its demand calibrated
* \author Steve Smith
* \param sectorDemand total demand for this sector
* \param totalfixedOutput total amount of fixed supply for this sector
* \param totalCalOutputs total amount of calibrated outputs for this sector
* \param allFixedOutput flag if all outputs from this sector are calibrated
* \param period Model period
* \warning If there is more than one building type in a calibration year then something else may need to be done
*/
void BuildingSubSector::adjustForCalibration( double sectorDemand, double totalfixedOutput, double totalCalOutputs, const bool allFixedOutput, const int period ) {
    Marketplace* marketplace = scenario->getMarketplace();

    for (int j=0;j<notech;j++) {
        // calibrate buildingServiceDemands
        string demandSectorName = techs[j][period]->getFuelName( );
        double calOutput = marketplace->getMarketInfo( demandSectorName, regionName, period, "calOutput" );

        // Here, pass in specific demand -- equal to demand for this service per unit floor space
        // NOTE: this is not adjusted for saturation, this is done in BuildingDmdTechnology
        double calDemand = 0;
        if ( sectorDemand != 0 ) {
            calDemand = calOutput / sectorDemand;
        }
        techs[j][period]->adjustForCalibration( calDemand );
    }
}
   