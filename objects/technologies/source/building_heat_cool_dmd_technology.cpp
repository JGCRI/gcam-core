/*! 
* \file building_heat_cool_dmd_technology.cpp
* \ingroup CIAM
* \brief The building heating service demand technology.
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/

// Standard Library headers
#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>

// User headers
#include "technologies/include/building_heat_cool_dmd_technology.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "marketplace/include/market_info.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//! Default constructor.
BuildingHeatCoolDmdTechnology::BuildingHeatCoolDmdTechnology() {
    fractionOfYearActive = 0;
}

//! Parses any input variables specific to derived classes
bool BuildingHeatCoolDmdTechnology::XMLDerivedClassParse( const string nodeName, const DOMNode* curr ) {
    if( BuildingGenericDmdTechnology::XMLDerivedClassParse( nodeName, curr ) ){
    }
    else if( nodeName == "fractionOfYearActive" ){
        fractionOfYearActive = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "intGainsMarketName" ){
        intGainsMarketName = XMLHelper<string>::getValueString( curr );
    }
    else {
        return false;
    }
    return true;
}

/*! \brief XML output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML
*
* \author Josh Lurz
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void BuildingHeatCoolDmdTechnology::toInputXMLDerived( ostream& out, Tabs* tabs ) const { 
    BuildingGenericDmdTechnology::toInputXMLDerived( out, tabs);
    XMLWriteElementCheckDefault( fractionOfYearActive, "fractionOfYearActive", out, tabs, 0.0 );
    XMLWriteElement( intGainsMarketName, "intGainsMarketName", out, tabs );
}	

//! XML output for viewing.
void BuildingHeatCoolDmdTechnology::toOutputXMLDerived( ostream& out, Tabs* tabs ) const {
    BuildingGenericDmdTechnology::toOutputXMLDerived( out, tabs);
    XMLWriteElementCheckDefault( fractionOfYearActive, "fractionOfYearActive", out, tabs, 0.0 );    
    XMLWriteElement( intGainsMarketName, "intGainsMarketName", out, tabs );
}

//! Write object to debugging xml output stream.
void BuildingHeatCoolDmdTechnology::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const { 
    BuildingGenericDmdTechnology::toDebugXMLDerived( period, out, tabs);
    XMLWriteElement( fractionOfYearActive, "fractionOfYearActive", out, tabs );    
    XMLWriteElement( intGainsMarketName, "intGainsMarketName", out, tabs );
}	

/*! \brief Initialize each period
*
* Transfer data that does not change to the technoogy
*
* \author Steve Smith
* \param aSubsectorInfo The subsectorInfo object. 
*/
void BuildingHeatCoolDmdTechnology::initCalc( const MarketInfo* aSubsectorInfo ) {

    aveInsulation = aSubsectorInfo->getItemValue( "aveInsulation" );
    floorToSurfaceArea = aSubsectorInfo->getItemValue( "floorToSurfaceArea" );
 
    BuildingGenericDmdTechnology::initCalc( aSubsectorInfo );    
}

/*! \brief calculate effective internal gains as they affect the demand for this technology
*
* For cooling technologies internal gains are negative
* 
* \author Steve Smith
*/
double BuildingHeatCoolDmdTechnology::getEffectiveInternalGains( const string& regionName, const int period ) {
Marketplace* marketplace = scenario->getMarketplace();
 
    return getInternalGainsSign() * marketplace->getPrice( intGainsMarketName, regionName, period ) * fractionOfYearActive;
}

/*! \brief Adjusts technology parameters as necessary to be consistent with calibration value.
*
* For these demand "technologies" the unitDemand needs to be adjusted so that output
* is consistant with calibrated input demand. This version is for heating demands, where internal gains subtract from demand.
*
* \author Steve Smith
* \param unitDemand calibrated unit demand (demand per unit floorspace) for this subsector
* \param regionName regionName
* \param aSubsectorInfo MarketInfo object (not used for this class so name is left out) 
* \param period model period
*/
void BuildingHeatCoolDmdTechnology::adjustForCalibration( double subSectorDemand, const string& regionName, const MarketInfo* aSubsectorInfo, const int period ) {
    
    // unitDemand (demand per unit area) is passed into this routine as subSectorDemand, but not adjusted for saturation and other parameters.    
    double unitDemand = subSectorDemand;

    // Production is equal to: unitDemand * saturation *(any other parameters) * dmd
    
    // Amount of service supplied is unitDemand times floorspace
    double floorSpace = aSubsectorInfo->getItemValue( "floorSpace" );
    double effectiveDemand = unitDemand * floorSpace;
    
    // Now adjust for internal gains
    effectiveDemand -= getEffectiveInternalGains( regionName, period );
    effectiveDemand = max( effectiveDemand, 0.0 );
    
    shrwts = ( effectiveDemand / floorSpace ) / getDemandFnPrefix( regionName, period );
 }

