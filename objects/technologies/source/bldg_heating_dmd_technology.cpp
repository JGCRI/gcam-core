/*! 
* \file technology.cpp
* \ingroup CIAMBuildingHeatingDmdTechnology
* \brief technology class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

// Standard Library headers
#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

// User headers
#include "technologies/include/bldg_heating_dmd_technology.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "emissions/include/ghg.h"
//#include "util/base/include/model_time.h"
//#include "marketplace/include/marketplace.h"
#include "containers/include/gdp.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/configuration.h"
#include "marketplace/include/market_info.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string BuildingHeatingDmdTechnology::XML_NAME1D = "heatingservice";
const string BuildingHeatingDmdTechnology::XML_NAME2D = "period";

// Technology class method definition

//! Default constructor.
BuildingHeatingDmdTechnology::BuildingHeatingDmdTechnology() {
    technology::initElementalMembers();
    internalGainFraction = 0;
}

//! Destructor
BuildingHeatingDmdTechnology::~BuildingHeatingDmdTechnology() {
}

//! Clone Function. Returns a deep copy of the current technology.
BuildingHeatingDmdTechnology* BuildingHeatingDmdTechnology::clone() const {
    return new BuildingHeatingDmdTechnology( *this );
}

//! Parses any input variables specific to derived classes
bool BuildingHeatingDmdTechnology::XMLDerivedClassParse( const string nodeName, const DOMNode* curr ) {
    if( BuildingDmdTechnology::XMLDerivedClassParse( nodeName, curr ) ){
    }
    else if( nodeName == "internalGainFraction" ){
        internalGainFraction = XMLHelper<double>::getValue( curr );
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
void BuildingHeatingDmdTechnology::toInputXMLDerived( ostream& out, Tabs* tabs ) const {  
    XMLWriteElementCheckDefault( saturation, "saturation", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( internalGainFraction, "internalGainFraction", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( unitDemand, "unitDemand", out, tabs, 0.0 );
}	

//! XML output for viewing.
void BuildingHeatingDmdTechnology::toOutputXMLDerived( ostream& out, Tabs* tabs ) const {
    XMLWriteElementCheckDefault( saturation, "saturation", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( internalGainFraction, "internalGainFraction", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( unitDemand, "unitDemand", out, tabs, 0.0 );
}

//! Write object to debugging xml output stream.
void BuildingHeatingDmdTechnology::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const { 
    XMLWriteElement( saturation, "saturation", out, tabs );
    XMLWriteElement( internalGainFraction, "internalGainFraction", out, tabs );
    XMLWriteElement( unitDemand, "unitDemand", out, tabs );
}	


/*! \brief Initialize each period
*
* Transfer data that does not change to the technoogy
*
* \author Steve Smith
* \param mSubsectorInfo The subsectorInfo object. 
*/
void BuildingHeatingDmdTechnology::initCalc( std::auto_ptr<MarketInfo> mSubsectorInfo ) {

    aveInsulation = mSubsectorInfo->getItemValue( "aveInsulation" );
    floorToSurfaceArea = mSubsectorInfo->getItemValue( "floorToSurfaceArea" );
    heatingDegreeDays = mSubsectorInfo->getItemValue( "heatingDegreeDays" );

    BuildingDmdTechnology::initCalc( mSubsectorInfo );    
    technology::initCalc( mSubsectorInfo );

}

/*! \brief Adjusts technology share weights to be consistent with calibration value.
* This is done only if there is more than one technology
* Calibration is, therefore, performed as part of the iteration process. 
* Since this can change derivatives, best to turn calibration off when using N-R solver.
*
* This routine adjusts technology shareweights so that relative shares are correct for each subsector.
* Note that all calibration values are scaled (up or down) according to total sectorDemand 
* -- getting the overall scale correct is the job of the TFE calibration
*
* \author Steve Smith
* \param subSectorDemand total demand for this subsector
*/
void BuildingHeatingDmdTechnology::adjustForCalibration( double subSectorDemand ) {
    
    // unitDemand is passed into this routine as subSectorDemand, but not adjusted for saturation.
    // So adjust for saturation and set variable.
    
    // Production is equal to: unitDemand * saturation * dmd
    // so unitDemand is equal to 
    unitDemand = 1;
    if ( saturation != 0 ) {
        unitDemand = subSectorDemand / ( saturation * aveInsulation * floorToSurfaceArea * heatingDegreeDays );
    }
}

//! Calculates fuel input and technology output.
/*! Unlike normal technologies, this DOES NOT add demands for fuels and ghg emissions to markets
*   The BuildingHeatingDmdTechnology just calculates demand for a service,
*   the actual fuel consumption and emissions take place in the corresponding supply sectors. 
* \author Sonny Kim
* \param regionName name of the region
* \param prodName name of the product for this sector
* \param gdp pointer to gdp object
* \param dmd total demand for this subsector
* \param per Model period
*/
void BuildingHeatingDmdTechnology::production(const string& regionName,const string& prodName,
                            double dmd, const GDP* gdp, const int per) {
    
    // dmd is in units of floor space
    input = unitDemand * saturation * aveInsulation * floorToSurfaceArea * heatingDegreeDays * dmd;
    // sjsTODO check to see if this function is correct
}
