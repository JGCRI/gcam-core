/*! 
* \file technology.cpp
* \ingroup CIAM
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
#include "technologies/include/building_dmd_technology.h"
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
const string BuildingDmdTechnology::XML_NAME1D = "buildingservice";
const string BuildingDmdTechnology::XML_NAME2D = "period";

// Technology class method definition

//! Default constructor.
BuildingDmdTechnology::BuildingDmdTechnology() {
    technology::initElementalMembers();
    unitDemand = 0; // This must be either read in or calibrated
    saturation = 1;
}

//! Destructor
BuildingDmdTechnology::~BuildingDmdTechnology() {
}

//! Clone Function. Returns a deep copy of the current technology.
BuildingDmdTechnology* BuildingDmdTechnology::clone() const {
    return new BuildingDmdTechnology( *this );
}

//! Parses any input variables specific to derived classes
bool BuildingDmdTechnology::XMLDerivedClassParse( const string nodeName, const DOMNode* curr ) {
    // additional read in for buildings
    if( nodeName == "unitDemand" ){
        unitDemand = XMLHelper<double>::getValue( curr );
        // For the usual base case, this is not read in but calibrated.
        // This needs to be read in for a policy case where this is no calibration.
    }
    if( nodeName == "saturation" ){
        saturation = XMLHelper<double>::getValue( curr );
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
void BuildingDmdTechnology::toInputXMLDerived( ostream& out, Tabs* tabs ) const {  
    XMLWriteElementCheckDefault( saturation, "saturation", out, tabs, 1.0 );
}	

//! XML output for viewing.
void BuildingDmdTechnology::toOutputXMLDerived( ostream& out, Tabs* tabs ) const {
    XMLWriteElementCheckDefault( saturation, "saturation", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( unitDemand, "unitDemand", out, tabs, 0.0 );
}

//! Write object to debugging xml output stream.
void BuildingDmdTechnology::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const { 
    XMLWriteElement( saturation, "saturation", out, tabs );
    XMLWriteElement( unitDemand, "unitDemand", out, tabs );
}	

/*! \brief Initialize each period
*
* Transfer data that does not change to the technoogy
*
* \author Steve Smith
* \param mSubsectorInfo The subsectorInfo object. 
*/
void BuildingDmdTechnology::initCalc( std::auto_ptr<MarketInfo> mSubsectorInfo ) {
}

/*! \brief calculate technology unnormalized shares
*
* Building technologies are really just calculating demands for specific servicies
* so shares are always 1
*
* This ensures that sector price is correctly calculated.
* 
* \author Steve Smith
* \param regionName region name
* \param per model period
*/
void BuildingDmdTechnology::calcShare( const string& regionName, const GDP* gdp, const int period ) {
    share = 1.0;
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
void BuildingDmdTechnology::adjustForCalibration( double subSectorDemand ) {
    
    // unitDemand is passed into this routine as subSectorDemand, but not adjusted for saturation.
    // So adjust for saturation and set variable.
    
    // Production is equal to: unitDemand * saturation * dmd
    // so unitDemand is equal to 
    unitDemand = 1;
    if ( saturation != 0 ) {
        unitDemand = subSectorDemand / saturation;
    }
}

//! Calculates fuel input and technology output.
/*! Unlike normal technologies, this DOES NOT add demands for fuels and ghg emissions to markets
*   The BuildingDmdTechnology just calculates demand for a service,
*   the actual fuel consumption and emissions take place in the corresponding supply sectors. 
* \author Sonny Kim
* \param regionName name of the region
* \param prodName name of the product for this sector
* \param gdp pointer to gdp object
* \param dmd total demand for this subsector
* \param per Model period
*/
void BuildingDmdTechnology::production(const string& regionName,const string& prodName,
                            double dmd, const GDP* gdp, const int per) {
    
    // dmd is in units of floor space
    input = unitDemand * saturation * dmd;
    // sjsTODO check to see if this function is correct
}
