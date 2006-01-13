/*! 
* \file building_generic_dmd_technology.cpp
* \ingroup CIAM
* \brief The building service demand technology.
* \author Steve Smith
*/

// Standard Library headers
#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>

// User headers
#include "technologies/include/building_generic_dmd_technology.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/iinfo.h"
#include "sectors/include/building_dmd_subsector.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string BuildingGenericDmdTechnology::XML_NAME1D = "buildingservice";

// Technology class method definition

//! Default constructor.
BuildingGenericDmdTechnology::BuildingGenericDmdTechnology() {
    saturation = 1;
    priceElasticity = 0;
}

//! Destructor
BuildingGenericDmdTechnology::~BuildingGenericDmdTechnology() {
}

//! Clone Function. Returns a deep copy of the current technology.
BuildingGenericDmdTechnology* BuildingGenericDmdTechnology::clone() const {
    return new BuildingGenericDmdTechnology( *this );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& BuildingGenericDmdTechnology::getXMLName1D() const {
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
const std::string& BuildingGenericDmdTechnology::getXMLNameStatic1D() {
	return XML_NAME1D;
}

/*
* \brief Perform initializations that only need to be done once per period.
* \warning This may need to be adjusted for multiple inputs
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aSubsectorInfo Parent information container.
* \param aDemographics Regional demographics container.
* \param aPeriod Model period.
*/
void BuildingGenericDmdTechnology::initCalc( const string& aRegionName,
                                             const string& aSectorName,
                                             const IInfo* aSubsectorInfo,
                                             const Demographic* aDemographics,
                                             const int aPeriod )
{
    Marketplace* marketplace = scenario->getMarketplace();
    string existingName;

    string serviceSupplySectorName = getFuelName( );
    string internGainsMktName = aSubsectorInfo->getString( BuildingDemandSubSector::getInternalGainsInfoName(), true );
    assert( !internGainsMktName.empty() ); // This should have always been properly set by the building subsecto
    IInfo* marketInfo = scenario->getMarketplace()->getMarketInfo( serviceSupplySectorName, aRegionName, aPeriod, true );

    if( !marketInfo ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "There does not appear to be a market for good "<< serviceSupplySectorName 
                << " in region " << aRegionName << "." << endl;
        existingName = "no market";   // do not try to set an info object to this market if there has been a user input error.
    }
    else {
        existingName = marketInfo->getString( BuildingDemandSubSector::getInternalGainsInfoName(), false );
    }
    
    if ( existingName.empty() ) {
        // Pass the internal gains market name to the market supplying this technology
        marketInfo->setString( BuildingDemandSubSector::getInternalGainsInfoName(), internGainsMktName );
    }
    else if ( existingName != internGainsMktName ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Building service supply sector "<< serviceSupplySectorName <<" in region "<< aRegionName
                    <<" appears to be pointing to two different demand sectors." << endl;
    }
    
    technology::initCalc( aRegionName, aSectorName, aSubsectorInfo, aDemographics, aPeriod );
}

//! Parses any input variables specific to derived classes
bool BuildingGenericDmdTechnology::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    // additional read in for buildings
    if( nodeName == "saturation" ){
        saturation = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "pElasticity" ){
        priceElasticity = XMLHelper<double>::getValue( curr );
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
void BuildingGenericDmdTechnology::toInputXMLDerived( ostream& out, Tabs* tabs ) const {  
    XMLWriteElementCheckDefault( saturation, "saturation", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( priceElasticity, "pElasticity", out, tabs, 0.0 );
}	

//! Write object to debugging xml output stream.
void BuildingGenericDmdTechnology::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const { 
    XMLWriteElement( saturation, "saturation", out, tabs );
    XMLWriteElement( priceElasticity, "pElasticity", out, tabs );
}	

/*!
* \brief calculate technology unnormalized shares
* \details Building technologies are really just calculating demands for
*          specific servicies so shares are always 1. This ensures that sector
*          price is correctly calculated.
* \author Steve Smith
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aGDP Regional GDP container.
* \param aPeriod Model period.
*/
void BuildingGenericDmdTechnology::calcShare( const string& aRegionName,
                                              const std::string& aSectorName,
                                              const GDP* aGDP,
                                              const int aPeriod )
{
	share = 1;
}

/*! \brief calculate effective internal gains as they affect the demand for this technology
*
* For non-heating and cooling technologies this always zero -- internal gains do not affect the demand for
* these technologies
* 
* \author Steve Smith
*/
double BuildingGenericDmdTechnology::getEffectiveInternalGains( const string& regionName, const int period ) {
    return 0;
}

/*! \brief Adjusts technology parameters as necessary to be consistent with calibration value.
*
* For these demand "technologies" the unitDemand needs to be adjusted so that output
* is consistant with calibrated input demand. This version works for demands that do not take into account internal gains.
*
* \author Steve Smith
* \param unitDemand calibrated unit demand (demand per unit floorspace) for this subsector
* \param regionName regionName
* \param aSubSectorInfo Info object (not used for this class so name is left out) 
* \param period model period
*/
void BuildingGenericDmdTechnology::adjustForCalibration( double subSectorDemand, const string& regionName,
														 const IInfo* aSubSectorInfo, const int period )
{
    // unitDemand (demand per unit area) is passed into this routine as subSectorDemand, but not adjusted for saturation and other parameters.    
    double unitDemand = subSectorDemand;

    // Production is equal to: unitDemand * saturation *(any other parameters) * dmd
    
     shrwts = unitDemand / getDemandFnPrefix( regionName, period );
 }

/*! \brief Calculates the amount of output from the technology.
* \details Unlike normal technologies, this does NOT add demands for fuels and
*          ghg emissions to markets. The BuildingGenericDmdTechnology just
*          calculates demand for a service, the actual fuel consumption and
*          emissions take place in the corresponding supply sectors. 
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aDemand Subsector demand for output.
* \param aGDP Regional GDP container.
* \param aPeriod Model period.
* \author Steve Smith
*/
void BuildingGenericDmdTechnology::production( const string& aRegionName,
                                               const string& aSectorName,
                                               const double aDemand,
                                               const GDP* aGDP,
                                               const int aPeriod )
{
	Marketplace* marketplace = scenario->getMarketplace();

	// dmd is in units of floor space
	double floorSpace = aDemand; 

	input = shrwts * getDemandFnPrefix( aRegionName, aPeriod ) * floorSpace
            + getEffectiveInternalGains( aRegionName, aPeriod );

	output = input = max( input, 0.0 ); // Make sure internal gains do not drive service less than zero

	// set demand for fuel in marketplace
	marketplace->addToDemand( fuelname, aRegionName, input, aPeriod );

}

//! Demand function prefix.
/*! The demand for this building service is equal to the value of this function
* times the amount of floor space and share weight. 
* This allows different demand technologies to have a different parameterization by only
* changing this function (which is used both to set demand and to calibrate the demand function coefficient).
*
* This version is generic, only includes a saturation parameter and the price response.
*
* \author Steve Smith
* \param regionName name of the region
* \param period Model period
*/
double BuildingGenericDmdTechnology::getDemandFnPrefix( const string& regionName, const int period )  {
Marketplace* marketplace = scenario->getMarketplace();

    double priceRatio = ( period > 1 ) ? 
        marketplace->getPrice( fuelname, regionName, period ) / 
        marketplace->getPrice( fuelname, regionName, 1) : 1;
            
    double prefixValue = saturation * pow( priceRatio, priceElasticity );
    
    // Make sure and do not return zero
    return ( prefixValue > 0 ) ? prefixValue : 1;
}
