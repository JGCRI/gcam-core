/*! 
* \file building_generic_dmd_technology.cpp
* \ingroup Objects
* \brief BuildingGenericDmdTechnology class source file.
* \author Steve Smith
*/

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
#include "technologies/include/iproduction_state.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

// Technology class method definition

/*! 
 * \brief Constructor.
 * \param aName Technology name.
 * \param aYear Technology year.
 */
BuildingGenericDmdTechnology::BuildingGenericDmdTechnology( const string& aName, const int aYear )
:Technology( aName, aYear ){
    saturation = 1;
    priceElasticity = 0;
}

//! Destructor
BuildingGenericDmdTechnology::~BuildingGenericDmdTechnology() {
}

void BuildingGenericDmdTechnology::postCalc( const string& aRegionName, const int aPeriod ) {
    Technology::postCalc( aRegionName, aPeriod );
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
	return getXMLNameStatic1D();
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
	const static string XML_NAME1D = "buildingservice";
	return XML_NAME1D;
}

void BuildingGenericDmdTechnology::completeInit( const string& aRegionName,
                                                 const string& aSectorName,
                                                 DependencyFinder* aDepFinder,
                                                 const IInfo* aSubsectorInfo,
                                                 ILandAllocator* aLandAllocator,
                                                 const GlobalTechnologyDatabase* aGlobalTechDB )
{
    Technology::completeInit( aRegionName, aSectorName, aDepFinder,
                              aSubsectorInfo, aLandAllocator, aGlobalTechDB );
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


    const string internGainsMktName =
        aSubsectorInfo->getString( BuildingDemandSubSector::getInternalGainsInfoName(), true );
    assert( !internGainsMktName.empty() ); // This should have always been properly set by the building subsecto
    
    const string serviceSupplySectorName = getFuelName();
    IInfo* marketInfo = scenario->getMarketplace()->getMarketInfo( serviceSupplySectorName, aRegionName, aPeriod, true );
    
    // This should have always been properly set by the building subsector
    assert( !internGainsMktName.empty() ); 
    
    string existingName;
    if( !marketInfo ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "There does not appear to be a market for good " << getFuelName() 
                << " in region "<< aRegionName << "." << endl;
        // Do not try to set an info object to this market if there has been a
        // user input error.
        existingName = "no market";   
    }
    else {
        existingName = marketInfo->getString( BuildingDemandSubSector::getInternalGainsInfoName(), false );
    }
    
    if ( existingName.empty() ) {
        // Pass the internal gains market name to the market supplying this
        // technology
        marketInfo->setString( BuildingDemandSubSector::getInternalGainsInfoName(), internGainsMktName );
    }
    else if ( existingName != internGainsMktName ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Building service supply sector "<< serviceSupplySectorName <<" in region "<< aRegionName
                    <<" appears to be pointing to two different demand sectors." << endl;
    }
    
    Technology::initCalc( aRegionName, aSectorName, aSubsectorInfo, aDemographics, aPeriod );
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
* \param aSectorName Sector name.
* \param aGDP Regional GDP container.
* \param aPeriod Model period.
* \return The building demand technology share which is always one.
*/
double BuildingGenericDmdTechnology::calcShare( const string& aRegionName,
                                                const string& aSectorName,
                                                const GDP* aGDP,
                                                const int aPeriod ) const
{
	return 1;
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
* \param aTechnologyDemand calibrated unit demand (demand per unit floorspace) for this subsector
* \param aRegionName regionName
* \param aSubSectorInfo Info object (not used for this class so name is left out) 
* \param aPeriod model period
*/
void BuildingGenericDmdTechnology::adjustForCalibration( double aTechnologyDemand,
                                                         const string& aRegionName,
														 const IInfo* aSubSectorInfo,
                                                         const int aPeriod )
{
    // Make sure this is only called in the technology's initial year.
    // Make sure this is only called in the technology's initial year.
    if( !mProductionState[ aPeriod ]->isOperating() ){
        return;
    }

    // unitDemand (demand per unit area) is passed into this routine as
    // subSectorDemand, but not adjusted for saturation and other parameters.    
    double unitDemand = aTechnologyDemand;
    // Production is equal to: unitDemand * saturation *(any other parameters) * dmd
	shrwts = unitDemand / getDemandFnPrefix( aRegionName, aPeriod );
    assert( shrwts >= 0 && util::isValidNumber( shrwts ) );
}

/*! \brief Calculates the amount of output from the technology.
* \details Unlike normal technologies, this does NOT add demands for fuels and
*          ghg emissions to markets. The BuildingGenericDmdTechnology just
*          calculates demand for a service, the actual fuel consumption and
*          emissions take place in the corresponding supply sectors. 
* \param aRegionName name of the region
* \param aSectorName name of the product for this sector
* \param aVariableDemand Total demand for this subsector
* \param aFixedOutputScaleFactor Scale factor to scale down fixed output.
* \param aGDP GDP object.
* \param aPeriod Model period
*/
void BuildingGenericDmdTechnology::production( const string& aRegionName,
                                               const string& aSectorName,
											   double aVariableDemand,
                                               double aFixedOutputScaleFactor,
											   const GDP* aGDP, const int aPeriod )
{
    if( !mProductionState[ aPeriod ]->isOperating() ){
        return;
    }
	
    // dmd is in units of floor space
	double floorSpace = aVariableDemand; 

	mInput[ aPeriod ] = shrwts * getDemandFnPrefix( aRegionName, aPeriod ) * floorSpace
            + getEffectiveInternalGains( aRegionName, aPeriod );

    double primaryOutput = mInput[ aPeriod ] = max( mInput[ aPeriod ], 0.0 ); // Make sure internal gains do not drive service less than zero

    calcEmissionsAndOutputs( aRegionName, mInput[ aPeriod ], primaryOutput, aGDP, aPeriod );

	// set demand for fuel in marketplace
	Marketplace* marketplace = scenario->getMarketplace();
	marketplace->addToDemand( mTechData->getFuelName(), aRegionName, mInput[ aPeriod ], aPeriod );
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
        marketplace->getPrice( mTechData->getFuelName(), regionName, period ) / 
        marketplace->getPrice( mTechData->getFuelName(), regionName, 1) : 1;
            
    double prefixValue = saturation * pow( priceRatio, priceElasticity );
    
    // Make sure and do not return zero
    assert( util::isValidNumber( prefixValue ) );
    return ( prefixValue > 0 ) ? prefixValue : 1;
}

void BuildingGenericDmdTechnology::calcCost( const string& aRegionName,
                                             const string& aSectorName,
											 const int aPeriod )
{
	Technology::calcCost( aRegionName, aSectorName, aPeriod );
}

double BuildingGenericDmdTechnology::getFuelCost( const string& aRegionName, const string& aSectorName,
												  const int aPeriod ) const 
{
	return Technology::getFuelCost( aRegionName, aSectorName, aPeriod );
}

double BuildingGenericDmdTechnology::getEfficiency( const int aPeriod ) const {
    return Technology::getEfficiency( aPeriod );
}

double BuildingGenericDmdTechnology::getNonEnergyCost( const int aPeriod ) const {
    return Technology::getNonEnergyCost( aPeriod );
}

