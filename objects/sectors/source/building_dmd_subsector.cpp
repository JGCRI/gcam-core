/*! 
* \file building_dmd_subsector.cpp
* \ingroup CIAM
* \brief The building demand subsector
* \author Steve Smith
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

#include "sectors/include/building_dmd_subsector.h"
#include "technologies/include/building_generic_dmd_technology.h"
#include "technologies/include/building_cooling_dmd_technology.h"
#include "technologies/include/building_heating_dmd_technology.h"

#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/gdp.h"
#include "util/logger/include/ilogger.h"
#include "marketplace/include/market_info.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string BuildingDemandSubSector::XML_NAME = "buildingSubSector";
const string INTERNAL_GAINS_MKT = "intGains";

/*! \brief Default constructor.
*
* Constructor initializes member variables with default values, sets vector sizes, etc.
*
* \author Sonny Kim, Steve Smith, Josh Lurz
*/
BuildingDemandSubSector::BuildingDemandSubSector( const string regionName, const string sectorName ) : Subsector( regionName, sectorName ){
    
    // resize vectors
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    output.resize( maxper );
    dayLighting.resize( maxper, 0 );
    nonEnergyCost.resize( maxper, 0 );
    aveInsulation.resize( maxper, 0 );
    floorToSurfaceArea.resize( maxper, 0 );
    
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& BuildingDemandSubSector::getXMLName() const {
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
const std::string& BuildingDemandSubSector::getXMLNameStatic() {
	return XML_NAME;
}

//! Virtual function which specifies the XML name of the children of this class, the type of technology.
bool BuildingDemandSubSector::isNameOfChild  ( const string& nodename ) const {
    if ( nodename == BuildingGenericDmdTechnology::getXMLNameStatic1D() ) {
        return true;
    }
    else if ( nodename == BuildingCoolingDmdTechnology::getXMLNameStatic1D() ){
        return true;
    }
    else if ( nodename == BuildingHeatingDmdTechnology::getXMLNameStatic1D() ){
        return true;
    } 
    else {
        return false;
    }
}

//! Virtual function to generate a child element or construct the appropriate technology.
technology* BuildingDemandSubSector::createChild( const string& nodename ) const {
    if ( nodename == BuildingGenericDmdTechnology::getXMLNameStatic1D() ) {
        return new BuildingGenericDmdTechnology();
    }
    else if ( nodename == BuildingCoolingDmdTechnology::getXMLNameStatic1D() ){
        return new BuildingCoolingDmdTechnology();
    }
    else if ( nodename == BuildingHeatingDmdTechnology::getXMLNameStatic1D() ){
        return new BuildingHeatingDmdTechnology();
    } 
    else {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "ERROR: No building demand technology created in subsector type " << getXMLNameStatic() << endl;
        return 0;
    }
}

/*! \brief Parses any input variables specific to derived classes
*
*/
bool BuildingDemandSubSector::XMLDerivedClassParse( const string nodeName, const DOMNode* curr ) {
    const Modeltime* modeltime = scenario->getModeltime();

   if( nodeName == "daylighting" ){
        XMLHelper<double>::insertValueIntoVector( curr, dayLighting, modeltime );
    } 
    else if( nodeName == "aveInsulation" ){
        XMLHelper<double>::insertValueIntoVector( curr, aveInsulation, modeltime );
    } 
    else if( nodeName == "nonenergycost" ){
        XMLHelper<double>::insertValueIntoVector( curr, nonEnergyCost, modeltime );
    } 
    else if( nodeName == "floorToSurfaceArea" ){
        XMLHelper<double>::insertValueIntoVector( curr, floorToSurfaceArea, modeltime );
    } else {
      return false;
    }
    return true;
}

/*! \brief XML output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML
*
* \author Steve Smith, Josh Lurz
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void BuildingDemandSubSector::toInputXMLDerived( ostream& out, Tabs* tabs ) const {  
    const Modeltime* modeltime = scenario->getModeltime();
   
    Subsector::toInputXMLDerived( out, tabs );
    XMLWriteVector( nonEnergyCost, "nonenergycost", out, tabs, modeltime, 0.0 );
    XMLWriteVector( dayLighting, "daylighting", out, tabs, modeltime, 0.0 );
    XMLWriteVector( aveInsulation, "aveInsulation", out, tabs, modeltime, 0.0 );
    XMLWriteVector( floorToSurfaceArea, "floorToSurfaceArea", out, tabs, modeltime, 1.0 );
}	


//! XML output for viewing.
void BuildingDemandSubSector::toOutputXMLDerived( ostream& out, Tabs* tabs ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    
    Subsector::toOutputXMLDerived( out, tabs );

    XMLWriteVector( nonEnergyCost, "nonenergycost", out, tabs, modeltime, 0.0 );
    XMLWriteVector( dayLighting, "daylighting", out, tabs, modeltime, 0.0 );
    XMLWriteVector( aveInsulation, "aveInsulation", out, tabs, modeltime, 0.0 );
    XMLWriteVector( floorToSurfaceArea, "floorToSurfaceArea", out, tabs, modeltime, 1.0 );
}

//! Write object to debugging xml output stream.
void BuildingDemandSubSector::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    Marketplace* marketplace = scenario->getMarketplace();

    Subsector::toDebugXMLDerived( period, out, tabs );

    XMLWriteElement( nonEnergyCost[ period ], "nonenergycost", out, tabs );
    XMLWriteElement( dayLighting[ period ], "dayLighting", out, tabs );
    XMLWriteElement( aveInsulation[ period ], "aveInsulation", out, tabs );
    XMLWriteElement( floorToSurfaceArea[ period ], "floorToSurfaceArea", out, tabs );
    string intGainsMarketName = INTERNAL_GAINS_MKT + sectorName + name;
    XMLWriteElement( marketplace->getPrice( intGainsMarketName, regionName, period ), "internalGains", out, tabs );
    
}

/*! \brief Complete the initialization
*
* This routine is only called once per model run
*
* \author Steve Smith
* \param aDependencyFinder The regional dependency finder.
* \warning markets are not necesarilly set when completeInit is called
*/
void BuildingDemandSubSector::completeInit( DependencyFinder* aDependencyFinder ) {
    Subsector::completeInit( aDependencyFinder );
    setUpSubSectorMarkets();
}

/*! \brief Set up the internal gain market for this building type
*
* The internal gain market is a trial value market used so that the internal
* gain values are always available
*
* \author Steve Smith
*/
void BuildingDemandSubSector::setUpSubSectorMarkets() {
    Marketplace* marketplace = scenario->getMarketplace();
    const Modeltime* modeltime = scenario->getModeltime();
    int maxPeriod = modeltime->getmaxper();

    vector<double> initValues;
    initValues.resize( maxPeriod, 0 );
    // name is of this market is  prefix plus sector name and subsector name.
    // This will always be unique to a region.
    string intGainsMarketName = INTERNAL_GAINS_MKT + sectorName + name;
    
    // always make internal gains trail markets regional
    if( marketplace->createMarket( regionName, regionName, intGainsMarketName, IMarketType::TRIAL_VALUE ) ) {
        
        // Set this market to solve
        for( int period = 1; period < maxPeriod; ++period ){
            marketplace->setMarketToSolve( intGainsMarketName, regionName, period );
            // Also initialize calibrated internal gains to zero for every period
            marketplace->setMarketInfo( intGainsMarketName, regionName, period, "calInternalGains", 0.0 );
        }
    }
    else {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << " ERROR: Market " <<  intGainsMarketName << " already exists. intGainsMarketName must be unique. " << endl;
    }
}

/*! \brief Adds building non-energy cost to subsector price
*
* The building subsector represents a building type. This building type has a non-energy cost which must
* be added to the normal sub-sector cost (which is normally just the weighted cost of the technologies)
*
* The energy service price is the sum of the subsector energy service prices -- these are not shared out.
* 
* No CO2 coefficient is calculated as this is a pure demand sector with no emissions.
*
* \author Steve Smith
* \param regionName region name
* \param period Model period
*/
void BuildingDemandSubSector::calcPrice( const int period ) {
    
    subsectorprice[period] = 0; // initialize to 0 for summing
    fuelprice[period] = 0; // initialize to 0 for summing
    // There is no fuel price as this sector does not directly consume fuels
    
    for ( unsigned int i=0; i< techs.size(); i++ ) {
        // calculate sum of all energy service costs
        subsectorprice[period] += techs[i][period]->getTechcost();
      }
    
    subsectorprice[ period ] += nonEnergyCost [ period ];
 }

/*! \brief Perform any initializations needed for each period.
*
* Put dayLighting and other subsector variables into subSectorInfo object so that these are available to 
* demand technologies
*
* \author Steve Smith
* \todo MarketInfo needs to be updated to handle string values so that internal gain market name can be passed and not specified in the input data
* \param period Model period
* \warning Function getFuelName will need to be changed once multiple inputs are implimented
*/
void BuildingDemandSubSector::initCalc( const MarketInfo* aSectorInfo,
                                        NationalAccount& aNationalAccount,
                                        Demographic* aDemographics,
                                        const MoreSectorInfo* aMoreSectorInfo,
                                        const int aPeriod )
{
    const Modeltime* modeltime = scenario->getModeltime();

    if ( aveInsulation[ aPeriod ] == 0 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "WARNING: Input variable aveInsulation = 0. Reset to 1." << endl;
        aveInsulation[ aPeriod ] = 1;
    }
    if ( floorToSurfaceArea[ aPeriod ] == 0 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "WARNING: Input variable floorToSurfaceArea = 0. Reset to 1." << endl;
        floorToSurfaceArea[ aPeriod ] = 1;
    }

    // Add items from sectorInfo
    mSubsectorInfo->addItem( "heatingDegreeDays", aSectorInfo->getItemValue( "heatingDegreeDays", true ) );
    mSubsectorInfo->addItem( "coolingDegreeDays", aSectorInfo->getItemValue( "coolingDegreeDays", true  ));

    // Add subsector data items
    mSubsectorInfo->addItem( "dayLighting", dayLighting[ aPeriod ] );
    mSubsectorInfo->addItem( "aveInsulation", aveInsulation[ aPeriod ] );
    mSubsectorInfo->addItem( "floorToSurfaceArea", floorToSurfaceArea[ aPeriod ] );

    // Pass the name of the internal gains market to each demand technology  
    // TODO -- this needs to be implimented once marketInfo can handle strings
    // TODO -- also add check in appropriate place that each supply is unique to a building subsector
    string internGainsMktName = INTERNAL_GAINS_MKT + sectorName + name;
    for ( unsigned int j = 0; j < techs.size(); j++ ) {
        string demandSectorName = techs[j][aPeriod]->getFuelName( );
    //    marketplace->setMarketInfo( demandSectorName, regionName, period, "internalGainMarketName", internGainsMktName );
    }
    
    // for building demand "technologies", share weights, unit demands, should always be carried forward
    // This needs to be set before Subsector::initCalc() is called since that is where share weights are interpolated
    
    // print warning to log if a value was read in
    if ( techScaleYear != modeltime->getEndYear() ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::DEBUG );
        mainLog << "techScaleYear was read in for building subsector " << name << ". Value ignored. " << endl;        
    }
    
    if ( aPeriod > modeltime->getyr_to_per( modeltime->getStartYear() ) ) {
        techScaleYear = modeltime->getper_to_yr( aPeriod - 1 );
    }
    else {
        techScaleYear = modeltime->getStartYear();
    }

    Subsector::initCalc( aSectorInfo, aNationalAccount, aDemographics, aMoreSectorInfo, aPeriod );
    
}

/*! \brief Wrapper method for calls to normalize and/or interpolate technology shareweights (for a previously calibrated period)
*
*   For demand technologies never normalize share weights (normalizeTechShareWeights is not called below)
*   and always copy share weights forward (set start and end periods equal)
*
* \author Steve Smith
* \param period Model period
* \pre called from Subsector::interpolateShareWeights
*/
void BuildingDemandSubSector::adjustTechnologyShareWeights( const int period ) {

    // Linearlly interpolate technology shareweights
    techShareWeightLinearInterpFn( period - 1, period - 1 );
}

/*! \brief Calls building technology calibration routine
*  
* For the building demand subSector there is no calibration
* (unless there is more than one building type in the base year)
*
* But each BuildingGenericDmdTechnology must have its demand calibrated
* \author Steve Smith
* \param sectorDemand total demand for this sector
* \param totalfixedOutput total amount of fixed supply for this sector
* \param totalCalOutputs total amount of calibrated outputs for this sector
* \param allFixedOutput flag if all outputs from this sector are calibrated
* \param period Model period
* \warning This only works for one building sub-sector at present -- subsector share weights are not changed
* \warning Function getFuelName will need to be changed once multiple inputs are implimented
*/
void BuildingDemandSubSector::adjustForCalibration( double sectorDemand, double totalfixedOutput, double totalCalOutputs, const bool allFixedOutput, const int period ) {
    const Modeltime* modeltime = scenario->getModeltime();
    Marketplace* marketplace = scenario->getMarketplace();

    // Add floorspace information to subsector info object so that it is available to the technology
    // Note that "raw" unit demand, unadjusted by internal gains, is passed as the demand variable
    // this is so that those demand sectors that do not need internal gains or other information will  
    // not have to access the sectorInfo object.
    mSubsectorInfo->addItem( "floorSpace", sectorDemand );
    
    for ( unsigned int j = 0; j < techs.size(); j++ ) {
        // calibrate buildingServiceDemands
        string demandSectorName = techs[j][period]->getFuelName( );
        double calOutput = marketplace->getMarketInfo( demandSectorName, regionName, period, "calOutput" );
        
        // Here, pass in specific demand -- equal to demand for this service per unit floor space
        // NOTE: this is not adjusted for saturation or other parameters, this is done in BuildingGenericDmdTechnology
        double unitDemand = 0;
        if ( sectorDemand != 0 ) {
            unitDemand = calOutput / sectorDemand;
        }
        techs[j][period]->adjustForCalibration( unitDemand, regionName, mSubsectorInfo.get(), period );
    }
    
    // If are adjusting for calibration, then will want to keep technology share weights constant after this point.
    techScaleYear = modeltime->getper_to_yr( period );
}

/*! \brief Sets the calibrationStatus variable to true if this Subsector, or underlying technologies, are calibrated.
*
* For the building demand subsectors, input is calibrated if any of the technology services have calibrated outputs
*
* \author Steve Smith
* \param period Model period
*/
void BuildingDemandSubSector::setCalibrationStatus( const int period ) {
    for ( unsigned int i=0; i < techs.size(); i++ ) {
        Marketplace* marketplace = scenario->getMarketplace();
        
        string inputName = techs[ i ][ period ]->getFuelName( );
        if ( marketplace->getMarketInfo( inputName, regionName, period, "calOutput" ) > 0 ) {
            calibrationStatus[ period ] = true;
            return;
        }
    }
}

/*! \brief returns Subsector output
*
* For building subsectors do not call sumOutput since outputs from technologies
* cannot be summed -- they are demand functions, not production functions and do
* not have outputs.
*
* \author Steve Smith
* \param period Model period
* \return sector output
*/
double BuildingDemandSubSector::getOutput( const int period ) {
    /*! \pre period is less than or equal to max period. */
   assert( period <= scenario->getModeltime()->getmaxper() );
   return output[period];
}

//! 
/*! \brief Output variables specific to building demand sub-sector.
*
* \author Steve Smith
*/
void BuildingDemandSubSector::MCDerivedClassOutput( ) const {
Marketplace* marketplace = scenario->getMarketplace();
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);

    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);

    dboutput4( regionName, "Price", sectorName + " NE Cost", name, "75$/Ser", nonEnergyCost );
    
    string intGainsMarketName = INTERNAL_GAINS_MKT + sectorName + name;
    for ( int m=0;m<maxper;m++) {
            temp[m] =  marketplace->getPrice( intGainsMarketName, regionName, m );
    }
     dboutput4( regionName, "General", sectorName + " internalGains", name, "??", temp );
}


/*! \brief Set building subsector output here and then call regular setoutput function.
*
* \author Steve Smith
* \param regionName region name
* \param prodName name of product for this sector
* \param demand Total demand for this product
* \param period Model period
*/
void BuildingDemandSubSector::setoutput( const double demand, const int period, const GDP* gdp ) {
   
    output[ period ] = share[ period ]* demand; 
    
    Subsector::setoutput( demand, period, gdp );
    
}


