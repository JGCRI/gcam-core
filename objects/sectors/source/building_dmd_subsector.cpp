/*! 
* \file building_dmd_subsector.cpp
* \ingroup CIAM
* \brief The building demand subsector
* \author Steve Smith
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
#include "util/base/include/ivisitor.h"
#include "containers/include/iinfo.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string BuildingDemandSubSector::XML_NAME = "buildingSubSector";

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

/*! \brief Return the string used to tag the name of the proper internal gains market.
*
* Returns the string used to tag the name of the proper internal gains market 
* for a supply sector supplying building energy services.
* The name of the internal gains market is derived from this base string.
* \author Steve Smith
* \return The constant INTERNAL_GAINS_INFO_NAME.
*/
const std::string& BuildingDemandSubSector::getInternalGainsInfoName() {
    const static string INTERNAL_GAINS_INFO_NAME = "internalGainMarketName";
    return INTERNAL_GAINS_INFO_NAME;
}

/*! \brief Return the name to be used for the internal gains market.
*
* Returns the name of the internal gains market 
* the sector name + the subsector name should always be unique.
*
* \author Steve Smith
* \param aSectorName Sector name.
* \return Name of the internal gains market for the subsector.
*/
const string BuildingDemandSubSector::getInternalGainsMarketName( const string aSectorName ) const {
    const string INTERNAL_GAINS_MKT_PREFIX = "intGains";
    return INTERNAL_GAINS_MKT_PREFIX + sectorName + name;
}

/*! \brief Returns true if the nodename is a valid child for this class.
*
* Virtual function which specifies the XML name of the possible technology children of this class.
* This function allows all technologies to be properly parsed using the base subsector code.
* \author Steve Smith
* \pre Needs cooresponding createChild() function
* \return True if nodename is a valid child of this class.
*/
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

/*! \brief Virtual function to generate a child element or construct the appropriate technology.
*
* \pre Needs cooresponding isNameOfChild() function
* \author Steve Smith
* \return returns a new child object of appropriate type.
*/
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
bool BuildingDemandSubSector::XMLDerivedClassParse( const string& aNodeName,
                                                    const DOMNode* aCurr )
{
    const Modeltime* modeltime = scenario->getModeltime();

   if( aNodeName == "daylighting" ){
        XMLHelper<double>::insertValueIntoVector( aCurr, dayLighting, modeltime );
    } 
    else if( aNodeName == "aveInsulation" ){
        XMLHelper<double>::insertValueIntoVector( aCurr, aveInsulation, modeltime );
    } 
    else if( aNodeName == "nonenergycost" ){
        XMLHelper<double>::insertValueIntoVector( aCurr, nonEnergyCost, modeltime );
    } 
    else if( aNodeName == "floorToSurfaceArea" ){
        XMLHelper<double>::insertValueIntoVector( aCurr, floorToSurfaceArea, modeltime );
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

//! Write object to debugging xml output stream.
void BuildingDemandSubSector::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    Marketplace* marketplace = scenario->getMarketplace();

    Subsector::toDebugXMLDerived( period, out, tabs );

    XMLWriteElement( nonEnergyCost[ period ], "nonenergycost", out, tabs );
    XMLWriteElement( dayLighting[ period ], "dayLighting", out, tabs );
    XMLWriteElement( aveInsulation[ period ], "aveInsulation", out, tabs );
    XMLWriteElement( floorToSurfaceArea[ period ], "floorToSurfaceArea", out, tabs );
    XMLWriteElement( marketplace->getPrice( getInternalGainsMarketName( sectorName ), regionName, period ),
                                            "internalGains", out, tabs );
    
}

/*! \brief Complete the initialization
*
* This routine is only called once per model run
*
* \author Steve Smith
* \param aSectorInfo Parent sector info object.
* \param aDependencyFinder The regional dependency finder.
* \param aLandAllocator Regional land allocator.
* \warning markets are not necesarilly set when completeInit is called
*/
void BuildingDemandSubSector::completeInit( const IInfo* aSectorInfo,
                                            DependencyFinder* aDependencyFinder,
                                            ILandAllocator* aLandAllocator )
{
    Subsector::completeInit( aSectorInfo, aDependencyFinder, aLandAllocator );
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
    const string intGainsMarketName = getInternalGainsMarketName( sectorName );
    
    // always make internal gains trail markets regional
    if( marketplace->createMarket( regionName, regionName, intGainsMarketName, IMarketType::TRIAL_VALUE ) ) {        
        // Set this market to solve
        for( int period = 1; period < maxPeriod; ++period ){
            marketplace->setMarketToSolve( intGainsMarketName, regionName, period );
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
* \todo IInfo needs to be updated to handle string values so that internal gain market name can be passed and not specified in the input data
* \param period Model period
* \warning Function getFuelName will need to be changed once multiple inputs are implimented
*/
void BuildingDemandSubSector::initCalc( NationalAccount& aNationalAccount,
                                        const Demographic* aDemographics,
                                        const MoreSectorInfo* aMoreSectorInfo,
                                        const int aPeriod )
{
    const Modeltime* modeltime = scenario->getModeltime();

    if ( aveInsulation[ aPeriod ] == 0 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Input variable aveInsulation = 0. Reset to 1." << endl;
        aveInsulation[ aPeriod ] = 1;
    }
    if ( floorToSurfaceArea[ aPeriod ] == 0 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Input variable floorToSurfaceArea = 0. Reset to 1." << endl;
        floorToSurfaceArea[ aPeriod ] = 1;
    }

    // Add subsector data items
    mSubsectorInfo->setDouble( "dayLighting", dayLighting[ aPeriod ] );
    mSubsectorInfo->setDouble( "aveInsulation", aveInsulation[ aPeriod ] );
    mSubsectorInfo->setDouble( "floorToSurfaceArea", floorToSurfaceArea[ aPeriod ] );

    // Put the name of the internal gains market into the info object so that technologies can access this information.
    // Note that this will only work because the derived class subsector initCalc() is called before the base class initCalc() 
    // -- which imediately calls technology::initCalc()
    mSubsectorInfo->setString( getInternalGainsInfoName(), getInternalGainsMarketName( sectorName ) );

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

    Subsector::initCalc( aNationalAccount, aDemographics, aMoreSectorInfo, aPeriod );
    
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
    mSubsectorInfo->setDouble( "floorSpace", sectorDemand );
    
    for ( unsigned int j = 0; j < techs.size(); j++ ) {
        // calibrate buildingServiceDemands
        const IInfo* marketInfo = marketplace->getMarketInfo( techs[j][period]->getFuelName(), regionName, period, false );
        // Market may not exist if the fuel is a fake fuel.
        double calOutput = marketInfo ? marketInfo->getDouble( "calOutput", true ) : 0;
        
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
        const IInfo* marketInfo = marketplace->getMarketInfo( techs[ i ][ period ]->getFuelName(), regionName,
                                                              period, false );
        // Market may not exist if the fuel is a fake fuel.
        double calOutput = marketInfo ? marketInfo->getDouble( "calOutput", true ) : 0;
        if ( calOutput > 0 ) {
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
double BuildingDemandSubSector::getOutput( const int period ) const {
    /*! \pre period is less than or equal to max period. */
   assert( period <= scenario->getModeltime()->getmaxper() );
   return output[period];
}

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
    
    for ( int m=0;m<maxper;m++) {
        temp[m] =  marketplace->getPrice( getInternalGainsMarketName( sectorName ), regionName, m );
    }
    dboutput4( regionName, "General", sectorName + " internalGains", name, "??", temp );
}


/*! \brief Set building subsector output here and then call regular setoutput function.
*
* \author Steve Smith
* \param regionName region name
* \param aDemand Total demand for this product.
* \param aGDP Regional GDP container.
* \param aPeriod Model period
*/
void BuildingDemandSubSector::setOutput( const double aDemand,
                                         const GDP* aGDP,
                                         const int aPeriod )
{
    output[ aPeriod ] = share[ aPeriod ] * aDemand; 
    Subsector::setOutput( aDemand, aGDP, aPeriod );
}


/*! \brief Calculate the building demand technology shares.
* \details Building demands are not substitutive, so the shares of all
*          technologies should be one.
* \author Josh Lurz
* \param aGDP GDP container.
* \param aPeriod Model period.
*/
void BuildingDemandSubSector::calcTechShares( const GDP* aGDP, const int aPeriod ) {
    double sum = 0;
    for( unsigned int i = 0; i < techs.size(); ++i ){
        // calculate technology cost
        techs[i][aPeriod]->calcCost( regionName, sectorName, aPeriod );
        // determine shares based on technology costs
        techs[i][aPeriod]->calcShare( regionName, sectorName, aGDP, aPeriod );
        
        /*! \invariant Technology shares must always be valid. */
        assert( util::isValidNumber( techs[ i ][ aPeriod ]->getShare() ) );
    }
}   

// Documentation is inherited.
void BuildingDemandSubSector::accept( IVisitor* aVisitor, const int aPeriod ) const {
    // Derived visit first.
    aVisitor->startVisitBuildingDemandSubsector( this, aPeriod );
    // Base class visit.
    Subsector::accept( aVisitor, aPeriod );
    // End the derived class visit.
    aVisitor->endVisitBuildingDemandSubsector( this, aPeriod );
}
