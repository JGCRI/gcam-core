/*!
* \file region_minicam.cpp
* \ingroup Objects
* \brief The RegionMiniCAM class source file.
* \author Sonny Kim
*/

//TODO: Clean up #includes

#include "util/base/include/definitions.h"
#include <fstream>
#include <string>
#include <vector>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include <algorithm>
#include <memory>
#include <stack>

#include "containers/include/region_minicam.h"
#include "containers/include/gdp.h"
#include "containers/include/scenario.h"
#include "containers/include/info_factory.h"
#include "containers/include/iinfo.h"
#include "containers/include/dependency_finder.h"
#include "sectors/include/cal_quantity_tabulator.h"

// TODO: This needs a factory.
#include "sectors/include/sector.h"
#include "sectors/include/supply_sector.h"
#include "sectors/include/production_sector.h"
#include "sectors/include/demand_sector.h"
#include "sectors/include/food_supply_sector.h"
#include "sectors/include/forest_demand_sector.h"
#include "sectors/include/forest_supply_sector.h"
#include "sectors/include/tran_sector.h"
#include "sectors/include/ag_sector.h"
#include "sectors/include/building_dmd_sector.h"
#include "sectors/include/building_supply_sector.h"
#include "sectors/include/export_sector.h"
#include "sectors/include/interm_supply_sector.h"

#include "resources/include/resource.h"
#include "resources/include/unlimited_resource.h"

#include "demographics/include/demographic.h"

#include "marketplace/include/marketplace.h"

#include "land_allocator/include/tree_land_allocator.h"
#include "emissions/include/emissions_summer.h"
#include "emissions/include/ghg_policy.h"
#include "emissions/include/total_sector_emissions.h"

#include "util/base/include/input_finder.h"
#include "util/base/include/summary.h"
#include "util/base/include/ivisitor.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/model_time.h"
#include "util/base/include/configuration.h"
#include "util/base/include/util.h"
#include "util/logger/include/ilogger.h"

#include "reporting/include/indirect_emissions_calculator.h"

using namespace std;
using namespace xercesc;

typedef std::vector<DemandSector*>::iterator DemandSectorIterator;
typedef std::vector<DemandSector*>::const_iterator CDemandSectorIterator;
typedef std::vector<AResource*>::iterator ResourceIterator;
typedef std::vector<AResource*>::const_iterator CResourceIterator;
typedef std::vector<GHGPolicy*>::iterator GHGPolicyIterator;
typedef std::vector<GHGPolicy*>::const_iterator CGHGPolicyIterator;
typedef std::vector<Sector*>::iterator SectorIterator;
typedef std::vector<Sector*>::reverse_iterator SectorReverseIterator;
typedef std::vector<Sector*>::const_iterator CSectorIterator;

extern Scenario* scenario;

//! Default constructor
RegionMiniCAM::RegionMiniCAM() {
    /*! \pre The modeltime object must be read-in before the Region can be
    *        parsed.
    */
    assert( scenario->getModeltime() );

    // Resize all vectors to maximum period
    const int maxper = scenario->getModeltime()->getmaxper();
    summary.resize( maxper );
    calibrationGDPs.resize( maxper );
    GDPcalPerCapita.resize( maxper );
    mLandUseCO2Emissions.resize( maxper );

    heatingDegreeDays = 0;
    coolingDegreeDays = 0;
    mRotationPeriod = 0;
    mInterestRate = 0;
}

//! Default destructor destroys sector, demsector, Resource, agSector, and
//! population objects.
RegionMiniCAM::~RegionMiniCAM() {
    clear();
}

//! Clear member variables and initialize elemental members.
void RegionMiniCAM::clear(){

    for ( DemandSectorIterator demIter = demandSector.begin(); demIter != demandSector.end(); ++demIter ) {
        delete *demIter;
    }

    for ( ResourceIterator rescIter = resources.begin(); rescIter != resources.end(); ++rescIter ) {
        delete *rescIter;
    }
}

// for parsing derived region classes
bool RegionMiniCAM::XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ) {
    if( nodeName == "PrimaryFuelCO2Coef" ) {
        primaryFuelCO2Coef[ XMLHelper<string>::getAttr( curr, "name" ) ] = XMLHelper<double>::getValue( curr );
    }

    else if( nodeName == GDP::getXMLNameStatic() ){
        parseSingleNode( curr, gdp, new GDP );
    }
    else if( nodeName == "coolingDegreeDays" ){
        coolingDegreeDays = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "heatingDegreeDays" ) {
        heatingDegreeDays = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "rotationPeriod" ) {
        mRotationPeriod = XMLHelper<int>::getValue( curr );
    }
    else if( nodeName == "interest-rate" ){
        mInterestRate = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == DepletableResource::getXMLNameStatic() ){
        parseContainerNode( curr, resources, new DepletableResource() );
    }
    else if( nodeName == FixedResource::getXMLNameStatic() ){
        parseContainerNode( curr, resources, new FixedResource() );
    }
    else if( nodeName == RenewableResource::getXMLNameStatic() ){
        parseContainerNode( curr, resources, new RenewableResource() );
    }
    else if( nodeName == UnlimitedResource::getXMLNameStatic() ){
        parseContainerNode( curr, resources, new UnlimitedResource );
    }
    else if( nodeName == SupplySector::getXMLNameStatic() ){
        parseContainerNode( curr, supplySector, supplySectorNameMap, new SupplySector( name ) );
    }
    // Intermittent supply sector is contained in supplySector
    else if( nodeName == IntermittentSupplySector::getXMLNameStatic() ){
        parseContainerNode( curr, supplySector, supplySectorNameMap, new IntermittentSupplySector( name ) );
    }
    else if( nodeName == BuildingSupplySector::getXMLNameStatic() ){
        parseContainerNode( curr, supplySector, supplySectorNameMap, new BuildingSupplySector( name ) );
    }
    else if( nodeName == ExportSector::getXMLNameStatic() ){
        parseContainerNode( curr, supplySector, supplySectorNameMap, new ExportSector( name ) );
    }
    else if( nodeName == FoodSupplySector::getXMLNameStatic() ) {
        parseContainerNode( curr, supplySector, supplySectorNameMap, new FoodSupplySector( name ) );
    }
    else if( nodeName == ForestSupplySector::getXMLNameStatic() ) {
        parseContainerNode( curr, supplySector, supplySectorNameMap, new ForestSupplySector( name ) );
    }
    else if( nodeName == DemandSector::getXMLNameStatic() ){
        parseContainerNode( curr, demandSector, new DemandSector( name ) );
    }
    else if( nodeName == ForestDemandSector::getXMLNameStatic() ){
        parseContainerNode( curr, demandSector, new ForestDemandSector( name ) );
    }
    else if( nodeName == BuildingDemandSector::getXMLNameStatic() ){
        parseContainerNode( curr, demandSector, new BuildingDemandSector( name ) );
    }
    else if( nodeName == TotalSectorEmissions::getXMLNameStatic() ){
        parseContainerNode( curr, mAggEmissionsCalculators, new TotalSectorEmissions );
    }
    // transportation sector is contained in demandSector
    else if( nodeName == TranSector::getXMLNameStatic() ){
        parseContainerNode( curr, demandSector, new TranSector( name ) );
    }
    else if ( nodeName == TreeLandAllocator::getXMLNameStatic() ) {
        parseSingleNode( curr, mLandAllocator, new TreeLandAllocator );
    }
    else if( nodeName == AgSector::getXMLNameStatic() ) {
        if( Configuration::getInstance()->getBool( "agSectorActive" ) ){
            parseSingleNode( curr, agSector, new AgSector );
        }
    }
    else if( nodeName == "land-use-co2-emissions" ){
        XMLHelper<Value>::insertValueIntoVector( curr, mLandUseCO2Emissions, scenario->getModeltime() );
    }
    // regional economic data
    else if( nodeName == "calibrationdata" ){
        // get all child nodes.
        DOMNodeList* nodeListChild = curr->getChildNodes();
        // loop through the child nodes.
        string nodeNameChild;
        for( unsigned int j = 0; j < nodeListChild->getLength(); j++ ){
            DOMNode* currChild = nodeListChild->item( j );
            nodeNameChild = XMLHelper<string>::safeTranscode( currChild->getNodeName() );
            const Modeltime* modeltime = scenario->getModeltime();

            if( nodeNameChild == "#text" ) {
                continue;
            }
            else if( nodeNameChild == "GDPcal" ) { // TODO: MOVE TO GDP
                XMLHelper<double>::insertValueIntoVector( currChild, calibrationGDPs, modeltime );
            }
            // Per-capita value -- is converted to total GDP using population
            else if( nodeNameChild == "GDPcalPerCapita" ) { // TODO: MOVE TO GDP
                XMLHelper<double>::insertValueIntoVector( currChild, GDPcalPerCapita, modeltime );
            }
            else {
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::WARNING );
                mainLog << "Unrecognized text string: " << nodeNameChild << " found while parsing region->calibrationdata." << endl;
            }
        }
    }
#if SORT_TESTING
    // A list representing the correct order in which to calculate the sectors.
    else if( nodeName == "SectorOrderList" ){
        // get all child nodes.
        string nodeNameChild;
        DOMNodeList* nodeListChild = curr->getChildNodes();
        // loop through the child nodes.
        for( unsigned int j = 0; j < nodeListChild->getLength(); j++ ){
            DOMNode* currChild = nodeListChild->item( j );
            nodeNameChild = XMLHelper<string>::safeTranscode( currChild->getNodeName() );

            if( nodeNameChild == "#text" ) {
                continue;
            }
            else if( nodeNameChild == "SectorName" ){
                sectorOrderList.push_back( XMLHelper<string>::getValue( currChild ) );
            }
            else {
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::WARNING );
                mainLog << "Unrecognized text string: " << nodeNameChild << " found while parsing region->SectorOrderList." << endl;
            }
        }
    }
#endif
    else {
        return false;
    }
    return true;
}


/*! Complete the initialization. Get the size of vectors, initialize AGLU,
*   create all markets, call complete initialization
*  functions for nested objects, update the fuel map, and find simultaneities.
* \param aGlobalTechDB Global Technology database.
* \todo I think since there is one indirect ghg object for each sector, it might
*       be better in sector. This may require deriving supply sector.
*/
void RegionMiniCAM::completeInit( const GlobalTechnologyDatabase* aGlobalTechDB ) {
    Region::completeInit( aGlobalTechDB );

    // Region info has no parent Info.
    mRegionInfo.reset( InfoFactory::constructInfo( 0 ) );
    mRegionInfo->setInteger( "rotationPeriod", mRotationPeriod );

    if( mInterestRate == 0 ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "No interest rate was read-in for region " << name << "." << endl;
    }

    // Add the interest rate to the region info.
    mRegionInfo->setDouble( "interest-rate", mInterestRate );

    // initialize demographic
    if( demographic.get() ){
        demographic->completeInit();
    }

    // Initialize the GDP
    if( gdp.get() ){
        gdp->initData( demographic.get() );
    }

    // Initialize the dependency finder.
    // This may need to move to init calc when markets change by period.
    DependencyFinder depFinder( scenario->getMarketplace(), name );
    for( SectorIterator sectorIter = supplySector.begin(); sectorIter != supplySector.end(); ++sectorIter ) {
        ( *sectorIter )->completeInit( mRegionInfo.get(), &depFinder, mLandAllocator.get(), aGlobalTechDB );
    }

    // Finish initializing agLu
    Configuration* conf = Configuration::getInstance();
    if( conf->getBool( "agSectorActive" ) && agSector.get() ){
        agSector->setGNP( calcFutureGDP() );
        if( demographic.get() ){
            agSector->setPop( demographic->getTotalPopVec() );
        }
        agSector->completeInit( name );
    }

    if ( mLandAllocator.get() ) {
        // This needs to be after completeInit for supply sectors is called because calibrated land use
        // need to be set by all production technologies (within their completeInit methods) that use land
        // before this function is called.
        mLandAllocator->completeInit( name, mRegionInfo.get() );
    }

    for( ResourceIterator resourceIter = resources.begin(); resourceIter != resources.end(); ++resourceIter ) {
        (*resourceIter)->completeInit( name, mRegionInfo.get() );
    }

    for( DemandSectorIterator demandSectorIter = demandSector.begin();
        demandSectorIter != demandSector.end(); ++demandSectorIter )
    {
        // Pass null in for the dependency finder so that the demand sector
        // technologies will not add their dependencies. This is because demand
        // sectors are not currently included in the ordering. Pass null for the land allocator
        // since demand sectors cannot allocate land.
        ( *demandSectorIter )->completeInit( mRegionInfo.get(), 0, 0, aGlobalTechDB );
    }

#if SORT_TESTING
    // Reorder sectors before sorting to test it.
    if( !sectorOrderList.empty() ){
        reorderSectors( sectorOrderList );
    }
#endif
    // Sort the sectors according to the order given by the dependency finder.

    // This needs to be at the end of completeInit so that all objects have
    // completed their calls to aDepFinder->addDependency. This should be moved
    // to RegionMiniCAM to avoid confusion.
    depFinder.createOrdering();
    reorderSectors( depFinder.getOrdering() );

    setupCalibrationMarkets();
}

/*! \brief Initialize the calibration markets.

* \todo Move GDP calibration parameters into GDP object
*/
void RegionMiniCAM::setupCalibrationMarkets() {
    if( !ensureGDP() ){
        return;
    }

    const Modeltime* modeltime = scenario->getModeltime();
    // Change per capita calibration values into absolute values
    for ( int period = 0; period < modeltime->getmaxper(); period++ ) {
        if ( GDPcalPerCapita [ period ] != 0 && demographic.get() ) {
            if ( calibrationGDPs[ period ] != 0 ) {
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::WARNING );
                mainLog << "Both GDPcal and GDPcalPerCapita read in region " << name << ". GDPcalPerCapita used." << endl;
            }
            // Convert from GDP/cap ($) to millions of dollars GDP total. Pop is in 1000's, so need an additional 1e3 scale to convert to millions
            // TODO: This will eventually write back out, so if this input file were used again the above warning
            // would be triggered.
            calibrationGDPs [ period ]  = GDPcalPerCapita [ period ] * demographic->getTotal( period )/1e3;
        }
    }

    if( Configuration::getInstance()->getBool( "CalibrationActive" ) ){
        gdp->setupCalibrationMarkets( name, calibrationGDPs );
    }
}

/*! \brief Reorder the sectors based on a list of sector names.
* \details This function is used to reorder a list of sectors using a supplied
*          ordering. In the partial-equilibrium model, the ordering of the
*          supply sectors is critical to the correct solving of the model. This
*          function handles errors as follows:
* <ul><li> If a sector is not specified in the list, the function will issue a
*          warning and remove the sector.</li>
*     <li> If a sector name in the ordering list is not an existing sector in
*          the model, a debugging warning will be issued and the sector will be
*          skipped. This can occur for resources and other non-sector
*          fuels.</li></ul>
* \param aOrderList A list of sector names in the order in which the sectors
*        should be put.
* \return Whether all sectors had orderings in the passed in order list.
*/
bool RegionMiniCAM::reorderSectors( const vector<string>& aOrderList ){
    // Check for an empty order list in which case the reordering should be
    // skipped. This occurs for SGM.
    if( aOrderList.empty() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Skipping sector reordering due to an empty order list. This is normal for SGM." << endl;
        return false;
    }

    // Create temporary copy of the sectors and the sector name map.
    vector<Sector*> originalOrder = supplySector;
    map<string,int> originalNameMap = supplySectorNameMap;

    // Clear the list of sectors and sectorNames.
    supplySector.clear();
    supplySectorNameMap.clear();

    // Get the dependency finder logger.
    ILogger& depFinderLog = ILogger::getLogger( "sector_dependencies" );

    // Loop through the sector order vector.
    typedef vector<string>::const_iterator NameIterator;
    typedef map<string,int>::iterator NameMapIterator;

    for( NameIterator currSectorName = aOrderList.begin(); currSectorName != aOrderList.end(); ++currSectorName ){
        NameMapIterator origSectorPosition = originalNameMap.find( *currSectorName );

        // Check if the sector name in the sector ordering list exists
        // currently.
        if( origSectorPosition != originalNameMap.end() ){
            // Assign the sector.
            supplySector.push_back( originalOrder[ origSectorPosition->second ] );
            supplySectorNameMap[ *currSectorName ] = static_cast<int>( supplySector.size() - 1 );

            // Remove the original sector mapping. This will allow us to clean
            // up sectors that were not assigned a new ordering. This also is
            // more efficient as there are less entries to search.
            originalNameMap.erase( origSectorPosition );
        }
        else {
            depFinderLog.setLevel( ILogger::DEBUG );
            depFinderLog << *currSectorName << " is not the name of an existing sector. "
                << "It will not be included in the sector ordering." << endl;
        } // end else
    } // end for.

    // Check if there are any unassigned sectors and remove them.
    for( NameMapIterator currSecName = originalNameMap.begin(); currSecName != originalNameMap.end(); ++currSecName ){
        ILogger& mainLog = ILogger::getLogger( "main_log");
        mainLog.setLevel( ILogger::ERROR );
        mainLog << currSecName->first << " was not assigned a position in the explicit sector ordering list." << endl
            << "This sector will be removed from the model." << endl;

        depFinderLog.setLevel( ILogger::ERROR );
        depFinderLog << currSecName->first << " was not assigned a position in the explicit sector ordering list." << endl
            << "This sector will be removed from the model." << endl;


        // This sector is not in the new list, so free its memory.
        delete originalOrder[ currSecName->second ];
    }
    // This was successful if the sector name map is empty, so that all sectors
    // were ordered.

    // Print out sector ordering for debugging purposes
    depFinderLog.setLevel( ILogger::DEBUG );
    depFinderLog << "SECTOR ORDERING FOR REGION " << name << endl;
    depFinderLog << "  ";
    for( NameIterator currSectorName = aOrderList.begin(); currSectorName != aOrderList.end(); ++currSectorName ){
        depFinderLog << *currSectorName << ", ";
    }
    depFinderLog << endl;

    return originalNameMap.empty();
}

void RegionMiniCAM::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    // Write out the Co2 Coefficients.
    for( map<string,double>::const_iterator coefAllIter = primaryFuelCO2Coef.begin(); coefAllIter != primaryFuelCO2Coef.end(); coefAllIter++ ) {
        XMLWriteElement( coefAllIter->second, "PrimaryFuelCO2Coef", out, tabs, 0, coefAllIter->first );
    }

    XMLWriteElementCheckDefault( heatingDegreeDays, "heatingDegreeDays", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( coolingDegreeDays, "coolingDegreeDays", out, tabs, 0.0 );
    XMLWriteVector( mLandUseCO2Emissions, "land-use-co2-emissions", out, tabs, scenario->getModeltime() );
    XMLWriteElementCheckDefault( mRotationPeriod, "rotationPeriod", out, tabs, 0 );
    XMLWriteElementCheckDefault( mInterestRate, "interest-rate", out, tabs, 0.0 );

    // write the xml for the class members.

    // write out data for land allocator
    if( mLandAllocator.get() ){
        mLandAllocator->toInputXML( out, tabs);
    }

    if( gdp.get() ){ // Check if gdp object exists
        gdp->toInputXML( out, tabs );
    }

    // write out the resources objects.
    for( CResourceIterator i = resources.begin(); i != resources.end(); i++ ){
        ( *i )->toInputXML( out, tabs );
    }

    // write out demand sector objects.
    for( CDemandSectorIterator k = demandSector.begin(); k != demandSector.end(); k++ ){
        ( *k )->toInputXML( out, tabs );
    }

    for( unsigned int i = 0; i < mAggEmissionsCalculators.size(); ++i ){
        mAggEmissionsCalculators[ i ]->toInputXML( out, tabs );
    }

    if( agSector.get() ){
        agSector->toInputXML( out, tabs );
    }
    else {
        tabs->writeTabs( out );
        out << "<agsector/>" << endl;
    }

    // Note: The count function is an STL algorithm that counts the number of
    // times a value occurs within the a range of a container. The first two
    // arguments to the function are the range of the container to search, the
    // third is the value to search for.
    if( ( count( calibrationGDPs.begin(), calibrationGDPs.end(), 0 )
        != static_cast<int>( calibrationGDPs.size() ) ) ){ // makes sure tags aren't printed if no real data

            // Write out regional economic data
            XMLWriteOpeningTag( "calibrationdata", out, tabs );

            // write out calibration GDP
            const Modeltime* modeltime = scenario->getModeltime();
            XMLWriteVector( calibrationGDPs, "GDPcal", out, tabs, modeltime, 0.0 );

            XMLWriteClosingTag( "calibrationdata", out, tabs );
            // End write out regional economic data
        } // close calibration IF
}

void RegionMiniCAM::toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const {
    // write out basic datamembers
    XMLWriteElement( heatingDegreeDays, "heatingDegreeDays", out, tabs );
    XMLWriteElement( coolingDegreeDays, "coolingDegreeDays", out, tabs );
    XMLWriteElement( mRotationPeriod, "rotationPeriod", out, tabs );
    XMLWriteElement( mInterestRate, "interest-rate", out, tabs );
    XMLWriteElement( static_cast<unsigned int>( resources.size() ), "numResources", out, tabs );
    XMLWriteElement( static_cast<unsigned int>( supplySector.size() ), "noSSec", out, tabs );
    XMLWriteElement( static_cast<unsigned int>( demandSector.size() ), "noDSec", out, tabs );
    XMLWriteElement( calibrationGDPs[ period ], "calibrationGDPs", out, tabs );
    XMLWriteElement( getEndUseServicePrice( period ), "priceSer", out, tabs );
    XMLWriteElement( mLandUseCO2Emissions[ period ], "land-use-co2-emissions", out, tabs );

    // Write out the Co2 Coefficients.
    for( map<string,double>::const_iterator coefAllIter = primaryFuelCO2Coef.begin(); coefAllIter != primaryFuelCO2Coef.end(); coefAllIter++ ) {
        XMLWriteElement( coefAllIter->second, "PrimaryFuelCO2Coef", out, tabs, 0, coefAllIter->first );
    }

    // write the xml for the class members.
    // write out the single population object.
    if( gdp.get() ){
        gdp->toDebugXML( period, out, tabs );
    }

    // Write out the land allocator.
    if ( mLandAllocator.get() ) {
        mLandAllocator->toDebugXML( period, out, tabs );
    }

    // write out the resources objects.
    for( CResourceIterator currResource = resources.begin(); currResource != resources.end(); ++currResource ){
        (*currResource)->toDebugXML( period, out, tabs );
    }

    // write out demand sector objects.
    for( CDemandSectorIterator currSector = demandSector.begin(); currSector != demandSector.end(); ++currSector ){
        (*currSector)->toDebugXML( period, out, tabs );
    }

    // Write out the single agSector object.
    // agSector->toDebugXML( period, out );

    for( unsigned int i = 0; i < mAggEmissionsCalculators.size(); ++i ){
        mAggEmissionsCalculators[ i ]->toDebugXML( period, out, tabs );
    }
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. This function may be virtual to be overridden by derived class
* pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& RegionMiniCAM::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. The "==" operator that is used when parsing, required this second
* function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const std::string& RegionMiniCAM::getXMLNameStatic() {
    // TODO: Change to region-minicam, this will require changing input files
    static const string XML_NAME = "region";
    return XML_NAME;
}

//! Calculate the region.
void RegionMiniCAM::calc( const int period, const bool doCalibrations ) {
    // Store configuration variables locally as statics.
    static const Configuration* conf = Configuration::getInstance();
    static const bool agSectorActive = conf->getBool( "agSectorActive" );
    static const bool calibrationActive = conf->getBool( "CalibrationActive" );

    // Write back calibrated values to the member variables.
    // These are still trial values.
    if( calibrationActive && gdp.get() ) {
        gdp->writeBackCalibratedValues( name, period );
    }
    // calculate regional GDP
    calcGDP( period );

    // determine prices for all supply sectors (e.g., refined fuels, electricity, etc.)
    calcFinalSupplyPrice( period );
    
    // Add land use CO2 emissions to the marketplace.
    Marketplace* marketplace = scenario->getMarketplace();

    const double GT_TO_MMT = 1000;
    marketplace->addToDemand( "CO2", name, mLandUseCO2Emissions[ period ] * GT_TO_MMT, period, false );

    // TODO: Remove this for separate demand and supply sectors.
    for( DemandSectorIterator currSector = demandSector.begin(); currSector != demandSector.end(); ++currSector ){
        (*currSector)->calcCosts( period );
    }

    // adjust GDP for energy cost changes
    adjustGDP( period );

    // determine end-use demand for energy and other goods
    calcEndUseDemand( period );

    // determine supply of final energy and other goods based on demand
    setFinalSupply( period );

    // determine supply of primary resources
    calcResourceSupply( period );

    if( agSectorActive && agSector.get() ){
        calcAgSector( period );
    }

    // Perform calibrations
    if( calibrationActive ) {
        calibrateRegion( doCalibrations, period );
    }
}

/*! Run the agLu Model and determine CO2 emitted.
* \param period Model time period
*/
void RegionMiniCAM::calcAgSector( const int period ) {
    agSector->runModel( period, name );
    agSector->carbLand( period, name );
}

/*! \brief Set regional ghg constraint from input data to market supply.
* \param period Model time period
*/
void RegionMiniCAM::calcResourceSupply( const int period ){
    if( !ensureGDP() ){
        return;
    }

    for( ResourceIterator currResource = resources.begin(); currResource != resources.end(); ++currResource ){
        (*currResource)->calcSupply( name, gdp.get(), period );
    }
}

/*!
 * \brief Calculate prices of all sectors.
 * \todo Move this functionality into sector.
 * \param period Model time period
 */
void RegionMiniCAM::calcFinalSupplyPrice( const int period ) {
    if( !ensureGDP() ){
        return;
    }

    for( SectorIterator currSupply = supplySector.begin(); currSupply != supplySector.end(); ++currSupply ){
        (*currSupply)->calcFinalSupplyPrice( gdp.get(), period );
    }

    if ( mLandAllocator.get() ) {
        mLandAllocator->calcFinalLandAllocation( name, period );
    }
}

/*! Calculates supply of final energy and other goods.
* \todo Move functionality into sector
* \param period Model time period
*/
void RegionMiniCAM::setFinalSupply( const int period ) {
    if( !ensureGDP() ){
        return;
    }

    // loop through all sectors in reverse once to get total output.
    typedef vector<Sector*>::reverse_iterator ReverseSectorIterator;
    for ( ReverseSectorIterator currSupply = supplySector.rbegin(); currSupply != supplySector.rend(); ++currSupply ) {
        (*currSupply)->supply( gdp.get(), period );
    }
}

/*! Calculate initial gdp value (without feedbacks)
*
* \param period Model time period
*/
void RegionMiniCAM::calcGDP( const int period ){
    if( !ensureGDP() || !ensureDemographics() ){
        return;
    }
    gdp->initialGDPcalc( period, demographic->getTotal( period ) );
}

/*! \brief Calculate forward-looking gdp (without feedbacks) for AgLU use.
* \details It is necessary to have a gdp without feedbacks so that all values
*          are known and AgLU can calibrate. This routine runs through each
*          period and calculates a series of gdp values without use of the
*          energy price feedback.
* \author Steve Smith, Josh Lurz
* \warning This will interfere with the normal gdp calculation if this is used
*          after model calc starts.
* \todo check to see if this works with AgLU. Not sure about conversions.
*/
const vector<double> RegionMiniCAM::calcFutureGDP() const {
    if( !ensureGDP() || !ensureDemographics() ){
        return vector<double>( 0 );
    }

    const Modeltime* modeltime = scenario->getModeltime();
    vector<double> gdps( modeltime->getmaxper() );

    for ( int period = 0; period < modeltime->getmaxper(); period++ ) {
        gdp->initialGDPcalc( period, demographic->getTotal( period ) );
        gdps[ period ] = gdp->getApproxScaledGDPperCap( period );
    }
    return gdps;
}

double RegionMiniCAM::getEndUseServicePrice( const int period ) const {
	double servicePrice = 0;
    for ( CDemandSectorIterator currDemSector = demandSector.begin(); currDemSector != demandSector.end(); ++currDemSector ) {
        servicePrice += (*currDemSector)->getWeightedEnergyPrice( gdp.get(), period );
    }
	return servicePrice;
}

/*! Adjust regional gdp for energy.
*
* \param period Model time period
* \todo Move this calculation down to GDP
*/
void RegionMiniCAM::adjustGDP( const int period ){
    if( !ensureGDP() ){
        return;
    }

    const Modeltime* modeltime = scenario->getModeltime();

    double tempratio = 1;
    if ( period > modeltime->getFinalCalibrationPeriod() ){
        tempratio = getEndUseServicePrice( period ) / getEndUseServicePrice( period - 1 );
    }
    gdp->adjustGDP( period, tempratio );
}

/*! \brief Perform regional calibration.
* \details Must be done after demands are calculated. Two levels of calibration
*          are possible. First at the sector or technology level (via.
*          calibrateSector method), or at the level of total final energy demand
*          (via calibrateTFE).
* \param doCalibrations Boolean for running or not running calibration routine
* \param period Model time period
*/
void RegionMiniCAM::calibrateRegion( const bool doCalibrations, const int period ) {
    // Do subsector and technology level energy calibration
    // can only turn off calibrations that do not involve markets
    if ( doCalibrations ) {
        // Calibrate demand sectors
        for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
            demandSector[ i ]->calibrateSector( gdp.get(), period );
        }

        // Calibrate supply sectors
        for ( unsigned int i = 0; i < supplySector.size(); i++ ) {
            supplySector[ i ]->calibrateSector( gdp.get(), period );
        }
    }
    if( !ensureGDP() ){
        return;
    }
    // TODO: Move this into GDP object.
    if( calibrationGDPs[ period ] > 0 ){
        const string goodName = "GDP";
        Marketplace* marketplace = scenario->getMarketplace();
        marketplace->addToSupply( goodName, name, gdp->getGDP( period ), period );
    }
}

/*! \brief Test to see if calibration worked for all sectors in this region
*
* Compares the sum of calibrated + fixed values to output of each sector.
*
* If calibrations are not on then will only printout out diagnostics (if
*
* \author Steve Smith
* \param period Model period
* \param calAccuracy value to which calibrations must match in order to pass
*        calibration test.
* \param printWarnings flag to turn on logging of warnings if calibrations are
*        not accurate.
* \return Boolean true if calibration is ok.
*/
bool RegionMiniCAM::isAllCalibrated( const int period, double calAccuracy, const bool printWarnings ) const {
    const static bool calOn = Configuration::getInstance()->getBool( "CalibrationActive" );
    
    // Don't check calibration in the base period or if calibration is off.
    if( !calOn || period == 0 ){
        return true;
    }

    bool returnVal = true;
    for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
        if ( !demandSector[ i ]->isAllCalibrated( period, calAccuracy, printWarnings ) ) {
            returnVal = false;
        }
    }

    for ( unsigned int i = 0; i < supplySector.size(); i++ ) {
        if ( !supplySector[ i ]->isAllCalibrated( period, calAccuracy, printWarnings ) ) {
            returnVal = false;
        }
    }

    // always return true if calibrations are not on.
    return returnVal;
}

/*! \brief Call any initializations that are only done once per period.
* \author Steve Smith, Josh Lurz, Sonny Kim
* \param period Model period
* \todo Once postCalc is present, add a check to verify that aggregate emissions worked properly
*/
void RegionMiniCAM::initCalc( const int period )
{

    // Set aggregate emissions factor to region info if needed
    for ( unsigned int i = 0; i < mAggEmissionsCalculators.size(); i++ ) {
        mAggEmissionsCalculators[ i ]->setAggregateEmissionFactor( name,
                                                                   supplySector,
                                                                   mRegionInfo.get(),
                                                                   period );
    }

    mRegionInfo->setDouble( "heatingDegreeDays", heatingDegreeDays );
    mRegionInfo->setDouble( "coolingDegreeDays", coolingDegreeDays );

    // Set the CO2 coefficients into the Marketplace before the Technologies and
    // GHGs are initialized so they can be accessed.
    setCO2CoefsIntoMarketplace( period );

    for( SectorIterator currSector = supplySector.begin(); currSector != supplySector.end(); ++currSector ){
        (*currSector)->initCalc( /*nationalAccount[ period ]*/0, demographic.get(), period );
    }
    for ( DemandSectorIterator currSector = demandSector.begin(); currSector != demandSector.end(); ++currSector ) {
        (*currSector)->initCalc( /*nationalAccount[ period ]*/0, demographic.get(), period  );
    }
    for( ResourceIterator currResource = resources.begin();
        currResource != resources.end(); ++currResource )
    {
            (*currResource)->initCalc( name, period );
    }
    for( ResourceIterator currResource = resources.begin(); currResource != resources.end(); ++currResource ){
        (*currResource)->initCalc( name, period );
    }
}

/*
* \brief Initialize the CO2 coefficients read in by the Region into the
*        marketplace.
* \details In each period the Region must set the CO2 coefficients for all goods
*          into the Marketplace, so that Technologies and GHGs can access them.
* \param aPeriod Period.
*/
void RegionMiniCAM::setCO2CoefsIntoMarketplace( const int aPeriod ){
    const static string CO2COEF = "CO2Coef";
    Marketplace* marketplace = scenario->getMarketplace();
    for( map<string, double>::const_iterator coef = primaryFuelCO2Coef.begin();
        coef != primaryFuelCO2Coef.end(); ++coef )
    {
        // Markets may not exist for incorrect fuel names.
        IInfo* fuelInfo = marketplace->getMarketInfo( coef->first, name, aPeriod, false );
        if( fuelInfo ){
            fuelInfo->setDouble( CO2COEF, coef->second );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::DEBUG );
            mainLog << "Cannot set emissions factor of zero for fuel " << coef->first
                    << " because the name does not match the name of a market." << endl;
        }
    }
}


/*!
 * \brief Call any calculations that are only done once per period after
 *        solution is found.
 * \author Sonny Kim
 * \param aPeriod Model period
 */
void RegionMiniCAM::postCalc( const int aPeriod ) {
    Region::postCalc( aPeriod );
    
    // Demand sectors
    for( DemandSectorIterator currSector = demandSector.begin(); currSector != demandSector.end(); ++currSector ){
        (*currSector)->postCalc( aPeriod );
    }

    // Post calculation for resource sectors
    for( ResourceIterator currResource = resources.begin(); currResource != resources.end(); ++currResource ){
        (*currResource)->postCalc( name, aPeriod );
     }
 }

/*! \brief Adjusts calibrated demands to be consistent with calibrated supply.
*
* The result of this routine, once called for all regions, is a set of calSupply
* and calDemand values in marketInfo. The calSupply variable has a non-negative
* value for each supply sector that is completely calibrated. Similarly, the
* calDemand variable has a non-negative value for each demand good that is
* completely calibrated. Only those markets where all supplies or demands are
* calibrated will have non-zero values for these variables.
*
* \author Steve Smith
* \param period Model period
*/

void RegionMiniCAM::setCalSuppliesAndDemands( const int period ) {
    // Check for fully calibrated/fixed supplies. This will search sectors and
    // resources for calibrated and fixed supples.
    CalQuantityTabulator calSupplyTabulator( "" );
    accept( &calSupplyTabulator, period );

    Marketplace* marketplace = scenario->getMarketplace();

    // Flag that the market is not all fixed.
    const double MARKET_NOT_ALL_FIXED = -1;

    // Name of the cal supply info string.
    const string CAL_SUPPLY_NAME = "calSupply";

    // Iterate over the entire map of goods.
    const CalQuantityTabulator::CalInfoMap& calSupplies = calSupplyTabulator.getSupplyInfo();

    for( CalQuantityTabulator::CalInfoMap::const_iterator iter = calSupplies.begin();
         iter != calSupplies.end(); ++iter )
    {
        // Get the market info for the good.
        // TODO: Require market info to exist once demand sectors are separated.
        IInfo* marketInfo = marketplace->getMarketInfo( iter->first, name, period, false );

        // Incorrect input files may cause the market to not exist. This will be
        // flagged elsewhere.
        if( !marketInfo ){
            continue;
        }

        double existingCalSupply = marketInfo->getDouble( CAL_SUPPLY_NAME, false );
        // Check is to see if some other region has flagged this as having all
        // inputs not fixed.
        if ( existingCalSupply != MARKET_NOT_ALL_FIXED && iter->second.mAllFixed ) {
            // If supply of this good has not been elimiated from the search and
            // output is fixed then add to fixed supply value.
            marketInfo->setDouble( CAL_SUPPLY_NAME, max( existingCalSupply, 0.0 )
                                   + iter->second.mCalQuantity + iter->second.mFixedQuantity );
        }
        else {
            // If supply of this good is not fixed then set flag to eliminate
            // this from other searches
            marketInfo->setDouble( CAL_SUPPLY_NAME, MARKET_NOT_ALL_FIXED );
        }
    }

    // Check for fully calibrated/fixed demands. Search through all supply
    // technologies
    for ( unsigned int i = 0; i < supplySector.size(); i++ ) {
        supplySector[ i ]->tabulateFixedDemands( period, gdp.get() );
    }

    // Check for fully calibrated/fixed demands. Search through all demand
    // technologies
    for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
        demandSector[ i ]->tabulateFixedDemands( period, gdp.get() );
    }
}

/*! \brief Initializes calDemand and calSupply values for all sectors
*
* All calSupply and calDemand values are reset to -1 This erases any information
* from a prior period and eliminates warning messages for value not found in
* information store
*
* \author Steve Smith
* \param period Model period
*/
void RegionMiniCAM::initializeCalValues( const int period ) {
    Marketplace* marketplace = scenario->getMarketplace();
    // This will get set to -1 if this market is not fixed and a value >= 0
    // otherwise.
    const double DEFAULT_CAL_VALUE = -2;

    typedef list<string>::const_iterator CI;
    for ( unsigned int i = 0; i < supplySector.size(); i++ ) {
        // Set default values for supply sector
        IInfo* marketInfo = marketplace->getMarketInfo( supplySector[ i ]->getName(), name, period, true );
        marketInfo->setDouble( "calSupply", DEFAULT_CAL_VALUE );
        marketInfo->setDouble( "calDemand", DEFAULT_CAL_VALUE );
    }

    for ( unsigned int i = 0; i < resources.size(); i++ ) {
        // Now prepare to loop through all the fuels used by this sector
        IInfo* resourceMarketInfo = marketplace->getMarketInfo( resources[ i ]->getName(), name, period, true );
        resourceMarketInfo->setDouble( "calSupply", DEFAULT_CAL_VALUE );
        resourceMarketInfo->setDouble( "calDemand", DEFAULT_CAL_VALUE );
    }
}

/*! \brief Sets input values for transitive dependancies
*
* If a supply sector has a fixed demand as an output then calculate the
* corresponding input. For every supply sector with a fixed demand (e.g.
* elec_T&D_bld), this routine checks each fuel used by that sector. If there is
* only one fuel that does not have a fixed demand (e.g. electricity) then the
* fixed demand for the sector is translated to a demand for the given fuel.
*
* Routine is repatedly called from world until all potential depndencies have
* been found. When any transitive dependency is found another iteration must be
* done (for all regions) to determine if this new fixed demand feeds into
* another sector.
*
* \author Steve Smith
* \param period Model period
*/
bool RegionMiniCAM::setImpliedCalInputs( const int period ) {
    Marketplace* marketplace = scenario->getMarketplace();
    const double MKT_NOT_ALL_FIXED = -1;

    bool calcIsDone = true;

    // Instantiate the relationship map here if it does not already exist
    if ( !fuelRelationshipMap.get() ) {
        // Clear relationship map for this iteration
        fuelRelationshipMap.reset( new FuelRelationshipMap() );
    }

    // For each sector, check if a fixed demand is an output. If supply is fixed
    // stop. Make sure all inputs are fixed demands
    for ( unsigned int i = 0; i < supplySector.size(); i++ ) {
		IInfo* sectorMarketInfo =
            marketplace->getMarketInfo( supplySector[ i ]->getName(), name,
                                        period, true );
        double sectorCalDemand = sectorMarketInfo->getDouble( "calDemand", false );
        double sectorCalSupply = sectorMarketInfo->getDouble( "calSupply", false );

        // Check to see if this sector supplies a fuel with a fixed demand. If
        // so, and this is not a fuel that already has a fixed supply, then
        // calculate the cooresponding fixed supply
        if ( sectorCalDemand >= 0 && sectorCalSupply < 0 ) {

            // Now prepare to loop through all the fuels used by this sector.
            // Get fuel consumption map for sector.
            InputFinder inputFinder;
            supplySector[ i ]->accept( &inputFinder, period );
            const list<string>& inputsUsed = inputFinder.getInputs();
            // Total directly calibrated outputs (not outputs infered from
            // market)
            double totalCalOutputs = 0;

            typedef list<string>::const_iterator CI;
            stack<string> adjustableInputs;
            for( CI currInput = inputsUsed.begin(); currInput != inputsUsed.end(); ++currInput ){

                // If inputs are all fixed for this fuel
                if ( supplySector[ i ]->inputsAllFixed( period, *currInput ) ) {
                    totalCalOutputs += supplySector[ i ]->getCalAndFixedOutputs( period, *currInput );
                }
                // else have an input that can be adjusted
                else {
                    adjustableInputs.push( *currInput );
                }
            }

            // If we found one, and only one, unfixed input then figure out what
            // this should be and set things appropriately
            if ( adjustableInputs.size() == 1 ) {
                // Calculate required input (which will be set to the
                // marketplace)
                double requiredOutput = sectorCalDemand;
                assert( requiredOutput >= sectorCalDemand );

                // subtract other calOutputs so that we only calculate the
                // output from the remaining fuel
                requiredOutput -= totalCalOutputs;

                ILogger& dependenciesLog = ILogger::getLogger( "sector_dependencies" );
                dependenciesLog.setLevel( ILogger::NOTICE );
                dependenciesLog << "  Transitive demand for "
                                << adjustableInputs.top()  
                                << " in sector " << supplySector[ i ]->getName()
                                << " in region " << name << " added with value "
                                << requiredOutput << endl; 

                supplySector[ i ]->setImpliedFixedInput( period,
                                                         adjustableInputs.top(),
                                                         requiredOutput );

                // Now that transitive demand has been set, clear demand for
                // this good so this won't be done again
                sectorMarketInfo->setDouble( "calDemand", MKT_NOT_ALL_FIXED );

                // Save this relationship information. First element of vector is
                // the root, or current root of chain and second element is a
                // vector of dependents

                vector<string> tempDependents;

                // Set up fuel relationship map For each fuel in the first (key)
                // position, the map lists every fuel that is derived, directly
                // or indirectly, on that fuel At this point in the routine, we
                // have found that a sector sectorName which has a fixed input,
                // has newFixedDemandInputName as an input So we need to add
                // this relationship to the fuelRelationshipMap. If the current
                // sectorName is present as a key, then it should be moved to
                // the dependents vector with the new fuel name as the key.
                if ( fuelRelationshipMap->find( supplySector[ i ]->getName() ) != fuelRelationshipMap->end() ) {
                    tempDependents = (*fuelRelationshipMap)[ supplySector[ i ]->getName() ];
                    fuelRelationshipMap->erase( supplySector[ i ]->getName() );
                 } 
                // If proper key is already there, then copy dependent list (the
                // new fuelname will be added to it below)
                else if ( fuelRelationshipMap->find( adjustableInputs.top() ) != fuelRelationshipMap->end() ) {
                    tempDependents = (*fuelRelationshipMap)[ adjustableInputs.top() ];
                }
                // In any event, current sector should be added as a key (this
                // overwrites previous dependents list if it existed).
                tempDependents.push_back( supplySector[ i ]->getName() );
                (*fuelRelationshipMap)[ adjustableInputs.top() ] = tempDependents;

                // If this demand does not correspond to a sector with a fixed
                // supply then we are not done with checking. Otherwise we are
                // done for this region if no sectors satisfy this criteria.
                const IInfo* demandMarketInfo = marketplace->getMarketInfo( adjustableInputs.top(), name, period, false );
                if ( demandMarketInfo && demandMarketInfo->getDouble( "calSupply", true ) < 0 ) {
                    calcIsDone = false;
                }
            }
            else if ( adjustableInputs.size() > 1 ) {
                // Print a warning, but only if calDemand is not zero (if
                // calDemand is zero then this is probably not a problem)
                if ( sectorCalDemand > 0 ) {
                    ILogger& dependenciesLog = ILogger::getLogger( "sector_dependencies" );
                    dependenciesLog.setLevel( ILogger::NOTICE );
                    dependenciesLog << "Couldn't calculate transitive demand for inputs ";

                    // TODO: This seems wrong. If there are multiple unfixed
                    // inputs why only reset the first?
                    string firstInput = adjustableInputs.top();
                    while( !adjustableInputs.empty() ){
                        dependenciesLog << adjustableInputs.top() << ", ";
                        adjustableInputs.pop();
                    }
                    
                    dependenciesLog << " for sector "
                                    << supplySector[ i ]->getName()
                                    << " in region " << name << endl;

                    // If can't calculate this demand then reset so that nothing
                    // is scaled.
                    IInfo* demandMarketInfo = marketplace->getMarketInfo( firstInput, name, period, false );
                    if( demandMarketInfo ){
                        demandMarketInfo->setDouble( "calDemand", MKT_NOT_ALL_FIXED );
                    }
                }
                else {
                    ILogger& dependenciesLog = ILogger::getLogger( "sector_dependencies" );
                    dependenciesLog.setLevel( ILogger::NOTICE );
                    dependenciesLog << "Couldn't calculate transitive demand for input " << adjustableInputs.top()
                                    << " but calibrated demand is zero. " << endl;
                }
            }
            else {
                ILogger& dependenciesLog = ILogger::getLogger( "sector_dependencies" );
                dependenciesLog.setLevel( ILogger::NOTICE );
                dependenciesLog << "Zero fixed inputs for " << supplySector[ i ]->getName() << endl;
            }
        } // End of fixed demand transitive calculation loop.
    } // End of sector loop

    return calcIsDone;
}

/*! \brief Scales input calibration values for fuels so that calibrated supplies
*          and demands are equal.
*
* Checks for all demands and scales calibration input values if necessary to
* assure that calibrated supplies and demands are identical, including
* transitive dependencies.
*
* \todo Use something other than the fuelmap (needs to be implemented) to find
*       what fuels a region/sector uses.
* \author Steve Smith
* \param period Model period
* \return Number of values scaled.
*/
int RegionMiniCAM::scaleCalInputs( const int period ) {
    Marketplace* marketplace = scenario->getMarketplace();
    Configuration* conf = Configuration::getInstance();

    if ( !fuelRelationshipMap->empty() ) {
        ILogger& dependenciesLog = ILogger::getLogger( "sector_dependencies" );
        dependenciesLog << endl << name << " regional dependencies as used for calibration input/output adjustments. Period " 
			            << period << endl << "These are direct and transitive dependencies only for sectors that potentially have calibration adjustments." << endl;
        
		typedef map<string, vector<string> >::const_iterator DepIterator;
        for( DepIterator fuelDependencyIter = fuelRelationshipMap->begin(); fuelDependencyIter != fuelRelationshipMap->end(); ++fuelDependencyIter ){
            dependenciesLog << fuelDependencyIter->first << " - ";
            for ( unsigned int i = 0; i < fuelDependencyIter->second.size(); i ++ ) {
                dependenciesLog << fuelDependencyIter->second[ i ] << ":";
            }
            dependenciesLog << endl;
        }
    }

    // For each demand that needs to be scaled, loop through all supply and
    // demand sectors and scale demand. To find this out, construct a regional
    // fuel map and then loop through each value and check for calSupply &
    // demand The end... If supply is fixed stop. Make sure all inputs are fixed
    // demands.
    int numberOfScaledValues = 0;

    InputFinder inputFinder;
    accept( &inputFinder, 0 );
    const list<string>& inputsUsed = inputFinder.getInputs();
    typedef list<string>::const_iterator CI;

    // Loop through each fuel used in this region
    for( CI currInput = inputsUsed.begin(); currInput != inputsUsed.end(); ++currInput ){
        const IInfo* fuelMarketInfo = marketplace->getMarketInfo( *currInput, name, period, false );
        if ( fuelMarketInfo ) {
                //  Determine if inputs for this fuel need to be scaled. Get total cal + fixed supply and demand values.
                double calSupplyValue = fuelMarketInfo->getDouble( "calSupply", false );
                double calDemandValue = fuelMarketInfo->getDouble( "calDemand", false );

                // If these are positive then may need to scale calInputs for this input
                if ( ( calSupplyValue > 0 ) && ( calDemandValue > 0 ) ) {
                    // If these are still positive then scale calInputValues for this input
                    if ( ( calSupplyValue > 0 ) && ( calDemandValue > 0 ) ) {
                        double scaleValue = calSupplyValue / calDemandValue;

                        if ( abs( 1 - scaleValue ) > util::getVerySmallNumber() ) {
                            ILogger& mainLog = ILogger::getLogger( "main_log" );
                            mainLog.setLevel( ILogger::DEBUG );
                            mainLog << "Scaling cal values by " <<  scaleValue << " for fuels ";
                            ILogger& calibrationLog = ILogger::getLogger( "calibration_log" );
                            calibrationLog.setLevel( ILogger::DEBUG );
                            calibrationLog << "Scaling cal values by " <<  scaleValue << " for fuels ";

                            // Repeat for all fuels derived from this one
                            vector<string> dependentFuels = (*fuelRelationshipMap)[ *currInput ];

                            // Note loop goes to 1 + number of fuels so that basefuel is covered.
                            for ( unsigned int i = 0; i <= dependentFuels.size(); ++i) {
                                ++numberOfScaledValues;

                                string fuelName;
                                if ( i == dependentFuels.size() ) {
                                    fuelName = *currInput; // Finish with base fuel in case that is used directly
                                } else {
                                    fuelName = dependentFuels [ i ];
                                }

                                mainLog << fuelName << ", ";
                                calibrationLog  << fuelName <<", ";
                                for ( unsigned int j = 0; j < demandSector.size(); j++ ) {
                                    demandSector[ j ]->scaleCalibratedValues( period, fuelName, scaleValue );
                                }
                                for ( unsigned int j = 0; j < supplySector.size(); j++ ) {
                                    supplySector[ j ]->scaleCalibratedValues( period, fuelName, scaleValue );
                                }
                            } // dependentFuels loop
                            mainLog << " in region " << name << endl;
                            calibrationLog << " in region " << name << endl;
                        }
                    } // end scaling loop
                    // If these were less than zero, then something was wrong with the input values
                    else if ( ( calSupplyValue < 0 ) || ( calDemandValue < 0 ) )  {
                        ILogger& mainLog = ILogger::getLogger( "main_log" );
                        mainLog.setLevel( ILogger::ERROR );
                        mainLog << "Input calibration and fixed values not consistent. Adjusted value < 0 for fuel " << *currInput << " in region " << name << endl ;
                        ILogger& calibrationLog = ILogger::getLogger( "calibration_log" );
                        calibrationLog.setLevel( ILogger::ERROR );
                        calibrationLog << "Input calibration and fixed values not consistent. Adjusted value < 0 for fuel " << *currInput << " in region " << name << endl ;
                        calibrationLog.setLevel( ILogger::DEBUG );
                        calibrationLog << "    calSupplyValue - fixedDemands: " << calSupplyValue << endl ;
                        calibrationLog << "    calSupplyValue - fixedDemands: " << calSupplyValue << endl ;
                    }
                 }
            } // end zTotal check loop
    } // end fuelCons loop

    // This is not needed anymore so release this memory
    fuelRelationshipMap.release();

    return numberOfScaledValues;
}

//! Calculate regional demand for energy and other goods for all sectors.
void RegionMiniCAM::calcEndUseDemand( const int period ) {
    if( !ensureGDP() ){
        return;
    }

    for ( DemandSectorIterator currDemSector = demandSector.begin(); currDemSector != demandSector.end(); ++currDemSector ){
        // calculate aggregate demand for end-use sector services
        // set fuel demand from aggregate demand for services
        (*currDemSector)->calcAggregateDemand( gdp.get(), demographic.get(), period );
    }

}

//! Calculate regional emissions from resources.
void RegionMiniCAM::calcEmissions( const int period ) {
    summary[period].clearemiss(); // clear emissions map

    // need to call emissions function but sum is not needed
    for ( unsigned int i = 0; i < supplySector.size(); i++ ) {
        supplySector[i]->emission(period);
        summary[period].updateemiss(supplySector[i]->getemission(period));
    }
    for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
        demandSector[i]->emission(period);
        summary[period].updateemiss(demandSector[i]->getemission(period));
    }

    // WARNING: These emissions are not currently added to emissions by fuel, so
    // the totals will not be equal.
    const double GT_TO_MMT = 1000;
    map<string, double> landUseSummary;
    if( mLandUseCO2Emissions[ period ].isInited() ){
        landUseSummary[ "CO2" ] = mLandUseCO2Emissions[ period ] * GT_TO_MMT;
        landUseSummary[ "CO2-net-terrestrial" ] = mLandUseCO2Emissions[ period ] * GT_TO_MMT;
        summary[period].updateemiss( landUseSummary );
    }

    // Determine land use change emissions and add to the summary.
    EmissionsSummer co2LandUseSummer( "CO2NetLandUse" );
    accept( &co2LandUseSummer, period );
    map<string,double> agEmissions;
    agEmissions[ "CO2NetLandUse" ] = co2LandUseSummer.getEmissions( period );
    summary[ period ].updateemiss( agEmissions );
}

/*! \brief Calculate regional emissions by fuel for reporting.
* \warning This function assumes emission has already been called, as this
*          function cannot clear the summary emissions.
* \param The global list of primary fuels.
* \param period The model period.
*/
void RegionMiniCAM::calcEmissFuel( const list<string>& aPrimaryFuelList, const int period )
{
    map<string, double> fuelemiss; // tempory emissions by fuel

    for( list<string>::const_iterator fuelIter = aPrimaryFuelList.begin();
        fuelIter != aPrimaryFuelList.end(); ++fuelIter )
    {
        double primaryCoef = objects::searchForValue( primaryFuelCO2Coef, *fuelIter );
        fuelemiss[ *fuelIter ] = summary[period].get_pemap_second( *fuelIter )
                                 * util::searchForValue( primaryFuelCO2Coef, *fuelIter );
    }

    summary[period].updateemiss(fuelemiss); // add CO2 emissions by fuel
}

//! Write all outputs to file.
void RegionMiniCAM::csvOutputFile() const {
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    // function protocol
    void fileoutput3(string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);

    // write population results to database
    if( demographic.get() ){
        demographic->csvOutputFile( name );
    }
    if( gdp.get() ){
        gdp->csvOutputFile( name );
    }

    // write total emissions for region
    for (int m= 0;m<maxper;m++) {
        temp[m] = summary[m].get_emissmap_second("CO2");
    }
    fileoutput3(name," "," "," ","CO2 emiss","MTC",temp);

    // write ag emissions for region
    for (int m= 0;m<maxper;m++) {
        temp[m] = summary[m].get_emissmap_second( "CO2NetLandUse" );
    }
    fileoutput3(name," "," "," ","Net Land Use CO2 emiss","MTC",temp);

    if ( mLandAllocator.get() ) {
        mLandAllocator->csvOutput( name );
    }

    // region primary energy consumption by fuel type
    typedef map<string,double>:: const_iterator CI;
    map<string,double> tpemap = summary[0].getpecons();
    for (CI pmap=tpemap.begin(); pmap!=tpemap.end(); ++pmap) {
        for (int m= 0;m<maxper;m++) {
            temp[m] = summary[m].get_pemap_second(pmap->first);
        }
        fileoutput3(name,"Pri Energy","Consumption","",pmap->first,"EJ",temp);
    }


    // regional Pri Energy Production Total
    for (int m= 0;m<maxper;m++) {
        temp[m] = summary[m].get_peprodmap_second("zTotal");
    }
    fileoutput3(name,"Pri Energy","total","","zTotal","EJ",temp);

    // Calculate indirect emissions so they can be written to the database.
    IndirectEmissionsCalculator indEmissCalc;
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        accept( &indEmissCalc, i );
    }

    // write resource results to file
    for ( unsigned int i = 0; i < resources.size(); i++ )
        resources[i]->csvOutputFile( name );

    // write supply sector results to file
    for ( unsigned int i = 0; i < supplySector.size(); i++ ) {
        supplySector[i]->csvOutputFile( gdp.get(), &indEmissCalc );
    }

    // write end-use sector demand results to file
    for ( unsigned int i = 0; i < demandSector.size(); i++ ){
        demandSector[i]->csvOutputFile( gdp.get(), &indEmissCalc );
    }
}

//! Write MiniCAM style outputs to file.
void RegionMiniCAM::dbOutput( const list<string>& aPrimaryFuelList ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper),temptot(maxper);
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);

    // write population results to database
    if( demographic.get() ){
        demographic->dbOutput( name );
    }
    if( gdp.get() ){
        gdp->dbOutput( name );
    }
    if ( mLandAllocator.get() ) {
        mLandAllocator->dbOutput( name );
    }

    // CO2 emissions by fuel
    for( list<string>::const_iterator fuelIter = aPrimaryFuelList.begin();
        fuelIter != aPrimaryFuelList.end(); fuelIter++ )
    {
        for ( int m= 0;m<maxper;m++) {
            temp[m] = summary[m].get_emissfuelmap_second( *fuelIter );
            temptot[m] += temp[m];
        }
        dboutput4(name,"CO2 Emiss","by Fuel",*fuelIter,"MTC",temp);
    }
    // add amount of geologic sequestration to emissions by fuel
    // todo change hardcoded category name
    for ( int m= 0;m<maxper;m++) {
        // note the negative value for sequestered amount
        temp[m] = - summary[m].get_emissmap_second( "CO2sequestGeologic" );
        temptot[m] += temp[m];
    }
    dboutput4(name,"CO2 Emiss","by Fuel","geologic sequestration","MTC",temp);

    // add amount of sequestration from non-energy use to emissions by fuel
    // todo change hardcoded category name
    for ( int m= 0;m<maxper;m++) {
        // note the negative value for sequestered amount
        temp[m] = - summary[m].get_emissmap_second( "CO2sequestNonEngy" );
        temptot[m] += temp[m];
    }
    dboutput4(name,"CO2 Emiss","by Fuel","non-energy use","MTC",temp);

    // total emissions by sector for region
    for ( int m= 0;m<maxper;m++) {
        temp[m] = summary[m].get_emissmap_second("CO2");
    }
    // CO2 emissions by fuel and sector totals use same value
    dboutput4(name,"CO2 Emiss","by Fuel","zTotal","MTC",temptot);
    dboutput4(name,"CO2 Emiss","by Sector","zTotal","MTC",temp);

    // total ag sector emissions by sector for region
    for ( int m= 0;m<maxper;m++) {
        temp[m] = summary[m].get_emissmap_second( "CO2NetLandUse" );
    }
    dboutput4(name, "CO2 Emiss", "Net Land Use", "total", "MTC", temp );

    // regional emissions for all greenhouse gases
    typedef map<string,double>:: const_iterator CI;
    map<string,double> temissmap = summary[0].getemission(); // get gases for period 0
    for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
        for ( int m= 0;m<maxper;m++) {
            temp[m] = summary[m].get_emissmap_second(gmap->first);
        }
        dboutput4(name,"Emissions","by gas",gmap->first,"MTC",temp);
    }

    // regional fuel consumption (primary and secondary) by fuel type
    map<string,double> tfuelmap = summary[0].getfuelcons();
    for (CI fmap=tfuelmap.begin(); fmap!=tfuelmap.end(); ++fmap) {
        for ( int m= 0;m<maxper;m++) {
            temp[m] = summary[m].get_fmap_second(fmap->first);
        }
        if( fmap->first == "" ){
            dboutput4(name,"Fuel Consumption"," Region by fuel","No Fuelname","EJ",temp);
        }
        else {
            dboutput4(name,"Fuel Consumption"," Region by fuel",fmap->first,"EJ",temp);
        }
    }

    // region primary energy consumption by fuel type
    map<string,double> tpemap = summary[0].getpecons();
    for (CI pmap=tpemap.begin(); pmap!=tpemap.end(); ++pmap) {
        for ( int m= 0;m<maxper;m++) {
            temp[m] = summary[m].get_pemap_second(pmap->first);
        }
        dboutput4(name,"Pri Energy","Consumption by fuel",pmap->first,"EJ",temp);
    }

    // region primary energy trade by fuel type
    tpemap = summary[0].getpetrade();
    for (CI pmap=tpemap.begin(); pmap!=tpemap.end(); ++pmap) {
        for ( int m= 0;m<maxper;m++) {
            temp[m] = summary[m].get_petrmap_second(pmap->first);
        }
        dboutput4(name,"Pri Energy","Trade by fuel",pmap->first,"EJ",temp);
    }

    // regional Pri Energy Production Total
    for ( int m= 0;m<maxper;m++) {
        temp[m] = summary[m].get_peprodmap_second("zTotal");
    }
    dboutput4(name,"Pri Energy","Production by Sector","zTotal","EJ",temp);

    // write resource results to database
    for ( unsigned int i = 0; i < resources.size(); i++ ) {
        resources[i]->dbOutput( name );
    }

    // Calculate indirect emissions so they can be written to the database.
        IndirectEmissionsCalculator indEmissCalc;
        for( int i = 0; i < modeltime->getmaxper(); ++i ){
            accept( &indEmissCalc, i );
    }

    // write supply sector results to database
    for ( unsigned int i = 0; i < supplySector.size(); i++ ) {
        supplySector[i]->dbOutput( gdp.get(), &indEmissCalc );
    }
    // write end-use sector demand results to database
    for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
        demandSector[i]->dbOutput( gdp.get(), &indEmissCalc );
    }
}

//! Initialize the market prices for the agricultural products.
void RegionMiniCAM::initializeAgMarketPrices( const vector<double>& pricesIn ) {
    if( agSector.get() ){
        agSector->initMarketPrices( name, pricesIn );
    }
}

//! update regional summaries for reporting
void RegionMiniCAM::updateSummary( const list<string>& aPrimaryFuelList, const int period ) {
    calcEmissions( period );

    summary[period].clearpeprod();
    summary[period].clearfuelcons();
    summary[period].clearemfuelmap();

    for ( unsigned int i = 0; i < resources.size(); i++ ) {
        summary[period].initpeprod(resources[i]->getName(),resources[i]->getAnnualProd( name, period));
    }
    for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
        // call update for demand sector
        demandSector[i]->updateSummary( aPrimaryFuelList, period );
        // update regional fuel consumption (primary and secondary) for demand sector
        summary[ period ].updatefuelcons( aPrimaryFuelList, demandSector[ i ]->getfuelcons( period ) );
        summary[ period ].updateemfuelmap( demandSector[ i ]->getemfuelmap( period ) );
    }
    for ( unsigned int i = 0; i < supplySector.size(); i++ ) {
        // call update for supply sector
        supplySector[i]->updateSummary( aPrimaryFuelList, period );
        // update regional fuel consumption (primary and secondary) for supply sector
        summary[period].updatefuelcons( aPrimaryFuelList, supplySector[i]->getfuelcons(period));
        summary[ period ].updateemfuelmap( supplySector[ i ]->getemfuelmap( period ) );
    }
    // update primary energy trade from consumption and production amounts
    summary[period].updatepetrade();

    calcEmissFuel( aPrimaryFuelList, period );
}

//! Return the summary object for the given period.
/*! \todo This is a temporary fix to get the global CO2. This should be
*         restructured.
* \param period Model period to return the summary for.
* \return The summary object.
*/
const Summary& RegionMiniCAM::getSummary( const int period ) const {
    return summary[ period ];
}

//! update regional output tables for reporting
void RegionMiniCAM::updateAllOutputContainers( const int period ) {
}

/*! \brief Check whether the GDP object exists and report a warning if it does not.
* \return Whether the GDP object exists.
* \author Josh Lurz
*/
bool RegionMiniCAM::ensureGDP() const {
    if( !gdp.get() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "GDP object has not been created and is required. "
            << "Check for a region name mismatch." << endl;
        return false;
    }
    return true;
}

/*! \brief Check whether the Demographics object exists and report a warning if it does not.
* \return Whether the Demographics object exists.
* \author Josh Lurz
*/
bool RegionMiniCAM::ensureDemographics() const {
    if( !demographic.get() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Population object has not been created and is required. "
            << "Check for a region name mismatch." << endl;
        return false;
    }
    return true;
}

/*! \brief Update a visitor for a Region.
* \param aVisitor Visitor to update.
* \param aPeriod Period to update.
*/
void RegionMiniCAM::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitRegionMiniCAM( this, aPeriod );
    Region::accept( aVisitor, aPeriod );

    // Visit GDP object
    if ( gdp.get() ){
        gdp->accept( aVisitor, aPeriod );
    }

    // Visit LandAllocator object
    if ( mLandAllocator.get() ){
        mLandAllocator->accept( aVisitor, aPeriod );
    }

    // loop for resources.
    for( CResourceIterator currResource = resources.begin(); currResource != resources.end(); ++currResource ){
        (*currResource)->accept( aVisitor, aPeriod );
    }

    // loop for demand sectors.
    for( CDemandSectorIterator currDem = demandSector.begin(); currDem != demandSector.end(); ++currDem ){
        (*currDem)->accept( aVisitor, aPeriod );
    }

    if( agSector.get() ){
        agSector->accept( aVisitor, aPeriod );
    }
    aVisitor->endVisitRegionMiniCAM( this, aPeriod );
}
