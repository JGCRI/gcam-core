/*! 
* \file region.cpp
* \ingroup Objects
* \brief The Region class source file.
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <fstream>
#include <string>
#include <vector>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include <algorithm>
#include <memory>

#include "containers/include/region.h"
#include "containers/include/gdp.h"
#include "containers/include/scenario.h"
#include "containers/include/national_account.h"
#include "containers/include/info_factory.h"
#include "containers/include/iinfo.h"
#include "containers/include/dependency_finder.h"

// TODO: This needs a factory.
#include "sectors/include/sector.h"
#include "sectors/include/supply_sector.h"
#include "sectors/include/production_sector.h"
#include "sectors/include/demand_sector.h"
#include "sectors/include/food_demand_sector.h"
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

#include "demographics/include/demographic.h"

#include "emissions/include/ghg_policy.h"
#include "emissions/include/indirect_emiss_coef.h"
#include "emissions/include/total_sector_emissions.h"

#include "marketplace/include/marketplace.h"

#include "land_allocator/include/tree_land_allocator.h"

#include "util/base/include/summary.h"
#include "util/base/include/ivisitor.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/model_time.h" 
#include "util/base/include/configuration.h"
#include "util/base/include/util.h"
#include "util/logger/include/ilogger.h"
#include "util/curves/include/curve.h"
#include "util/curves/include/point_set_curve.h"
#include "util/curves/include/xy_data_point.h"
#include "util/curves/include/point_set.h"
#include "util/curves/include/explicit_point_set.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string Region::XML_NAME = "region";

//! Default constructor
Region::Region() {
    /*! \pre The modeltime object must be read-in before the Region can be
    *        parsed. 
    */
    assert( scenario->getModeltime() );

    // Resize all vectors to maximum period
    const int maxper = scenario->getModeltime()->getmaxper();
    TFEcalb.resize( maxper ); // Total Final Energy calibration value
    TFEPerCapcalb.resize( maxper ); // Total Final Energy per capita calibration value. This is converted to total TFE using population.
    mEnergyServicePrice.resize( maxper ); // aggregate price for demand services
    carbonTaxPaid.resize( maxper ); // total regional carbon taxes paid
    summary.resize( maxper ); // summary object for reporting
    calibrationGDPs.resize( maxper ); // GDPs for calibration
    GDPcalPerCapita.resize( maxper ); // GDP per capita for calibration. This is converted to an absolute GDP using population.
    nationalAccount.resize( maxper ); // National accounts for each period

    heatingDegreeDays = 0;
    coolingDegreeDays = 0;
    mRotationPeriod = 0;
}

//! Default destructor destroys sector, demsector, Resource, agSector, and
//! population objects.
Region::~Region() {
    clear();
}

//! Clear member variables and initialize elemental members.
void Region::clear(){
    for ( SectorIterator secIter = supplySector.begin(); secIter != supplySector.end(); secIter++ ) {
        delete *secIter;
    }

    for ( DemandSectorIterator demIter = demandSector.begin(); demIter != demandSector.end(); ++demIter ) {
        delete *demIter;
    }

    for ( ResourceIterator rescIter = resources.begin(); rescIter != resources.end(); ++rescIter ) {
        delete *rescIter;
    }
    for( GHGPolicyIterator policyIter = mGhgPolicies.begin(); policyIter != mGhgPolicies.end(); ++policyIter ){
        delete *policyIter;
    }
}

/*! Return the region name.
* \return The string name of the region is returned.
*/
const string& Region::getName() const {
    return name;
}

/*! 
* \brief Sets the data members from the XML input.
* \details This function parses all XML data from Region down to the lowest set
*          of objects. As the XML data is parsed, new objects are continually
*          added to the object container using the push_back routine.
* \param node XML DOM node of the region
* \todo Change the diagnosic "assert( node );" to fail with a more informative
*       error (file, previous node?, location?)
*/
void Region::XMLParse( const DOMNode* node ){
    // make sure we were passed a valid node.
    assert( node );

    // get the name attribute.
    name = XMLHelper<string>::getAttrString( node, "name" );

    // get all child nodes.
    DOMNodeList* nodeList = node->getChildNodes();

    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        else if( nodeName == "PrimaryFuelCO2Coef" ) {
            primaryFuelCO2Coef[ XMLHelper<string>::getAttrString( curr, "name" ) ] = XMLHelper<double>::getValue( curr );
        }

        else if( nodeName == Demographic::getXMLNameStatic() ){
            parseSingleNode( curr, demographic, new Demographic );
        }
        else if( nodeName == GDP::getXMLNameStatic() ){
            parseSingleNode( curr, gdp, new GDP );
        }
        else if( nodeName == NationalAccount::getXMLNameStatic() ){
            // This is not correct.
            int per = scenario->getModeltime()->getyr_to_per( XMLHelper<int>::getAttr( curr, "year" ) );
            // put in a check for correct year and per
            nationalAccount[per].XMLParse( curr );
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
        else if( nodeName == DepletableResource::getXMLNameStatic() ){
            parseContainerNode( curr, resources, resourceNameMap, new DepletableResource() );
        }
        else if( nodeName == FixedResource::getXMLNameStatic() ){
            parseContainerNode( curr, resources, resourceNameMap, new FixedResource() );
        }
        else if( nodeName == RenewableResource::getXMLNameStatic() ){
            parseContainerNode( curr, resources, resourceNameMap, new RenewableResource() );
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
        else if( nodeName == DemandSector::getXMLNameStatic() ){
            parseContainerNode( curr, demandSector, demandSectorNameMap, new DemandSector( name ) );
        }
        else if( nodeName == FoodDemandSector::getXMLNameStatic() ){
            parseContainerNode( curr, demandSector, demandSectorNameMap, new FoodDemandSector( name ) );
        }
        else if( nodeName == FoodSupplySector::getXMLNameStatic() ) {
            parseContainerNode( curr, supplySector, supplySectorNameMap, new FoodSupplySector( name ) );
        }
        else if( nodeName == ForestDemandSector::getXMLNameStatic() ){
            parseContainerNode( curr, demandSector, demandSectorNameMap, new ForestDemandSector( name ) );
        }
        else if( nodeName == ForestSupplySector::getXMLNameStatic() ) {
            parseContainerNode( curr, supplySector, supplySectorNameMap, new ForestSupplySector( name ) );
        }
        else if( nodeName == BuildingDemandSector::getXMLNameStatic() ){
            parseContainerNode( curr, demandSector, demandSectorNameMap, new BuildingDemandSector( name ) );
        }
        else if( nodeName == TotalSectorEmissions::getXMLNameStatic() ){
            parseContainerNode( curr, mAggEmissionsCalculators, totalSectorEmissionsNameMap, new TotalSectorEmissions );
        }
        // transportation sector is contained in demandSector
        else if( nodeName == TranSector::getXMLNameStatic() ){
            parseContainerNode( curr, demandSector, demandSectorNameMap, new TranSector( name ) );
        } 
        else if ( nodeName == TreeLandAllocator::getXMLNameStatic() ) {
            parseSingleNode( curr, mLandAllocator, new TreeLandAllocator );
        }
        else if( nodeName == AgSector::getXMLNameStatic() ) {
            if( Configuration::getInstance()->getBool( "agSectorActive" ) ){
                parseSingleNode( curr, agSector, new AgSector );
            }
        }
        else if( nodeName == GHGPolicy::getXMLNameStatic() ){
            parseContainerNode( curr, mGhgPolicies, mGhgPoliciesNameMap, new GHGPolicy() );
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
                else if(nodeNameChild == "TFEcalb") {
                    XMLHelper<double>::insertValueIntoVector( currChild, TFEcalb, modeltime );
                }
                // Per-capita value -- is converted to total TFE using population
                else if(nodeNameChild == "TFEPerCapcalb") {
                    XMLHelper<double>::insertValueIntoVector( currChild, TFEPerCapcalb, modeltime );
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
                    sectorOrderList.push_back( XMLHelper<string>::getValueString( currChild ) );
                }
                else {
                    ILogger& mainLog = ILogger::getLogger( "main_log" );
                    mainLog.setLevel( ILogger::WARNING );
                    mainLog << "Unrecognized text string: " << nodeNameChild << " found while parsing region->SectorOrderList." << endl;
                }
            }
        }
#endif
        else if( XMLDerivedClassParse(nodeName, curr) ){
            // Do nothing but avoid printing the error.
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing region." << endl;
        }
    }
}

// for parsing derived region classes
bool Region::XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ) {
    return false;
}


/*! Complete the initialization. Get the size of vectors, initialize AGLU,
*   create all markets, call complete initialization 
*  functions for nested objects, update the fuel map, and find simultaneities.
* \todo I think since there is one indirect ghg object for each sector, it might
*       be better in sector. This may require deriving supply sector.
*/
void Region::completeInit() {
    // Region info has no parent Info.
    mRegionInfo.reset( InfoFactory::constructInfo( 0 ) );
    mRegionInfo->setInteger( "rotationPeriod", mRotationPeriod );

    // initialize demographic
    if( demographic.get() ){
        demographic->completeInit();
    }

    // Initialize the GDP
    if( gdp.get() ){
        gdp->initData( demographic.get() );
    }

    const Modeltime* modeltime = scenario->getModeltime();
    // Change per capita calibration values into absolute values
    for ( int period = 0; period < modeltime->getmaxper(); period++ ) {
        // Change per capita calibration values into absolute values
        if ( TFEPerCapcalb [ period ] != 0 && demographic.get() ) {
            if ( TFEcalb[ period ] != 0 ) {
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::WARNING );
                mainLog << "Both TFEcalb and TFEPerCapcalb read in region "
                        << name << " for period " << period << ". TFEperCap used." << endl;
            }
            // Convert from GJ/cap to EJ. Pop is in 1000's, so need an additional 1e6 scale to get from GJ to EJ
            // TODO: This will eventually write back out, so if this input file were used again the above warning
            // would be triggered.
            TFEcalb[ period ] = TFEPerCapcalb[ period ] * demographic->getTotal( period ) / 1e6;
        }
    }

    // Initialize the dependency finder.
    // This may need to move to init calc when markets change by period.
    auto_ptr<DependencyFinder> depFinder( new DependencyFinder( scenario->getMarketplace(), name ) );
    for( SectorIterator sectorIter = supplySector.begin(); sectorIter != supplySector.end(); ++sectorIter ) {
        ( *sectorIter )->completeInit( mRegionInfo.get(), depFinder.get(), mLandAllocator.get() );
        Emcoef_ind temp( ( *sectorIter )->getName() );
        emcoefInd.push_back( temp );
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
        // This needs to be after completeInit for supply sectors is called
        // TODO: Because...?
        mLandAllocator->completeInit( name, mRegionInfo.get() );
    }

    for( ResourceIterator resourceIter = resources.begin(); resourceIter != resources.end(); ++resourceIter ) {
        (*resourceIter)->completeInit( name );
    }

    for( DemandSectorIterator demandSectorIter = demandSector.begin();
        demandSectorIter != demandSector.end(); ++demandSectorIter )
    {
        // Pass null in for the dependency finder so that the demand sector
        // technologies will not add their dependencies. This is because demand
        // sectors are not currently included in the ordering. Pass null for the land allocator
        // since demand sectors cannot allocate land.
        ( *demandSectorIter )->completeInit( mRegionInfo.get(), 0, 0 );
    }

    for( GHGPolicyIterator ghgPolicy = mGhgPolicies.begin(); ghgPolicy != mGhgPolicies.end(); ++ghgPolicy ){
        (*ghgPolicy)->completeInit( name );
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
    depFinder->createOrdering();
    reorderSectors( depFinder->getOrdering() );

    setupCalibrationMarkets();
}

/*! \brief Initialize the calibration markets. 

* \todo Move GDP calibration parameters into GDP object
*/
void Region::setupCalibrationMarkets() {
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
bool Region::reorderSectors( const vector<string>& aOrderList ){
    // Check for an empty order list in which case the reordering shoud be
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

        // Check if the sector name in the sector orderling list exists
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

/*! 
* \brief Write datamembers to datastream in XML format. Calls XMLWriteElement
*        function from the XMLHelper class for the actual writing.
* \param out Output file in XML format.
* \param tabs Tabs object used to track the number of tabs to print.
* \ref faqitem1 
*/
void Region::toInputXML( ostream& out, Tabs* tabs ) const {
    XMLWriteOpeningTag ( getXMLName(), out, tabs, name );

    // Write out the Co2 Coefficients. 
    for( map<string,double>::const_iterator coefAllIter = primaryFuelCO2Coef.begin(); coefAllIter != primaryFuelCO2Coef.end(); coefAllIter++ ) {
        XMLWriteElement( coefAllIter->second, "PrimaryFuelCO2Coef", out, tabs, 0, coefAllIter->first );
    }

    for( map<string,double>::const_iterator coefPriIter = carbonTaxFuelCoef.begin(); coefPriIter != carbonTaxFuelCoef.end(); coefPriIter++ ) {
        XMLWriteElement( coefPriIter->second, "CarbonTaxFuelCoef", out, tabs, 0, coefPriIter->first );
    }

    XMLWriteElementCheckDefault( heatingDegreeDays, "heatingDegreeDays", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( coolingDegreeDays, "coolingDegreeDays", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( mRotationPeriod, "rotationPeriod", out, tabs, 0 );

    // write the xml for the class members.
    // write out the single population object.
    if( demographic.get() ){
        demographic->toInputXML( out, tabs);
    }

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

    // write out supply sector objects.
    for( CSectorIterator j = supplySector.begin(); j != supplySector.end(); j++ ){
        ( *j )->toInputXML( out, tabs );
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
    // write out mGhgPolicies objects.
    for( CGHGPolicyIterator l = mGhgPolicies.begin(); l != mGhgPolicies.end(); l++ ){
        ( *l )->toInputXML( out, tabs );
    }

    // Note: The count function is an STL algorithm that counts the number of
    // times a value occurs within the a range of a container. The first two
    // arguments to the function are the range of the container to search, the
    // third is the value to search for.
    if( ( count( calibrationGDPs.begin(), calibrationGDPs.end(), 0 ) != static_cast<int>( calibrationGDPs.size() ) ) 
        || ( count( TFEcalb.begin(), TFEcalb.end(), 0 ) != static_cast<int>( TFEcalb.size() ) ) ){ // makes sure tags aren't printed if no real data

            // Write out regional economic data
            XMLWriteOpeningTag( "calibrationdata", out, tabs ); 

            // write out calibration GDP
            const Modeltime* modeltime = scenario->getModeltime();
            for( unsigned int m = 0; m < calibrationGDPs.size(); m++ ){
                XMLWriteElementCheckDefault( calibrationGDPs[ m ], "GDPcal", out, tabs, 0.0, modeltime->getper_to_yr( m ) );
            }

            // write out TFE calibration values
            for( unsigned int m = 0; m < TFEcalb.size(); m++ ) {
                XMLWriteElementCheckDefault( TFEcalb[ m ],"TFEcalb", out, tabs, 0.0, modeltime->getper_to_yr( m ) );
            }
            XMLWriteClosingTag( "calibrationdata", out, tabs );
            // End write out regional economic data
        } // close calibration IF

        toInputXMLDerived( out, tabs );

        // finished writing xml for the class members.
        XMLWriteClosingTag( getXMLName(), out, tabs );
}

void Region::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    // do nothing
}

/*! \brief Write datamembers to datastream in XML format for debugging purposes.  
* Calls XMLWriteElement function from the XMLHelper class for the actual
* writing. Calls debug functions in other contained objects. 
*
* \param period Model time period
* \param out Output file for debugging purposes in XML format
* \param tabs Tabs object used to track the number of tabs to print.
*/
void Region::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    XMLWriteOpeningTag ( getXMLName(), out, tabs, name );

    // write out basic datamembers
    XMLWriteElement( heatingDegreeDays, "heatingDegreeDays", out, tabs );
    XMLWriteElement( coolingDegreeDays, "coolingDegreeDays", out, tabs );
    XMLWriteElement( mRotationPeriod, "rotationPeriod", out, tabs );
    XMLWriteElement( static_cast<unsigned int>( mGhgPolicies.size() ), "noGhg", out, tabs );
    XMLWriteElement( static_cast<unsigned int>( resources.size() ), "numResources", out, tabs );
    XMLWriteElement( static_cast<unsigned int>( supplySector.size() ), "noSSec", out, tabs );
    XMLWriteElement( static_cast<unsigned int>( demandSector.size() ), "noDSec", out, tabs );
    XMLWriteElement( calibrationGDPs[ period ], "calibrationGDPs", out, tabs );
    XMLWriteElement( mEnergyServicePrice[ period ], "mEnergyServicePrice", out, tabs );
    XMLWriteElement( carbonTaxPaid[ period ], "carbonTaxPaid", out, tabs );
    XMLWriteElement( TFEcalb[ period ], "TFECalb", out, tabs );
    XMLWriteElement( getTotFinalEnergy( period ), "TotalFinalEnergy", out, tabs );

    // Write out the Co2 Coefficients. 
    for( map<string,double>::const_iterator coefAllIter = primaryFuelCO2Coef.begin(); coefAllIter != primaryFuelCO2Coef.end(); coefAllIter++ ) {
        XMLWriteElement( coefAllIter->second, "PrimaryFuelCO2Coef", out, tabs, 0, coefAllIter->first );
    }

    for( map<string,double>::const_iterator coefPriIter = carbonTaxFuelCoef.begin(); coefPriIter != carbonTaxFuelCoef.end(); coefPriIter++ ) {
        XMLWriteElement( coefPriIter->second, "CarbonTaxFuelCoef", out, tabs, 0, coefPriIter->first );
    }
    // write the xml for the class members.
    // write out the single population object.
    if( gdp.get() ){
        gdp->toDebugXML( period, out, tabs );
    }
    if( demographic.get() ){
        demographic->toDebugXML( period, out, tabs );
    }

    // Write out the land allocator.
    if ( mLandAllocator.get() ) {
        mLandAllocator->toDebugXML( period, out, tabs );
    }

    // write out the resources objects.
    for( CResourceIterator currResource = resources.begin(); currResource != resources.end(); ++currResource ){
        (*currResource)->toDebugXML( period, out, tabs );
    }

    // write out supply sector objects.
    for( CSectorIterator j = supplySector.begin(); j != supplySector.end(); j++ ){
        ( *j )->toDebugXML( period, out, tabs );
    }

    // write out demand sector objects.
    for( CDemandSectorIterator currSector = demandSector.begin(); currSector != demandSector.end(); ++currSector ){
        (*currSector)->toDebugXML( period, out, tabs );
    }

    // Write out the single agSector object.
    // agSector->toDebugXML( period, out );

    // write out mGhgPolicies objects.
    for( CGHGPolicyIterator currPolicy = mGhgPolicies.begin(); currPolicy != mGhgPolicies.end(); ++currPolicy ){
        (*currPolicy)->toDebugXML( period, out, tabs );
    }

    for( unsigned int i = 0; i < mAggEmissionsCalculators.size(); ++i ){
        mAggEmissionsCalculators[ i ]->toDebugXML( period, out, tabs );
    }

    toDebugXMLDerived( period, out, tabs );
    // Finished writing xml for the class members.

    XMLWriteClosingTag( getXMLName(), out, tabs );
}

void Region::toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const {
    // do nothing
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
const std::string& Region::getXMLName() const {
    return XML_NAME;
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
const std::string& Region::getXMLNameStatic() {
    return XML_NAME;
}

//! Calculate the region.
void Region::calc( const int period, const bool doCalibrations ) {
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
    // determine supply of primary resources
    calcResourceSupply( period );
    // determine prices for all supply sectors (e.g., refined fuels, electricity, etc.)
    calcFinalSupplyPrice( period );
    // calculate enduse service price
    calcEndUsePrice( period );
    // adjust GDP for energy cost changes
    adjustGDP( period );

    // determine end-use demand for energy and other goods
    calcEndUseDemand( period );

    // determine supply of final energy and other goods based on demand
    setFinalSupply( period );

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
void Region::calcAgSector( const int period ) {
    agSector->runModel( period, name );
    agSector->carbLand( period, name );
}

/*! \brief Set regional ghg constraint from input data to market supply.
* \param period Model time period
*/
void Region::calcResourceSupply( const int period ){
    if( !ensureGDP() ){
        return;
    }

    for( ResourceIterator currResource = resources.begin(); currResource != resources.end(); ++currResource ){
        (*currResource)->calcSupply( name, gdp.get(), period );
    }
}

/*! Calculate prices of refined fuels and electricity.
* \todo Move this functionality into sector.
* \param period Model time period
*/
void Region::calcFinalSupplyPrice( const int period ) {
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
void Region::setFinalSupply( const int period ) {
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
void Region::calcGDP( const int period ){
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
const vector<double> Region::calcFutureGDP() const {
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

/*! Calculate demand sector aggregate price.
*
* \param period Model time period
*/
void Region::calcEndUsePrice( const int period ) {

    mEnergyServicePrice[ period ] = 0;

    for ( DemandSectorIterator currDemSector = demandSector.begin(); currDemSector != demandSector.end(); ++currDemSector ) {
        (*currDemSector)->calcShare( period, gdp.get() );       

        // calculate service price for each demand sector

        // calculate aggregate service price for region
        mEnergyServicePrice[ period ] += (*currDemSector)->getWeightedEnergyPrice( period );

        // calculate service price elasticity for each demand sector
        // or use read in value, temporary code
        // Note: If this is set to false the model solves better generally. -JPL
        bool useReadinData = true;
        // do nothing if false
        if (!useReadinData) {
            (*currDemSector)->calcPriceElasticity( period );
        } 
    }
}

/*! Adjust regional gdp for energy.
*
* \param period Model time period
* \todo Move this calculation down to GDP
*/
void Region::adjustGDP( const int period ){
    if( !ensureGDP() ){
        return;
    }

    const Modeltime* modeltime = scenario->getModeltime();

    double tempratio = 1;
    if ( period > modeltime->getyr_to_per(1990) ) {
        tempratio = mEnergyServicePrice[period]/mEnergyServicePrice[period-1];
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
void Region::calibrateRegion( const bool doCalibrations, const int period ) {
    // Do subsector and technology level energy calibration
    // can only turn off calibrations that do not involve markets
    if ( doCalibrations ) {
        // Calibrate demand sectors
        for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
            demandSector[ i ]->calibrateSector( period );
        }

        // Calibrate supply sectors
        for ( unsigned int i = 0; i < supplySector.size(); i++ ) {
            supplySector[ i ]->calibrateSector( period );
        }
        // Calibrate Regional TFE
        if ( !isEnergyDemandAllCalibrated( period ) ) {
            calibrateTFE( period );
        } else {
            // do nothing now. Need to make a variant of the total cal outputs
            // function so that can compare TFE with cal value.
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

/*! \brief Returns whether all demand sectors have calibrated energy demands.
* \param period Model time period
* \return Whether all demand sectors have calibrated energy demands.
*/
bool Region::isEnergyDemandAllCalibrated( const int period ) const {
    // Only check if sector has non-zero energy demand
    for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
        if ( !demandSector[ i ]->isEnergyUseFixed( period ) ){
            return false;
        }
    }

    return true;
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
bool Region::isAllCalibrated( const int period, double calAccuracy, const bool printWarnings ) const {
    const static bool calOn = Configuration::getInstance()->getBool( "CalibrationActive" );
    bool returnVal = true;

    if ( calOn || printWarnings ) {
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

        // Check Regional TFE calibration if exists
        if ( ( !isEnergyDemandAllCalibrated( period ) && TFEcalb[ period ]  > 0 ) && calOn ) {
            if ( fabs( calcTFEscaleFactor( period ) - 1.0 ) > calAccuracy ) {
                if ( printWarnings ) {
                    ILogger& mainLog = ILogger::getLogger( "main_log" );
                    mainLog.setLevel( ILogger::WARNING );
                    mainLog << "TFE Calibration is off by " << calcTFEscaleFactor( period )
                        << " in period " <<  period << " for region " << name << "." << endl;
                }
                returnVal = false;
            }
        }
    }

    // always return true if calibrations are not on.
    return calOn ? returnVal : true;
}

//! Calibrate total final energy Demand for this region.
/*! Adjusts AEEI in each demand sector until TFE is equal to the calibration value.
/warning assumes sectors have all energy or non-energy outputs
*/
void Region::calibrateTFE( const int period ) {
    // Ratio of TFE in sector to cal value
    double scaleFactor = calcTFEscaleFactor( period ); // value of zero means cal value was zero

    // Don't calibrate unless non zero value of TFE
    if ( scaleFactor  > 0 ) { 
        // Scale each sector's output to approach calibration value
        // But only do this for sectors that have energy outputs
        // And do not scale is all outputs are calibrated (detailed calibration will take care of this)
        for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
            if ( !demandSector[ i ]-> isEnergyUseFixed( period ) ) {
                demandSector[ i ]->scaleOutput( period , scaleFactor );
            }
        }
    }
}

/*! \brief Return TFE for all demand sectors
*
* \author Steve Smith
* \param period Model period
* \return Double total final energy for all demand sectors
*/
double Region::getTotFinalEnergy( const int period ) const {

    // Calculate total final energy demand for all demand sectors
    double totalFinalEnergy = 0;
    for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
        totalFinalEnergy += demandSector[ i ]->getEnergyInput( period );
    }

    return totalFinalEnergy;
}

/*! \brief Return ratio of TFE and calibrated value
*
* Returns 0 if TFE calib value is zero, which indicates there is no calibration
*
* \author Steve Smith
* \param period Model period
* \return Double ratio TFE in model to calibrated value.
*/
double Region::calcTFEscaleFactor( const int period ) const {

    if ( TFEcalb[ period ]  > 0 ) {
        // Calculate total final energy demand for all demand sectors
        double totalFinalEnergy = getTotFinalEnergy( period );

        if ( totalFinalEnergy > util::getVerySmallNumber() ) {
            return TFEcalb[ period ] / totalFinalEnergy;
        }
    }
    return 0;
}

/*! \brief Call any initializations that are only done once per period.
* \author Steve Smith
* \param period Model period
* \todo Once postCalc is present, add a check to verify that aggregate emissions worked properly
*/
void Region::initCalc( const int period ) 
{

    // Set aggregate emissions factor to region info if needed
    for ( unsigned int i = 0; i < mAggEmissionsCalculators.size(); i++ ) {
        mAggEmissionsCalculators[ i ]->setAggregateEmissionFactor( supplySector, mRegionInfo.get(), period );
    }

    mRegionInfo->setDouble( "heatingDegreeDays", heatingDegreeDays );
    mRegionInfo->setDouble( "coolingDegreeDays", coolingDegreeDays );

    for( SectorIterator currSector = supplySector.begin(); currSector != supplySector.end(); ++currSector ){
        (*currSector)->initCalc( nationalAccount[ period ], demographic.get(), period );
    }
    for ( DemandSectorIterator currSector = demandSector.begin(); currSector != demandSector.end(); ++currSector ) {
        (*currSector)->initCalc( nationalAccount[ period ], demographic.get(), period  ); 
    }

    // Add CO2 coefficients to the marketplace.
    const static string CO2COEF = "CO2Coef";
    Marketplace* marketplace = scenario->getMarketplace();
    for( map<string, double>::const_iterator coef = primaryFuelCO2Coef.begin();
        coef != primaryFuelCO2Coef.end(); ++coef )
    {
        // Markets may not exist for incorrect fuel names.
        IInfo* fuelInfo = marketplace->getMarketInfo( coef->first, name, period, false );
        if( fuelInfo ){
            fuelInfo->setDouble( CO2COEF, coef->second );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            // Currently this error occurs frequently, this could be NOTICE.
            if ( coef->second == 0 ) {
                mainLog.setLevel( ILogger::DEBUG );
                mainLog << "Cannot set emissions factor of zero for fuel " << coef->first
                    << " because the name does not match the name of a market." << endl;
            }
            else {
                mainLog.setLevel( ILogger::DEBUG );
                mainLog << "Cannot set emissions factor for fuel " << coef->first
                    << " because the name does not match the name of a market." << endl;
            }
        }
    }

    // Make sure TFE is same as calibrated values
    if ( isEnergyDemandAllCalibrated( period ) ) {
        double totalFinalEnergy = 0;
        for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
            totalFinalEnergy += demandSector[ i ]->getCalAndFixedInputs( period, "allInputs" );
        }
        if ( TFEcalb[ period ] != 0 ) {
            double scaleFactor = totalFinalEnergy / TFEcalb[ period ];
            if ( fabs( scaleFactor - 1 ) > util::getSmallNumber() ) {
                ILogger& calibrationLog = ILogger::getLogger( "calibration_log" );
                calibrationLog.setLevel( ILogger::DEBUG );
                calibrationLog << "TFE in region " << name << " scaled by: "<< scaleFactor << endl;
                TFEcalb[ period ] = totalFinalEnergy; 
            }
        }
    }
    checkData( period );
}

/*! \brief Adjusts calibrated demands to be consistant with calibrated supply.
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

void Region::setCalSuppliesAndDemands( const int period ) {
    // Check for fully calibrated/fixed supplies
    for ( unsigned int i = 0; i < supplySector.size(); i++ ) {
        supplySector[ i ]->setCalibratedSupplyInfo( period );
    }

    // Check for fully calibrated/fixed resources
    for ( unsigned int i = 0; i < resources.size(); i++ ) {
        resources[i]->setCalibratedSupplyInfo( period, name );
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
void Region::initializeCalValues( const int period ) {
    Marketplace* marketplace = scenario->getMarketplace();
    // This will get set to -1 if this market is not fixed and a value >= 0
    // otherwise.
    const double DEFAULT_CAL_VALUE = -2;

    updateSummary( list<string>(), period );    // Dummy call to final supply to setup fuel map.

    typedef map<string,double>:: const_iterator CI;
    for ( unsigned int i = 0; i < supplySector.size(); i++ ) {
        // Now prepare to loop through all the fuels used by this sector. Get
        // fuel consumption map for sector
        map<string, double> fuelcons = supplySector[ i ]->getfuelcons( period );

        // Set default values for supply sector
        IInfo* marketInfo = marketplace->getMarketInfo( supplySector[ i ]->getName(), name, period, true );
        marketInfo->setDouble( "calSupply", DEFAULT_CAL_VALUE );
        marketInfo->setDouble( "calDemand", DEFAULT_CAL_VALUE );
        marketInfo->setDouble( "calFixedDemand", DEFAULT_CAL_VALUE );

        // then for all fuels used by this sector
        for( CI fuelUsedIter = fuelcons.begin(); fuelUsedIter != fuelcons.end(); ++fuelUsedIter ){
            IInfo* fuelMarketInfo = marketplace->getMarketInfo( fuelUsedIter->first, name, period, false );
            // Fuel market may not exist.
            if( fuelMarketInfo ){
                fuelMarketInfo->setDouble( "calSupply", DEFAULT_CAL_VALUE );
                fuelMarketInfo->setDouble( "calDemand", DEFAULT_CAL_VALUE );
                fuelMarketInfo->setDouble( "calFixedDemand", DEFAULT_CAL_VALUE );
            }
        }
    }

    for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
        // Now prepare to loop through all the fuels used by this sector. Get
        // fuel consumption map for sector
        map<string, double> fuelcons = demandSector[ i ]->getfuelcons( period ); 

        for( CI fuelUsedIter = fuelcons.begin(); fuelUsedIter != fuelcons.end(); ++fuelUsedIter ){
            IInfo* fuelMarketInfo = marketplace->getMarketInfo( fuelUsedIter->first, name, period, false );
            // Fuel market may not exist.
            if( fuelMarketInfo ){
                fuelMarketInfo->setDouble( "calSupply", DEFAULT_CAL_VALUE );
                fuelMarketInfo->setDouble( "calDemand", DEFAULT_CAL_VALUE );
                fuelMarketInfo->setDouble( "calFixedDemand", DEFAULT_CAL_VALUE );
            }
        }
    }

    for ( unsigned int i = 0; i < resources.size(); i++ ) {
        // Now prepare to loop through all the fuels used by this sector
        IInfo* resourceMarketInfo = marketplace->getMarketInfo( resources[ i ]->getName(), name, period, true );
        resourceMarketInfo->setDouble( "calSupply", DEFAULT_CAL_VALUE );
        resourceMarketInfo->setDouble( "calDemand", DEFAULT_CAL_VALUE );
        resourceMarketInfo->setDouble( "calFixedDemand", DEFAULT_CAL_VALUE );
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
bool Region::setImpliedCalInputs( const int period ) {
    Configuration* conf = Configuration::getInstance();
    Marketplace* marketplace = scenario->getMarketplace();
    const double MKT_NOT_ALL_FIXED = -1;
    bool debugChecking = conf->getBool( "debugChecking" );
    bool calcIsDone = true;

    // Instantiate the relationship map here if it does not already exist
    if ( !fuelRelationshipMap.get() ) {
        // Clear relationship map for this iteration
        fuelRelationshipMap.reset( new FuelRelationshipMap() ); 
    }

    // For each sector, check if a fixed demand is an output. If supply is fixed
    // stop. Make sure all inputs are fixed demands
    for ( unsigned int jsec = 0; jsec < supplySector.size(); jsec++ ) {
        string sectorName = supplySector[ jsec ]->getName();
        IInfo* sectorMarketInfo = marketplace->getMarketInfo( sectorName, name, period, true );
        double sectorCalDemand = sectorMarketInfo->getDouble( "calDemand", false );

        // Check to see if this sector supplies a fuel with a fixed demand. If
        // so, and this is not a fuel that already has a fixed supply, then
        // calculate the cooresponding fixed supply
        if ( sectorCalDemand >= 0  && ( sectorMarketInfo->getDouble( "calSupply", false ) < 0 ) ) {

            // Now prepare to loop through all the fuels used by this sector.
            // Get fuel consumption map for sector.
            map<string, double> fuelcons = supplySector[ jsec ]->getfuelcons( period );
            // Total directly calibrated outputs (not outputs infered from
            // market)
            double totalCalOutputs = 0;
            // Count of number of fixed market inputs to this sector. This is to
            // make sure there is only one.
            int numbFixedMktInputs = 0;

            string newFixedDemandInputName;
            typedef map<string,double>:: const_iterator CI;
            for( CI fuelUsedIter = fuelcons.begin(); fuelUsedIter != fuelcons.end(); ++fuelUsedIter ){

                // If inputs are all fixed for this fuel
                if ( supplySector[ jsec ]->inputsAllFixed( period, fuelUsedIter->first ) ) {
                    totalCalOutputs += supplySector[ jsec ]->getCalAndFixedOutputs( period, fuelUsedIter->first );
                } 
                // else have an input that can be adjusted
                else {
                    numbFixedMktInputs ++;
                    newFixedDemandInputName = fuelUsedIter->first;
                }
            }

            // If we found one, and only one, unfixed input then figure out what
            // this should be and set things appropriately
            if ( numbFixedMktInputs == 1 ) {
                // Calculate required input (which will be set to the
                // marketplace)
                double requiredOutput = sectorCalDemand; 
                // subtract other calOutputs so that we only calculate the
                // output from the remaining fuel
                requiredOutput -= totalCalOutputs;

                if ( debugChecking ) {
                    ILogger& dependenciesLog = ILogger::getLogger( "sector_dependencies" );
                    dependenciesLog.setLevel( ILogger::NOTICE );
                    dependenciesLog << "  Transitive demand for " << newFixedDemandInputName  
                        << " in sector " << sectorName
                        << " in region " << name << " added with value " << requiredOutput << endl; 
                }

                supplySector[ jsec ]->setImpliedFixedInput( period, newFixedDemandInputName, requiredOutput );

                // Now that transitive demand has been set, clear demand for
                // this good so this won't be done again
                sectorMarketInfo->setDouble( "calDemand", MKT_NOT_ALL_FIXED );

                // Save this relationship information First element of vector is
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
                if ( fuelRelationshipMap->find( sectorName ) != fuelRelationshipMap->end() ) {
                    tempDependents = (*fuelRelationshipMap)[ sectorName ];
                    fuelRelationshipMap->erase( sectorName );
                } 
                // If proper key is already there, then copy dependent list (the
                // new fuelname will be added to it below)
                else if ( fuelRelationshipMap->find( newFixedDemandInputName ) != fuelRelationshipMap->end() ) {
                    tempDependents = (*fuelRelationshipMap)[ newFixedDemandInputName ];
                }
                // In any event, current sector should be added as a key (this
                // overwrites previous dependents list if it existed)
                tempDependents.push_back( sectorName );
                (*fuelRelationshipMap)[ newFixedDemandInputName ] = tempDependents;

                // If this demand does not correspond to a sector with a fixed
                // supply then we are not done with checking. Otherwise we are
                // done for this region if no sectors satisfy this criteria.
                const IInfo* demandMarketInfo = marketplace->getMarketInfo( newFixedDemandInputName, name, period, false );
                if ( demandMarketInfo && demandMarketInfo->getDouble( "calSupply", true ) < 0 ) {
                    calcIsDone = false;
                }
            } 
            else if ( numbFixedMktInputs > 1 ) {
                // Print a warning, but only if calDemand is not zero (if
                // calDemand is zero then this is probably not a problem)
                if ( sectorCalDemand > 0 ) {
                    ILogger& mainLog = ILogger::getLogger( "main_log" );
                    mainLog.setLevel( ILogger::NOTICE );
                    ILogger& dependenciesLog = ILogger::getLogger( "sector_dependencies" );
                    dependenciesLog.setLevel( ILogger::NOTICE );
                    mainLog << "Couldn't calculate transitive demand for input "<< newFixedDemandInputName <<" sector: " <<  sectorName << " region: " << name << endl;
                    dependenciesLog << "Couldn't calculate transitive demand for input "<< newFixedDemandInputName <<" sector: " <<  sectorName << " region: " << name << endl;
                    // If can't calculate this demand then reset so that nothing
                    // is scaled.
                    IInfo* demandMarketInfo = marketplace->getMarketInfo( newFixedDemandInputName, name, period, false );
                    if( demandMarketInfo ){
                        demandMarketInfo->setDouble( "calDemand", MKT_NOT_ALL_FIXED );
                    }
                }
            }
        } // End of fixed demand transitive calculation loop        
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
int Region::scaleCalInputs( const int period ) {
    Marketplace* marketplace = scenario->getMarketplace();
    Configuration* conf = Configuration::getInstance();

    if ( conf->getBool( "PrintSectorDependencies", false ) && !fuelRelationshipMap->empty() ) {
        ILogger& dependenciesLog = ILogger::getLogger( "sector_dependencies" );
        dependenciesLog.setLevel( ILogger::NOTICE );
        dependenciesLog << endl;
        dependenciesLog << name << " REGIONAL DEPENDENCIES as used for calibration input/output adjustments. Period "<< period << endl;
        dependenciesLog << "These are direct and transitive dependencies only for sectors that potentially have calibration adjustments" << endl;
        typedef map<std::string, std::vector<std::string> > :: const_iterator CIMAP;
        for( CIMAP fuelDependencyIter = fuelRelationshipMap->begin(); fuelDependencyIter != fuelRelationshipMap->end(); ++fuelDependencyIter ){
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

    map<string,double> fuelCons = summary[0].getfuelcons();
    typedef map<string,double>:: const_iterator CI;

    // Loop through each fuel used in this region
    for( CI fuelIter = fuelCons.begin(); fuelIter != fuelCons.end(); ++fuelIter ){
        const IInfo* fuelMarketInfo = marketplace->getMarketInfo( fuelIter->first, name, period, false );
        if ( fuelMarketInfo ) {
            //  Determine if inputs for this fuel need to be scaled. Get total cal + fixed supply and demand values.
            double calSupplyValue = fuelMarketInfo->getDouble( "calSupply", false );
            double calDemandValue = fuelMarketInfo->getDouble( "calDemand", false );

            // If these are positive then may need to scale calInputs for this input
            if ( ( calSupplyValue > 0 ) && ( calDemandValue > 0 ) ) {

                // First, fixed values are not scaled, so adjust for this.
                // The reason fixed values are not scaled is that fixed values can be used in instances where
                // a simultaneity would otherwise occur during calibration checking (such as export markets)
                double calFixedDemandValue = fuelMarketInfo->getDouble( "calFixedDemand", true );
                calFixedDemandValue = max( 0.0, calFixedDemandValue );
                calSupplyValue -= calFixedDemandValue;
                calDemandValue -= calFixedDemandValue;

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
                        vector<string> dependentFuels = (*fuelRelationshipMap)[ fuelIter->first ];

                        // Note loop goes to 1 + number of fuels so that basefuel is covered.
                        for ( unsigned int i = 0; i <= dependentFuels.size(); ++i) {
                            ++numberOfScaledValues; 

                            string fuelName;
                            if ( i == dependentFuels.size() ) {
                                fuelName = fuelIter->first; // Finish with base fuel in case that is used directly
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
                    mainLog << "Input calibration and fixed values not consistent. Adjusted value < 0 for fuel " << fuelIter->first << " in region " << name << endl ;
                    ILogger& calibrationLog = ILogger::getLogger( "calibration_log" );
                    calibrationLog.setLevel( ILogger::ERROR );
                    calibrationLog << "Input calibration and fixed values not consistent. Adjusted value < 0 for fuel " << fuelIter->first << " in region " << name << endl ;
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

/*! \brief Perform any sector level data consistancy checks
*
* \author Steve Smith
* \param period Model period
*/
void Region::checkData( const int period ) {
    for( SectorIterator currSector = supplySector.begin(); currSector != supplySector.end(); ++currSector ){
        (*currSector)->checkSectorCalData( period );
    }
    for ( DemandSectorIterator currSector = demandSector.begin(); currSector != demandSector.end(); ++currSector ) {
        (*currSector)->checkSectorCalData( period ); 
    }
}

//! Calculate regional demand for energy and other goods for all sectors.
void Region::calcEndUseDemand( const int period ) {
    if( !ensureGDP() ){
        return;
    }

    for ( DemandSectorIterator currDemSector = demandSector.begin(); currDemSector != demandSector.end(); ++currDemSector ){
        // calculate aggregate demand for end-use sector services
        // set fuel demand from aggregate demand for services
        (*currDemSector)->aggdemand( gdp.get(), period );
    }

}

//! Calculate regional emissions from resources.
void Region::calcEmissions( const int period ) {
    summary[period].clearemiss(); // clear emissions map

    // need to call emissions function but sum is not needed
    for ( unsigned int i = 0; i < supplySector.size(); i++ ) {
        supplySector[i]->emission(period);
        summary[period].updateemiss(supplySector[i]->getemission(period));
        emcoefInd[i].setemcoef(supplySector[i]->getemfuelmap(period), 
            supplySector[i]->getOutput(period));
    }
    for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
        demandSector[i]->emission(period);
        summary[period].updateemiss(demandSector[i]->getemission(period));
    }
    // Add CO2 emissions from the agsector.
    if( agSector.get() ){
        map<string,double> agEmissions;
        agEmissions[ "CO2NetLandUse" ] = agSector->getLandUseEmissions( period );
        summary[ period ].updateemiss( agEmissions );
    }

    if ( mLandAllocator.get() ) {
        mLandAllocator->calcEmission( name, gdp.get(), period );
        mLandAllocator->updateSummary( summary[ period ], period );
    }

}

/*! \brief Calculate regional emissions by fuel for reporting.
* \warning This function assumes emission has already been called, as this
*          function cannot clear the summary emissions.
* \param The global list of primary fuels.
* \param period The model period.
*/
void Region::calcEmissFuel( const list<string>& aPrimaryFuelList, const int period )
{
    map<string, double> fuelemiss; // tempory emissions by fuel

    for( list<string>::const_iterator fuelIter = aPrimaryFuelList.begin();
        fuelIter != aPrimaryFuelList.end(); ++fuelIter )
    {
        fuelemiss[ *fuelIter ] = summary[period].get_pemap_second( *fuelIter ) * primaryFuelCO2Coef[ *fuelIter ];
    }

    summary[period].updateemiss(fuelemiss); // add CO2 emissions by fuel
}

//! Calculate regional indirect emissions from intermediate and final demand sectors.
void Region::emissionInd( const int period ){
    for( SectorIterator currSector = supplySector.begin(); currSector != supplySector.end(); ++currSector ){
        (*currSector)->indemission( period, emcoefInd );
    }
    for( DemandSectorIterator currSector = demandSector.begin(); currSector != demandSector.end(); ++currSector ){
        (*currSector)->indemission( period, emcoefInd );
    }
}

//! Calculate total carbon tax paid in the region by all supply and demand
//! sectors.
void Region::calcTotalCarbonTaxPaid( const int period ) {
    carbonTaxPaid[ period ] = 0; // initialize total regional carbon taxes paid 
    // Loop through supply sectors.
    for( CSectorIterator currSector = supplySector.begin(); currSector != supplySector.end(); ++currSector ){
        carbonTaxPaid[ period ] += (*currSector)->getTotalCarbonTaxPaid( period );
    }
    // Loop through demand sectors.
    for( CDemandSectorIterator currSector = demandSector.begin(); currSector != demandSector.end(); ++currSector ){
        carbonTaxPaid[ period ] += (*currSector)->getTotalCarbonTaxPaid( period );
    }
}

//! Write all outputs to file.
void Region::csvOutputFile() const {
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

    // regional total carbon taxes paid
    fileoutput3(name," "," "," ","C tax revenue","Mil90$",carbonTaxPaid);

    // write total emissions for region
    for (int m=0;m<maxper;m++) {
        temp[m] = summary[m].get_emissmap_second("CO2"); 
    }
    fileoutput3(name," "," "," ","CO2 emiss","MTC",temp);

    // write ag emissions for region
    for (int m=0;m<maxper;m++) {
        temp[m] = summary[m].get_emissmap_second( "CO2NetLandUse" ); 
    }
    fileoutput3(name," "," "," ","Net Land Use CO2 emiss","MTC",temp);

    // TFE for this region
    for (int m=0;m<maxper;m++) {
        temp[m] = getTotFinalEnergy(m);
    }
    fileoutput3(name," "," "," ","TFE","EJ",temp);

    if ( mLandAllocator.get() ) {
        mLandAllocator->csvOutput( name );
    }

    // region primary energy consumption by fuel type
    typedef map<string,double>:: const_iterator CI;
    map<string,double> tpemap = summary[0].getpecons();
    for (CI pmap=tpemap.begin(); pmap!=tpemap.end(); ++pmap) {
        for (int m=0;m<maxper;m++) {
            temp[m] = summary[m].get_pemap_second(pmap->first);
        }
        fileoutput3(name,"Pri Energy","Consumption","",pmap->first,"EJ",temp);
    }

    // regional Pri Energy Production Total
    for (int m=0;m<maxper;m++) {
        temp[m] = summary[m].get_peprodmap_second("zTotal");
    }
    fileoutput3(name,"Pri Energy","total","","zTotal","EJ",temp);

    // write resource results to file
    for ( unsigned int i = 0; i < resources.size(); i++ ) 
        resources[i]->csvOutputFile( name );

    // write supply sector results to file
    for ( unsigned int i = 0; i < supplySector.size(); i++ ) {
        supplySector[i]->csvOutputFile();
        supplySector[i]->subsec_outfile();
    }

    // write end-use sector demand results to file
    for ( unsigned int i = 0; i < demandSector.size(); i++ ){
        demandSector[i]->csvOutputFile();	
        demandSector[i]->subsec_outfile();
    }
}

//! Write MiniCAM style outputs to file.
void Region::dbOutput( const list<string>& aPrimaryFuelList ) const {
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
    // regional total carbon taxes paid
    dboutput4(name,"General","CarbonTax","revenue","90US$",carbonTaxPaid);

    // CO2 emissions by fuel
    for( list<string>::const_iterator fuelIter = aPrimaryFuelList.begin();
        fuelIter != aPrimaryFuelList.end(); fuelIter++ )
    {
        for ( int m=0;m<maxper;m++) {
            temp[m] = summary[m].get_emissfuelmap_second( *fuelIter );
            temptot[m] += temp[m];
        }
        dboutput4(name,"CO2 Emiss","by Fuel",*fuelIter,"MTC",temp);
    }
    // add amount of geologic sequestration to emissions by fuel
    // todo change hardcoded category name
    for ( int m=0;m<maxper;m++) {
        // note the negative value for sequestered amount
        temp[m] = - summary[m].get_emissmap_second( "CO2sequestGeologic" );
        temptot[m] += temp[m];
    }
    dboutput4(name,"CO2 Emiss","by Fuel","geologic sequestration","MTC",temp);

    // add amount of sequestration from non-energy use to emissions by fuel
    // todo change hardcoded category name
    for ( int m=0;m<maxper;m++) {
        // note the negative value for sequestered amount
        temp[m] = - summary[m].get_emissmap_second( "CO2sequestNonEngy" );
        temptot[m] += temp[m];
    }
    dboutput4(name,"CO2 Emiss","by Fuel","non-energy use","MTC",temp);

    // total emissions by sector for region
    for ( int m=0;m<maxper;m++) {
        temp[m] = summary[m].get_emissmap_second("CO2");
    }
    // CO2 emissions by fuel and sector totals use same value
    dboutput4(name,"CO2 Emiss","by Fuel","zTotal","MTC",temptot);
    dboutput4(name,"CO2 Emiss","by Sector","zTotal","MTC",temp);

    // total ag sector emissions by sector for region
    for ( int m=0;m<maxper;m++) {
        temp[m] = summary[m].get_emissmap_second( "CO2NetLandUse" );
    }
    dboutput4(name, "CO2 Emiss", "Net Land Use", "total", "MTC", temp );

    // regional emissions for all greenhouse gases
    typedef map<string,double>:: const_iterator CI;
    map<string,double> temissmap = summary[0].getemission(); // get gases for period 0
    for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
        for ( int m=0;m<maxper;m++) {
            temp[m] = summary[m].get_emissmap_second(gmap->first);
        }
        dboutput4(name,"Emissions","by gas",gmap->first,"MTC",temp);
    }

    // regional total end-use service demand for all demand sectors
    for ( int m=0;m<maxper;m++) {
        temp[m] = 0; // initialize temp to 0 for each period
        for ( unsigned int i = 0; i < demandSector.size(); i++ ) { // sum for all period and demand sectors
            temp[m] += demandSector[i]->getService( m );
        }
    }
    dboutput4(name,"End-Use Service","by Sector","zTotal","Ser Unit",temp);

    // regional total end-use service demand without Tech Change for all demand sectors
    for ( int m=0;m<maxper;m++) {
        temp[m] = 0; // initialize temp to 0 for each period
        for ( unsigned int i = 0; i < demandSector.size(); i++ ) { // sum for all period and demand sectors
            temp[m] += demandSector[i]->getServiceWoTC( m );
        }
    }
    dboutput4(name,"End-Use Service","by Sector w/o TC","zTotal","Ser Unit",temp);

    // TFE for this region
    for ( int m=0;m<maxper;m++) {
        temp[m] = getTotFinalEnergy(m);
    }
    dboutput4(name,"Final Energy Cons","total","total","EJ",temp);

    // regional fuel consumption (primary and secondary) by fuel type
    map<string,double> tfuelmap = summary[0].getfuelcons();
    for (CI fmap=tfuelmap.begin(); fmap!=tfuelmap.end(); ++fmap) {
        for ( int m=0;m<maxper;m++) {
            temp[m] = summary[m].get_fmap_second(fmap->first);
        }
        if( fmap->first == "" ){
            cout << "Error: Empty fuel name." << endl;
            dboutput4(name,"Fuel Consumption","by fuel","No Fuelname","EJ",temp);
        }
        else {
            dboutput4(name,"Fuel Consumption","by fuel",fmap->first,"EJ",temp);
        }
    }

    // region primary energy consumption by fuel type
    map<string,double> tpemap = summary[0].getpecons();
    for (CI pmap=tpemap.begin(); pmap!=tpemap.end(); ++pmap) {
        for ( int m=0;m<maxper;m++) {
            temp[m] = summary[m].get_pemap_second(pmap->first);
        }
        dboutput4(name,"Pri Energy","Consumption by fuel",pmap->first,"EJ",temp);
    }

    // region primary energy trade by fuel type
    tpemap = summary[0].getpetrade();
    for (CI pmap=tpemap.begin(); pmap!=tpemap.end(); ++pmap) {
        for ( int m=0;m<maxper;m++) {
            temp[m] = summary[m].get_petrmap_second(pmap->first);
        }
        dboutput4(name,"Pri Energy","Trade by fuel",pmap->first,"EJ",temp);
    }

    // regional Pri Energy Production Total
    for ( int m=0;m<maxper;m++) {
        temp[m] = summary[m].get_peprodmap_second("zTotal");
    }
    dboutput4(name,"Pri Energy","Production by Sector","zTotal","EJ",temp);

    // write resource results to database
    for ( unsigned int i = 0; i < resources.size(); i++ ) {
        resources[i]->dbOutput( name );
    }
    // write supply sector results to database
    for ( unsigned int i = 0; i < supplySector.size(); i++ ) {
        supplySector[i]->dbOutput();
    }
    // write end-use sector demand results to database
    for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
        demandSector[i]->dbOutput();
    }
}

//! Initialize the market prices for the agricultural products.
void Region::initializeAgMarketPrices( const vector<double>& pricesIn ) { 
    agSector->initMarketPrices( name, pricesIn );
}

//! update regional summaries for reporting
void Region::updateSummary( const list<string>& aPrimaryFuelList, const int period ) {
    calcEmissions( period );
    calcTotalCarbonTaxPaid( period );

    summary[period].clearpeprod();
    summary[period].clearfuelcons();

    for ( unsigned int i = 0; i < resources.size(); i++ ) {
        summary[period].initpeprod(resources[i]->getName(),resources[i]->getAnnualProd(period));
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
const Summary& Region::getSummary( const int period ) const {
    return summary[ period ];
}

/*! \brief Set a policy for the region.
* \details Searches through the list of the region's taxes for a tax with the
*          same name as aTax. If the tax is found, it is deleted and replaced
*          with aTax. Otherwsie aTax is added to the end of the tax vector.
* \param aTax Tax to add.
*/
void Region::setTax( const GHGPolicy* aTax ){
    /*! \pre Tax is not null. */
    assert( aTax );

    // Check if the tax is applicable.
    if( !aTax->isApplicable( name ) ){
        return;
    }

    GHGPolicy* insertedTax = 0;
    // Search for an existing policy to replace.
    for( unsigned int i = 0; i < mGhgPolicies.size(); i++ ){
        if( mGhgPolicies[ i ]->getName() == aTax->getName() ){
            delete mGhgPolicies[ i ];
            // Create a copy of the tax.
            mGhgPolicies[ i ] = insertedTax = aTax->clone();
        }
    }

    // Need to insert the tax.
    if( !insertedTax ){
        insertedTax = aTax->clone();
        mGhgPolicies.push_back( insertedTax );
    }

    // Initialize the tax.
    insertedTax->completeInit( name );
}

/*! \brief A function to generate a ghg emissions quantity curve based on an
*          already performed model run.
* \details This function used the information stored in it to create a curve,
*          with each datapoint containing a time period and an amount of gas
*          emissions. These values are retrieved from the emissions.
* \note The user is responsible for deallocating the memory in the returned
*       Curve.
* \author Josh Lurz
* \param ghgName The name of the ghg to create a curve for.
* \return A Curve object representing ghg emissions quantity by time period.
*/
const Curve* Region::getEmissionsQuantityCurve( const string& ghgName ) const {
    /*! \pre The run has been completed. */
    const Modeltime* modeltime = scenario->getModeltime();

    auto_ptr<ExplicitPointSet> emissionsPoints( new ExplicitPointSet() );

    for( int i = 0; i < scenario->getModeltime()->getmaxper(); i++ ) {
        XYDataPoint* currPoint = new XYDataPoint( modeltime->getper_to_yr( i ), summary[ i ].get_emissmap_second( ghgName ) );
        emissionsPoints->addPoint( currPoint );
    }

    Curve* emissionsCurve = new PointSetCurve( emissionsPoints.release() );
    emissionsCurve->setTitle( ghgName + " emissions curve" );
    emissionsCurve->setXAxisLabel( "year" );
    emissionsCurve->setYAxisLabel( "emissions quantity" );

    return emissionsCurve;
}

/*! \brief A function to generate a ghg emissions price curve based on an
*          already performed model run.
* \details This function used the information stored in it to create a curve,
*          with each datapoint containing a time period and the price gas
*          emissions. These values are retrieved from the marketplace. 
* \note The user is responsible for deallocating the memory in the returned
*       Curve.
* \author Josh Lurz
* \param ghgName The name of the ghg to create a curve for.
* \return A Curve object representing the price of ghg emissions by time period. 
*/
const Curve* Region::getEmissionsPriceCurve( const string& ghgName ) const {
    /*! \pre The run has been completed. */
    const Modeltime* modeltime = scenario->getModeltime();
    const Marketplace* marketplace = scenario->getMarketplace();

    auto_ptr<ExplicitPointSet> emissionsPoints( new ExplicitPointSet() );

    for( int i = 0; i < modeltime->getmaxper(); i++ ) {
        XYDataPoint* currPoint = new XYDataPoint( modeltime->getper_to_yr( i ), marketplace->getPrice( ghgName, name, i ) );
        emissionsPoints->addPoint( currPoint );
    }

    Curve* emissionsCurve = new PointSetCurve( emissionsPoints.release() );
    emissionsCurve->setTitle( ghgName + " emissions tax curve" );
    emissionsCurve->setXAxisLabel( "year" );
    emissionsCurve->setYAxisLabel( "emissions tax" );

    return emissionsCurve;
}

/*! \brief Initialize the marketplaces in the base year to get initial demands
*          through each sector.
* \author Pralit Patel
* \param period The period is usually the base period
*/
void Region::updateMarketplace( const int period ){
    for( vector<Sector*>::iterator currSec = supplySector.begin(); currSec != supplySector.end(); ++currSec ){
        (*currSec)->updateMarketplace( period );
    }
}

/*! \brief Function to finalize objects after a period is solved.
* \details This function is used to calculate and store variables which are only
*          needed after the current period is complete. 
* \param aPeriod The period to finalize.
* \todo Finish this function.
* \author Josh Lurz
*/
void Region::finalizePeriod( const int aPeriod ){
    // Finalize sectors.
    for( SectorIterator sector = supplySector.begin(); sector != supplySector.end(); ++sector ){
        (*sector)->finalizePeriod( aPeriod );
    }
}

//! update regional output tables for reporting
void Region::updateAllOutputContainers( const int period ) { 
}

// TODO: These should be removed.
/*! \brief For outputing SGM data to a flat csv File, wouldn't need to do
*          anything for miniCAM.
* \author Pralit Patel
* \param period 
*/
void Region::csvSGMOutputFile( ostream& aFile, const int period ) const {
}
/*! \brief For outputing general SGM results to a flat csv File 
* \author Sonny Kim
*/
void Region::csvSGMGenFile( ostream& aFile ) const {
}

/*! \brief Check whether the GDP object exists and report a warning if it does not.
* \return Whether the GDP object exists.
* \author Josh Lurz
*/
bool Region::ensureGDP() const {
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
bool Region::ensureDemographics() const {
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
void Region::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitRegion( this, aPeriod );

    // Visit demographics object.
    if( demographic.get() ){
        demographic->accept( aVisitor, aPeriod );
    }

    // Visit gdp object.
    if( gdp.get() ){
        gdp->accept( aVisitor, aPeriod );
    }

    // for national account
    if( aPeriod == -1 ){
        for( unsigned int i = 0; i < nationalAccount.size(); ++i ){
            nationalAccount[ i ].accept( aVisitor, aPeriod );
        }
    }
    else {
        nationalAccount[ aPeriod ].accept( aVisitor, aPeriod );
    }
    // loop for resources.
    for( CResourceIterator currResource = resources.begin(); currResource != resources.end(); ++currResource ){
        (*currResource)->accept( aVisitor, aPeriod );
    }
    // loop for supply sectors
    for( CSectorIterator currSec = supplySector.begin(); currSec != supplySector.end(); ++currSec ){
        (*currSec)->accept( aVisitor, aPeriod );
    }
    // loop for demand sectors.
    for( CDemandSectorIterator currDem = demandSector.begin(); currDem != demandSector.end(); ++currDem ){
        (*currDem)->accept( aVisitor, aPeriod );
    }
    aVisitor->endVisitRegion( this, aPeriod );
}
