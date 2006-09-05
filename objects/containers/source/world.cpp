/*! 
* \file world.cpp
* \ingroup Objects
* \brief world class source file.
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <vector>
#include <map>
#include <algorithm>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "util/base/include/xml_helper.h"
#include "containers/include/world.h"
#include "containers/include/region.h"
#include "containers/include/region_cge.h"
#include "sectors/include/ag_sector.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/configuration.h"
#include "util/base/include/util.h"
#include "util/base/include/summary.h"
#include "util/curves/include/curve.h"
#include "util/curves/include/point_set_curve.h"
#include "util/curves/include/point_set.h"
#include "util/curves/include/explicit_point_set.h"
#include "util/curves/include/xy_data_point.h"
#include "solution/util/include/calc_counter.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/ivisitor.h"
#include "climate/include/iclimate_model.h"
// Could hide with a factory method.
#include "climate/include/magicc_model.h"
#include "util/base/include/hash_map.h"
#include "util/base/include/atom_registry.h"
#include "emissions/include/emissions_summer.h"
#include "technologies/include/global_technology_database.h"

using namespace std;
using namespace xercesc;

extern "C" { void _stdcall AG2INITC( double[14][12] ); };
extern Scenario* scenario;

//! Default constructor.
World::World():
doCalibrations( true ),
calcCounter( 0 )
{
}

//! World destructor. 
World::~World(){
    clear();
}

//! Helper member function for the destructor. Performs memory deallocation. 
void World::clear(){
    for ( RegionIterator regionIter = regions.begin(); regionIter != regions.end(); regionIter++ ) {
        delete *regionIter;
    }
}

//! parses World xml object
void World::XMLParse( const DOMNode* node ){
    // assume we are passed a valid node.
    assert( node );

    // get all the children.
    DOMNodeList* nodeList = node->getChildNodes();

    for( unsigned int i = 0;  i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        else if( nodeName == GlobalTechnologyDatabase::getXMLNameStatic() ) {
            parseSingleNode( curr, globalTechDB, new GlobalTechnologyDatabase() );
        }
        // MiniCAM regions
        else if( nodeName == Region::getXMLNameStatic() ){
            parseContainerNode( curr, regions, regionNamesToNumbers, new Region() );
        }
		// Read in parameters for climate model
        else if( nodeName == MagiccModel::getXMLNameStatic() ){
            parseSingleNode( curr, mClimateModel , new MagiccModel( scenario->getModeltime() ) );
        }
		// SGM regions
        else if( nodeName == RegionCGE::getXMLNameStatic() ){
            parseContainerNode( curr, regions, regionNamesToNumbers, new RegionCGE() );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing World." << endl;
        }
    }
}

/*! \brief Complete the initialization
*
* This routine is only called once per model run
*
* \author Josh Lurz
*/
void World::completeInit() {

    // Initialize the region lookup hashmap.
    createFastLookupMap();

    // Finish initializing all the regions.
    for( RegionIterator regionIter = regions.begin(); regionIter != regions.end(); regionIter++ ) {
        ( *regionIter )->completeInit( globalTechDB.get() );
    }

    // Initialize AgLU
    Configuration* conf = Configuration::getInstance();
    if( conf->getBool( "agSectorActive" ) ) {
        initAgLu();
    }

    //If none has been read in, instantiate the default climate model
    if ( !mClimateModel.get() ) {
        mClimateModel.reset( new MagiccModel( scenario->getModeltime() ) );
    }
    
    //Inititalize Climate Model
    mClimateModel->completeInit( scenario->getName() );
}
/*! \brief Initialize the region partial derivative calculation hash map.
* \details Constructs a mapping of region Atom to index within the region
*          vector. This allows for rapid lookups of region numbers during
*          derivative calculations where not all regions are calculated at once.
*          This must be done before markets are created since the markets will
*          store the atoms of the regions that they contain.
*/
void World::createFastLookupMap(){
    // Construct the hashmap as 20 percent full for performance.
    mRegionLookupMap.reset( new FastRegionLookupMap( regions.size() * 5 ) );

    // Add each region id to number mapping to the hashmap.
    for( unsigned int i = 0; i < regions.size(); ++i ){
        // An atom does not need to be created if this is the second or later run
        // of a batch of scenarios.
        const objects::Atom* regionID = objects::AtomRegistry::getInstance()->findAtom( regions[ i ]->getName() );
        
        if( !regionID ){
            // Construct an atom for the region name. The Atom will automatically be
            // registered.
            regionID = new objects::Atom( regions[ i ]->getName() );
        }

        // Add an entry to the hashmap for the id.
        mRegionLookupMap->insert( make_pair( regionID, i ) );
    }
}

//! Initialize the AgLu model.
void World::initAgLu() {
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Initializing agLU..." << endl;

    double prices[ 14 ][ 12 ]; 
#if(__HAVE_FORTRAN__)
    AG2INITC( prices );
#endif
    for ( unsigned int j = 0; j < regions.size(); j++ ) {
        vector<double> tempVec( 12 );
        for ( int k = 0; k < AgSector::getNumAgMarkets(); k++ ) {
            tempVec[ k ] = prices[ j ][ k ];
        }
        regions[ j ]->initializeAgMarketPrices( tempVec );
    }
}

//! Write out datamembers to XML output stream.
void World::toInputXML( ostream& out, Tabs* tabs ) const {

    XMLWriteOpeningTag ( getXMLNameStatic(), out, tabs );

    if( globalTechDB.get() ) {
        globalTechDB->toInputXML( out, tabs );
    }

    for( CRegionIterator i = regions.begin(); i != regions.end(); i++ ){
        ( *i )->toInputXML( out, tabs );
    }
    
    // Climate model parameters
    if ( mClimateModel.get() ) {
        mClimateModel->toInputXML( out, tabs );
    }

    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLNameStatic(), out, tabs );
}

//! Write out XML for debugging purposes.
/*! \warning This only call Region::toInputXML for the US. */
void World::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    XMLWriteOpeningTag ( getXMLNameStatic(), out, tabs, "", period );

    // write the xml for the class members.

    scenario->getMarketplace()->toDebugXML( period, out, tabs );

    if( globalTechDB.get() ) {
        globalTechDB->toDebugXML( period, out, tabs );
    }

    // Only print debug XML information for the specified region to avoid
    // unmanagably large XML files.
    const static string debugRegion = Configuration::getInstance()->getString( "debug-region", "USA" );
    for( CRegionIterator i = regions.begin(); i != regions.end(); i++ ) {
        if( ( *i )->getName() == debugRegion ){
            ( *i )->toDebugXML( period, out, tabs );
        }
    }

    // Climate model parameters
    if ( !mClimateModel.get() ) {
        mClimateModel->toDebugXML( period, out, tabs );
    }

    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLNameStatic(), out, tabs );
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
const std::string& World::getXMLNameStatic() {
    const static string XML_NAME = "world";
    return XML_NAME;
}

/*! \brief Returns the name of the World.
* \details Although there is only one World in the model so a name is
*          unnecessary, this is needed for compatibility with the IParsable
*          interface. The function returns instead the XML name.
* \note In the future World object may have read-in names.
* \return The name of the world.
*/
const string& World::getName() const {
    return getXMLNameStatic();
}

//! initialize anything that won't change during the calculation
/*! Examples: share weight scaling due to previous calibration, 
* cumulative technology change, etc.
*/
void World::initCalc( const int period ) {
    Configuration* conf = Configuration::getInstance();
    if( conf->getBool( "CalibrationActive" ) ){
        // cal consistency check needs to be before initCalc so calInput values have already been scaled if necessary
        // If any values are scaled, then this is checked at least one more time to see if further scalings are necessary
        const unsigned int MAX_CALCHECK_CALCS = 10;
        unsigned int calCheckIterations = 0;
        
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::DEBUG );
        mainLog << "Performing world calibration consistency check " << endl;
        while ( calCheckIterations < MAX_CALCHECK_CALCS && checkCalConsistancy( period ) ) {
            ++calCheckIterations;
        }
    }
    // Reset the calc counter.
    calcCounter->startNewPeriod();
    for( vector<Region*>::iterator i = regions.begin(); i != regions.end(); i++ ){
        // Add supplies and demands to the marketplace in the base year for checking data consistency
        // and for getting demand and supply totals.
        // Need to update markets here after markets have been null by scenario.
        // TODO: This should be combined with check data.
        if( period == 0 ){
            ( *i )->updateMarketplace( period );
        }
        ( *i )->initCalc( period );
    }
}

/*! \brief Assure that calibrated inputs and outputs are consistant.
*
* Function determines if calibrated supplies and demands are consistent and 
* scales demands if necessary to make this so. The calsupply and demand values are put into corresponding
* values in marketInfo. These are then used to determine transitive fixed demands (for example the calibrated
* demand for electricity due to a calibrated demand for elec_T&D_buildings).
* If all supply and demand for a good are calibrated or otherwise fixed then calibration 
* demand values are adjusted for consistency
* Each of these steps is called separately for each region. This, and using markets as the basis of calculating 
* cal supplies and demands means that both regional and global (or anything in-between) markets are handled 
* appropriately.
*
* \author Steve Smith
* \warning A simultaneity in the base year may or may not be handled correctly.
* \return Whether any values were adjusted this period.
*/
bool World::checkCalConsistancy( const int period ) {

    // Don't check for this unless calibration is active
    Configuration* conf = Configuration::getInstance();
    if( conf->getBool( "CalibrationActive" ) ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        
        //Setup for checking by initializing fixed supplies and demands counter to null value
         for( RegionIterator i = regions.begin(); i != regions.end(); ++i ){
            ( *i )->initializeCalValues( period );
        }

        //Setup for checking by adding up all fixed supplies and demands
         for( RegionIterator i = regions.begin(); i != regions.end(); ++i ){
            ( *i )->setCalSuppliesAndDemands( period );
        }

        // Now walk through sectors in each region to calculate dependent (transitive) outputs 
        bool calIsDone = false;
        int iteration = 0;
        while (!calIsDone ) {
            calIsDone = true;
             for ( RegionIterator i = regions.begin(); i != regions.end(); ++i ){
                // If any region returns status as not done (false) then save that status
                if ( !( *i )->setImpliedCalInputs( period ) ) {
                    calIsDone = false;
                }
            }
            ++iteration;
            // In case user sets up infinite loop in input, break.
            if (iteration > 50 ) {
                mainLog.setLevel( ILogger::ERROR );
                mainLog << "Calibration check stuck in large loop. Aborting. " << endl;
                return true;
            }
        }
        
        int numberOfScaledValues = 0;
        
        // Scale calibrated input values once all supplies and demands have been counted
        for( RegionIterator i = regions.begin(); i != regions.end(); ++i ){
            numberOfScaledValues += ( *i )->scaleCalInputs( period );
        }
        
        if ( numberOfScaledValues > 0 ) {
            mainLog.setLevel( ILogger::DEBUG );
            mainLog << numberOfScaledValues << " markets had demand calibration values scaled. " << endl;
        }
        
        return numberOfScaledValues > 0; // If any values were scaled, set flag to true
    } 
    // else branch for calibrations not active
    else {
        return false;
    }
}

/*! \brief Calculate supply and demand and emissions for all regions.
* \details Loops through regions so that regions can define their own
*          calculation ordering.
* \param aPeriod Period to calculate.
* \param aRegionsToCalc Optional subset of the regions to calculate, if excluded
*        all regions will be calculated.
*/
void World::calc( const int aPeriod, const AtomVector& aRegionsToCalc ) {   
    // Get the list of valid region numbers to solve.
    const vector<unsigned int> regionNumbersToCalc = getRegionIndexesToCalculate( aRegionsToCalc );

    /*! \invariant The number of regions to calculate must be between 0 and the
    *              number of regions inclusive. 
    */
    assert( regionNumbersToCalc.size() <= regions.size() );

    // Increment the world.calc count based on the number of regions to solve. 
    if( calcCounter ){
        calcCounter->incrementCount( static_cast<double>( regionNumbersToCalc.size() ) / static_cast<double>( regions.size() ) );
    }
    
    // Perform calculation loop on each region to calculate. 
    for ( vector<unsigned int>::const_iterator currIndex = regionNumbersToCalc.begin(); currIndex != regionNumbersToCalc.end(); ++currIndex ) {
        regions[ *currIndex ]->calc( aPeriod, doCalibrations );
    }
}

//! Update all summary information for reporting
// Orginally in world.calc, removed to call only once after solved
void World::updateSummary( const list<string> aPrimaryFuelList, const int period ) {
    for( RegionIterator i = regions.begin(); i != regions.end(); i++ ){
        ( *i )->updateSummary( aPrimaryFuelList, period );
        ( *i )->updateAllOutputContainers( period );
    }
}

/*! Calculates the global emissions.
*/
void World::runClimateModel() {
    // Declare visitors which will aggregate emissions by period.
    EmissionsSummer co2Summer( "CO2" );
    EmissionsSummer co2LandUseSummer( "CO2NetLandUse" );

    // The Climate model reads in data for the base period, so skip passing it in.
    for( int period = 1; period < scenario->getModeltime()->getmaxper(); ++period){

        const double MMT_TO_TG = 1000;
        
        // Sum the emissions for the period.
        accept( &co2Summer, period );
        accept( &co2LandUseSummer, period );

        // Only set emissions if they are valid. If these are not set
        // MAGICC will use the default values.
        if( co2Summer.areEmissionsSet( period ) ){
            mClimateModel->setEmissions( "CO2", period,
                                          co2Summer.getEmissions( period )
                                          / MMT_TO_TG );
        }

        if( co2LandUseSummer.areEmissionsSet( period ) ){
            mClimateModel->setEmissions( "CO2NetLandUse", period,
                                          co2LandUseSummer.getEmissions( period )
                                          / MMT_TO_TG );
        }
    }
    // Run the model.
    mClimateModel->runModel();
}

//! write results for all regions to file
void World::csvOutputFile() const {

    // Write global data
    csvGlobalDataFile();
    
    for( CRegionIterator i = regions.begin(); i != regions.end(); i++ ){
        ( *i )->csvOutputFile();
    }
}

//! write global results to file
void World::csvGlobalDataFile() const {
    const int maxper = scenario->getModeltime()->getmaxper();
    vector<double> temp(maxper);
    // function protocol
    void fileoutput3(string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);

    // write total emissions for World
    for ( int m = 0; m < maxper; m++ ){
        // Sum emissions by period.
        for( CRegionIterator iter = regions.begin(); iter != regions.end(); ++iter ) {
            // This interface needs to be fixed.
            temp[ m ] += ( *iter )->getSummary( m ).get_emissmap_second( "CO2" );
        }
    }
    fileoutput3( "global"," "," "," ","CO2 emiss","MTC",temp);

    // Write out concentrations.
    mClimateModel->printFileOutput();
}

//! MiniCAM style output to database
void World::dbOutput( const list<string>& aPrimaryFuelList ) const {
    // Write out concentrations
    mClimateModel->printDBOutput();

    // call regional output
    for( CRegionIterator i = regions.begin(); i != regions.end(); i++ ){
        ( *i )->dbOutput( aPrimaryFuelList );
    }
}

//! turn on calibrations
void World::turnCalibrationsOn() {
    doCalibrations = true;
}

//! turn off calibrations
void World::turnCalibrationsOff(){
    doCalibrations = false;
}

//! return calibration setting
bool World::getCalibrationSetting() const {
    return doCalibrations;
}


/*! \brief Test to see if calibration worked for all regions
*
* Compares the sum of calibrated + fixed values to output of each sector.
*
* \author Steve Smith
* \param period Model period
* \param calAccuracy Calibration tolerance.
* \param printWarnings flag to turn on logging of warnings if calibrations are
*        not accurate
* \return Boolean true if calibration is ok.
*/
bool World::isAllCalibrated( const int period, double calAccuracy, const bool printWarnings ) const {
    bool isAllCalibrated = true;
    for( CRegionIterator i = regions.begin(); i != regions.end(); i++ ){
        isAllCalibrated &= ( *i )->isAllCalibrated( period, calAccuracy, printWarnings );
    }
    return isAllCalibrated;
}

/*! \brief This function returns a special mapping of strings to ints for use in
*          the outputs. 
* \details This map is created such that global maps to zero, region 0 maps to
*          1, etc. It is similiar to the regionNamesToNumbers map but has the
*          global element and each region number in the regionMap is 1 + the
*          number in the regionNamesToNumbers map.
* \warning This function should only be used by the database output functions. 
* \return The map of region names to numbers.
*/
const map<string,int> World::getOutputRegionMap() const {
    map<string,int> regionMap;

    for ( unsigned int i = 0; i < regions.size(); i++ ) {
        regionMap[regions[i]->getName()] = i+1; // start index from 1
    }
    // hardcode for now
    regionMap["global"] = 0;
    return regionMap;
}

/*! \brief This function returns a vector of IDs for all regions that exist in
*          the world.
* \details This function creates a vector of region IDs in the same order in
*          which they exist in the world.
* \return A constant vector of region IDs.
*/
const World::AtomVector World::getRegionIDs() const {
    AtomVector regionIDs;
    
    // Iterate over the regions and lookup the ID for each region.
    for( CRegionIterator i = regions.begin(); i != regions.end(); ++i ) {
        // Find the atom for the region name.
        const objects::Atom* regionID = objects::AtomRegistry::getInstance()->findAtom( (*i)->getName() );
        /*! \invariant The region atom has already been created, otherwise the
        *              atom was not registered when the fast lookup map was
        *              created. 
        */
        assert( regionID );
        
        /*! \invariant The ID of region atom found is equal to the region name. */
        assert( regionID->getID() == (*i)->getName() );
        regionIDs.push_back( regionID );
    }
    /*! \post The size of the region ID vector is equal to the size of the
    *         region vector. 
    */
    assert( regionIDs.size() == regions.size() );
    return regionIDs;
}

/*! \brief Set a fixed tax for all regions.
* \param aTax Tax.
*/
void World::setTax( const GHGPolicy* aTax ){
    for( RegionIterator iter = regions.begin(); iter != regions.end(); ++iter ){
        (*iter)->setTax( aTax );
    }
}

/*! \brief Get the climate model.
* \return The climate model.
*/
const IClimateModel* World::getClimateModel() const {
    return mClimateModel.get();
}

/*! \brief A function to generate a series of ghg emissions quantity curves based on an already performed model run.
* \details This function used the information stored in it to create a series of curves, one for each region,
* with each datapoint containing a time period and an amount of gas emissions.
* \note The user is responsible for deallocating the memory in the returned Curves.
* \author Josh Lurz
* \param ghgName The name of the ghg to create a set of curves for.
* \return A map with keys as region names and Curves as values representing the quantity of ghg emissions by time period.
*/
const map<const string,const Curve*> World::getEmissionsQuantityCurves( const string& ghgName ) const {
    /*! \pre The run has been completed. */
    const string GLOBAL_NAME = "global";

    map<const string,const Curve*> emissionsQCurves;

    for( CRegionIterator rIter = regions.begin(); rIter != regions.end(); rIter++ ){
        emissionsQCurves[ (*rIter)->getName() ] = (*rIter)->getEmissionsQuantityCurve( ghgName );
    }

    // Add an entry for the global emissions. Should do this better. 
    ExplicitPointSet* globalQs = new ExplicitPointSet();
    const Marketplace* marketplace = scenario->getMarketplace();
    const Modeltime* modeltime = scenario->getModeltime();

    for( int per = 0; per < modeltime->getmaxper(); per++ ){
        globalQs->addPoint( new XYDataPoint( modeltime->getper_to_yr( per ), marketplace->getDemand( ghgName, "USA", per ) ) );
    }
    emissionsQCurves[ GLOBAL_NAME ] = new PointSetCurve( globalQs );
    return emissionsQCurves;
}

/*! \brief A function to generate a series of ghg emissions price curves based on an already performed model run.
* \details This function used the information stored in it to create a series of curves, one for each period,
* with each datapoint containing a time period and the price gas emissions. 
* \note The user is responsible for deallocating the memory in the returned Curves.
* \author Josh Lurz
* \param ghgName The name of the ghg to create a set of Curves for.
* \return A map with keys as region names and Curves as values representing the price of ghg emissions by time period. 
*/
const map<const string,const Curve*> World::getEmissionsPriceCurves( const string& ghgName ) const {
    /*! \pre The run has been completed. */
    map<const string,const Curve*> emissionsPCurves;
    const string GLOBAL_NAME = "global";
    
    for( CRegionIterator rIter = regions.begin(); rIter != regions.end(); rIter++ ){
        emissionsPCurves[ (*rIter)->getName() ] = (*rIter)->getEmissionsPriceCurve( ghgName );
    }

    // Add an entry for the global emissions. Should do this better. 
    ExplicitPointSet* globalQs = new ExplicitPointSet();
    const Marketplace* marketplace = scenario->getMarketplace();
    const Modeltime* modeltime = scenario->getModeltime();
    for( int per = 0; per < modeltime->getmaxper(); per++ ){
        globalQs->addPoint( new XYDataPoint( modeltime->getper_to_yr( per ), marketplace->getPrice( ghgName, "USA", per ) ) );
    }
    emissionsPCurves[ GLOBAL_NAME ] = new PointSetCurve( globalQs );
    return emissionsPCurves;
}

//! Set a pointer to an object which tracks the number of calls to world.calc that are made.
void World::setCalcCounter( CalcCounter* calcCounter ){
    /*! \pre The calcCounter is not null. */
    assert( calcCounter );
    this->calcCounter = calcCounter;
}

/*! \brief Protected function which takes a listing of region names to calculate
*          and returns a list of region indexes to calculate. 
* \details This function translates a passed in list of region names to solve to
*          a vector of valid region numbers to solve. If passed an empty list,
*          it will return the full list of region numbers to solve, this is the
*          default. It checks whether the region name is valid, but does not
*          check for duplicates.
* \param aRegionsToCalc A vector of region names to calculate.
* \return A vector of region numbers to solve.
*/
const vector<unsigned int> World::getRegionIndexesToCalculate( const AtomVector& aRegionsToCalc ){
    vector<unsigned int> regionNumbersToCalc;
    // Check for the empty list of names, return the full list of region numbers.
    if ( aRegionsToCalc.empty() ) {
        for( unsigned int regionNumber = 0; regionNumber < regions.size(); ++regionNumber ) {
            regionNumbersToCalc.push_back( regionNumber );
        }
    }
    // Check if each name is valid and add its region number to the vector if it is. 
    else {
        for( AtomVector::const_iterator regionID = aRegionsToCalc.begin();
             regionID != aRegionsToCalc.end(); ++regionID )
        {
            // Lookup the region atom in the world's fast lookup map.
            FastRegionLookupMap::const_iterator iter = mRegionLookupMap->find( *regionID );

            /*! \invariant The region ID was found otherwise the solution
            *              mechanism is instructing the world to calculate a
            *              non-existant region. 
            */
            assert( iter != mRegionLookupMap->end() );

            /*! \invariant The ID of the atom is equal to the region name of the
            *              region at the index. 
            */
            assert( (*regionID)->getID() == regions[ iter->second ]->getName() );
            
            // Add the index to the list of indexes to calculate.
            regionNumbersToCalc.push_back( iter->second );
        }
    }

    /*! \post The number of regions to calculate is between 0 and the total
    *         number of regions inclusive.
    */
    assert( regionNumbersToCalc.size() <= regions.size() );

    return regionNumbersToCalc;
}

/*! \brief Function to finalize objects after a period is solved.
* \details This function is used to calculate and store variables which are only needed after the current
* period is complete. 
* \param aPeriod The period to finalize.
* \author Josh Lurz
*/
void World::finalizePeriod( const int aPeriod ){
    // Finalize sectors.
    for( RegionIterator region = regions.begin(); region != regions.end(); ++region ){
        (*region)->finalizePeriod( aPeriod );
    }
}

void World::csvSGMOutputFile( ostream& aFile, const int period ) const {
    for( CRegionIterator rIter = regions.begin(); rIter != regions.end(); ++rIter ){
        ( *rIter )->csvSGMOutputFile( aFile, period );
    }
}

void World::csvSGMGenFile( ostream& aFile ) const {
    for( CRegionIterator rIter = regions.begin(); rIter != regions.end(); ++rIter ){
        ( *rIter )->csvSGMGenFile( aFile );
    }
}

/*! \brief Update a visitor for the World.
* \param aVisitor Visitor to update.
* \param aPeriod Period to update.
*/
void World::accept( IVisitor* aVisitor, const int aPeriod ) const {
	aVisitor->startVisitWorld( this, aPeriod );
	
	// Visit the marketplace
	scenario->getMarketplace()->accept( aVisitor, aPeriod );
	
	// Visit the climate model.
	mClimateModel->accept( aVisitor, aPeriod );

	// loop for regions
	for( CRegionIterator currRegion = regions.begin(); currRegion != regions.end(); ++currRegion ){
		(*currRegion)->accept( aVisitor, aPeriod );
    }

    aVisitor->endVisitWorld( this, aPeriod );
}
