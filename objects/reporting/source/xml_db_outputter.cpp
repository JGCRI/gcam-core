/*
* This software, which is provided in confidence, was prepared by employees of
* Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
* Battelle has certain unperfected rights in the software which should not be
* copied or otherwise disseminated outside your organization without the express
* written authorization from Battelle. All rights to the software are reserved
* by Battelle. Battelle makes no warranty, express or implied, and assumes no
* liability or responsibility for the use of this software.
*/

/*!
* \file xml_db_outputter.cpp
* \ingroup Objects
* \brief The XMLDBOutputter class source file.
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"

// Only compile this code if the XML database is turned on.
#if( __USE_XML_DB__ )
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "containers/include/world.h"
#include "containers/include/region.h"
#include "containers/include/region_minicam.h"
#include "containers/include/region_cge.h"
#include "containers/include/iinfo.h"
#include "resources/include/resource.h"
#include "sectors/include/sector.h"
#include "sectors/include/subsector.h"
#include "sectors/include/building_dmd_subsector.h"
#include "technologies/include/technology.h"
#include "emissions/include/aghg.h"
#include "util/base/include/model_time.h"
#include "containers/include/output_meta_data.h"
#include "marketplace/include/marketplace.h"
#include "marketplace/include/market.h"
#include "climate/include/iclimate_model.h"
#include "climate/include/magicc_model.h"
#include "resources/include/subresource.h"
#include "resources/include/grade.h"
#include "demographics/include/demographic.h"
#include "demographics/include/population.h"
#include "demographics/include/population_mini_cam.h"
#include "demographics/include/population_sgm_rate.h"
#include "demographics/include/population_sgm_fixed.h"
#include "demographics/include/age_cohort.h"
#include "demographics/include/gender.h"
#include "util/base/include/configuration.h"
#include "containers/include/gdp.h"
#include "land_allocator/include/land_leaf.h"
#include "emissions/include/icarbon_calc.h"
#include "util/base/include/atom.h"
#include "land_allocator/include/land_node.h"
#include "technologies/include/ioutput.h"
#include "technologies/include/base_technology.h"
#include "technologies/include/expenditure.h"
#include "technologies/include/production_technology.h"
#include "functions/include/input.h"
#include "consumers/include/household_consumer.h"
#include "consumers/include/govt_consumer.h"
#include "consumers/include/trade_consumer.h"
#include "consumers/include/invest_consumer.h"
#include "sectors/include/factor_supply.h"
#include "sectors/include/more_sector_info.h"
#include "util/base/include/util.h"
#include "reporting/include/indirect_emissions_calculator.h"
#include "technologies/include/default_technology.h"
#include "technologies/include/iproduction_state.h"
#include "util/base/include/auto_file.h"

// Whether to write a text file with the contents that are to be inserted
// into the XML database.
#define DEBUG_XML_DB 0

#ifdef DEBUG_XML_DB
#include "util/base/include/auto_file.h"
#endif

#include <ctime>
#include "dbxml/DbXml.hpp"

#include <string>

#include "reporting/include/xml_db_outputter.h"

extern Scenario* scenario; // for modeltime

// TODO: Remove global time variable.
extern time_t gGlobalTime;

using namespace DbXml;
using namespace std;



XMLDBOutputter::DBContainer::DBContainer():mDBEnvironment( 0 ){
}

/*! \brief Destructor for the database container.
* \details This is a workaround for memory deallocation problems. Forcing the
*          container to deallocate first is necessary, and would not work with
*          simple class ordering.
*/
XMLDBOutputter::DBContainer::~DBContainer() {
    mContainerWrapper.reset();
    mManager.reset();
}

/*! \brief Constructor
* \param aContainer XmlContainer to wrap.
*/
XMLDBOutputter::DBContainer::XMLContainerWrapper::XMLContainerWrapper( DbXml::XmlContainer aContainer )
:mContainer( aContainer )
{}

/*! \brief Constructor
*/
XMLDBOutputter::XMLDBOutputter():
mTabs( new Tabs ),
mGDP( 0 )
{}

/*!
 * \brief Destructor
 * \note This needs to be explicitly defined for incompletely defined members
 *       to be deleted correctly.
 */
XMLDBOutputter::~XMLDBOutputter(){
}

/*! \brief Write the output to the database.
* \details Write the accumulated output to the database. This will open the
*          database, create a unique ID for the scenario and a container with
*          that name, and then write the output as a complete XML document to
*          the database.
* \author Josh Lurz
*/
void XMLDBOutputter::finish() const {
    // Create the database container and its related structures.
    auto_ptr<DBContainer> dbContainer( createContainer() );

    // Check if creating the container failed.
    if( !dbContainer.get() ){
        // An error message will have been printed by create container.
        return;
    }

    // Create an update context which is needed to insert the document.
    XmlUpdateContext context = dbContainer->mManager->createUpdateContext();

    // Create a unique document name.
    const string docName = createContainerName( scenario->getName() );

#if DEBUG_XML_DB
    {
        AutoOutputFile debugOutput( "debug_db.xml" );
        debugOutput << mBuffer.str() << endl;
    }
#endif

    // Insert the document into the database.
    try {
        dbContainer->mContainerWrapper->mContainer.putDocument( docName,  // The document's name
                               mBuffer.str(), // This would be more efficient
                                              // by writing the to stream
                                              // automatically.
                               context, // The update context
                               0 ); // Flags
    } catch ( const DbXml::XmlException& e ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Failed to insert the document into the database. " << e.what() << endl;
    }
}

/*! \brief Create an initialized XML database manager.
* \return An auto pointer to an initialized XML database manager, null if one
*         could not be created.
*/
auto_ptr<XMLDBOutputter::DBContainer> XMLDBOutputter::createContainer() {
    // Create a database container.
    auto_ptr<DBContainer> dbContainer( new DBContainer );

    // Create a database environment.
    dbContainer->mDBEnvironment = new DbEnv( 0 );

    // Set the database cache size to 100 megabytes. This will ensure maximum
    // database performance.
    const unsigned int CACHE_SIZE = 100 * 1024 * 1024;
    dbContainer->mDBEnvironment->set_cachesize(0, CACHE_SIZE, 1);

    // Get the location to open the environment.
    const Configuration* conf = Configuration::getInstance();
    const string xmldbContainerName = conf->getFile( "xmldb-location", "database.dbxml" );
    string environmentLocation;

    // The path separator could go either way.
    int indexOfDir = xmldbContainerName.find_last_of( '/' );
    // string::npos means that it was not found.
    if( indexOfDir == string::npos ) {
        // was not a UNIX path separator, try Windows
        indexOfDir = xmldbContainerName.find_last_of( '\\' );    
        if( indexOfDir == string::npos ) {
            // No path separators mean current directory.
            environmentLocation = ".";
        }
    }
 
    // if indexOfDir has a real value then substring out the path
    // otherwise it will already have been set to "."
    if( indexOfDir != string::npos ) {
        environmentLocation = xmldbContainerName.substr( 0, 
            indexOfDir );
    }

    try {
        // Open the environment.
        dbContainer->mDBEnvironment->open( environmentLocation.c_str(),
                                        DB_INIT_MPOOL | DB_CREATE | DB_INIT_LOCK, 0 );
    }
    catch( const DbException& e ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Failed to open the database environment: " << e.what() << endl;
        return auto_ptr<DBContainer>();
    }

    // Create a manager object from the environment.
    dbContainer->mManager.reset( new XmlManager( dbContainer->mDBEnvironment,
                                                 DBXML_ADOPT_DBENV ) );

    // Open the container. It will be closed automatically when the manager goes out of scope.
    try {
        dbContainer->mContainerWrapper.reset( new DBContainer::XMLContainerWrapper(
                                             dbContainer->mManager->openContainer( xmldbContainerName, 
                                             DB_CREATE | DBXML_INDEX_NODES) ) );
    }
    catch ( const DbXml::XmlException& e ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Failed to open the container: " << e.what() << endl;
        return auto_ptr<DBContainer>();
    }
    return dbContainer;
}

/*! \brief Create a unique name for the container given a scenario name.
* \param aScenarioName Name of the scenario.
* \return A unique container name.
*/
const string XMLDBOutputter::createContainerName( const string& aScenarioName ){
    // Create a MiniCAM style run ID.
    const long runID = util::createMinicamRunID( gGlobalTime );

    // Create a unique document name. Document names within a container must be
    // unique.
    return aScenarioName + util::toString( runID );
}

/*! \brief Append data at a given location to an already written database container.
* \details
* \param aData Data to append to the container.
* \param aLocation XPath of the location to add the data.
* \return Whether the data was added successfully.
*/
bool XMLDBOutputter::appendData( const string& aData, const string& aLocation ) {
    // Check that both data and a location have been set.
    if( aData.empty() || aLocation.empty() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Cannot append data to the XML database because ";
        if( aData.empty() ){
            mainLog << "data ";
        }
        else {
            mainLog << "location ";
        }
        mainLog << "string is empty." << endl;
        return false;
    }

    // Create a database container to use for the update.
    auto_ptr<DBContainer> dbContainer( createContainer() );

    // Check if creating the container failed.
    if( !dbContainer.get() ){
        // An error message will have been printed by create container.
        return false;
    }

    // Create a unique document name.
    const string docName = createContainerName( scenario->getName() );

    // Try to get the document from the database.
    try {
        // Get the document which will be modified.
        const string docXPath = "doc('dbxml:"
                                      + dbContainer->mContainerWrapper->mContainer.getName()
                                      + "/" + docName + "')";

        // Create the XPath will which locate where to insert the content.


        XmlQueryContext queryContext = dbContainer->mManager->createQueryContext();
        XmlQueryExpression locationExpr = dbContainer->mManager->prepare( docXPath + aLocation,
                                                                          queryContext );

        // Create a modification context.
        XmlModify modifier = dbContainer->mManager->createModify();

        // Add the insertion command.
        modifier.addInsertAfterStep( locationExpr, XmlModify::Element, "", aData );

        // Execute the command.
        XmlUpdateContext updateContext = dbContainer->mManager->createUpdateContext();

        XmlResults results = dbContainer->mManager->query( docXPath, queryContext );
        modifier.execute( results, queryContext, updateContext );
    }
    catch ( const DbXml::XmlException& e ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Failed to insert the cost information into the XML database. "
                << e.what() << endl;
        return false;
    }
    return true;
}

void XMLDBOutputter::startVisitScenario( const Scenario* aScenario, const int aPeriod ){
    // write heading for XML input file
    mBuffer << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" << endl;
    mBuffer << "<" << aScenario->getXMLNameStatic() << " name=\""
            << aScenario->getName() << "\" date=\""
            << util::XMLCreateDate( gGlobalTime ) << "\">" << endl;
    mTabs->increaseIndent();
    mTabs->writeTabs( mBuffer );
}

void XMLDBOutputter::endVisitScenario( const Scenario* aScenario, const int aPeriod ){
    // Write the closing scenario tag.
    XMLWriteClosingTag( aScenario->getXMLNameStatic(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitOutputMetaData( const OutputMetaData* aOutputMetaData,
                                               const int aPeriod )
{
    // Don't write opening and closing tags directly because toInputXML will do it.
    // Write the internal data. The input XML format will work.
    aOutputMetaData->toInputXML( mBuffer, mTabs.get() );
}

void XMLDBOutputter::endVisitOutputMetaData( const OutputMetaData* aOutputMetaData,
                                            const int aPeriod )
{
    // Don't write opening and closing tags directly because toInputXML will do it.
}

void XMLDBOutputter::startVisitWorld( const World* aWorld, const int aPeriod ){
    // Write the opening world tag.
    XMLWriteOpeningTag( aWorld->getXMLNameStatic(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::endVisitWorld( const World* aWorld, const int aPeriod ){
    // Write the closing world tag.
    XMLWriteClosingTag( aWorld->getXMLNameStatic(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitRegion( const Region* aRegion,
                                       const int aPeriod )
{
    // Store the region name.
    assert( mCurrentRegion.empty() );
    mCurrentRegion = aRegion->getName();

    // Calculate indirect emissions coefficients for all Technologies in a Region.
    mIndirectEmissCalc.reset( new IndirectEmissionsCalculator );

    // The XML db outputter is always called in all-period mode but the indirect
    // emissions calculator only works in single period mode.
    assert( aPeriod == -1 );
    for( int m = 0; m < scenario->getModeltime()->getmaxper(); ++m ){
        aRegion->accept( mIndirectEmissCalc.get(), m );
    }
}

void XMLDBOutputter::endVisitRegion( const Region* aRegion,
                                     const int aPeriod )
{
}

void XMLDBOutputter::startVisitRegionMiniCAM( const RegionMiniCAM* aRegionMiniCAM, const int aPeriod ) {
    // Store the region's GDP object.
    assert( !mGDP );
    mGDP = aRegionMiniCAM->gdp.get();

    // Write the opening region tag and the type of the base class.
    XMLWriteOpeningTag( aRegionMiniCAM->getXMLName(), mBuffer, mTabs.get(),
        aRegionMiniCAM->getName(), 0, Region::getXMLNameStatic() );
}

void XMLDBOutputter::endVisitRegionMiniCAM( const RegionMiniCAM* aRegionMiniCAM, const int aPeriod ) {
    assert( !mCurrentRegion.empty() );
    assert( mGDP );

    // Clear the region name.
    mCurrentRegion.clear();
    mGDP = 0;

    // Write the closing region tag.
    XMLWriteClosingTag( aRegionMiniCAM->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitRegionCGE( const RegionCGE* aRegionCGE, const int aPeriod ) {
       // Write the opening region tag and the type of the base class.
    XMLWriteOpeningTag( aRegionCGE->getXMLName(), mBuffer, mTabs.get(),
        aRegionCGE->getName(), 0, Region::getXMLNameStatic() );
}

void XMLDBOutputter::endVisitRegionCGE( const RegionCGE* aRegionCGE, const int aPeriod ) {
    assert( !mCurrentRegion.empty() );
    // Clear the region name.
    mCurrentRegion.clear();

    // Write the closing region tag.
    XMLWriteClosingTag( aRegionCGE->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitResource( const AResource* aResource,
                                         const int aPeriod )
{
    // Write the opening resource tag and the type of the base class.
    XMLWriteOpeningTag( aResource->getXMLName(), mBuffer, mTabs.get(),
        aResource->getName(), 0, "resource" );

    // Store resource units
    mCurrentPriceUnit = aResource->mPriceUnit;
    mCurrentOutputUnit = aResource->mOutputUnit;

    //write out the output
    const Modeltime* modeltime = scenario->getModeltime();
    for( int per = 0; per < modeltime->getmaxper(); ++per ){
        writeItem( "output", mCurrentOutputUnit, 
            aResource->getAnnualProd( mCurrentRegion, per ), per );
    }

    // We want to write the keywords last due to limitations in 
    // XPath we could be searching for them using following-sibling
    if( !aResource->mKeywordMap.empty() ) {
        XMLWriteElementWithAttributes( "", "keyword", mBuffer, mTabs.get(), aResource->mKeywordMap );
    }
}

void XMLDBOutputter::endVisitResource( const AResource* aResource,
                                       const int aPeriod )
{
    // Write the closing resource tag.
    XMLWriteClosingTag( aResource->getXMLName(), mBuffer, mTabs.get() );
    // Clear the current resource.
    mCurrentPriceUnit.clear();
    mCurrentOutputUnit.clear();
}

void XMLDBOutputter::startVisitSubResource( const SubResource* aSubResource,
                                            const int aPeriod )
{
    // Write the opening subresource tag and the type of the base class.
    XMLWriteOpeningTag( aSubResource->getXMLName(), mBuffer, mTabs.get(),
        aSubResource->getName(), 0, "subresource" );

    // Write out annual production.
    const Modeltime* modeltime = scenario->getModeltime();
    for( int per = 0; per < modeltime->getmaxper(); ++per ){
        writeItem( "production", mCurrentOutputUnit, aSubResource->getAnnualProd( per ), per );
    }
}

void XMLDBOutputter::endVisitSubResource( const SubResource* aSubResource,
                                          const int aPeriod )
{
    // Write the closing subresource tag.
    XMLWriteClosingTag( aSubResource->getXMLName(), mBuffer, mTabs.get() );
}

/*! \brief Write the output for a grade.
* \param aGrade Grade for which to write output.
* \param aPeriod Period which is ignored because all periods are output together.
* \todo Available is actually constant, figure out whether to write it out at
*       the subresource or redo grade.
*/
void XMLDBOutputter::startVisitGrade( const Grade* aGrade, const int aPeriod ){
    /*! \pre The function should always be called with the all period output. */
    // Write the opening subresource tag and.
    XMLWriteOpeningTag( aGrade->getXMLName(), mBuffer, mTabs.get(), aGrade->getName() );

    // Write out the cost.
    const Modeltime* modeltime = scenario->getModeltime();
    for( int per = 0; per < modeltime->getmaxper(); ++per ){
        writeItem( "cost", mCurrentPriceUnit, aGrade->getCost( per ), per );
    }

    // Output the available. This doesn't currently work correctly.
    for( int per = 0; per < modeltime->getmaxper(); ++per ){
        writeItem( "available", mCurrentOutputUnit, aGrade->getAvail(), per );
    }
}

void XMLDBOutputter::endVisitGrade( const Grade* aGrade, const int aPeriod ){
    // Write the closing subresource tag.
    XMLWriteClosingTag( aGrade->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitSector( const Sector* aSector, const int aPeriod ){
    // Store the sector name and units.
    mCurrentSector = aSector->getName();
    mCurrentPriceUnit = aSector->mPriceUnit;
    mCurrentOutputUnit = aSector->mOutputUnit;
    mCurrentInputUnit = aSector->mInputUnit;

    // Write the opening sector tag and the type of the base class.
    XMLWriteOpeningTag( aSector->getXMLName(), mBuffer, mTabs.get(),
        aSector->getName(), 0, "sector" );

    // Loop over the periods to output sector information.
    const Modeltime* modeltime = scenario->getModeltime();
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        writeItem( "cost", mCurrentPriceUnit, aSector->getPrice( mGDP, i ), i );
    }

    // We want to write the keywords last due to limitations in 
    // XPath we could be searching for them using following-sibling
    if( !aSector->mKeywordMap.empty() ) {
        XMLWriteElementWithAttributes( "", "keyword", mBuffer, mTabs.get(), aSector->mKeywordMap );
    }
}

void XMLDBOutputter::endVisitSector( const Sector* aSector, const int aPeriod ){
    // Write the closing sector tag.
    XMLWriteClosingTag( aSector->getXMLName(), mBuffer, mTabs.get() );

    // Clear the current sector.
    mCurrentSector.clear();
    mCurrentPriceUnit.clear();
    mCurrentOutputUnit.clear();
    mCurrentInputUnit.clear();
}

void XMLDBOutputter::startVisitSubsector( const Subsector* aSubsector,
                                          const int aPeriod )
{
    // Write the opening subsector tag and the type of the base class.
    XMLWriteOpeningTag( aSubsector->getXMLName(), mBuffer, mTabs.get(),
        aSubsector->getName(), 0, Subsector::getXMLNameStatic() );

    // Loop over the periods to output subsector information.
    // The loops are separated so the types are grouped together, as is required for
    // valid XML.
    const Modeltime* modeltime = scenario->getModeltime();
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        writeItem( "subsector-share", "%", aSubsector->calcShare( i, mGDP ), i );
    }
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        writeItem( "cost", mCurrentPriceUnit, aSubsector->getPrice( mGDP, i ), i );
    }
}

void XMLDBOutputter::endVisitSubsector( const Subsector* aSubsector,
                                        const int aPeriod )
{
    // Write the closing subsector tag.
    XMLWriteClosingTag( aSubsector->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitBuildingDemandSubsector( const BuildingDemandSubSector* aSubsector,
                                                        const int aPeriod )
{

    const Marketplace* marketplace = scenario->getMarketplace();
    const string intGainsMarketName
        = aSubsector->getInternalGainsMarketName( mCurrentSector );
 
    const Modeltime* modeltime = scenario->getModeltime();
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        double internalGains = marketplace->getPrice( intGainsMarketName,
                                                      mCurrentRegion, i );

        writeItem( "internal-gains", mCurrentOutputUnit, internalGains, i );
    }
}

void XMLDBOutputter::endVisitBuildingDemandSubsector( const BuildingDemandSubSector* aSubsector,
                                                      const int aPeriod )
{
    // Don't write out anything.
}

void XMLDBOutputter::startVisitTechnology( const Technology* aTechnology,
                                           const int aPeriod )
{
    assert( aPeriod == -1 );

    // Store the fuel name so the GHG can write it out with emissions.
    mCurrentFuel = aTechnology->mTechData->getFuelName();

    // Write the opening technology tag and the type of the base class.
    XMLWriteOpeningTag( aTechnology->getXMLName1D(), mBuffer, mTabs.get(),
                        aTechnology->getName(), aTechnology->year,
                        DefaultTechnology::getXMLNameStatic1D() );

    const Modeltime* modeltime = scenario->getModeltime();

    // Write out primary output.
    for( int curr = 0; curr < modeltime->getmaxper(); ++curr ){
        if( !aTechnology->mProductionState[ curr ]->isOperating() ){
            continue;
        }
        map<string, string> attrs;
        attrs[ "year" ] = util::toString( modeltime->getper_to_yr( curr ) );
        attrs[ "unit" ] = mCurrentOutputUnit;
        XMLWriteElementWithAttributes( aTechnology->getOutput( curr ),
                                       "output", mBuffer, mTabs.get(), attrs );
    }
   
    // Write out input
    for( int curr = 0; curr < modeltime->getmaxper(); ++curr ){
        if( !aTechnology->mProductionState[ curr ]->isOperating() ){
            continue;
        }

        // For writing the input add an attribute which specifies the fuel.
        map<string, string> attrs;
        attrs[ "fuel-name" ] = mCurrentFuel;
        attrs[ "year" ] = util::toString( modeltime->getper_to_yr( curr ) );
        attrs[ "unit" ] = mCurrentInputUnit;
        XMLWriteElementWithAttributes( aTechnology->getInput( curr ),
            "input", mBuffer, mTabs.get(), attrs );
    }

    // Write out secondary outputs.
    // TODO: This is a stopgap solution. Need to consider how to do this in the long run.
    for( unsigned iOutput = 1; iOutput < aTechnology->mOutputs.size(); ++iOutput ){    
        for( int curr = 0; curr < modeltime->getmaxper(); ++curr ){
            if( !aTechnology->mProductionState[ curr ]->isOperating() ){
                continue;
            }
            map<string, string> attrs;
            attrs[ "year" ] = util::toString( modeltime->getper_to_yr( curr ) );
            attrs[ "name" ] = aTechnology->mOutputs[ iOutput ]->getName( ); 
           //TODO need to add units for this
           // attrs[ "unit" ] = mCurrentOutputUnit; 
            XMLWriteElementWithAttributes( aTechnology->mOutputs[ iOutput ]->getPhysicalOutput( curr ),
                                           "secondary-output", mBuffer, mTabs.get(), attrs );
        }
    }
    
    // Determine the investment period of the technology.
    int investPeriod = modeltime->getyr_to_per( aTechnology->year );

    // Write out the non-energy cost assumed to be in same unit as sector price.
    writeItem( "non-energy-cost", mCurrentPriceUnit,
               aTechnology->getNonEnergyCost( investPeriod ), -1 );

    // Write out fuel cost which include fuel price, efficiency and tech change.
    writeItem( "fuel-cost", mCurrentPriceUnit,
                aTechnology->getFuelCost( mCurrentRegion,
                                          mCurrentSector, investPeriod ), -1 );

    // Write out total cost which includes fuel and non-energy costs.
    writeItem( "cost", mCurrentPriceUnit,
                aTechnology->getCost( investPeriod ), -1 );

    // Write efficiency of the technology.
    writeItem( "efficiency", "%",
                aTechnology->getEfficiency( investPeriod ), -1 );

    for( int curr = 0; curr < modeltime->getmaxper(); ++curr ){
        if( !aTechnology->mProductionState[ curr ]->isOperating() ){
            continue;
        }
        // Calculate the technology's indirect emissions.
        mCurrIndirectEmissions[ curr ] = aTechnology->getInput( curr )
         * mIndirectEmissCalc->getUpstreamEmissionsCoefficient( mCurrentFuel,
                                                                curr );
    }

    // We want to write the keywords last due to limitations in 
    // XPath we could be searching for them using following-sibling
    if( !aTechnology->mKeywordMap.empty() ) {
        XMLWriteElementWithAttributes( "", "keyword", mBuffer, mTabs.get(), aTechnology->mKeywordMap );
    }
}

void XMLDBOutputter::endVisitTechnology( const Technology* aTechnology,
                                         const int aPeriod )
{
    // Clear the stored technology information.
    mCurrentFuel.clear();
    fill( mCurrIndirectEmissions.begin(), mCurrIndirectEmissions.end(), 0.0 );

    // Write the closing technology tag.
    XMLWriteClosingTag( aTechnology->getXMLName1D(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitGHG( const AGHG* aGHG, const int aPeriod ){
    // XML DB outputter should always be called on all periods at once.
    // TODO: Currently when the all periods flag is passed to subsector,
    // it calls technology visit with the period of the technology. This assert
    // should be turned back on when that is sorted out.
    // assert( aPeriod == -1 );

    // Write out the opening element tag of the GHG and the type of the base class.
    XMLWriteOpeningTag( aGHG->getXMLName(), mBuffer, mTabs.get(), aGHG->getName(),
                        0, "GHG" );

    // Write out emissions.
    map<string, string> attrs;

    if( !mCurrentFuel.empty() ) {
        attrs[ "fuel-name" ] = mCurrentFuel;
    }

    const Modeltime* modeltime = scenario->getModeltime();
    double currEmission = 0.0;
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        attrs[ "year" ] = util::toString( modeltime->getper_to_yr( i ) );
        currEmission = aGHG->getEmission( i );
        // No need to write out zero emissions as this will happen often and
        // will waste space.
        if( !objects::isEqual<double>( currEmission, 0.0 ) ) {
            attrs[ "unit" ] = aGHG->mEmissionsUnit;
            XMLWriteElementWithAttributes( currEmission, "emissions",
                mBuffer, mTabs.get(), attrs );
        }
    }
        // Write indirect emissions if this is CO2.
    if( aGHG->getName() == "CO2" ){
        for( int i = 0; i < modeltime->getmaxper(); ++i ){
            // Skip writing zeros to save space.
            if( util::isEqual( mCurrIndirectEmissions[ i ], 0.0 ) ){
                continue;
            }
            writeItem( "indirect-emissions", aGHG->mEmissionsUnit, mCurrIndirectEmissions[ i ], i );
        }
    }
}

void XMLDBOutputter::endVisitGHG( const AGHG* aGHG, const int aPeriod ){
    // Write the closing ghg tag.
    XMLWriteClosingTag( aGHG->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitMarketplace( const Marketplace* aMarketplace,
                                            const int aPeriod )
{
    // Write the opening marketplace tag.
    XMLWriteOpeningTag( Marketplace::getXMLNameStatic(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::endVisitMarketplace( const Marketplace* aMarketplace,
                                          const int aPeriod )
{
    // Write the closing marketplace tag.
    XMLWriteClosingTag( Marketplace::getXMLNameStatic(), mBuffer, mTabs.get() );
    mCurrentMarket.clear();
    mCurrentPriceUnit.clear();
    mCurrentOutputUnit.clear();

}

void XMLDBOutputter::startVisitMarket( const Market* aMarket,
                                       const int aPeriod )
{
    // TODO: What should happen if period != -1 or the period of the market?
    // Write the opening market tag.
    const int year = scenario->getModeltime()->getper_to_yr( aMarket->period );
    XMLWriteOpeningTag( Market::getXMLNameStatic(), mBuffer, mTabs.get(),
                        aMarket->getName(), year );

    XMLWriteElement( aMarket->good, "MarketGoodOrFuel",mBuffer, mTabs.get() );
    XMLWriteElement( aMarket->region, "MarketRegion", mBuffer, mTabs.get() );

    // if next market clear out units to be updated
    if( mCurrentMarket != aMarket->getName() ){
        mCurrentMarket.clear();
        mCurrentMarket = aMarket->getName();
        mCurrentPriceUnit.clear();
        mCurrentOutputUnit.clear();
    }
    // Store unit information from base period
    if( aMarket->period == 0 ) {
        if( mCurrentPriceUnit.empty() ){
            mCurrentPriceUnit = aMarket->getMarketInfo()->getString( "price-unit", true );
        }
        if( mCurrentOutputUnit.empty() ){
            mCurrentOutputUnit = aMarket->getMarketInfo()->getString( "output-unit", true );
        }
    }

    writeItem( "price", mCurrentPriceUnit, aMarket->price, -1 );
    writeItem( "demand", mCurrentOutputUnit, aMarket->demand, -1 );
    writeItem( "supply", mCurrentOutputUnit, aMarket->supply, -1 );

    for( vector<const objects::Atom*>::const_iterator i = aMarket->getContainedRegions().begin();
        i != aMarket->getContainedRegions().end(); i++ )
    {
        XMLWriteElement( (*i)->getID(), "ContainedRegion", mBuffer, mTabs.get() );
    }
}

void XMLDBOutputter::endVisitMarket( const Market* aMarket, const int aPeriod ){
    // Write the closing market tag.
    XMLWriteClosingTag( Market::getXMLNameStatic(), mBuffer, mTabs.get() );
}

/*! \brief Visit the climate model.
* \param aClimateModel Model for which to perform output.
* \param aPeriod Period is ignored as all periods are printed together.
* \todo Temperature, sea level, forcings.
*/
void XMLDBOutputter::startVisitClimateModel( const IClimateModel* aClimateModel,
                                             const int aPeriod )
{
    /*! \pre The function should always be called with the all period output. */
    assert( aPeriod == -1 );
    // Write the opening tag.
    XMLWriteOpeningTag( "climate-model", mBuffer, mTabs.get() );
    int outputInterval
        = Configuration::getInstance()->getInt( "climateOutputInterval",
                                   scenario->getModeltime()->gettimestep( 0 ) );

    // print at least to 2100 if interval is set appropriately
    int endingYear = max( scenario->getModeltime()->getEndYear(), 2100 );

    // Write the concentrations for the request period.
    for( int year = scenario->getModeltime()->getStartYear();
         year <= endingYear; year += outputInterval )
    {
         writeItemUsingYear( "CO2-concentration", "PPM",
                             aClimateModel->getConcentration( "CO2", year ),
                             year );
         writeItemUsingYear( "CH4-concentration", "PPB",
                             aClimateModel->getConcentration( "CH4", year ),
                             year );
         writeItemUsingYear( "N2O-concentration", "PPB",
                             aClimateModel->getConcentration( "N2O", year ),
                             year );
         writeItemUsingYear( "C2F6-concentration", "PPT",
                             aClimateModel->getConcentration( "C2F6", year ),
                             year );
         writeItemUsingYear( "HCFC125-concentration", "PPT",
                             aClimateModel->getConcentration( "HCFC125", year ),
                             year );
         writeItemUsingYear( "HCFC134a-concentration", "PPT",
                             aClimateModel->getConcentration( "HCFC134A", year ),
                             year );
         writeItemUsingYear( "HCFC143A-concentration", "PPT",
                             aClimateModel->getConcentration( "HCFC143A", year ),
                             year );
         writeItemUsingYear( "HCFC245fa-concentration", "PPT",
                             aClimateModel->getConcentration( "HCFC245fa", year ),
                             year );
         writeItemUsingYear( "SF6-concentration", "PPT",
                             aClimateModel->getConcentration( "SF6", year ),
                             year );
         writeItemUsingYear( "CF4-concentration", "PPT",
                             aClimateModel->getConcentration( "CF4", year ),
                             year );
    }

    // Write total radiative forcing
    for( int year = scenario->getModeltime()->getStartYear();
         year <= endingYear; year += outputInterval )
    {
        writeItemUsingYear( "forcing-total", "W/m^2",
                             aClimateModel->getTotalForcing( year ),
                             year );
    }

    // Write net terrestrial uptake
    for( int year = scenario->getModeltime()->getStartYear();
         year <= endingYear; year += outputInterval )
    {
        writeItemUsingYear( "net-terrestrial-uptake", "GtC",
                             aClimateModel->getNetTerrestrialUptake( year ),
                             year );
    }

    // Write net ocean uptake
    for( int year = scenario->getModeltime()->getStartYear();
         year <= endingYear; year += outputInterval )
    {
        writeItemUsingYear( "net-ocean-uptake", "GtC",
                             aClimateModel->getNetOceanUptake( year ),
                             year );
    }

    // Global-mean temperature
    for( int year = scenario->getModeltime()->getStartYear();
         year <= endingYear; year += outputInterval )
    {
        writeItemUsingYear( "global-mean-temperature", "degreesC",
                             aClimateModel->getTemperature( year ),
                             year );
    }
}

void XMLDBOutputter::endVisitClimateModel( const IClimateModel* aClimateModel,
                                           const int aPeriod )
{
    // Write the closing tag.
    XMLWriteClosingTag( "climate-model", mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitDemographic( const Demographic* aDemographic,
                                            const int aPeriod )
{
    XMLWriteOpeningTag( aDemographic->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::endVisitDemographic( const Demographic* aDemographic,
                                          const int aPeriod )
{
    XMLWriteClosingTag( aDemographic->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitPopulation( const Population* aPopulation,
                                           const int aPeriod )
{
    // TODO: Always write out the total independent of the type. Is this the
    // right thing to do?
    writeItem( "total-population", aPopulation->mPopulationUnit, aPopulation->getTotal(), 0 );
}

void XMLDBOutputter::endVisitPopulation( const Population* aPopulation, const int aPeriod ){
}

void XMLDBOutputter::startVisitPopulationMiniCAM( const PopulationMiniCAM* aPopulation, const int aPeriod ){
    XMLWriteOpeningTag( PopulationMiniCAM::getXMLNameStatic(), mBuffer, mTabs.get(),
                        "", aPopulation->getYear() );
}

void XMLDBOutputter::endVisitPopulationMiniCAM( const PopulationMiniCAM* aPopulation, const int aPeriod ){
    XMLWriteClosingTag( PopulationMiniCAM::getXMLNameStatic(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitPopulationSGMRate( const PopulationSGMRate* aPopulation, const int aPeriod ){
    XMLWriteOpeningTag( PopulationSGMRate::getXMLNameStatic(), mBuffer, mTabs.get(),
                        "", aPopulation->getYear() );
}

void XMLDBOutputter::endVisitPopulationSGMRate( const PopulationSGMRate* aPopulation, const int aPeriod ){
    XMLWriteClosingTag( PopulationSGMRate::getXMLNameStatic(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitPopulationSGMFixed( const PopulationSGMFixed* aPopulation, const int aPeriod ){
    XMLWriteOpeningTag( PopulationSGMFixed::getXMLNameStatic(), mBuffer, mTabs.get(),
                        "", aPopulation->getYear() );
}

void XMLDBOutputter::endVisitPopulationSGMFixed( const PopulationSGMFixed* aPopulation, const int aPeriod ){
    XMLWriteClosingTag( PopulationSGMFixed::getXMLNameStatic(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitAgeCohort( const AgeCohort* aAgeCohort, const int aPeriod ){
    // have to write out tag by hand because of the "ageGroup" attribtue
    Tabs* tabs = mTabs.get();
    tabs->writeTabs( mBuffer );
    mBuffer << "<" << AgeCohort::getXMLNameStatic() << " ageGroup=\"" << aAgeCohort->getAgeGroup()
        << "\">" << endl;
    tabs->increaseIndent();
}

void XMLDBOutputter::endVisitAgeCohort( const AgeCohort* aAgeCohort, const int aPeriod ){
    XMLWriteClosingTag( AgeCohort::getXMLNameStatic(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitGender( const Gender* aGender, const int aPeriod ){
    XMLWriteOpeningTag( aGender->getXMLName(), mBuffer, mTabs.get() );
    writeItem( "population", "thous", aGender->getPopulation(), 0 );
}

void XMLDBOutputter::endVisitGender( const Gender* aGender, const int aPeriod ){
    XMLWriteClosingTag( aGender->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitGDP( const GDP* aGDP, const int aPeriod ){
    // Write the opening gdp tag.
    XMLWriteOpeningTag( GDP::getXMLNameStatic(), mBuffer, mTabs.get() );

    // Loop over the periods to output GDP information.
    // The loops are separated so the types are grouped together, as is required for
    // valid XML.
    const Modeltime* modeltime = scenario->getModeltime();
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        writeItem( "total-labor-productivity", "%/yr",
                   aGDP->getTotalLaborProductivity( i ), i );
    }
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        writeItem( "gdp-mer", aGDP->mGDPUnit, aGDP->getGDP( i ), i );
    }
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        writeItem( "gdp-per-capita-mer", "Thous90US$/per", aGDP->getGDPperCap( i ),
                   i );
    }
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        writeItem( "gdp-per-capita-ppp", "Thous90US$/per",
                   aGDP->getPPPGDPperCap( i ), i );
    }
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        writeItem( "gdp-mer-no-priceadj", aGDP->mGDPUnit, aGDP->getGDPNotAdjusted( i ), i );
    }
}

void XMLDBOutputter::endVisitGDP( const GDP* aGDP, const int aPeriod ){
    XMLWriteClosingTag( GDP::getXMLNameStatic(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitLandNode( const LandNode* aLandNode,
                                         const int aPeriod ){
    XMLWriteOpeningTag( LandNode::getXMLNameStatic(), mBuffer, mTabs.get(),
                        aLandNode->getName() );
}

void XMLDBOutputter::endVisitLandNode( const LandNode* aLandNode,
                                       const int aPeriod )
{
    XMLWriteClosingTag( LandNode::getXMLNameStatic(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitLandLeaf( const LandLeaf* aLandLeaf,
                                         const int aPeriod )
{
    // Write the opening gdp tag.
    XMLWriteOpeningTag( "LandLeaf", mBuffer, mTabs.get(), aLandLeaf->getName() );

    // Loop over the periods to output LandLeaf information.
    // The loops are separated so the types are grouped together, as is required for
    // valid XML.
    const Modeltime* modeltime = scenario->getModeltime();
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        writeItem( "land-allocation", "000Ha",
           aLandLeaf->getTotalLandAllocation( ALandAllocatorItem::eAnyLand, i ),
           i );
    }
    
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        writeItem( "intrinsic-rate", "$/kHa", aLandLeaf->mIntrinsicRate[ i ], i );
    }

    //TODO
    // Could save space by checking if this is an unmanaged land leaf, which does not have this value
    // Or using a derived class write pattern
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        writeItem( "intrinsic-yield-mode", "GCal/kHa", aLandLeaf->mIntrinsicYieldMode[ i ], i );
    }

    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        writeItem( "cal-observed-yield", "GCal/kHa", aLandLeaf->mCalObservedYield[ i ], i );
    }

}

void XMLDBOutputter::endVisitLandLeaf( const LandLeaf* aLandLeaf, const int aPeriod ){
    XMLWriteClosingTag( "LandLeaf", mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitCarbonCalc( const ICarbonCalc* aCarbon, const int aPeriod ){
    // Carbon Calc does not create a tag for the sake of simplicity for the dataviewer.
    // This allows all the LandLeaf data to be at one level.

    // Loop over the periods to output Carbon information.
    // The loops are separated so the types are grouped together, as is required for
    // valid XML.

    // Printing yearly values would be too much data.
    const Modeltime* modeltime = scenario->getModeltime();
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        int year = modeltime->getper_to_yr( i );
        writeItem( "land-use-change-emission", "MtC",
                   aCarbon->getNetLandUseChangeEmission( year ), i );
    }
}
void XMLDBOutputter::endVisitCarbonCalc( const ICarbonCalc* aCarbonP, const int aPeriod ){
    // Does nothing since the land-use-change-emission was brought a level up
    // in the output.
}

void XMLDBOutputter::startVisitBaseTechnology( const BaseTechnology* aBaseTech, const int aPeriod ) {

    const Modeltime* modeltime = scenario->getModeltime();
    if( aPeriod == -1 ) {
        for( int i = 0; i < modeltime->getmaxper(); ++i ){
            // avoid writing zeros for the sake of saving space
            if( !objects::isEqual<double>( aBaseTech->mOutputs[ i ], 0.0 ) ) {
                int year = modeltime->getper_to_yr( i );
                XMLWriteElement( aBaseTech->mOutputs[ i ], "output",
                    mBuffer, mTabs.get(), year );
            }
        }
    }
    else {
        int year = modeltime->getper_to_yr( aPeriod );
        XMLWriteElement( aBaseTech->mOutputs[ aPeriod ], "output", mBuffer, mTabs.get(), year );
    }
}

void XMLDBOutputter::endVisitBaseTechnology( const BaseTechnology* aBaseTech, const int aPeriod ) {
}

void XMLDBOutputter::startVisitExpenditure( const Expenditure* aExpenditure, const int aPeriod ) {
    const Modeltime* modeltime = scenario->getModeltime();
    // should I make and getXMLNameStatic() form expenditure?
    // TODO: figure out a way to not write expenditure elements if it will have no children.
    XMLWriteOpeningTag( "expenditure", mBuffer, mTabs.get(), "", modeltime->getper_to_yr( aPeriod ) );

    double currValue = 0.0;
    for( int i = 0; i < Expenditure::END; ++i ) {
        currValue = aExpenditure->getValue( static_cast< Expenditure::ExpenditureType >( i ) );
        if( !objects::isEqual<double>( currValue, 0.0 ) ) {
            XMLWriteElement( currValue,
                aExpenditure->enumToXMLName( static_cast< Expenditure::ExpenditureType >( i ) ),
                mBuffer, mTabs.get() );
        }
    }
}

void XMLDBOutputter::endVisitExpenditure( const Expenditure* aExpenditure, const int aPeriod ) {
    XMLWriteClosingTag( "expenditure", mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitInput( const Input* aInput, const int aPeriod ) {
    XMLWriteOpeningTag( aInput->getXMLName(), mBuffer, mTabs.get(), aInput->getName(), 0, "input" );
    const Modeltime* modeltime = scenario->getModeltime();
    for( int i = 0; i < modeltime->getmaxper(); ++i ) {
        double currValue = aInput->getDemandCurrency( i );
        int currYear = modeltime->getper_to_yr( i );
        // avoid writing zeros to save space
        if( !objects::isEqual<double>( currValue, 0.0 ) ) {
            XMLWriteElement( currValue, "demand-currency",
                mBuffer, mTabs.get(), currYear );
        }

        currValue = aInput->getPricePaid( i );
        if( !objects::isEqual<double>( currValue, 0.0 ) ) {
            XMLWriteElement( currValue, "price-paid",
                mBuffer, mTabs.get(), currYear );
        }
    }
}

void XMLDBOutputter::endVisitInput( const Input* aInput, const int aPeriod ) {
    XMLWriteClosingTag( aInput->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitHouseholdConsumer( const HouseholdConsumer* aHouseholdConsumer,
                                                 const int aPeriod )
{
    XMLWriteOpeningTag( aHouseholdConsumer->getXMLName(), mBuffer, mTabs.get(), aHouseholdConsumer->getName(),
        aHouseholdConsumer->getYear(), "baseTechnology" );

    XMLWriteElement( aHouseholdConsumer->landDemand, "land-demand", mBuffer, mTabs.get() );
    XMLWriteElement( aHouseholdConsumer->laborDemand, "labor-demand", mBuffer, mTabs.get() );
    XMLWriteElement( aHouseholdConsumer->householdLandDemand, "household-land-demand", mBuffer, mTabs.get() );
    XMLWriteElement( aHouseholdConsumer->householdLaborDemand, "household-labor-demand", mBuffer, mTabs.get() );

    XMLWriteElement( aHouseholdConsumer->socialSecurityTaxRate, "social-security-taxrate", mBuffer, mTabs.get() );
    XMLWriteElement( aHouseholdConsumer->incomeTaxRate, "income-tax-rate", mBuffer, mTabs.get() );
    XMLWriteElement( aHouseholdConsumer->personsPerHousehold, "persons-per-household", mBuffer, mTabs.get() );
    XMLWriteElement( aHouseholdConsumer->numberOfHouseholds, "number-of-households", mBuffer, mTabs.get() );
    XMLWriteElement( aHouseholdConsumer->totalLandArea, "total-land-area", mBuffer, mTabs.get() );

    // label as govt-transfer?
    XMLWriteElement( aHouseholdConsumer->transfer, "transfer", mBuffer, mTabs.get() );

    XMLWriteElement( aHouseholdConsumer->landSupply, "land-supply", mBuffer, mTabs.get() );
    XMLWriteElement( aHouseholdConsumer->laborSupplyMale, "labor-supply-male", mBuffer, mTabs.get() );
    XMLWriteElement( aHouseholdConsumer->laborSupplyFemale, "labor-supply-female", mBuffer, mTabs.get() );

    XMLWriteElement( aHouseholdConsumer->workingAgePopMale, "working-age-pop-male", mBuffer, mTabs.get() );
    XMLWriteElement( aHouseholdConsumer->workingAgePopFemale, "working-age-pop-female", mBuffer, mTabs.get() );
}

void XMLDBOutputter::endVisitHouseholdConsumer( const HouseholdConsumer* aHouseholdConsumer,
                                               const int aPeriod )
{
    XMLWriteClosingTag( aHouseholdConsumer->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitGovtConsumer( const GovtConsumer* aGovtConsumer, const int aPeriod ) {
    XMLWriteOpeningTag( aGovtConsumer->getXMLName(), mBuffer, mTabs.get(), aGovtConsumer->getName(),
        aGovtConsumer->getYear(), "baseTechnology" );

    XMLWriteElement( aGovtConsumer->mTaxProportional.get(), "proportional-tax", mBuffer, mTabs.get() );
    XMLWriteElement( aGovtConsumer->mTaxAdditive.get(), "additive-tax", mBuffer, mTabs.get() );
    XMLWriteElement( aGovtConsumer->mTaxCorporate.get(), "corporate-income-tax", mBuffer, mTabs.get() );
    XMLWriteElement( aGovtConsumer->mTaxIBT.get(), "indirect-buisness-tax", mBuffer, mTabs.get() );
    XMLWriteElement( aGovtConsumer->mRho.get(), "rho", mBuffer, mTabs.get() );
}

void XMLDBOutputter::endVisitGovtConsumer( const GovtConsumer* aGovtConsumer, const int aPeriod ) {
    XMLWriteClosingTag( aGovtConsumer->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitTradeConsumer( const TradeConsumer* aTradeConsumer, const int aPeriod ) {
    XMLWriteOpeningTag( aTradeConsumer->getXMLName(), mBuffer, mTabs.get(), aTradeConsumer->getName(),
        aTradeConsumer->getYear(), "baseTechnology" );
}

void XMLDBOutputter::endVisitTradeConsumer( const TradeConsumer* aTradeConsumer, const int aPeriod ) {
    XMLWriteClosingTag( aTradeConsumer->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitInvestConsumer( const InvestConsumer* aInvestConsumer, const int aPeriod ) {
    XMLWriteOpeningTag( aInvestConsumer->getXMLName(), mBuffer, mTabs.get(), aInvestConsumer->getName(),
        aInvestConsumer->getYear(), "baseTechnology" );
}

void XMLDBOutputter::endVisitInvestConsumer( const InvestConsumer* aInvestConsumer, const int aPeriod ) {
    XMLWriteClosingTag( aInvestConsumer->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitProductionTechnology( const ProductionTechnology* aProductionTechnology,
                                                    const int aPeriod )
{
    XMLWriteOpeningTag( aProductionTechnology->getXMLName(), mBuffer, mTabs.get(), aProductionTechnology->getName(),
        aProductionTechnology->getYear(), "baseTechnology" );

    // could this info have been found elsewhere?
    XMLWriteElement( aProductionTechnology->capital, "capital", mBuffer, mTabs.get());

    XMLWriteElement( aProductionTechnology->mExpectedProfitRateReporting, "expected-profit-rate", mBuffer, mTabs.get());

    const Modeltime* modeltime = scenario->getModeltime();
    for( int i = 0; i < modeltime->getmaxper(); ++i ) {
        double value = aProductionTechnology->getAnnualInvestment( i );
        if( !objects::isEqual<double>( value, 0.0 ) ) {
            XMLWriteElement( value, "annual-investment", mBuffer, mTabs.get(),
                modeltime->getper_to_yr( i ) );
        }
        value = aProductionTechnology->mProfits[ i ];
        if( !objects::isEqual<double>( value, 0.0 ) ) {
            XMLWriteElement( value, "profit", mBuffer, mTabs.get(),
                modeltime->getper_to_yr( i ) );
        }
        value = aProductionTechnology->mCostsReporting[ i ];
        if( !objects::isEqual<double>( value, 0.0 ) ) {
            XMLWriteElement( value, "cost", mBuffer, mTabs.get(),
                modeltime->getper_to_yr( i ) );
        }
    }
}

void XMLDBOutputter::endVisitProductionTechnology( const ProductionTechnology* aProductionTechnology,
                                                  const int aPeriod )
{
    XMLWriteClosingTag( aProductionTechnology->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitFactorSupply( const FactorSupply* aFactorSupply, const int aPeriod ) {
    // put year on this element or on the price?
    XMLWriteOpeningTag( FactorSupply::getXMLNameStatic(), mBuffer, mTabs.get(), aFactorSupply->getName() );

    const Modeltime* modeltime = scenario->getModeltime();
    const Marketplace* marketplace = scenario->getMarketplace();
    for( int i = 0; i < modeltime->getmaxper(); ++i ) {
        double pricePaid = ( marketplace->getPrice(aFactorSupply->getName(), mCurrentRegion, i) +
            ( aFactorSupply->moreSectorInfo->getValue(MoreSectorInfo::TRANSPORTATION_COST)
            * aFactorSupply->moreSectorInfo->getValue(MoreSectorInfo::TRAN_COST_MULT) )
            * aFactorSupply->moreSectorInfo->getValue(MoreSectorInfo::PROPORTIONAL_TAX_RATE)
            + aFactorSupply->moreSectorInfo->getValue(MoreSectorInfo::ADDITIVE_TAX) ) // add carbon taxes
            * 1 ;
        XMLWriteElement( pricePaid, "price", mBuffer, mTabs.get(), modeltime->getper_to_yr( i ) );
    }
}

void XMLDBOutputter::endVisitFactorSupply( const FactorSupply* aFactorSupply, const int aPeriod ) {
    XMLWriteClosingTag( FactorSupply::getXMLNameStatic(), mBuffer, mTabs.get() );
}

/*!
 * \brief Write a single item to the XML database.
 * \details Helper function which writes a single value, with an element
 *          name, unit, and a period, to the XML database.
 * \param aName Element name.
 * \param aUnit Unit of the item.
 * \param aValue Value to write.
 * \param aPeriod Period of the value. -1 indicates not to write a year.
 */
void XMLDBOutputter::writeItem( const string& aName,
                                const string& aUnit,
                                const double aValue,
                                const int aPeriod )
{
    int year = 0;
    if( aPeriod != -1 ){
        const Modeltime* modeltime = scenario->getModeltime();
        year = modeltime->getper_to_yr( aPeriod );
    }
    writeItemUsingYear( aName, aUnit, aValue, year );
}

/*!
 * \brief Write a single item to the XML database.
 * \details Helper function which writes a single value, with an element
 *          name, unit, and a period, to the XML database.
 * \param aName Element name.
 * \param aUnit Unit of the item.
 * \param aValue Value to write.
 * \param aYear Year of the value. Zero indicates not to write a year.
 */
void XMLDBOutputter::writeItemUsingYear( const string& aName,
                                         const string& aUnit,
                                         const double aValue,
                                         const int aYear )
{
    map<string, string> attributeMap;
    attributeMap[ "unit" ] = aUnit;

    if( aYear != 0 ){
        attributeMap[ "year" ] = util::toString( aYear );
    }
    XMLWriteElementWithAttributes( aValue, aName, mBuffer, mTabs.get(),
                                   attributeMap );
}

#endif
