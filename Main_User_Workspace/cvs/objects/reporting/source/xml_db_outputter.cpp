/*
 * LEGAL NOTICE
 * This computer software was prepared by Battelle Memorial Institute,
 * hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
 * with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
 * CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
 * sentence must appear on any copies of this computer software.
 * 
 * EXPORT CONTROL
 * User agrees that the Software will not be shipped, transferred or
 * exported into any country or used in any manner prohibited by the
 * United States Export Administration Act or any other applicable
 * export laws, restrictions or regulations (collectively the "Export Laws").
 * Export of the Software may require some form of license or other
 * authority from the U.S. Government, and failure to obtain such
 * export control license may result in criminal liability under
 * U.S. laws. In addition, if the Software is identified as export controlled
 * items under the Export Laws, User represents and warrants that User
 * is not a citizen, or otherwise located within, an embargoed nation
 * (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
 *     and that User is not otherwise prohibited
 * under the Export Laws from receiving the Software.
 * 
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*!
* \file xml_db_outputter.cpp
* \ingroup Objects
* \brief The XMLDBOutputter class source file for writing results to xml database.
* \details This source file contains the definition for the startVisit and endVisit methods
*          for each class that the visitor visits.  Additional information is stored in the
*          visitor itself to help with writing to xml database.
* \author Josh Lurz, Sonny Kim, Pralit Patel
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
#include "sectors/include/afinal_demand.h"
#include "sectors/include/energy_final_demand.h"
#include "sectors/include/sector.h"
#include "sectors/include/subsector.h"
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
#include "technologies/include/food_production_technology.h"
#include "technologies/include/forest_production_technology.h"
#include "technologies/include/expenditure.h"
#include "technologies/include/production_technology.h"
#include "functions/include/sgm_input.h"
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
#include "technologies/include/ioutput.h"
#include "functions/include/minicam_input.h"
#include "functions/include/iinput.h"
#include "technologies/include/tran_technology.h"
#include "land_allocator/include/land_use_history.h"
#include "ccarbon_model/include/carbon_box.h"
#include "ccarbon_model/include/carbon_box_model.h"
#include "ccarbon_model/include/npp.h"
#include "ccarbon_model/include/acarbon_flow.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include "ccarbon_model/include/environmental_info.h"
#include <typeinfo>

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
        writeItem( "share", "%", aSubsector->calcShare( i, mGDP ), i );
    }
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        double currValue = aSubsector->getPrice( mGDP, i );
        if( !objects::isEqual<double>( currValue, 0.0 ) ) {
            writeItem( "cost", mCurrentPriceUnit, currValue, i );
        }
    }
}

void XMLDBOutputter::endVisitSubsector( const Subsector* aSubsector,
                                        const int aPeriod )
{
    // Write the closing subsector tag.
    XMLWriteClosingTag( aSubsector->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitEnergyFinalDemand( const EnergyFinalDemand* aEnergyFinalDemand, const int aPeriod ){
    // Write the opening finalDemand tag and the type of the base class.
    XMLWriteOpeningTag( aEnergyFinalDemand->getXMLNameStatic(), mBuffer, mTabs.get(),
        aEnergyFinalDemand->getName(), 0, "final-demand" );

    // Loop over the periods to output final demand sector information.
    const Modeltime* modeltime = scenario->getModeltime();
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        writeItem( "service", mCurrentOutputUnit, aEnergyFinalDemand->mServiceDemands[ i ], i );
    }
}

void XMLDBOutputter::endVisitEnergyFinalDemand( const EnergyFinalDemand* aEnergyFinalDemand, const int aPeriod ){


    // Write the closing finalDemand tag.
    XMLWriteClosingTag( aEnergyFinalDemand->getXMLNameStatic(), mBuffer, mTabs.get() );
}


void XMLDBOutputter::startVisitBaseTechnology( const BaseTechnology* aBaseTech, const int aPeriod ) {

    const Modeltime* modeltime = scenario->getModeltime();
    if( aPeriod == -1 ) {
        for( int i = 0; i < modeltime->getmaxper(); ++i ){
            // avoid writing zeros for the sake of saving space
            double currOutput = aBaseTech->mOutputs[ 0 ]->getCurrencyOutput( aPeriod );
            if( !objects::isEqual<double>( currOutput, 0.0 ) ) {
                int year = modeltime->getper_to_yr( i );
                XMLWriteElement( currOutput, "output",
                    mBuffer, mTabs.get(), year );
            }
        }
    }
    else {
        int year = modeltime->getper_to_yr( aPeriod );
        XMLWriteElement( aBaseTech->mOutputs[ aPeriod ], "output", mBuffer, mTabs.get(), year );
    }
}

void XMLDBOutputter::endVisitBaseTechnology( const BaseTechnology* aBaseTech, const int aPeriod ){
}

/* \brief Visit the Technology.
 * \note aPeriod is not the vintage of the technology but the current period.
 * \param aTechnology 
 * \param aPeriod 
 */
void XMLDBOutputter::startVisitTechnology( const Technology* aTechnology, const int aPeriod ){
    // Store the fuel name so the GHG can write it out with emissions.
    mCurrentFuel = aTechnology->mTechData->getFuelName();
    // Store the pointer to the current technology so that children of technology can access 
    // information on current technology.
    mCurrentTechnology = aTechnology;

    // write the technology tag and it's children in temp buffers so that we can
    // check if anything was really written out and avoid writing blank technologies
    stringstream* parentBuffer = new stringstream();
    stringstream* childBuffer = new stringstream();    
    
    // the opening tag goes in the parent buffer
    XMLWriteOpeningTag( aTechnology->getXMLName1D(), *parentBuffer, mTabs.get(),
        aTechnology->getName(), aTechnology->year,
        DefaultTechnology::getXMLNameStatic1D() );
        
    // put the buffers on a stack so that we have the correct ordering
    mBufferStack.push( parentBuffer );
    mBufferStack.push( childBuffer );

    // children of technology go in the child buffer
    for( int curr = 0; curr <= aPeriod; ++curr ){
        // Same check is done for objects contained by technology.
        if( !isTechnologyOperating( curr ) ){
            continue;
        }
        // Calculate the technology's indirect emissions.
 //        mCurrIndirectEmissions[ curr ] = aTechnology->getInput( curr )
 //       * mIndirectEmissCalc->getUpstreamEmissionsCoefficient( mCurrentFuel,
 //                                                              curr );
        // Write out total cost which includes fuel and non-energy costs.
        double currValue = aTechnology->getCost( curr );
        if( !objects::isEqual<double>( currValue, 0.0 ) ) {
            writeItemToBuffer( aTechnology->getCost( curr ), "cost", 
                *childBuffer, mTabs.get(), curr, mCurrentPriceUnit );
        }
    }
}

void XMLDBOutputter::endVisitTechnology( const Technology* aTechnology,
                                         const int aPeriod )
{
    // Clear the stored technology information.
    mCurrentFuel.clear();
    mCurrentTechnology = 0; // reset technology pointer to null
    fill( mCurrIndirectEmissions.begin(), mCurrIndirectEmissions.end(), 0.0 );

    // Write the technology (open tag, children, and closing tag) 
    // if the child buffer is not empty
    stringstream* childBuffer = popBufferStack();
    stringstream* parentBuffer = popBufferStack();
    if( !childBuffer->str().empty() ){
        mBuffer << parentBuffer->str() << childBuffer->str();
        // We want to write the keywords last due to limitations in 
        // XPath we could be searching for them using following-sibling
        if( !aTechnology->mKeywordMap.empty() ) {
            XMLWriteElementWithAttributes( "", "keyword", mBuffer, mTabs.get(), aTechnology->mKeywordMap );
        }
        XMLWriteClosingTag( aTechnology->getXMLName1D(), mBuffer, mTabs.get() );
    }
    else {
        // if we don't write the closing tag we still need to decrease the indent
        mTabs->decreaseIndent();
    }
    
    // clean up any extra buffers
    delete childBuffer;
    delete parentBuffer;
    
    // we should have cleared out all buffers by this point
    assert( mBufferStack.empty() );
}

void XMLDBOutputter::startVisitTranTechnology( const TranTechnology* aTranTechnology, const int aPeriod ) {
    // tran startVisitTranTechnology gets visited after startVisitTechnology which implies
    // mBufferStack.top() is the child buffer for technology
    writeItemToBuffer( aTranTechnology->mLoadFactor, "load-factor", 
        *mBufferStack.top(), mTabs.get(), -1, "load/veh" );
    writeItemToBuffer( aTranTechnology->mServiceOutput, "service-output",
        *mBufferStack.top(), mTabs.get(), -1, mCurrentOutputUnit );    
}

void XMLDBOutputter::endVisitTranTechnology( const TranTechnology* aTranTechnology, const int aPeriod ) {
    // do nothing
}

void XMLDBOutputter::startVisitMiniCAMInput( const MiniCAMInput* aInput, const int aPeriod ) {
    // we use startVisitInput to write out the generic input information, however
    // startVisitInput will never be called by an accept so we do it here
    startVisitInput( aInput, aPeriod );
        
    // We want to write the keywords last due to limitations in 
    // XPath we could be searching for them using following-sibling
    // note that mBufferStack.top() is the child buffer for input
    if( !aInput->mKeywordMap.empty() && !mBufferStack.top()->str().empty() ) {
        XMLWriteElementWithAttributes( "", "keyword", *mBufferStack.top(), mTabs.get(), 
            aInput->mKeywordMap );
    }
}
void XMLDBOutputter::endVisitMiniCAMInput( const MiniCAMInput* aInput, const int aPeriod ) {
    // call the endVisitInput explicitly becuase it is never called by an accept method.
    endVisitInput( aInput, aPeriod );
}

void XMLDBOutputter::startVisitInput( const IInput* aInput, const int aPeriod ) {
    // TODO: there needs to be a ->getXMLName() in IInput so that instead of having
    // input as the tag it will have the input's correct xml name
    
    // write the input tag and it's children in temp buffers so that we can
    // check if anything was really written out and avoid writing blank inputs
    stringstream* parentBuffer = new stringstream();
    stringstream* childBuffer = new stringstream();

    // the opening tag gets written in the parent buffer
    XMLWriteOpeningTag( aInput->getXMLReportingName(), *parentBuffer, mTabs.get(), aInput->getName(), 0, "input" );
    
    // put the buffers on a stack so that we have the correct ordering
    mBufferStack.push( parentBuffer );
    mBufferStack.push( childBuffer );

    const Modeltime* modeltime = scenario->getModeltime();
    map<string, string> attrs;

    // children of input go in the child buffer
    for( int i = 0; i <= aPeriod; ++i ) {
        if( !isTechnologyOperating( i ) ){
            continue;
        }

        int currYear = modeltime->getper_to_yr( i );
        attrs[ "vintage" ] = util::toString( currYear );
        // Avoid writing zeros to save space.
        // Write price paid for input.
        double currValue = aInput->getPricePaid( mCurrentRegion, i );
        if( !objects::isEqual<double>( currValue, 0.0 ) ) {
            attrs[ "unit" ] = mCurrentPriceUnit;
            XMLWriteElementWithAttributes( currValue, "price-paid", *childBuffer,
                mTabs.get(), attrs );
        }

        // Write physical demand (not currency demand) for input.
        currValue = aInput->getPhysicalDemand( i );
        if( !objects::isEqual<double>( currValue, 0.0 ) ) {
            attrs[ "unit" ] = "";
            if ( aInput->hasTypeFlag( IInput::ENERGY ) ) {
               // get the unit for this input good from the marketplace
               Marketplace* marketplace = scenario->getMarketplace();
               const IInfo* marketInfo = marketplace->getMarketInfo( aInput->getName(), mCurrentRegion, 0, false );
               if ( marketInfo ) {
                  attrs[ "unit" ] = marketInfo->getString( "output-unit", true );
               }
            }
            // if not energy input or market did not exist, then use default unit assignment
            if ( attrs[ "unit" ] == "" ) {
               attrs[ "unit" ] = mCurrentInputUnit;
            }
            XMLWriteElementWithAttributes( currValue, "demand-physical", *childBuffer,
                mTabs.get(), attrs );
        }

        // Write currency demand for input.
        currValue = aInput->getCurrencyDemand( i );
        if( !objects::isEqual<double>( currValue, 0.0 ) ) {
            attrs[ "unit" ] = mCurrentPriceUnit;
            XMLWriteElementWithAttributes( currValue, "demand-currency", *childBuffer,
                mTabs.get(), attrs );
        }

        // Write the IO coefficient for the input.
        currValue = aInput->getCoefficient( i );
        if( !objects::isEqual<double>( currValue, 0.0 ) ) {
            attrs[ "unit" ] = "unitless";
            XMLWriteElementWithAttributes( currValue, "IO-coefficient", *childBuffer,
                mTabs.get(), attrs );
        }

        // Write the carbon content of the input.
        currValue = aInput->getCarbonContent( i );
        if( !objects::isEqual<double>( currValue, 0.0 ) ) {
            attrs[ "unit" ] = "MTC";
            XMLWriteElementWithAttributes( currValue, "carbon-content", *childBuffer,
                mTabs.get(), attrs );
        }
    }
}

void XMLDBOutputter::endVisitInput( const IInput* aInput, const int aPeriod ) {
    // Write the input (open tag, children, and closing tag) to the buffer at
    // the top of the stack only if the child buffer is not empty
    stringstream* childBuffer = popBufferStack();
    stringstream* parentBuffer = popBufferStack();
    if( !childBuffer->str().empty() ){
        // retBuffer is still on the top of the stack
        stringstream* retBuffer = mBufferStack.top();
        (*retBuffer) << parentBuffer->str() << childBuffer->str();
        XMLWriteClosingTag( aInput->getXMLReportingName(), *retBuffer, mTabs.get() );
    }
    else {
        // if we don't write the closing tag we still need to decrease the indent
        mTabs->decreaseIndent();
    }
    
    // clean up any extra buffers
    delete childBuffer;
    delete parentBuffer;
}

void XMLDBOutputter::startVisitOutput( const IOutput* aOutput, const int aPeriod ) {
    // write the output tag and it's children in temp buffers so that we can
    // check if anything was really written out and avoid writing blank outputs
    stringstream* parentBuffer = new stringstream();
    stringstream* childBuffer = new stringstream();

    // the opening tag gets written in the parent buffer
    XMLWriteOpeningTag( aOutput->getXMLReportingName(), *parentBuffer, mTabs.get(), aOutput->getName(), 0, "output" );

    // put the buffers on a stack so that we have the correct ordering
    mBufferStack.push( parentBuffer );
    mBufferStack.push( childBuffer );
    
    const Modeltime* modeltime = scenario->getModeltime();

    map<string, string> attrs;

    // Write out physical output.
    // loop for all vintages up to current period
    // children of output go in the child buffer
    for( int curr = 0; curr <= aPeriod; ++curr ){
        if( !isTechnologyOperating( curr ) ){
            continue;
        }
        attrs[ "vintage" ] = util::toString( modeltime->getper_to_yr( curr ) );
        attrs[ "unit" ] = mCurrentOutputUnit;
        // Avoid writing zeros to save space.
        // Write physical output for each output.
        double currValue = aOutput->getPhysicalOutput( curr );
        if( !objects::isEqual<double>( currValue, 0.0 ) ) {
            XMLWriteElementWithAttributes( currValue, "physical-output", *childBuffer,
                mTabs.get(), attrs );
        }
    }
}

void XMLDBOutputter::endVisitOutput( const IOutput* aOutput, const int aPeriod ) {
    // Write the output (open tag, children, and closing tag) to the buffer at
    // the top of the stack only if the child buffer is not empty
    stringstream* childBuffer = popBufferStack();
    stringstream* parentBuffer = popBufferStack();
    if( !childBuffer->str().empty() ){
        // retBuffer is still at the top of the stack
        stringstream* retBuffer = mBufferStack.top();
        (*retBuffer) << parentBuffer->str() << childBuffer->str();
        XMLWriteClosingTag( aOutput->getXMLReportingName(), *retBuffer, mTabs.get() );
    }
    else {
        // if we don't write the closing tag we still need to decrease the indent
        mTabs->decreaseIndent();
    }
    
    // clean up any extra buffers
    delete childBuffer;
    delete parentBuffer;
}

void XMLDBOutputter::startVisitFoodProductionTechnology( const FoodProductionTechnology* aFoodProductionTechnology,
                                                         const int aPeriod )
{
    // Write out the non-energy cost assumed to be in same unit as sector price.
    writeItemToBuffer( aFoodProductionTechnology->variableCost, "variableCost", 
        *mBufferStack.top(), mTabs.get(), -1, "" );
    writeItemToBuffer( aFoodProductionTechnology->agProdChange, "agProdChange", 
        *mBufferStack.top(), mTabs.get(), -1, "" );
}

void XMLDBOutputter::endVisitFoodProductionTechnology( const FoodProductionTechnology* aTechnology,
                                                       const int aPeriod )
{
}

void XMLDBOutputter::startVisitForestProductionTechnology( const ForestProductionTechnology* aForestProductionTechnology,
                                                         const int aPeriod )
{
    startVisitFoodProductionTechnology( aForestProductionTechnology, aPeriod );
    
    // get period from year member variable
    const Modeltime* modeltime = scenario->getModeltime();
    int techPeriod = modeltime->getyr_to_per( aForestProductionTechnology->year );
   
    // Write out the harvested area for this period.
    // This is *not* physical harvested area. It is an adjusted harvested area, accounting for the fixed model
    // "rotation period" of 45 years.
    double landAllocation = aForestProductionTechnology->mLandAllocator->getLandAllocation( 
                            aForestProductionTechnology->landType, 
                            aForestProductionTechnology->mName, techPeriod );
    writeItemToBuffer( landAllocation, "adj-harvested-area", 
        *mBufferStack.top(), mTabs.get(), -1, "" );

    // Write out the harvested area for future period.
    landAllocation = aForestProductionTechnology->mLandAllocator->getLandAllocation( 
                            aForestProductionTechnology->landType, 
                            aForestProductionTechnology->mName, 
                            aForestProductionTechnology->getHarvestPeriod( techPeriod ) );
    writeItemToBuffer( landAllocation, "adj-future-harvested-area", 
        *mBufferStack.top(), mTabs.get(), -1, "" );

}

void XMLDBOutputter::endVisitForestProductionTechnology( const ForestProductionTechnology* aForestProductionTechnology,
                                                       const int aPeriod )
{
}

/*
void XMLDBOutputter::startVisitForestProductionTechnology( const FoodProductionTechnology* aFoodProductionTechnology,
                                                         const int aPeriod )
{
    startVisitFoodProductionTechnology( aFoodProductionTechnology, aPeriod );
    
    // Write out the harvested area for this period.
    // This is *not* physical harvested area. It is an adjusted harvested area, accounting for the fixed model
    // "rotation period" of 45 years.
    double landAllocation = aFoodProductionTechnology->mLandAllocator->getLandAllocation( 
                            aFoodProductionTechnology->landType, 
                            aFoodProductionTechnology->mName, aPeriod );
    writeItem( "adj-harvested-area", "",landAllocation, -1 );

    // Write out the harvested area for future period.
    landAllocation = aFoodProductionTechnology->mLandAllocator->getLandAllocation( 
                            aFoodProductionTechnology->landType, 
                            aFoodProductionTechnology->mName, 
                            aFoodProductionTechnology->getHarvestPeriod( aPeriod ) );
    writeItem( "adj-future-harvested-area", "",landAllocation, -1 );

}


void XMLDBOutputter::endVisitForestProductionTechnology( const FoodProductionTechnology* aTechnology,
                                                       const int aPeriod )
{
}
*/

void XMLDBOutputter::startVisitGHG( const AGHG* aGHG, const int aPeriod ){
    // write the ghg tag and it's children in temp buffers so that we can
    // check if anything was really written out and avoid writing blank ghgs
    stringstream* parentBuffer = new stringstream();
    stringstream* childBuffer = new stringstream();

    // the opening tag gets written in the parent buffer
    XMLWriteOpeningTag( aGHG->getXMLName(), *parentBuffer, mTabs.get(), aGHG->getName(),
                0, "GHG" );

    // put the buffers on a stack so that we have the correct ordering
    mBufferStack.push( parentBuffer );
    mBufferStack.push( childBuffer );

    map<string, string> attrs;
    const Modeltime* modeltime = scenario->getModeltime();
    double currEmission = 0.0;

    // children of ghg go in the child buffer
    for( int i = 0; i <= aPeriod; ++i ){
        if( !isTechnologyOperating( i ) ){
            continue;
        }
        attrs[ "year" ] = util::toString( modeltime->getper_to_yr( i ) );
        currEmission = aGHG->getEmission( i );
        // Avoid writing zeros to save space.
        // Write GHG emissions.
        if( !objects::isEqual<double>( currEmission, 0.0 ) ) {
            attrs[ "unit" ] = aGHG->mEmissionsUnit;
            XMLWriteElementWithAttributes( currEmission, "emissions",
                *childBuffer, mTabs.get(), attrs );
        }
    }
    // Write sequestered amount of GHG emissions .
    for( int i = 0; i <= aPeriod; ++i ){
        if( !isTechnologyOperating( i ) ){
            continue;
        }
        attrs[ "year" ] = util::toString( modeltime->getper_to_yr( i ) );
        currEmission = aGHG->getEmissionsSequestered( i );
        // No need to write out zero sequestered emissions as this will happen often and
        // will waste space.
        if( !objects::isEqual<double>( currEmission, 0.0 ) ) {
            attrs[ "unit" ] = aGHG->mEmissionsUnit;
            XMLWriteElementWithAttributes( currEmission, "emissions-sequestered",
                *childBuffer, mTabs.get(), attrs );
        }
    }
    // Write emissions by fuel if sector is primary energy.
    for( int i = 0; i <= aPeriod; ++i ){
        if( !isTechnologyOperating( i ) ){
            continue;
        }
        attrs[ "year" ] = util::toString( modeltime->getper_to_yr( i ) );
        currEmission = aGHG->getEmissFuel( i );
        // No need to write out zero emissions as this will happen often and
        // will waste space.
        if( !objects::isEqual<double>( currEmission, 0.0 ) ) {
            attrs[ "fuel-name" ] = mCurrentSector;
            attrs[ "unit" ] = aGHG->mEmissionsUnit;
            XMLWriteElementWithAttributes( currEmission, "emissions-by-fuel",
                *childBuffer, mTabs.get(), attrs );
        }
    }
    // Write indirect emissions if this is CO2.
    if( aGHG->getName() == "CO2" ){
        for( int i = 0; i <= aPeriod; ++i ){
            if( !isTechnologyOperating( i ) ){
                continue;
            }
            // Skip writing zeros to save space.
            if( util::isEqual( mCurrIndirectEmissions[ i ], 0.0 ) ){
                continue;
            }
            writeItemToBuffer( mCurrIndirectEmissions[ i ], "indirect-emissions", *childBuffer, 
                mTabs.get(), i, aGHG->mEmissionsUnit );
        }
    }
}

void XMLDBOutputter::endVisitGHG( const AGHG* aGHG, const int aPeriod ){
    // Write the ghg (open tag, children, and closing tag) to the buffer at
    // the top of the stack only if the child buffer is not empty
    stringstream* childBuffer = popBufferStack();
    stringstream* parentBuffer = popBufferStack();
    if( !childBuffer->str().empty() ){
        // retBuffer is still on the top of the stack
        stringstream* retBuffer = mBufferStack.top();
        (*retBuffer) << parentBuffer->str() << childBuffer->str();
        XMLWriteClosingTag( aGHG->getXMLName(), *retBuffer, mTabs.get() );
    }
    else {
        // if we don't write the closing tag we still need to decrease the indent
        mTabs->decreaseIndent();
    }
    
    // clean up any extra buffers
    delete childBuffer;
    delete parentBuffer;
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

void XMLDBOutputter::startVisitCarbonBox( const CarbonBox* aCarbonBox,
                                          const int aPeriod ){
    const Modeltime* modeltime = scenario->getModeltime();
    XMLWriteOpeningTag( CarbonBox::getXMLNameStatic(), mBuffer, mTabs.get(), aCarbonBox->getName() );
    int endingYear = scenario->getModeltime()->getEndYear();
    const Configuration* conf = Configuration::getInstance();
    const int outputInterval = conf->getInt( "carbon-stock-output-interval", 10 );
    const int startingYear = max( conf->getInt( "carbon-output-start-year", 1990 ), CarbonModelUtils::getStartYear() );
 
    for( int aYear = startingYear; aYear < endingYear; aYear += outputInterval ){
        XMLWriteElement( aCarbonBox->mStock->getStock( aYear ),
                         "carbon-stock", mBuffer, mTabs.get(), aYear );
    }
    //! not sure if I want to be generic as compare aCarbonBox->mStock->getName() == NPP::getXMLNameStatic
    //! this will cause big O(d) comparison with an if statement.
    //! if I specific compare with "NPP", the comparison is big O(1) instead of big (d)

    if( aCarbonBox->matches( eNPP ) ){
        for ( int aYear = startingYear; aYear < endingYear; aYear +=outputInterval ){
            XMLWriteElement( aCarbonBox->mStock->getNPPOverAreaRatio( aYear ),
                         "NPPRatio", mBuffer, mTabs.get(), aYear );
        } //! end of year loop
    } //! end of NPP loop
}

void XMLDBOutputter::endVisitCarbonBox(const CarbonBox *aCarbonBox, const int aPeriod){
    XMLWriteClosingTag( CarbonBox::getXMLNameStatic(), mBuffer, mTabs.get() );
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
    // The loops are separated so the types are grouped together to make it easier to
    // read the XML. Note this writes total land allocation (so all land sums to same total)
    // This does not report the land harvested in a given year.
    const Modeltime* modeltime = scenario->getModeltime();
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        writeItem( "land-allocation", "000Ha",
           aLandLeaf->getTotalLandAllocation( ALandAllocatorItem::eAnyLand, i ),
           i );
    }
    
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        writeItem( "intrinsic-rate", "$/kHa", aLandLeaf->mIntrinsicRate[ i ], i );
    }

    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        // TODO: consider having a writeItem that takes a Value and will only
        // write if the Value isInited
        if( aLandLeaf->mIntrinsicYieldMode[ i ].isInited() ) {
            writeItem( "intrinsic-yield-mode", "GCal/kHa", aLandLeaf->mIntrinsicYieldMode[ i ], i );
        }
        if( aLandLeaf->mCalObservedYield[ i ].isInited() ) {
            writeItem( "cal-observed-yield", "GCal/kHa", aLandLeaf->mCalObservedYield[ i ], i );
        }
        if( aLandLeaf->mYield[ i ].isInited() ) {
            writeItem( "yield", "GCal/kHa", aLandLeaf->mYield[ i ], i );
        }
    }
}

void XMLDBOutputter::endVisitLandLeaf( const LandLeaf* aLandLeaf, const int aPeriod ){
    XMLWriteClosingTag( "LandLeaf", mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitCarbonCalc( const ICarbonCalc* aCarbonCalc, const int aPeriod ){
    XMLWriteOpeningTag( CarbonBoxModel::getXMLNameStatic(), mBuffer, mTabs.get() );

    // Loop over the periods to output Carbon information.

    // Printing yearly values would be too much data.
    const Modeltime* modeltime = scenario->getModeltime();
    const int startingYear = max( Configuration::getInstance()->getInt( "carbon-output-start-year", 1990 ), CarbonModelUtils::getStartYear() );
    int outputInterval = Configuration::getInstance()->getInt( "climateOutputInterval",modeltime->gettimestep( 0 ) );
    
    for( int aYear = startingYear; 
             aYear <= modeltime->getper_to_yr( modeltime->getmaxper() - 1 ) || aYear == modeltime->getper_to_yr( modeltime->getmaxper() - 1 ); 
             aYear += outputInterval ){
        // TODO -- need ability to write out units for non-period based items.
        XMLWriteElement( aCarbonCalc->getNetLandUseChangeEmission( aYear ),
                         "land-use-change-emission", mBuffer, mTabs.get(), aYear );
        XMLWriteElement( aCarbonCalc->getActualAboveGroundCarbonDensity( aYear ),
                         "above-ground-carbon", mBuffer, mTabs.get(), aYear );
        XMLWriteElement( aCarbonCalc->getActualBelowGroundCarbonDensity( aYear ),
                         "below-ground-carbon", mBuffer, mTabs.get(), aYear );
     }
}

void XMLDBOutputter::endVisitCarbonCalc( const ICarbonCalc* aCarbonCalc, const int aPeriod ){
    XMLWriteClosingTag( CarbonBoxModel::getXMLNameStatic(), mBuffer, mTabs.get() );
} 

void XMLDBOutputter::startVisitCarbonBoxModel( const CarbonBoxModel* aCarbonBoxModel, const int aPeriod ){
    const Configuration* conf = Configuration::getInstance();
    const int outputInterval = conf->getInt( "land-use-history-interval", 10 );
    const int endingYear = scenario->getModeltime()->getEndYear();
    const int startingYear = max( conf->getInt( "carbon-output-start-year", 1990 ), CarbonModelUtils::getStartYear() );

    const EnvironmentalInfo* tempEnv = aCarbonBoxModel->getEnvironmentalInfo();

    if( tempEnv ){
        for( int aYear = startingYear; aYear <= endingYear; aYear += outputInterval ){
            double landUse = tempEnv->getLandUse( aYear );
            writeItemUsingYear( "land-use", "000Ha", landUse, aYear );
        } // End of year loop
    } // end of if( tempEnv )
}

void XMLDBOutputter::endVisitCarbonBoxModel( const CarbonBoxModel* aCarbonBoxModel, const int aPeriod ) {
    // do nothing as this is for a derived class parse
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

void XMLDBOutputter::startVisitSGMInput( const SGMInput* aInput, const int aPeriod ) {
    XMLWriteOpeningTag( aInput->getXMLName(), mBuffer, mTabs.get(), aInput->getName(), 0, "input" );
    const Modeltime* modeltime = scenario->getModeltime();
    for( int i = 0; i < modeltime->getmaxper(); ++i ) {
        double currValue = aInput->getCurrencyDemand( i );
        int currYear = modeltime->getper_to_yr( i );
        // avoid writing zeros to save space
        if( !objects::isEqual<double>( currValue, 0.0 ) ) {
            XMLWriteElement( currValue, "demand-currency",
                mBuffer, mTabs.get(), currYear );
        }
        currValue = aInput->getPricePaid( mCurrentRegion, i );
        if( !objects::isEqual<double>( currValue, 0.0 ) ) {
            XMLWriteElement( currValue, "price-paid",
                mBuffer, mTabs.get(), currYear );
        }
    }
}

void XMLDBOutputter::endVisitSGMInput( const SGMInput* aInput, const int aPeriod ) {
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
 * \brief Write a single item to the string stream buffer.
 * \details Helper function which writes a single value, with an element
 *          name, unit, and a period, to a string stream buffer and the XML database.
 * \param aName Element name.
 * \param aUnit Unit of the item.
 * \param aValue Value to write.
 * \param aPeriod Period of the value. -1 indicates not to write a year.
 */
void XMLDBOutputter::writeItemToBuffer( const double aValue,
                                        const string& aName,
                                        ostream& out,
                                        const Tabs* tabs,
                                        const int aPeriod,
                                        const string& aUnit )
{
    map<string, string> attributeMap;
    attributeMap[ "unit" ] = aUnit;
    int year = 0;
    
    // Do not write out year if aPeriod = -1.
    if( aPeriod != -1 ){
        const Modeltime* modeltime = scenario->getModeltime();
        year = modeltime->getper_to_yr( aPeriod );
        attributeMap[ "year" ] = util::toString( year );
    }

    XMLWriteElementWithAttributes( aValue, aName, out, tabs,
                                   attributeMap );
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

/**
 * \brief Function to test if technology is operating.
 * \return True or false.
 * \author Sonny Kim
 */
bool XMLDBOutputter::isTechnologyOperating( const int aPeriod ){
    bool isOperating = false;
    // If operating and has output greater than zero.
    if( mCurrentTechnology->mProductionState[ aPeriod ]->isOperating() ){
        //if( mCurrentTechnology->getOutput( aPeriod ) > 0 ){
            isOperating = true;
        //}
    }
    // If not operating but is fixed output.
    else {
        if( mCurrentTechnology->mFixedOutput != IProductionState::fixedOutputDefault() ){
            if( mCurrentTechnology->getOutput( aPeriod ) > 0 ){
                isOperating = true;
            }
        }
    }
    return isOperating;
}

/**
 * \brief Pops the buffer off of the top of the stack and returns it.
 * \details A convience method so that the pop can be in one line instead of two.
 * \return The buffer that was at the top of the stack.
 * \author Pralit Patel
 */
 stringstream* XMLDBOutputter::popBufferStack(){
    stringstream* ret = mBufferStack.top();
    mBufferStack.pop();
    return ret;
}

#endif
