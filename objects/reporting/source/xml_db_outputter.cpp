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
#include "resources/include/resource.h"
#include "sectors/include/sector.h"
#include "sectors/include/subsector.h"
#include "sectors/include/building_dmd_subsector.h"
#include "technologies/include/technology.h"
#include "emissions/include/ghg.h"
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
#include "demographics/include/age_cohort.h"
#include "demographics/include/gender.h"
#include "util/base/include/configuration.h"
#include "containers/include/gdp.h"
#include "land_allocator/include/land_leaf.h"
#include "emissions/include/icarbon_calc.h"
#include "util/base/include/atom.h"
#include "land_allocator/include/land_node.h"

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
*          container to deallocate first is neccessary, and would not work with
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
XMLDBOutputter::XMLDBOutputter(){
    mTabs.reset( new Tabs );
    mCurrentTechYear = 0;
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

/*! \brief Create an intiialized XML database manager.
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
    const string environmentLocation = conf->getFile( "xmldb-environment", "." );
    
    // Open the environment.
    dbContainer->mDBEnvironment->open( environmentLocation.c_str(),
                                       DB_INIT_MPOOL | DB_CREATE | DB_INIT_LOCK, 0 ); 

    // Create a manager object from the environment.
    dbContainer->mManager.reset( new XmlManager( dbContainer->mDBEnvironment,
                                                 DBXML_ADOPT_DBENV ) );
    
    // Open the container. It will be closed automatically when the manager goes out of scope.
    const string containerName = conf->getFile( "xmldb-location", "database.dbxml" );
    
    try {
        dbContainer->mContainerWrapper.reset( new DBContainer::XMLContainerWrapper(
                                             dbContainer->mManager->openContainer( containerName, DB_CREATE ) ) );
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
    XMLWriteOpeningTag( aWorld->getXMLNameStatic(), mBuffer, mTabs.get(),
                        aWorld->getName() );
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
    // Write the opening region tag and the type of the base class.
    XMLWriteOpeningTag( aRegion->getXMLName(), mBuffer, mTabs.get(),
        aRegion->getName(), 0, Region::getXMLNameStatic() );
}

void XMLDBOutputter::endVisitRegion( const Region* aRegion,
                                     const int aPeriod )
{
    assert( !mCurrentRegion.empty() );
    // Clear the region name.
    mCurrentRegion.clear();

    // Write the closing region tag.
    XMLWriteClosingTag( aRegion->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitResource( const Resource* aResource,
                                         const int aPeriod )
{
    // Write the opening resource tag and the type of the base class.
    XMLWriteOpeningTag( aResource->getXMLName(), mBuffer, mTabs.get(),
        aResource->getName(), 0, "resource" );
}

void XMLDBOutputter::endVisitResource( const Resource* aResource,
                                       const int aPeriod )
{
    // Write the closing resource tag.
    XMLWriteClosingTag( aResource->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitSubResource( const SubResource* aSubResource,
                                            const int aPeriod )
{
    // Write the opening subresource tag and the type of the base class.
    XMLWriteOpeningTag( aSubResource->getXMLName(), mBuffer, mTabs.get(),
        aSubResource->getName(), 0, "subresource" );
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
    XMLWriteOpeningTag( aGrade->getXMLName(), mBuffer, mTabs.get(),
                        aGrade->getName() );

    // Write out the cost.
    const Modeltime* modeltime = scenario->getModeltime();
    for( int per = 0; per < modeltime->getmaxper(); ++per ){
        XMLWriteElement( aGrade->getCost( per ), "cost", mBuffer, mTabs.get(),
                         modeltime->getper_to_yr( per ) );
    }
    
    // Output the available. This doesn't currently work correctly.
    for( int per = 0; per < modeltime->getmaxper(); ++per ){
        XMLWriteElement( aGrade->getAvail(), "available", mBuffer, mTabs.get(),
                         modeltime->getper_to_yr( per ) );
    }
}

void XMLDBOutputter::endVisitGrade( const Grade* aGrade, const int aPeriod ){
    // Write the closing subresource tag.
    XMLWriteClosingTag( aGrade->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitSector( const Sector* aSector, const int aPeriod ){
    // Store the sector name.
    mCurrentSector = aSector->getName();

    // Write the opening sector tag and the type of the base class.
    XMLWriteOpeningTag( aSector->getXMLName(), mBuffer, mTabs.get(),
        aSector->getName(), 0, "sector" );
}

void XMLDBOutputter::endVisitSector( const Sector* aSector, const int aPeriod ){
    // Write the closing sector tag.
    XMLWriteClosingTag( aSector->getXMLName(), mBuffer, mTabs.get() );

    // Clear the current sector.
    mCurrentSector.clear();
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
        int year = modeltime->getper_to_yr( i );
        // Write out the share.
        XMLWriteElement( aSubsector->share[ i ], "subsector-share", mBuffer,
                         mTabs.get(), year );
    }
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        int year = modeltime->getper_to_yr( i );
        // Write the cost.
        XMLWriteElement( aSubsector->subsectorprice[ i ], "subsector-cost",
                         mBuffer, mTabs.get(), year );
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
    const string intGainsMarketName = aSubsector->getInternalGainsMarketName( mCurrentSector );
    
    const Modeltime* modeltime = scenario->getModeltime();
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        int year = modeltime->getper_to_yr( i );
        // Write out the building internal gains.
        double internalGains = marketplace->getPrice( intGainsMarketName,
                                                      mCurrentRegion, i );
        XMLWriteElement( internalGains, "internal-gains", mBuffer,
                         mTabs.get(), year );
    }
}

void XMLDBOutputter::endVisitBuildingDemandSubsector( const BuildingDemandSubSector* aSubsector,
                                                      const int aPeriod )
{
    // Don't write out anything.
}

void XMLDBOutputter::startVisitTechnology( const technology* aTechnology,
                                           const int aPeriod )
{
    // Store the fuel name so the GHG can write it out with emissions.
    mCurrentFuel = aTechnology->fuelname;
    mCurrentTechYear = aTechnology->year;

    // Write the opening technology tag and the type of the base class.
    // TODO: Use functions to get techcost, necost, fuelcost.
    XMLWriteOpeningTag( aTechnology->getXMLName1D(), mBuffer, mTabs.get(),
        aTechnology->getName(), aTechnology->year, technology::getXMLNameStatic1D() );
    XMLWriteElement( aTechnology->output, "output", mBuffer, mTabs.get() );

    // For writing the input add an attribute which specifies the fuel.
    {
        map<string, string> attrs;
        attrs[ "fuel-name" ] = aTechnology->fuelname;
        XMLWriteElementWithAttributes( aTechnology->input, "input", mBuffer,
                                       mTabs.get(), attrs );
    }

    // Write the tech cost.
    XMLWriteElement( aTechnology->techcost, "tech-cost", mBuffer, mTabs.get() );

    // Write out the non-energy cost.
    XMLWriteElement( aTechnology->necost, "non-energy-cost", mBuffer, mTabs.get() );

    // Write out the fuel cost.
    XMLWriteElement( aTechnology->fuelcost, "fuel-cost", mBuffer, mTabs.get() );

    // Write out the share.
    XMLWriteElement( aTechnology->share, "tech-share", mBuffer, mTabs.get() );
}

void XMLDBOutputter::endVisitTechnology( const technology* aTechnology,
                                         const int aPeriod )
{
    // Clear the stored fuel name and technology year.
    mCurrentFuel.clear();
    mCurrentTechYear = 0;

    // Write the closing technology tag.
    XMLWriteClosingTag( aTechnology->getXMLName1D(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitGHG( const Ghg* aGHG, const int aPeriod ){
    // XML DB outputter should always be called on all periods at once.
    // TODO: Currently when the all periods flag is passed to subsector, 
    // it calls technology visit with the period of the technology. This assert
    // should be turned back on when that is sorted out.
    // assert( aPeriod == -1 );

    // Write out the opening element tag of the GHG and the type of the base class.
    XMLWriteOpeningTag( aGHG->getXMLName(), mBuffer, mTabs.get(), aGHG->getName(),
                        0, Ghg::getXMLNameStatic() );

    // Write out emissions.
    map<string, string> attrs;

    // If there is no fuel name than the GHG is a land use GHG.
    attrs[ "fuel-name" ] = mCurrentFuel.empty() ? "land-use" : mCurrentFuel;

    // If the current tech year is zero than this is a land use GHG and 
    // all periods should be written to the database.
    const Modeltime* modeltime = scenario->getModeltime();
    if( mCurrentTechYear == 0 ){
        for( int i = 0; i < modeltime->getmaxper(); ++i ){
            attrs[ "year" ] = util::toString( modeltime->getper_to_yr( i ) );
            XMLWriteElementWithAttributes( aGHG->getEmission( i ), "emissions",
                                           mBuffer, mTabs.get(), attrs );
        }
    }
    // Otherwise write emissions for the technology. TODO: This must be fixed
    // for vintaging because technology may have emissions in multiple periods.
    else {
        attrs[ "year" ] = util::toString( mCurrentTechYear );
        int currPeriod = scenario->getModeltime()->getyr_to_per( mCurrentTechYear );
        XMLWriteElementWithAttributes( aGHG->getEmission( currPeriod ), "emissions",
                                       mBuffer, mTabs.get(), attrs );
    }
}

void XMLDBOutputter::endVisitGHG( const Ghg* aGHG, const int aPeriod ){
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
}

void XMLDBOutputter::startVisitMarket( const Market* aMarket,
                                       const int aPeriod )
{
    // What should happen if period != -1 or the period of the market?
    // Write the opening market tag.
    const int year = scenario->getModeltime()->getper_to_yr( aMarket->period );
    XMLWriteOpeningTag( Market::getXMLNameStatic(), mBuffer, mTabs.get(),
                        aMarket->getName(), year );
    XMLWriteElement( aMarket->good, "MarketGoodOrFuel",mBuffer, mTabs.get() );
    XMLWriteElement( aMarket->region, "MarketRegion", mBuffer, mTabs.get() );
    XMLWriteElement( aMarket->price, "price", mBuffer, mTabs.get() );
    XMLWriteElement( aMarket->demand, "demand", mBuffer, mTabs.get() );
    XMLWriteElement( aMarket->supply, "supply", mBuffer, mTabs.get() );

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
    XMLWriteOpeningTag( MagiccModel::getXMLNameStatic(), mBuffer, mTabs.get() );

    // Write the concentrations for the request period.
    for( int per = 0; per < scenario->getModeltime()->getmaxper(); ++per ){
        XMLWriteElement( aClimateModel->getConcentration( "CO2", scenario->getModeltime()->getper_to_yr( per ) ), "co2-concentration",
                         mBuffer, mTabs.get(), scenario->getModeltime()->getper_to_yr( per ) );
    }
}

void XMLDBOutputter::endVisitClimateModel( const IClimateModel* aClimateModel, const int aPeriod ){
    // Write the closing tag.
    XMLWriteClosingTag( MagiccModel::getXMLNameStatic(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitDemographic( const Demographic* aDemographic, const int aPeriod ){
    XMLWriteOpeningTag( aDemographic->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::endVisitDemographic( const Demographic* aDemographic, const int aPeriod ){
    XMLWriteClosingTag( aDemographic->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitPopulation( const Population* aPopulation, const int aPeriod ){
    XMLWriteOpeningTag( aPopulation->getXMLName(), mBuffer, mTabs.get(),
                        "", aPopulation->getYear() );
    // Always write out the total independent of the type. Is this the right thing to do?
    XMLWriteElement( aPopulation->getTotal(), "total-population", mBuffer, mTabs.get() );
}

void XMLDBOutputter::endVisitPopulation( const Population* aPopulation, const int aPeriod ){
    XMLWriteClosingTag( aPopulation->getXMLName(), mBuffer, mTabs.get() );
}
    
void XMLDBOutputter::startVisitAgeCohort( const AgeCohort* aAgeCohort, const int aPeriod ){
    XMLWriteOpeningTag( AgeCohort::getXMLNameStatic(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::endVisitAgeCohort( const AgeCohort* aAgeCohort, const int aPeriod ){
    XMLWriteClosingTag( AgeCohort::getXMLNameStatic(), mBuffer, mTabs.get() );
}
    
void XMLDBOutputter::startVisitGender( const Gender* aGender, const int aPeriod ){
    XMLWriteOpeningTag( aGender->getXMLName(), mBuffer, mTabs.get() );
    // Write the population.
    XMLWriteElement( aGender->getPopulation(), "population", mBuffer, mTabs.get() );
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
        int year = modeltime->getper_to_yr( i );
        // Write out the labor productivity growth rate.
        XMLWriteElement( aGDP->getTotalLaborProductivity( i ),
                         "total-labor-productivity", mBuffer, mTabs.get(), year );
    }
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        int year = modeltime->getper_to_yr( i );
        // Write out Market GDP
        XMLWriteElement( aGDP->getGDP( i ), "gdp-mer", mBuffer, mTabs.get(), year );
    }
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        int year = modeltime->getper_to_yr( i );
        // Write out Market GDP
        XMLWriteElement( aGDP->getGDPperCap( i ), "gdp-per-capita-mer", mBuffer,
                         mTabs.get(), year );
    }
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        int year = modeltime->getper_to_yr( i );
        // Write out Market GDP
        XMLWriteElement( aGDP->getPPPGDPperCap( i ), "gdp-per-capita-ppp",
                         mBuffer, mTabs.get(), year );
    }
}

void XMLDBOutputter::endVisitGDP( const GDP* aGDP, const int aPeriod ){
    XMLWriteClosingTag( GDP::getXMLNameStatic(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitLandNode( const LandNode* aLandNode, const int aPeriod ){
    XMLWriteOpeningTag( LandNode::getXMLNameStatic(), mBuffer, mTabs.get(), aLandNode->getName() );
}

void XMLDBOutputter::endVisitLandNode( const LandNode* aLandNode, const int aPeriod ){
    XMLWriteClosingTag( LandNode::getXMLNameStatic(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitLandLeaf( const LandLeaf* aLandLeaf, const int aPeriod ){
	// Write the opening gdp tag.
    XMLWriteOpeningTag( LandLeaf::getXMLNameStatic(), mBuffer, mTabs.get(), aLandLeaf->getName() );

    // Loop over the periods to output LandLeaf information.
    // The loops are separated so the types are grouped together, as is required for
    // valid XML.
    const Modeltime* modeltime = scenario->getModeltime();
	for( int i = 0; i < modeltime->getmaxper(); ++i ){
        int year = modeltime->getper_to_yr( i );
        // Write out the labor productivity growth rate.
		XMLWriteElement( aLandLeaf->getLandAllocation( aLandLeaf->getName(), i ),
                         "land-allocation", mBuffer, mTabs.get(), year );
    }
}

void XMLDBOutputter::endVisitLandLeaf( const LandLeaf* aLandLeaf, const int aPeriod ){
	XMLWriteClosingTag( LandLeaf::getXMLNameStatic(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitCarbonCalc( const ICarbonCalc* aCarbon, const int aPeriod ){
	// Write the opening gdp tag.
    XMLWriteOpeningTag( "carbon-calc", mBuffer, mTabs.get() );

    // Loop over the periods to output Carbon information.
    // The loops are separated so the types are grouped together, as is required for
    // valid XML.

    // Printing yearly values would be too much data.
    const Modeltime* modeltime = scenario->getModeltime();
	for( int i = 0; i < modeltime->getmaxper(); ++i ){
        int year = modeltime->getper_to_yr( i );
        // Write out the carbon emissions.
        XMLWriteElement( aCarbon->getNetLandUseChangeEmission( year ),
                         "land-use-change-emission", mBuffer, mTabs.get(), year );
    }
}
void XMLDBOutputter::endVisitCarbonCalc( const ICarbonCalc* aCarbonP, const int aPeriod ){
	XMLWriteClosingTag( "carbon-calc", mBuffer, mTabs.get() );
}

#endif
