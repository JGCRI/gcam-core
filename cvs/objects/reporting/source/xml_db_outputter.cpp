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
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
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

#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "containers/include/world.h"
#include "containers/include/region.h"
#include "containers/include/region_minicam.h"
#include "containers/include/iinfo.h"
#include "resources/include/resource.h"
#include "sectors/include/afinal_demand.h"
#include "sectors/include/energy_final_demand.h"
#include "sectors/include/sector.h"
#include "sectors/include/subsector.h"
#include "sectors/include/nesting_subsector.h"
#include "technologies/include/technology.h"
#include "emissions/include/aghg.h"
#include "technologies/include/icapture_component.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "marketplace/include/market.h"
#include "climate/include/iclimate_model.h"
#include "climate/include/magicc_model.h"
#include "resources/include/subresource.h"
#include "resources/include/reserve_subresource.h"
#include "resources/include/renewable_subresource.h"
#include "resources/include/smooth_renewable_subresource.h"
#include "resources/include/grade.h"
#include "demographics/include/demographic.h"
#include "demographics/include/population.h"
#include "demographics/include/population_mini_cam.h"
#include "demographics/include/age_cohort.h"
#include "demographics/include/gender.h"
#include "util/base/include/configuration.h"
#include "containers/include/gdp.h"
#include "land_allocator/include/land_leaf.h"
#include "ccarbon_model/include/icarbon_calc.h"
#include "ccarbon_model/include/land_carbon_densities.h"
#include "util/base/include/atom.h"
#include "land_allocator/include/land_node.h"
#include "technologies/include/ioutput.h"
#include "technologies/include/base_technology.h"
#include "technologies/include/ag_production_technology.h"
#include "technologies/include/expenditure.h"
#include "functions/include/node_input.h"
#include "containers/include/national_account.h"
#include "util/base/include/util.h"
#include "technologies/include/default_technology.h"
#include "technologies/include/iproduction_state.h"
#include "util/base/include/auto_file.h"
#include "technologies/include/ioutput.h"
#include "functions/include/minicam_input.h"
#include "functions/include/iinput.h"
#include "sectors/include/tran_subsector.h"
#include "technologies/include/tran_technology.h"
#include "land_allocator/include/land_use_history.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include "util/base/include/version.h"
#include "consumers/include/gcam_consumer.h"
#include "functions/include/building_node_input.h"
#include "functions/include/building_service_input.h"
#include "functions/include/satiation_demand_function.h"
#include "functions/include/food_demand_input.h"
#include <typeinfo>

// Whether to write a text file with the contents that are to be inserted
// into the XML database.
#define DEBUG_XML_DB 0

#ifdef DEBUG_XML_DB
#include <boost/iostreams/tee.hpp>
#include <boost/iostreams/device/file.hpp>
#endif

#if( !__HAVE_JAVA__ )
#include <boost/iostreams/device/null.hpp>
#endif

#include <ctime>

#include <string>
#include <sstream>

#include <cmath>

#include "reporting/include/xml_db_outputter.h"

extern Scenario* scenario; // for modeltime

// TODO: Remove global time variable.
extern time_t gGlobalTime;

using namespace std;
using namespace boost::iostreams;


#if( __HAVE_JAVA__ )
// Static initialize the JavaVM to be null
JavaVM* XMLDBOutputter::JNIContainer::mJavaVM = 0;

/*!
 * \brief Constructor for the JNI container.
 * \see createContainer()
 */
XMLDBOutputter::JNIContainer::JNIContainer():
mJavaEnv( 0 )
{
}

/*!
 * \brief Destructor for the JNI container.
 */
XMLDBOutputter::JNIContainer::~JNIContainer() {
    if( mJavaEnv ) {
        mJavaEnv->DeleteGlobalRef( mWriteDBClass );
        mJavaEnv->DeleteGlobalRef( mWriteDBInstance );
    }

    // Apparently this is a bug since the beginning of time for Java, the DestroyJavaVM
    // does not actually close it.  So if you try to close then reopen the VM it will
    // give an error.  The best advice is to just not close it. Resources may be leaked.
    /*
    if( mJavaVM ) {
        mJavaVM->DestroyJavaVM();
    }*/
}
#endif

/*! \brief Constructor
*/
XMLDBOutputter::XMLDBOutputter():
mTabs( new Tabs ),
mGDP( 0 ),
mSubsectorDepth( 0 )
#if( __HAVE_JAVA__ )
,mJNIContainer( createContainer( false ) )
#endif
{
#if( DEBUG_XML_DB )
    // Have data written to mBuffer go to the debug_db file as well.
    file_sink debugDBSink( "debug_db.xml" );
    // Use a "tee" filter to ensure data gets to all sinks.
    tee_filter<file_sink> teeDebugFilter( debugDBSink );
    mBuffer.push( teeDebugFilter );
#endif

#if( __HAVE_JAVA__ )
    // Set Java as the sink of data for mBuffer.
    SendToJavaIOSink sendToJavaSink( mJNIContainer.get() );
    mBuffer.push( sendToJavaSink );
#else
    mBuffer.push( null_sink() );
#endif
}

/*!
 * \brief Destructor
 * \note This needs to be explicitly defined for incompletely defined members
 *       to be deleted correctly.
 */
XMLDBOutputter::~XMLDBOutputter(){
}

/*!
 * \brief A utility method which can be used as a preliminary check to make sure
 *        all of the various compononents and libraries required to write to the
 *        database are found.
 * \details This method would be used early in the model run rather than wait until
 *          the entire scenario has run only to find out we were unable to write the
 *          results.  Note it may still be possible some error occurs when we go
 *          to actually write to the database since we will not make any attempt
 *          at writing to it during this check.
 * \return True if it appears writing to the datbase would have been successful.
 */
bool XMLDBOutputter::checkJavaWorking() {
#if( __HAVE_JAVA__ )
    auto_ptr<JNIContainer> testContainer = createContainer( true );
    // if we get back a null container then some error occured
    // createContainer would have already print any error messages.
    return testContainer.get();
#else
    // we can skip this check if we have disabled Java
    return true;
#endif
}

/*!
 * \brief Write the output to the database.
 * \details In order to keep the memory usage down data has been writing to the
 *          database as XML was being generated.  We will signal that no more data
 *          will be generated and wait for it to finish here.
 */
void XMLDBOutputter::finish() const {
    // Close mBuffer so that no more data can be written.
    close( mBuffer, ios_base::out );

#if( __HAVE_JAVA__ )
    if( !mJNIContainer.get() ) {
        // Failed to start Java, just return as an appropriate error message would
        // have already been given.
        return;
    }
    // First we need to look up the appropriate "finish" Java method with no
    // arguments and void return: "()V" then call it.
    jmethodID finishMID = mJNIContainer->mJavaEnv->GetMethodID( mJNIContainer->mWriteDBClass, "finish", "()V" );
    if( !finishMID ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Failed to find JNI method: finish" << endl;
        return;
    }

    // The java method will wait until the database is done processing all data
    // before returning.
    mJNIContainer->mJavaEnv->CallVoidMethod( mJNIContainer->mWriteDBInstance, finishMID );
#endif
}

/*!
 * \brief A method to inform us that no more data will be appended to the open database so we can
 *        now run any addtional processing necessary and close the database.
 * \details We will simply call the finalizeAndClose method on the XMLDBDriver to do the work.
 *          It may potentially run queries if configured then close the database.
 */
void XMLDBOutputter::finalizeAndClose() {
#if( __HAVE_JAVA__ )
    // Call finalizeAndClose on the XMLDBDriver if it was successfully opened in the first place.
    if( mJNIContainer.get() ) {
        // First we need to look up the appropriate "finalizeAndClose" Java method with no
        // arguments and void return: "()V" then call it.
        jmethodID finalizeMID = mJNIContainer->mJavaEnv->GetMethodID( mJNIContainer->mWriteDBClass,
                "finalizeAndClose", "()V" );
        if( !finalizeMID ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::SEVERE );
            mainLog << "Failed to find JNI method: finalizeAndClose" << endl;
            return;
        }

        // The java method will (potentially) run queries then close the database
        // before returning.
        mJNIContainer->mJavaEnv->CallVoidMethod( mJNIContainer->mWriteDBInstance, finalizeMID );
    }
#endif
}

#if( __HAVE_JAVA__ )
/*!
 * \brief Create an initialized Java environment.
 * \param aTestingOnly A flag if set indicates we don't want to actually start the
 *                     process for writing, instead are only interested if all of
 *                     the Java machinery is in place to successfully write to the DB.
 * \return An initialized Java environment with the Write DB class loaded and
 *         ready to accept data to write/alter to the database.  If an error occurs
 *         a null container will be returned.
 */
auto_ptr<XMLDBOutputter::JNIContainer> XMLDBOutputter::createContainer( const bool aTestingOnly ) {
    // Create a Java instance.
    auto_ptr<JNIContainer> jniContainer( new JNIContainer );

    // Ensure the user wants this output
    const Configuration* conf = Configuration::getInstance();
    if( !conf->shouldWriteFile( "xmldb-location" ) ) {
        jniContainer.reset( 0 );
        return jniContainer;
    }

    // Start the Java VM with the following settings
    JavaVMInitArgs vmArgs;
    JavaVMOption* options = new JavaVMOption[ 2 ];
    // Note that JNI will not expand the wildcards in the classpath as it would
    // in every other means of setting the classpath.  To work aroudnd this we
    // will need to use a custom class loader that will do the expansion prior to
    // loading any classes.
    const string classpath = "-Djava.class.path=XMLDBDriver.jar" + string( PATH_SEPARATOR ) + string( JARS_LIB )
        + string( PATH_SEPARATOR ) + "../output/modelinterface/ModelInterface.jar";
    options[ 0 ].optionString = const_cast<char*>( classpath.c_str() );
    options[ 1 ].optionString = const_cast<char*>( "-Djava.system.class.loader=WildcardExpandingClassLoader" );
    vmArgs.version = JNI_VERSION_1_6;
    vmArgs.nOptions = 2;
    vmArgs.options = options;
    vmArgs.ignoreUnrecognized = false;
    if( !jniContainer->mJavaVM ) {
        JNI_CreateJavaVM( &jniContainer->mJavaVM, (void**)&jniContainer->mJavaEnv, &vmArgs );
    }
    else {
        jniContainer->mJavaVM->AttachCurrentThread( (void**)&jniContainer->mJavaEnv, &vmArgs );
    }
    delete[] options;

    // Ensure that the Java VM opened successfully.
    if( !jniContainer->mJavaVM || !jniContainer->mJavaEnv ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Failed to start Java." << endl;
        jniContainer.reset( 0 );
        return jniContainer;
    }

    // Find the Java class that will do the work of putting the XML document into
    // the database.
    // Note that we need to make this class reference "global" so that we can use
    // it again in later.
    const string writeDBClassName = "XMLDBDriver";
    jniContainer->mWriteDBClass = reinterpret_cast<jclass>( jniContainer->mJavaEnv->NewGlobalRef(
        jniContainer->mJavaEnv->FindClass( writeDBClassName.c_str() ) ) );
    if( !jniContainer->mWriteDBClass ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Failed to find Java class " << writeDBClassName << " to write to the XML database." << endl;
        jniContainer.reset( 0 );
        return jniContainer;
    }

    // Find the constructor: "<init>" for the class which takes two string:
    // "(Ljava/lang/String;Ljava/lang/String)V".  The arguments are the database, and
    // a unique name to call the document that we will put into the database.
    jmethodID writeDBCtorMID = jniContainer->mJavaEnv->GetMethodID( jniContainer->mWriteDBClass,
        "<init>", "(Ljava/lang/String;Ljava/lang/String;)V" );
    if( !writeDBCtorMID ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Failed to find the appropriate constructor of Java class " << writeDBClassName << "." << endl;
        jniContainer.reset( 0 );
        return jniContainer;
    }

    // If we are only testing if Java works we need to exit now as calling the constructor
    // will initiate the database write
    if( aTestingOnly ) {
        jniContainer->mWriteDBInstance = 0;
        return jniContainer;
    }

    // Get the location to open the environment.
    string xmldbContainerName = conf->getFile( "xmldb-location", "database_basexdb" );
    if( conf->shouldAppendScnToFile( "xmldb-location") ) {
        // note that util::appendScenarioToFileName searches for a '.' between which to insert
        // the scenario name however a '.' is not a valid character in a BaseX DB name so we
        // will just append it to the end.
        xmldbContainerName = xmldbContainerName.append( scenario->getName() );
    }
    const string docName = createContainerName( scenario->getName() );

    // Convert the C++ string to a Java String so that they can be passed to the constructor.
    jstring jXMLDBContainerName = jniContainer->mJavaEnv->NewStringUTF( xmldbContainerName.c_str() );
    jstring jDocName = jniContainer->mJavaEnv->NewStringUTF( docName.c_str() );

    // Call the constructor to get an instance of writeDBClassName.
    jniContainer->mWriteDBInstance = jniContainer->mJavaEnv->NewGlobalRef(
        jniContainer->mJavaEnv->NewObject( jniContainer->mWriteDBClass, writeDBCtorMID, jXMLDBContainerName, jDocName ) );
    if( !jniContainer->mWriteDBInstance ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Failed to construct the Java class " << writeDBClassName << "." << endl;
        jniContainer.reset( 0 );
        return jniContainer;
    }

    return jniContainer;
}
#endif

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

#if( __HAVE_JAVA__ )
    // Check if creating the container failed.
    if( !mJNIContainer.get() ){
        // An error message will have been printed by create container.
        return false;
    }

    // Find the appendData method for the class which takes two string arguments:
    // "(Ljava/lang/String;Ljava/lang/String;)Z".  The arguments are the data, and
    // an XPath which gives the location after which to insert the data.  It will
    // return a bool "Z" if it successfully appended the data or not.
    jmethodID appendDataMID = mJNIContainer->mJavaEnv->GetMethodID( mJNIContainer->mWriteDBClass,
        "appendData", "(Ljava/lang/String;Ljava/lang/String;)Z" );
    if( !appendDataMID ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Failed to find the appendData Java method" << endl;
        return false;
    }

    // Convert the C++ string to a Java String so that they can be passed to the Java method.
    jstring jData = mJNIContainer->mJavaEnv->NewStringUTF( aData.c_str() );
    jstring jLocation = mJNIContainer->mJavaEnv->NewStringUTF( aLocation.c_str() );

    // Call the appendData method
    return mJNIContainer->mJavaEnv->CallBooleanMethod( mJNIContainer->mWriteDBInstance, appendDataMID, jData, jLocation );
#else
    return false;
#endif
}

void XMLDBOutputter::startVisitScenario( const Scenario* aScenario, const int aPeriod ){
    // write heading for XML input file
    mBuffer << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" << endl;
    mBuffer << "<" << aScenario->getXMLNameStatic() << " name=\""
            << aScenario->getName() << "\" date=\""
            << util::XMLCreateDate( gGlobalTime ) << "\">" << endl;
    mTabs->increaseIndent();

    // Write model version information
    mTabs->writeTabs( mBuffer );
    mBuffer << "<model-version>ver_" << __ObjECTS_VER__ << "_r" << __REVISION_NUMBER__
        << "</model-version>" << endl;
}

void XMLDBOutputter::endVisitScenario( const Scenario* aScenario, const int aPeriod ){
    // Write the closing scenario tag.
    XMLWriteClosingTag( aScenario->getXMLNameStatic(), mBuffer, mTabs.get() );
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
}

void XMLDBOutputter::endVisitRegion( const Region* aRegion,
                                     const int aPeriod )
{
}

void XMLDBOutputter::startVisitRegionMiniCAM( const RegionMiniCAM* aRegionMiniCAM, const int aPeriod ) {
    // Store the region's GDP object.
    assert( !mGDP );
    mGDP = aRegionMiniCAM->mGDP;

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
        writeItem( "cumulative-production", mCurrentOutputUnit, aSubResource->getCumulProd( per ), per );
    }
}

void XMLDBOutputter::endVisitSubResource( const SubResource* aSubResource,
                                          const int aPeriod )
{
    // Write the closing subresource tag.
    XMLWriteClosingTag( aSubResource->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitReserveSubResource( const ReserveSubResource* aSubResource, const int aPeriod )
{
    startVisitSubResource( aSubResource, aPeriod );
}

void XMLDBOutputter::endVisitReserveSubResource( const ReserveSubResource* aSubResource, const int aPeriod )
{
    endVisitSubResource( aSubResource, aPeriod );
}

void XMLDBOutputter::startVisitSubRenewableResource( const SubRenewableResource* aSubResource,
                                                     const int aPeriod )
{
    // Write the opening subresource tag and the type of the base class.
    XMLWriteOpeningTag( aSubResource->getXMLNameStatic(), mBuffer, mTabs.get(),
                       aSubResource->getName(), 0, "subresource" );
    
    // Write out annual production and maximum available renewable resource.
    const Modeltime* modeltime = scenario->getModeltime();
    for( int per = 0; per < modeltime->getmaxper(); ++per ){
        writeItem( "max-annual-subresource", mCurrentOutputUnit, aSubResource->getMaxAnnualSubResource(per), per );
        writeItem( "production", mCurrentOutputUnit, aSubResource->getAnnualProd( per ), per );
        writeItem( "cumulative-production", mCurrentOutputUnit, aSubResource->getCumulProd( per ), per );
    }
}

void XMLDBOutputter::endVisitSubRenewableResource( const SubRenewableResource* aSubResource,
                                                        const int aPeriod )
{
    // Write the closing subresource tag.
    XMLWriteClosingTag( aSubResource->getXMLNameStatic(), mBuffer, mTabs.get() );
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
        double currCost = aSector->getPrice( mGDP, i );
        if( !std::isnan( currCost ) ) {
            writeItem( "cost", mCurrentPriceUnit, currCost, i );
        }
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
    map<string, string> attrs;
    attrs["name"] = aSubsector->getName();
    attrs["type"] = Subsector::getXMLNameStatic();
    int currDepth = mSubsectorDepth++;
    // optimization to avoid writing depth of zero, which is the vast majority
    // of the cases, to save space in the DB.
    if( currDepth > 0 ) {
        attrs["depth"] = util::toString( currDepth );
    }
    XMLWriteOpeningTag( aSubsector->getXMLName(), mBuffer, mTabs.get(), attrs );

    // Loop over the periods to output subsector information.
    // The loops are separated so the types are grouped together, as is required for
    // valid XML.
    const Modeltime* modeltime = scenario->getModeltime();
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        writeItem( "share-weight", "none", aSubsector->getShareWeight( i ), i );
    }
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        double currValue = aSubsector->getPrice( mGDP, i );
        if( !objects::isEqual<double>( currValue, 0.0 ) && !std::isnan( currValue ) ) {
            writeItem( "cost", mCurrentPriceUnit, currValue, i );
        }
    }
}

void XMLDBOutputter::endVisitSubsector( const Subsector* aSubsector,
                                        const int aPeriod )
{
    --mSubsectorDepth;
    // Write the closing subsector tag.
    XMLWriteClosingTag( aSubsector->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitNestingSubsector( const NestingSubsector* aSubsector,
                                                 const int aPeriod )
{
    startVisitSubsector( aSubsector, aPeriod );
}
void XMLDBOutputter::endVisitNestingSubsector( const NestingSubsector* aSubsector,
                                               const int aPeriod )
{
    endVisitSubsector( aSubsector, aPeriod );
}

void XMLDBOutputter::startVisitTranSubsector( const TranSubsector* aTranSubsector, const int aPeriod ) {
    const Modeltime* modeltime = scenario->getModeltime();
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        double currValue = aTranSubsector->mSpeed[ i ];
        if( !objects::isEqual<double>( currValue, 0.0 ) ) {
            writeItem( "speed", "km/hr", currValue, i );
        }
        currValue = aTranSubsector->mTimeValueMult[ i ];
        if( !objects::isEqual<double>( currValue, 0.0 ) ) {
            writeItem( "time-value-multiplier", "N/A", currValue, i );
        }
    }
}

void XMLDBOutputter::endVisitTranSubsector( const TranSubsector* aTranSubsector, const int aPeriod ) {
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
    XMLWriteClosingTag( aEnergyFinalDemand->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitBaseTechnology( const BaseTechnology* aBaseTech, const int aPeriod ) {
    // writing blank technologies is not a big concern for sgm so just create a child buffer
    // for it's children to write to and write the technology anyway even if the buffer was empty
    mBufferStack.push( new stringstream() );
}

void XMLDBOutputter::endVisitBaseTechnology( const BaseTechnology* aBaseTech, const int aPeriod ){
    // write out anything written to the child buffer
    ostream* childBuffer = popBufferStack();
    if( childBuffer->rdbuf()->in_avail() ) {
        mBuffer << childBuffer->rdbuf();
    }
    
    // clean up the child buffer
    delete childBuffer;
}

/* \brief Visit the Technology.
 * \note aPeriod is not the vintage of the technology but the current period.
 * \param aTechnology 
 * \param aPeriod 
 */
void XMLDBOutputter::startVisitTechnology( const Technology* aTechnology, const int aPeriod ){
    // Store the pointer to the current technology so that children of technology can access 
    // information on current technology.
    mCurrentTechnology = aTechnology;

    // write the technology tag and it's children in temp buffers so that we can
    // check if anything was really written out and avoid writing blank technologies
    stringstream* parentBuffer = new stringstream();
    stringstream* childBuffer = new stringstream();
    
    // the opening tag goes in the parent buffer
    // TODO: Inconsistent use of year attribute.  Technology vintage written out
    // with "year" attribute.
    XMLWriteOpeningTag( aTechnology->getXMLName(), *parentBuffer, mTabs.get(),
        aTechnology->getName(), aTechnology->mYear,
        DefaultTechnology::getXMLNameStatic() );
        
    // put the buffers on a stack so that we have the correct ordering
    mBufferStack.push( parentBuffer );
    mBufferStack.push( childBuffer );

    if( !objects::isEqual<double>( aTechnology->getShareWeight(), 0.0 ) ) {
        XMLWriteElement( aTechnology->getShareWeight(), "share-weight", *childBuffer, mTabs.get() );
    }

    // Do not write out default capacity factor of 1
    XMLWriteElementCheckDefault( aTechnology->getCapacityFactor(), "capacity-factor", *childBuffer, mTabs.get() , 1.0 );

    // children of technology go in the child buffer
    for( int curr = 0; curr <= aPeriod; ++curr ){
        // Write out total cost which includes fuel and non-energy costs
        // for new investments only.
        // TODO: Inconsistent use of year attribute.  WriteItemToBuffer writes
        // "year" attribute in addition to the technology "year" attribute.
        if( mCurrentTechnology->mProductionState[ curr ] && mCurrentTechnology->mProductionState[ curr ]->isNewInvestment() ){
            double currValue = aTechnology->getCost( curr );
            if( !objects::isEqual<double>( currValue, 0.0 ) && !std::isnan( currValue ) ) {
                writeItemToBuffer( aTechnology->getCost( curr ), "cost", 
                    *childBuffer, mTabs.get(), curr, mCurrentPriceUnit );
            }
        }
    }
}

void XMLDBOutputter::endVisitTechnology( const Technology* aTechnology,
                                         const int aPeriod )
{
    // Clear the stored technology information.
    mCurrentTechnology = 0; // reset technology pointer to null
    fill( mCurrIndirectEmissions.begin(), mCurrIndirectEmissions.end(), 0.0 );

    // Write the technology (open tag, children, and closing tag) 
    // if the child buffer is not empty
    iostream* childBuffer = popBufferStack();
    iostream* parentBuffer = popBufferStack();
    if( /*!childBuffer->str().empty()*/ childBuffer->rdbuf()->in_avail() ){
        mBuffer << parentBuffer->rdbuf() << childBuffer->rdbuf();
        // We want to write the keywords last due to limitations in 
        // XPath we could be searching for them using following-sibling
        if( !aTechnology->mKeywordMap.empty() ) {
            XMLWriteElementWithAttributes( "", "keyword", mBuffer, mTabs.get(), aTechnology->mKeywordMap );
        }
        XMLWriteClosingTag( aTechnology->getXMLName(), mBuffer, mTabs.get() );
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
    if( !aInput->mKeywordMap.empty() && mBufferStack.top()->rdbuf()->in_avail()/*->str().empty()*/ ) {
        XMLWriteElementWithAttributes( "", "keyword", *mBufferStack.top(), mTabs.get(), 
            aInput->mKeywordMap );
    }
}
void XMLDBOutputter::endVisitMiniCAMInput( const MiniCAMInput* aInput, const int aPeriod ) {
    // call the endVisitInput explicitly becuase it is never called by an accept method.
    endVisitInput( aInput, aPeriod );
}

void XMLDBOutputter::startVisitInput( const IInput* aInput, const int aPeriod ) {
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
    // note minicam is using real periods at this point
    double maxPer = aPeriod == -1 ? modeltime->getmaxper() -1 : aPeriod;
    for( int i = 0; i <= maxPer; ++i ) {
        // isTechnologyOperating will crash for sgm so avoid calling it
        if( aPeriod != -1 && !isTechnologyOperating( i ) ){
            continue;
        }

        int currYear = modeltime->getper_to_yr( i );
        attrs[ "vintage" ] = util::toString( currYear );
        // Avoid writing zeros to save space.
        // Write price paid for input.
        double currValue;
        if( !aInput->hasTypeFlag( IInput::ENERGY ) ) {
            currValue = aInput->getPricePaid( mCurrentRegion, i );
            if( !objects::isEqual<double>( currValue, 0.0 ) ) {
                attrs[ "unit" ] = mCurrentPriceUnit;
                XMLWriteElementWithAttributes( currValue, "price-paid", *childBuffer,
                    mTabs.get(), attrs );
            }
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
                  attrs[ "unit" ] = marketInfo->getString( "output-unit", false );
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
        if( !objects::isEqual<double>( currValue, 0.0 ) &&
            // hack to avoid writing out IO-coefficient for non-energy inputs
            !objects::isEqual<double>( aInput->getPhysicalDemand( i ), 0.0 ) )
        {
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
    iostream* childBuffer = popBufferStack();
    iostream* parentBuffer = popBufferStack();
    if( /*!childBuffer->str().empty()*/ childBuffer->rdbuf()->in_avail() ){
        // retBuffer is still on the top of the stack
        ostream* retBuffer = mBufferStack.top();
        (*retBuffer) << parentBuffer->rdbuf() << childBuffer->rdbuf();
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

    // note minicam is using real periods at this point where as sgm is
    // always using -1
    double maxPer = aPeriod == -1 ? modeltime->getmaxper() -1 : aPeriod;
    // Write out physical output.
    // loop for all vintages up to current period
    // children of output go in the child buffer
    for( int curr = 0; curr <= maxPer; ++curr ){
        // isTechnologyOperating will crash for sgm so avoid calling it
        if( aPeriod != -1 && !isTechnologyOperating( curr ) ){
            continue;
        }
        attrs[ "vintage" ] = util::toString( modeltime->getper_to_yr( curr ) );
        // Avoid doing the expensive units lookup when the good is the same as the
        // current sector we can just use the sector units.
        attrs[ "unit" ] = aOutput->getName() == mCurrentSector ? mCurrentOutputUnit : aOutput->getOutputUnits( mCurrentRegion );
        // Avoid writing zeros to save space.
        // Write physical output for each output.
        double currValue = aOutput->getPhysicalOutput( curr );
        if( !objects::isEqual<double>( currValue, 0.0 ) ) {
            XMLWriteElementWithAttributes( currValue, "physical-output", *childBuffer,
                mTabs.get(), attrs );
        }
        // Write currency output for each output. Note that it may be possible to convert
        // from currency to physical and vice-versa so for the sake of saving space maybe
        // don't write both?
        currValue = aOutput->getCurrencyOutput( curr );
        if( !objects::isEqual<double>( currValue, 0.0 ) ) {
            XMLWriteElementWithAttributes( currValue, "currency-output", *childBuffer,
                mTabs.get(), attrs );
        }
    }
}

void XMLDBOutputter::endVisitOutput( const IOutput* aOutput, const int aPeriod ) {
    // Write the output (open tag, children, and closing tag) to the buffer at
    // the top of the stack only if the child buffer is not empty
    iostream* childBuffer = popBufferStack();
    iostream* parentBuffer = popBufferStack();
    if( /*!childBuffer->str().empty()*/childBuffer->rdbuf()->in_avail() ){
        // retBuffer is still at the top of the stack
        iostream* retBuffer = mBufferStack.top();
        (*retBuffer) << parentBuffer->rdbuf() << childBuffer->rdbuf();
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

void XMLDBOutputter::startVisitAgProductionTechnology( const AgProductionTechnology* aAgProductionTechnology,
                                                         const int aPeriod )
{
    // Write out the non-energy cost assumed to be in same unit as sector price.
    writeItemToBuffer( aAgProductionTechnology->mNonLandVariableCost, "nonLandVariableCost", 
        *mBufferStack.top(), mTabs.get(), -1, "" );
    writeItemToBuffer( aAgProductionTechnology->mAgProdChange, "agProdChange", 
        *mBufferStack.top(), mTabs.get(), -1, "" );
    writeItemToBuffer( aAgProductionTechnology->mYield, "yield", 
        *mBufferStack.top(), mTabs.get(), -1, "" );
    writeItemToBuffer( aAgProductionTechnology->mHarvestsPerYear, "harvestsPerYear", 
        *mBufferStack.top(), mTabs.get(), -1, "" );
}

void XMLDBOutputter::endVisitAgProductionTechnology( const AgProductionTechnology* aTechnology,
                                                       const int aPeriod )
{
}

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

    // children of input go in the child buffer
    // note minicam is using real periods at this point where as sgm is
    // always using -1 
    double maxPer = aPeriod == -1 ? modeltime->getmaxper() -1 : aPeriod;

    // children of ghg go in the child buffer
    for( int i = 0; i <= maxPer; ++i ){
        // isTechnologyOperating will crash for sgm so avoid calling it
        if( aPeriod != -1 && !isTechnologyOperating( i ) ){
            continue;
        }

        attrs[ "year" ] = util::toString( modeltime->getper_to_yr( i ) );
        attrs[ "unit" ] = aGHG->mEmissionsUnit;
        currEmission = aGHG->getEmission( i );
        // Avoid writing zeros to save space.
        // Write GHG emissions.
        if( !objects::isEqual<double>( currEmission, 0.0 ) ) {
            XMLWriteElementWithAttributes( currEmission, "emissions",
                *childBuffer, mTabs.get(), attrs );
        }

        // Write sequestered amount of GHG emissions.
        // TODO: Note replicating old behavior of including geologic and feedstock
        // sequestration but is that what we really wanted in the first place?
        currEmission = mCurrentTechnology && mCurrentTechnology->mCaptureComponent ?
            mCurrentTechnology->mCaptureComponent->getSequesteredAmount( aGHG->getName(), true, i ) +
            mCurrentTechnology->mCaptureComponent->getSequesteredAmount( aGHG->getName(), false, i ) : 0.0;
        if( !objects::isEqual<double>( currEmission, 0.0 ) ) {
            XMLWriteElementWithAttributes( currEmission, "emissions-sequestered",
                *childBuffer, mTabs.get(), attrs );
        }

        // Write indirect emissions if this is CO2.
        if( aGHG->getName() == "CO2" && !util::isEqual( mCurrIndirectEmissions[ i ], 0.0 ) ){
            writeItemToBuffer( mCurrIndirectEmissions[ i ], "indirect-emissions", *childBuffer, 
                mTabs.get(), i, aGHG->mEmissionsUnit );
        }
    }
}

void XMLDBOutputter::endVisitGHG( const AGHG* aGHG, const int aPeriod ){
    // Write the ghg (open tag, children, and closing tag) to the buffer at
    // the top of the stack only if the child buffer is not empty
    iostream* childBuffer = popBufferStack();
    iostream* parentBuffer = popBufferStack();
    if( /*!childBuffer->str().empty()*/childBuffer->rdbuf()->in_avail() ){
        // retBuffer is still on the top of the stack
        iostream* retBuffer = mBufferStack.top();
        (*retBuffer) << parentBuffer->rdbuf() << childBuffer->rdbuf();
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
    // Write the opening market tag.
    XMLWriteOpeningTag( Market::getXMLNameStatic(), mBuffer, mTabs.get(),
                        aMarket->getName(), aMarket->getYear() );

    XMLWriteElement( aMarket->getGoodName(), "MarketGoodOrFuel",mBuffer, mTabs.get() );
    XMLWriteElement( aMarket->getRegionName(), "MarketRegion", mBuffer, mTabs.get() );

    // if next market clear out units to be updated
    if( mCurrentMarket != aMarket->getName() ){
        mCurrentMarket.clear();
        mCurrentMarket = aMarket->getName();
        mCurrentPriceUnit.clear();
        mCurrentOutputUnit.clear();
    }
    // Store unit information from base period
    if( aMarket->getYear() == scenario->getModeltime()->getStartYear() ) {
        if( mCurrentPriceUnit.empty() ){
            mCurrentPriceUnit = aMarket->getMarketInfo()->getString( "price-unit", false );
        }
        if( mCurrentOutputUnit.empty() ){
            mCurrentOutputUnit = aMarket->getMarketInfo()->getString( "output-unit", false );
        }
    }

    writeItem( "price", mCurrentPriceUnit, aMarket->getPrice(), -1 );
    writeItem( "demand", mCurrentOutputUnit, aMarket->getRawDemand(), -1 );
    writeItem( "supply", mCurrentOutputUnit, aMarket->getRawSupply(), -1 );

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
         writeItemUsingYear( "HFC125-concentration", "PPT",
                             aClimateModel->getConcentration( "HFC125", year ),
                             year );
         writeItemUsingYear( "HFC134a-concentration", "PPT",
                             aClimateModel->getConcentration( "HFC134A", year ),
                             year );
         writeItemUsingYear( "HFC143A-concentration", "PPT",
                             aClimateModel->getConcentration( "HFC143A", year ),
                             year );
         writeItemUsingYear( "HFC245fa-concentration", "PPT",
                             aClimateModel->getConcentration( "HFC245fa", year ),
                             year );
        writeItemUsingYear( "HFC227ea-concentration", "PPT",
                             aClimateModel->getConcentration( "HFC227ea", year ),
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
        // Kyoto Forcing
        writeItemUsingYear( "forcing-Kyoto", "W/m^2",
                             aClimateModel->getForcing( "CO2", util::round( year ) )
        + aClimateModel->getForcing( "CH4", util::round( year ) )
        + aClimateModel->getForcing( "N2O", util::round( year ) )
        + aClimateModel->getForcing( "HFC125", util::round( year ) )
        + aClimateModel->getForcing( "HFC134A", util::round( year ) )
        + aClimateModel->getForcing( "HFC143A", util::round( year ) )
        + aClimateModel->getForcing( "HFC227ea", util::round( year ) )
        + aClimateModel->getForcing( "HFC245fa", util::round( year ) )
        + aClimateModel->getForcing( "SF6", util::round( year ) )
        + aClimateModel->getForcing( "CF4", util::round( year ) )
        + aClimateModel->getForcing( "C2F6", util::round( year ) )
        + aClimateModel->getForcing( "OtherHC", util::round( year ) ),
                             year );

        // HFCs Forcing
        writeItemUsingYear( "forcing-HFCs", "W/m^2",
                             aClimateModel->getForcing( "HFC125", util::round( year ) )
        + aClimateModel->getForcing( "HFC134A", util::round( year ) )
        + aClimateModel->getForcing( "HFC143A", util::round( year ) )
        + aClimateModel->getForcing( "HFC227ea", util::round( year ) )
        + aClimateModel->getForcing( "HFC245fa", util::round( year ) )
        + aClimateModel->getForcing( "HFC23", util::round( year ) )
        + aClimateModel->getForcing( "HFC32", util::round( year ) ),
                             year );

                // Long-lived Forcing
        writeItemUsingYear( "forcing-halocarbons", "W/m^2",
        aClimateModel->getForcing( "HFC125", util::round( year ) )
        + aClimateModel->getForcing( "HFC134A", util::round( year ) )
        + aClimateModel->getForcing( "HFC143A", util::round( year ) )
        + aClimateModel->getForcing( "HFC227ea", util::round( year ) )
        + aClimateModel->getForcing( "HFC245fa", util::round( year ) )
        + aClimateModel->getForcing( "SF6", util::round( year ) )
        + aClimateModel->getForcing( "CF4", util::round( year ) )
        + aClimateModel->getForcing( "C2F6", util::round( year ) )
        + aClimateModel->getForcing( "OtherHC", util::round( year ) )
        + aClimateModel->getForcing( "Montreal", util::round( year ) ),
                             year );
        
        // PFCs Forcing
        writeItemUsingYear( "forcing-PFCs", "W/m^2",
        aClimateModel->getForcing( "CF4", util::round( year ) )
        + aClimateModel->getForcing( "C2F6", util::round( year ) ),
                             year );

        // CO2 Forcing
        writeItemUsingYear( "forcing-CO2", "W/m^2",
                             aClimateModel->getForcing( "CO2", util::round( year ) ),
                             year );
		
		// CH4 Forcing
        writeItemUsingYear( "forcing-CH4", "W/m^2",
						   aClimateModel->getForcing( "CH4", util::round( year ) ),
						   year );
		
		// N2O Forcing
        writeItemUsingYear( "forcing-N2O", "W/m^2",
						   aClimateModel->getForcing( "N2O", util::round( year ) ),
						   year );

		// SO2 Forcing
        writeItemUsingYear( "forcing-SO2", "W/m^2",
						   aClimateModel->getForcing( "SO2", util::round( year ) ),
						   year );
		
		// DirSO2 Forcing
        writeItemUsingYear( "forcing-DirSO2", "W/m^2",
						   aClimateModel->getForcing( "DirSO2", util::round( year ) ),
						   year );
		
		// TropO3 Forcing
        writeItemUsingYear( "forcing-TropO3", "W/m^2",
						   aClimateModel->getForcing( "TropO3", util::round( year ) ),
						   year );
		
		// BC Forcing
        writeItemUsingYear( "forcing-BC", "W/m^2",
						   aClimateModel->getForcing( "BC", util::round( year ) ),
						   year );
		
		// OC Forcing
        writeItemUsingYear( "forcing-OC", "W/m^2",
						   aClimateModel->getForcing( "OC", util::round( year ) ),
						   year );
		
        // StratH2O Forcing
        writeItemUsingYear( "forcing-StratH2O", "W/m^2",
                           aClimateModel->getForcing( "StratH2O", util::round( year ) ),
                           year );
        
        // Albedo Forcing
        writeItemUsingYear( "forcing-Albedo", "W/m^2",
                           aClimateModel->getForcing( "Albedo", util::round( year ) ),
                           year );
        
        // Add forcing for the larger F-gases individually
        // SF6 Forcing
        writeItemUsingYear( "forcing-SF6", "W/m^2",
        aClimateModel->getForcing( "SF6", util::round( year ) ),
                             year );
        
        // CF4 Forcing
        writeItemUsingYear( "forcing-CF4", "W/m^2",
                           aClimateModel->getForcing( "CF4", util::round( year ) ),
                           year );
              
        // HFC125 Forcing
        writeItemUsingYear( "forcing-HFC125", "W/m^2",
                           aClimateModel->getForcing( "HFC125", util::round( year ) ),
                           year );
        
        // HFC134a Forcing
        writeItemUsingYear( "forcing-HFC134a", "W/m^2",
                           aClimateModel->getForcing( "HFC134A", util::round( year ) ),
                           year );
        
        // HFC23 Forcing
        writeItemUsingYear( "forcing-HFC23", "W/m^2",
                           aClimateModel->getForcing( "HFC23", util::round( year ) ),
                           year );
        
        // HFC143a Forcing
        writeItemUsingYear( "forcing-HFC143a", "W/m^2",
                           aClimateModel->getForcing( "HFC143A", util::round( year ) ),
                           year );
        
        // HFC245fa Forcing
        writeItemUsingYear( "forcing-HFC245fa", "W/m^2",
                           aClimateModel->getForcing( "HFC245fa", util::round( year ) ),
                           year );
        
        // HFC32 Forcing
        writeItemUsingYear( "forcing-HFC32", "W/m^2",
                           aClimateModel->getForcing( "HFC32", util::round( year ) ),
                           year );
        
		// long-lived F-gas Forcing
        writeItemUsingYear( "forcing-longlivedFgas", "W/m^2",
						   aClimateModel->getForcing( "SF6", util::round( year ) )
						   + aClimateModel->getForcing( "CF4", util::round( year ) )
						   + aClimateModel->getForcing( "C2F6", util::round( year ) ),
						   year );
		
		// Montreal gas Forcing
        writeItemUsingYear( "forcing-Montreal", "W/m^2",
						   aClimateModel->getForcing( "Montreal", util::round( year ) ),
						   year );
		
        // Total Forcing
        writeItemUsingYear( "forcing-total", "W/m^2",
                            aClimateModel->getTotalForcing( year ),
                             year );
        
        // RCP Forcing
        writeItemUsingYear( "forcing-RCP", "W/m^2",
                           aClimateModel->getForcing( "RCP", year ),
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
    // Write the opening LandLeaf tag except we need to decompose the name and write
    // each out as an attribute.
    // TODO: just put this in as a utility such as XMLWriteOpeningTagWithAttributes?
    map<string, string> decomposedNames = decomposeLandName( aLandLeaf->getName() );
    mTabs->writeTabs( mBuffer );
    mBuffer << "<" << LandLeaf::getXMLNameStatic();
    typedef typename map<string, string>::const_iterator MapIterator;
    for( MapIterator entry = decomposedNames.begin(); entry != decomposedNames.end(); ++entry ){
        mBuffer << " " << entry->first <<"=\"" << entry->second << "\"";
    }
    mBuffer << ">" << std::endl;
    mTabs->increaseIndent();

    // Loop over the periods to output LandLeaf information.
    // The loops are separated so the types are grouped together to make it easier to
    // read the XML. Note this writes total land allocation (so all land sums to same total)
    // This does not report the land harvested in a given year.
    const Modeltime* modeltime = scenario->getModeltime();
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        writeItem( "land-allocation", "thous km2",
           aLandLeaf->getLandAllocation( aLandLeaf->getName(), i ),
           i );
    }
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        writeItem( "share", "", aLandLeaf->getShare( i ), i );
    }  
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        writeItem( "profit-rate", "$/thous km2", aLandLeaf->mProfitRate[ i ], i );
    }
    for( int i = 0; i < modeltime->getmaxper(); ++i ){
        writeItem( "share-weight", "", aLandLeaf->mShareWeight[ i ], i );
    }
}

void XMLDBOutputter::endVisitLandLeaf( const LandLeaf* aLandLeaf, const int aPeriod ){
    XMLWriteClosingTag( "LandLeaf", mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitCarbonCalc( const ICarbonCalc* aCarbonCalc, const int aPeriod ){
    XMLWriteOpeningTag( LandCarbonDensities::getXMLNameStatic(), mBuffer, mTabs.get() );

    // Loop over the periods to output Carbon information.

    // Printing yearly values would be too much data.
    const Modeltime* modeltime = scenario->getModeltime();
    const int startingYear = max( Configuration::getInstance()->getInt( "carbon-output-start-year", 1990 ), CarbonModelUtils::getStartYear() );
    int outputInterval = Configuration::getInstance()->getInt( "climateOutputInterval",modeltime->gettimestep( 0 ) );
    
    for( int aYear = startingYear; 
             aYear <= modeltime->getper_to_yr( modeltime->getmaxper() - 1 ) || aYear == modeltime->getper_to_yr( modeltime->getmaxper() - 1 ); 
             aYear += outputInterval ){
        writeItemUsingYear( "land-use-change-emission", "MtC/yr", aCarbonCalc->getNetLandUseChangeEmission( aYear ), aYear );
        writeItemUsingYear( "above-land-use-change-emission", "MtC/yr", aCarbonCalc->getNetLandUseChangeEmissionAbove( aYear ), aYear );
        writeItemUsingYear( "below-land-use-change-emission", "MtC/yr", aCarbonCalc->getNetLandUseChangeEmissionBelow( aYear ), aYear );
     }
    
    for( int aYear = modeltime->getStartYear(); 
             aYear <= modeltime->getper_to_yr( modeltime->getmaxper() - 1 ) || aYear == modeltime->getper_to_yr( modeltime->getmaxper() - 1 ); 
             aYear += outputInterval ){
        XMLWriteElement( aCarbonCalc->getActualAboveGroundCarbonDensity( aYear ),
                        "above-ground-carbon-density", mBuffer, mTabs.get(), aYear );
        XMLWriteElement( aCarbonCalc->getActualBelowGroundCarbonDensity( aYear ),
                        "below-ground-carbon-density", mBuffer, mTabs.get(), aYear );
        
    }
    for( int aYear = modeltime->getStartYear();
             aYear <= modeltime->getper_to_yr( modeltime->getmaxper() - 1 ) || aYear == modeltime->getper_to_yr( modeltime->getmaxper() - 1 );
             aYear += outputInterval ){
        writeItemUsingYear( "above-ground-carbon-stock", "MtC", aCarbonCalc->getAboveGroundCarbonStock( aYear ), aYear );

    }
}

void XMLDBOutputter::endVisitCarbonCalc( const ICarbonCalc* aCarbonCalc, const int aPeriod ){
    XMLWriteClosingTag( LandCarbonDensities::getXMLNameStatic(), mBuffer, mTabs.get() );
} 

void XMLDBOutputter::startVisitExpenditure( const Expenditure* aExpenditure, const int aPeriod ) {
    // write the expenditure tag and it's children in temp buffers so that we can
    // check if anything was really written out and avoid writing blank expenditures
    stringstream* parentBuffer = new stringstream();
    stringstream* childBuffer = new stringstream();

    // the opening tag gets written in the parent buffer
    const Modeltime* modeltime = scenario->getModeltime();
    // should I make and getXMLNameStatic() form expenditure?
    XMLWriteOpeningTag( "expenditure", *parentBuffer, mTabs.get(), "", modeltime->getper_to_yr( aPeriod ) );

    // put the buffers on a stack so that we have the correct ordering
    mBufferStack.push( parentBuffer );
    mBufferStack.push( childBuffer );

    double currValue = 0.0;
    for( int i = 0; i < Expenditure::END; ++i ) {
        currValue = aExpenditure->getValue( static_cast< Expenditure::ExpenditureType >( i ) );
        if( !objects::isEqual<double>( currValue, 0.0 ) ) {
            XMLWriteElement( currValue,
                aExpenditure->enumToXMLName( static_cast< Expenditure::ExpenditureType >( i ) ),
                *childBuffer, mTabs.get() );
        }
    }
}

void XMLDBOutputter::endVisitExpenditure( const Expenditure* aExpenditure, const int aPeriod ) {
    // Write the expenditure (open tag, children, and closing tag) to the buffer at
    // the top of the stack only if the child buffer is not empty
    iostream* childBuffer = popBufferStack();
    iostream* parentBuffer = popBufferStack();
    if( /*!childBuffer->str().empty()*/childBuffer->rdbuf()->in_avail() ){
        // retBuffer is still at the top of the stack
        iostream* retBuffer = mBufferStack.top();
        (*retBuffer) << parentBuffer->rdbuf() << childBuffer->rdbuf();
        XMLWriteClosingTag( "expenditure", *retBuffer, mTabs.get() );
    }
    else {
        // if we don't write the closing tag we still need to decrease the indent
        mTabs->decreaseIndent();
    }
    
    // clean up any extra buffers
    delete childBuffer;
    delete parentBuffer;
}

void XMLDBOutputter::startVisitNodeInput( const NodeInput* aNodeInput, const int aPeriod ) {
    // write the nodeInput tag and it's children in temp buffers so that we can
    // check if anything was really written out and avoid writing blank nodeInputs
    // which will be very often since we are currently only writing aidads paramaters
    stringstream* parentBuffer = new stringstream();
    stringstream* childBuffer = new stringstream();

    XMLWriteOpeningTag( NodeInput::getXMLNameStatic(), *parentBuffer, mTabs.get(), aNodeInput->getName() );

    // put the buffers on a stack so that we have the correct ordering
    mBufferStack.push( parentBuffer );
    mBufferStack.push( childBuffer );

    // note that the price/income (alpha/beta) do not really change over time so we will just
    // use the base period
    if( !objects::isEqual<double>( aNodeInput->getPriceElasticity( 0 ), 0.0 ) ||
        !objects::isEqual<double>( aNodeInput->getIncomeElasticity( 0 ), 0.0 ) ) {
        // we have AIDADS/LES paramaters so write them out
        XMLWriteElement( aNodeInput->getPricePaid( "", aPeriod ), "price", *childBuffer, mTabs.get() );
        XMLWriteElement( aNodeInput->getPhysicalDemand( aPeriod ), "demand", *childBuffer, mTabs.get() );
    }
}

void XMLDBOutputter::endVisitNodeInput( const NodeInput* aNodeInput, const int aPeriod ) {
    // Write the nodeInput (open tag, children, and closing tag) to the buffer at
    // the top of the stack only if the child buffer is not empty
    iostream* childBuffer = popBufferStack();
    iostream* parentBuffer = popBufferStack();
    if( /*!childBuffer->str().empty()*/childBuffer->rdbuf()->in_avail() ){
        // retBuffer is still at the top of the stack
        iostream* retBuffer = mBufferStack.top();
        (*retBuffer) << parentBuffer->rdbuf() << childBuffer->rdbuf();
        XMLWriteClosingTag( NodeInput::getXMLNameStatic(), *retBuffer, mTabs.get() );
    }
    else {
        // if we don't write the closing tag we still need to decrease the indent
        mTabs->decreaseIndent();
    }
    
    // clean up any extra buffers
    delete childBuffer;
    delete parentBuffer;
}

void XMLDBOutputter::startVisitNationalAccount( const NationalAccount* aNationalAccount, const int aPeriod ) {
    // national accounts are visited by period so the year attribute can be converted from aPeriod
    const Modeltime* modeltime = scenario->getModeltime();
    XMLWriteOpeningTag( NationalAccount::getXMLNameStatic(), mBuffer, mTabs.get(), "", modeltime->getper_to_yr( aPeriod ) );

    // TODO: move enumToXMLName to public so these element names are not hard coded here
    double currValue = aNationalAccount->getAccountValue( NationalAccount::GNP_NOMINAL );
    if( !objects::isEqual<double>( currValue, 0.0 ) ) {
        XMLWriteElement( currValue, "GNP-nominal", mBuffer, mTabs.get() );
    }
    currValue = aNationalAccount->getAccountValue( NationalAccount::GNP_REAL );
    if( !objects::isEqual<double>( currValue, 0.0 ) ) {
        XMLWriteElement( currValue, "GNP-real", mBuffer, mTabs.get() );
    }
    currValue = aNationalAccount->getAccountValue( NationalAccount::CARBON_TAX );
    if( !objects::isEqual<double>( currValue, 0.0 ) ) {
        XMLWriteElement( currValue, "carbon-tax", mBuffer, mTabs.get() );
    }
}

void XMLDBOutputter::endVisitNationalAccount( const NationalAccount* aNationalAccount, const int aPeriod ) {
    XMLWriteClosingTag( NationalAccount::getXMLNameStatic(), mBuffer, mTabs.get() ) ;
}

void XMLDBOutputter::startVisitGCAMConsumer( const GCAMConsumer* aGCAMConsumer, const int aPeriod ) {
    XMLWriteOpeningTag( aGCAMConsumer->getXMLName(), mBuffer, mTabs.get(), aGCAMConsumer->getName() );

    const Modeltime* modeltime = scenario->getModeltime();
    XMLWriteVector( aGCAMConsumer->mSubregionalIncome, "subregional-percapita-income", mBuffer, mTabs.get(), modeltime );
    XMLWriteVector( aGCAMConsumer->mSubregionalPopulation, "subregional-population", mBuffer, mTabs.get(), modeltime );
}

void XMLDBOutputter::endVisitGCAMConsumer( const GCAMConsumer* aGCAMConsumer, const int aPeriod ) {
    XMLWriteClosingTag( aGCAMConsumer->getXMLName(), mBuffer, mTabs.get() );
}

void XMLDBOutputter::startVisitBuildingNodeInput( const BuildingNodeInput* aBuildingNodeInput, const int aPeriod ) {
    // write the BuildingNodeInput tag and it's children in temp buffers so that we can
    // check if anything was really written out and avoid writing blank node inputs
    stringstream* parentBuffer = new stringstream();
    stringstream* childBuffer = new stringstream();

    XMLWriteOpeningTag( BuildingNodeInput::getXMLNameStatic(), *parentBuffer, mTabs.get(),
        aBuildingNodeInput->getName() );

    // put the buffers on a stack so that we have the correct ordering
    mBufferStack.push( parentBuffer );
    mBufferStack.push( childBuffer );

    writeItemToBuffer( aBuildingNodeInput->getSatiationDemandFunction()->mSatiationImpedance,
                       "satiation-impedance", *childBuffer, mTabs.get(), 1, "unitless" );
    writeItemToBuffer( aBuildingNodeInput->getSatiationDemandFunction()->mSatiationLevel,
                       "satiation-level", *childBuffer, mTabs.get(), 1, "GJ/m^2" );
    const Modeltime* modeltime = scenario->getModeltime();
    for( int per = 0; per < modeltime->getmaxper(); ++per ) {
        double price = aBuildingNodeInput->getPricePaid( mCurrentRegion, per );
        if( !objects::isEqual<double>( price, 0.0 ) ) {
            writeItemToBuffer( price, "price",
                *childBuffer, mTabs.get(), per, "1975$/m^2" );
        }
        double floorspace = aBuildingNodeInput->getPhysicalDemand( per );
        if( !objects::isEqual<double>( floorspace, 0.0 ) ) {
            writeItemToBuffer( floorspace, "floorspace",
                *childBuffer, mTabs.get(), per, "billion m^2" );
        }
    }
}

void XMLDBOutputter::endVisitBuildingNodeInput( const BuildingNodeInput* aBuildingNodeInput, const int aPeriod ) {
    // Write the BuildingNodeInput (open tag, children, and closing tag) to the buffer at
    // the top of the stack only if the child buffer is not empty
    iostream* childBuffer = popBufferStack();
    iostream* parentBuffer = popBufferStack();
    if( /*!childBuffer->str().empty()*/childBuffer->rdbuf()->in_avail() ){
        // retBuffer is still at the top of the stack
        iostream* retBuffer = mBufferStack.top();
        (*retBuffer) << parentBuffer->rdbuf() << childBuffer->rdbuf();
        XMLWriteClosingTag( BuildingNodeInput::getXMLNameStatic(), *retBuffer, mTabs.get() );
    }
    else {
        // if we don't write the closing tag we still need to decrease the indent
        mTabs->decreaseIndent();
    }
    
    // clean up any extra buffers
    delete childBuffer;
    delete parentBuffer;
}

void XMLDBOutputter::startVisitBuildingServiceInput( const BuildingServiceInput* aBuildingServiceInput, const int aPeriod ) {
    startVisitInput( aBuildingServiceInput, aPeriod );

    writeItemToBuffer( aBuildingServiceInput->getSatiationDemandFunction()->mSatiationImpedance,
                       "satiation-impedance", *mBufferStack.top(), mTabs.get(), 1, "unitless" );
    writeItemToBuffer( aBuildingServiceInput->getSatiationDemandFunction()->mSatiationLevel,
                       "satiation-level", *mBufferStack.top(), mTabs.get(), 1, "GJ/m^2" );
    const Modeltime* modeltime = scenario->getModeltime();
    for( int per = 0; per < modeltime->getmaxper(); ++per ) {
        double serviceDensity = aBuildingServiceInput->mServiceDensity[ per ];
        if( !objects::isEqual<double>( serviceDensity, 0.0 ) ) {
            writeItemToBuffer( serviceDensity, "service-density",
                *mBufferStack.top(), mTabs.get(), per, "GJ/m^2" );
        }
    }
}

void XMLDBOutputter::endVisitBuildingServiceInput( const BuildingServiceInput* aBuildingServiceInput, const int aPeriod ) {
    endVisitInput( aBuildingServiceInput, aPeriod );
}

void XMLDBOutputter::startVisitFoodDemandInput( const FoodDemandInput* aFoodDemandInput, const int aPeriod ) {
    // set the correct price/demand units which will be used in startVisitInput
    mCurrentPriceUnit = "2005$/Mcal/day";
    mCurrentInputUnit = "Pcal/yr";
    startVisitInput( aFoodDemandInput, aPeriod );
    mCurrentPriceUnit.clear();
    mCurrentInputUnit.clear();

    const Modeltime* modeltime = scenario->getModeltime();
    for( int per = 0; per < modeltime->getmaxper(); ++per ) {
        double perCapConv = aFoodDemandInput->getAnnualDemandConversionFactor( per );
        double perCapDemand = aFoodDemandInput->getPhysicalDemand( per ) / perCapConv;
        if( !objects::isEqual<double>( perCapDemand, 0.0 ) ) {
            writeItemToBuffer( perCapDemand, "demand-percap",
                *mBufferStack.top(), mTabs.get(), per, "Kcal/per/day" );
        }
        writeItemToBuffer( aFoodDemandInput->mRegionalBias[ per ], "regional-bias",
            *mBufferStack.top(), mTabs.get(), per, "Kcal/per/day" );
    }
}

void XMLDBOutputter::endVisitFoodDemandInput( const FoodDemandInput* aFoodDemandInput, const int aPeriod ) {
    endVisitInput( aFoodDemandInput, aPeriod );
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
    return mCurrentTechnology->isOperating( aPeriod );
}

/**
 * \brief Pops the buffer off of the top of the stack and returns it.
 * \details A convience method so that the pop can be in one line instead of two.
 * \return The buffer that was at the top of the stack.
 * \author Pralit Patel
 */
 iostream* XMLDBOutputter::popBufferStack(){
    iostream* ret = mBufferStack.top();
    mBufferStack.pop();
    return ret;
}

/**
 * \brief The name of LandLeaf / AgProductionTechnology actually compose of potentially
 *        several identifiers.  This method decomposes that name into it's component
 *        identifiers so each can be written as it's own attribute.
 * \details This method will attempt to parse out identifiers by spliting on underscore
 *          characters from right to left first checking to see if it finds:
 *          "mgmt-tech"=(hi|lo)
 *          "water"=(IRR|RFD)
 *          The next token will be assumed to be the "land-region" and the rest will
 *          be assumed to be "crop".  Note the full name will also be included in the
 *          results under "name".
 * \return A map of identifiers to their corresponding values as detailed above.
 */
map<string, string> XMLDBOutputter::decomposeLandName( string aLandName ) {
    bool foundLandRegion = false;
    size_t pos;
    map<string, string> ret;
    // Include the full unparsed name in the results.
    ret[ "name " ] = aLandName;
    
    // Split the name on underscores from right to left.  Once we have found the
    // land-region we will assume the rest of the name is just the crop (some of
    // which include underscores in their names).
    while( !foundLandRegion && ( pos = aLandName.rfind( '_' ) ) != string::npos ) {
        string currName = aLandName.substr( pos + 1, aLandName.length() - pos );
        aLandName.erase( pos, aLandName.length() );
        if( currName == "hi" || currName == "lo" ) {
            ret[ "mgmt-tech" ] = currName;
        }
        else if( currName == "IRR" || currName == "RFD" ) {
            ret[ "water" ] = currName;
        }
        else {
            ret[ "land-region" ] = currName;
            foundLandRegion = true;
        }
    }
    ret[ "crop" ] = aLandName;
    
    return ret;
}

#if( __HAVE_JAVA__ )
/*!
 * \brief Constructs a boost IO sink that sends data to Java.
 * \details The constructor will need to initialize JNI boiler plate and set up
 *          a buffer to pass the data over to Java.  It will also do a bunch of
 *          error checking and set the error flag as appropriate.
 * \param aJNIContainer A weak pointer to the container which holds the Java VM
 *                      references.  May be null if it did not initialize properly.
 */
XMLDBOutputter::SendToJavaIOSink::SendToJavaIOSink( const JNIContainer* aJNIContainer )
:mJNIContainer( aJNIContainer ),
// Get the receiveDataFromGCAM method from the write DB class with arguments of a byte
// array "[B", an integer "I", and a return type of bool "Z" 
mReceiveDataMID( aJNIContainer ? aJNIContainer->mJavaEnv->GetMethodID( aJNIContainer->mWriteDBClass, "receiveDataFromGCAM", "([BI)Z") : 0 ),
// The same buffer size as the one used in Java, if we try to tune this we should
// adjust it both here and in Java.
BUFFER_SIZE( 1024 * 1024 ),
mJNIBuffer( aJNIContainer ? aJNIContainer->mJavaEnv->NewByteArray( BUFFER_SIZE  ) : 0 ),
// If any of the required JNI data structures were not properly set then set the error flag.
mErrorFlag( !mJNIContainer || !mReceiveDataMID || !mJNIBuffer )
{
}

/*!
 * \brief Destructor
 */
XMLDBOutputter::SendToJavaIOSink::~SendToJavaIOSink() {
    // Nothing to do since this class does not manage the memory for
    // the JNI Container data.
}

/*!
 * \brief Read bytes as they are generated and pass them through to Java.
 * \param aData The current buffer of data that needs to be sent.
 * \param aLength How many chars from the buffer should be read.
 * \warning If there was an error for any reason while dealing with Java the error flag
 *          will be set and no more data will be sent.
 */
streamsize XMLDBOutputter::SendToJavaIOSink::write( const char *aData, std::streamsize aLength ) {
    streamsize offset = 0;
    const jbyte* jniData = reinterpret_cast<const jbyte*>( aData );
    while( !mErrorFlag && offset < aLength ) {
        streamsize numRead = min( aLength - offset, BUFFER_SIZE );
        mJNIContainer->mJavaEnv->SetByteArrayRegion( mJNIBuffer, 0, numRead, jniData+offset );
        mErrorFlag = mJNIContainer->mJavaEnv->CallBooleanMethod( mJNIContainer->mWriteDBInstance,
            mReceiveDataMID, mJNIBuffer, numRead );
        offset += numRead;
    }
    return offset;
}
#endif
