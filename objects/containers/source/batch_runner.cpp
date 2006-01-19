/*! 
* \file batch_runner.cpp
* \ingroup objects
* \brief BatchRunner class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
#if defined(_MSC_VER)
#pragma warning( disable : 4503 )
#endif 

#include "util/base/include/definitions.h"
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "containers/include/batch_runner.h"
#include "containers/include/scenario_runner_factory.h"
#include "util/base/include/timer.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/configuration.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/scenario.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! Constructor
*/
BatchRunner::BatchRunner(){ 
}

//! Destructor
BatchRunner::~BatchRunner(){
}

/*! \brief Setup the batch runner.
* \details Initialize the batch runner by parsing the XML batch configuration file. This function
* does not initialize the individual runs.
* \param aTimer The timer used to print out the amount of time spent performing operations.
* \param aScenComponents A list of scenario add on files to read in, ignored by the BatchRunner.
* \return Whether setup was successful. Currently always true.
*/
bool BatchRunner::setupScenario( Timer& aTimer, const string aName, const list<string> aScenComponents ){
    // Get the name of the batch file from the Configuration.
    const string batchFile = Configuration::getInstance()->getFile( "BatchFileName" );
    
    // Parse the batch file.
    return XMLHelper<void>::parseXML( batchFile, this );
}

/*! \brief Run the set of Scenarios as instructed by the batch configuration file.
* \details This is the main function of the BatchRunner which determines all permutations
* of the ComponentSets and runs a Scenario for each.
* \param aSinglePeriod This parameter is currently ignored.
* \param aTimer The timer used to print out the amount of time spent performing operations.
* \todo Handle duplicate names. Print a warning at least.
* \return Whether all model runs solved successfully.
*/
bool BatchRunner::runScenario( const int aSinglePeriod, Timer& aTimer ){
    // Quick error checking for empty readin.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    if( mComponentSet.empty() ){
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "No scenario sets to run!" << endl;
        return false;
    }

    // Initialize each components iterator to the beginning of the vector. 
    for( ComponentSet::iterator currSet = mComponentSet.begin(); currSet != mComponentSet.end(); ++currSet ){
        currSet->mFileSetIterator = currSet->mFileSets.begin();
    }
    
    bool shouldExit = false;
    bool success = true;
    while( !shouldExit ){
        // Perform the currently setup run.
        // Create the list of addons.
        Component fileSetsToRun;

        // Loop through the ComponentSet to create the current scenario.
        for( ComponentSet::const_iterator currSet = mComponentSet.begin(); currSet != mComponentSet.end(); ++currSet ){
            fileSetsToRun.mFileSets.push_back( *( currSet->mFileSetIterator ) );
            fileSetsToRun.mName += currSet->mFileSetIterator->mName;
        }
        // Run it.
        success &= runSingleScenario( fileSetsToRun, aTimer );

        // Loop forward to find a position to increment.
        for( ComponentSet::iterator outPos = mComponentSet.begin(); outPos != mComponentSet.end(); ++outPos ){
            // Increment the position.
            outPos->mFileSetIterator++;

            // Check if it reached the end.
            if( outPos->mFileSetIterator == outPos->mFileSets.end() ){
                // Reset it to the beginning.
                outPos->mFileSetIterator = outPos->mFileSets.begin();

                // If we just reset the last position, exit the loop.
                if( outPos == --mComponentSet.end() ){
                    shouldExit = true;
                }
            }
            else {
                break;
            }
        }
    }
    return success;
}

/*! \brief Print the BatchRunner output.
* \param aTimer The timer used to print out the amount of time spent performing operations.
* \param aCloseDB Whether to close the database and output variable IDs. Defaults to true.
*/
void BatchRunner::printOutput( Timer& aTimer, const bool aCloseDB ) const {
    // Print out any scenarios that did not solve.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    if( mUnsolvedNames.empty() ){
        mainLog << "All model runs completed successfully." << endl;
    }
    else {
        mainLog << "Model runs that did not solve correctly: " << endl;
        for( vector<string>::const_iterator name = mUnsolvedNames.begin(); name != mUnsolvedNames.end(); ++name ){
            mainLog << *name << endl;
        }
    }
}

/*! \brief Helper function which runs a single scenario created by the BatchRunner.
* \details This function expans the list of FileSets into a list of scenario components files
* to parse. The scenario name is created by combining the names of all the filesets with the name
* read from the configruration file. The function selects the appropriate type of ScenarioRunner 
* from the configuration file, and initializes it with the list of scenario components. 
* It then runs the Scenario and prints its output.
* \param aComponents A map of FileSets which is expanded to create the list of scenario components to read in.
* \param aTimer The timer used to print out the amount of time spent performing operations.
* \return Whether the model run solved successfully.
*/
bool BatchRunner::runSingleScenario( const Component aComponents, Timer& aTimer ){
    // Expand the file sets into a flat list and a scenario information string.
    list<string> components;

    for( vector<FileSet>::const_iterator currFileSet = aComponents.mFileSets.begin(); currFileSet != aComponents.mFileSets.end(); ++currFileSet ){
        for( vector<File>::const_iterator currFile = currFileSet->mFiles.begin(); currFile != currFileSet->mFiles.end(); ++currFile ){
            components.push_back( currFile->mPath );
        }
    }
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::WARNING );
    mainLog << "Running scenario " << aComponents.mName << "..." << endl;

    // Check if cost curve creation is needed.
    const Configuration* conf = Configuration::getInstance();
    if( conf->getBool( "find-path" ) ){
        mInternalRunner = ScenarioRunnerFactory::create( "policy-target-runner" );
    }
    else if( conf->getBool( "createCostCurve" ) ){
        mInternalRunner = ScenarioRunnerFactory::create( "mac-generator-scenario-runner" );
    }
    // Run a standard scenario.
    else {
        mInternalRunner = ScenarioRunnerFactory::create( "single-scenario-runner" );
    }

    // Set the global scenario pointer, as the internal runner was null when
    // main tried to set it.
    scenario = mInternalRunner->getInternalScenario();
    assert( scenario );

    // Setup the scenario.
    bool success = mInternalRunner->setupScenario( aTimer, aComponents.mName, components );
    // Check if setting up the scenario, which often includes parsing,
    // succeeded.
    if( !success ){
        return false;
    }

    // Run the scenario.
    success = mInternalRunner->runScenario( Scenario::RUN_ALL_PERIODS, aTimer );
    
    // Print the output.
    mInternalRunner->printOutput( aTimer );
    
    // If the run failed, add to the list of failed runs. CHECK ME!
    if( !success ){
        mUnsolvedNames.push_back( aComponents.mName );
    }
    return success;
}

/*! \brief Function which parses the XML BatchRunner configuration file.
* \details This function searches for base level XML tags and dispatches them
* to the correct helper function to be parsed. Currently only parses "ComponentSet" tags.
* \param aRoot Root node of the DOM tree.
*/
bool BatchRunner::XMLParse( const DOMNode* aRoot ){
    // assume we were passed a valid node.
    assert( aRoot );
    
    // get the children of the node.
    DOMNodeList* nodeList = aRoot->getChildNodes();
    
    // loop through the children
    for ( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == XMLHelper<void>::text() ) {
            continue;
        }
        // This is a three level XMLParse. Breaking up for now.
        else if ( nodeName == "ComponentSet" ){
            XMLParseComponentSet( curr );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing BatchScenarioRunner." << endl;
        }
    }
    return true;
}

/*! \brief Helper function to parse a single scenario set element.
* \details This function parses a single ComponentSet and adds it to the BatchRunner's list 
* of ComponentSets. It dispatches any FileSets it finds to the XMLParseFileSet helper function.
* \param aNode DOM node corresponding to the current ComponentSet.
*/
void BatchRunner::XMLParseComponentSet( const DOMNode* aNode ){
    // assume we were passed a valid node.
    assert( aNode );
    
    // Create a new Component
    Component newComponent;

    // Get the name of the component set. 
    newComponent.mName = XMLHelper<string>::getAttrString( aNode, XMLHelper<void>::name() );

    // get the children of the node.
    DOMNodeList* nodeList = aNode->getChildNodes();

    // loop through the children
    for ( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == XMLHelper<void>::text() ) {
            continue;
        }
        else if ( nodeName == "FileSet" ){
            XMLParseFileSet( curr, newComponent );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing ComponentSet." << endl;
        }
    }
    // Add the new component
    mComponentSet.push_back( newComponent );
}

/*! \brief Helper function to parse a single FileSet element.
* \details This function parses a single FileSet and adds it to the passed in ComponentSet's
* list of FileSets. 
* \param aNode DOM node corresponding to the current FileSet.
* \param aCurrComponent The ComponentSet to add this FileSet to.
*/
void BatchRunner::XMLParseFileSet( const DOMNode* aNode, Component& aCurrComponent ){
    // assume we were passed a valid node.
    assert( aNode );
    
    // Create the new file set and set the name.
    FileSet newFileSet;
    newFileSet.mName = XMLHelper<string>::getAttrString( aNode, XMLHelper<void>::name() );

    // get the children of the node.
    DOMNodeList* nodeList = aNode->getChildNodes();

    // loop through the children
    for ( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == XMLHelper<void>::text() ) {
            continue;
        }
        else if ( nodeName == "Value" ){
            // Create the new File
            File newFile;
            // Get the name of the file.
            newFile.mName = XMLHelper<string>::getAttrString( curr, XMLHelper<void>::name() );
            // Get the full path of the file.
            newFile.mPath = XMLHelper<string>::getValueString( curr );
            // Add the file to the current new file set.
            newFileSet.mFiles.push_back( newFile );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing FileSet." << endl;
        }
    }
    // Add the new file set to the current component.
    aCurrComponent.mFileSets.push_back( newFileSet );
}

const string& BatchRunner::getXMLNameStatic(){
    static const string XML_NAME = "batch-runner";
    return XML_NAME;
}

/*! \brief Get the internal scenario.
* \return The internal scenario.
*/
Scenario* BatchRunner::getInternalScenario(){
    return mInternalRunner->getInternalScenario();
}

/*! \brief Get the internal scenario.
* \return Constant pointer to the internal scenario.
*/
const Scenario* BatchRunner::getInternalScenario() const {
    return mInternalRunner->getInternalScenario();
}
