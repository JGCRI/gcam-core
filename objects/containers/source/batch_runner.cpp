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

#include <iostream>
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include "containers/include/batch_runner.h"
#include "containers/include/mac_generator_scenario_runner.h"
#include "containers/include/single_scenario_runner.h"
#include "util/base/include/timer.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/configuration.h"
#include "containers/include/scenario.h"
#include "util/logger/include/ilogger.h"
using namespace std;
using namespace xercesc;

/*! Constructor
* \param aBatchFileName The filename to use as the batch information file.
*/
BatchRunner::BatchRunner( const string& aBatchFileName ):
mBatchFileName( aBatchFileName ){ 
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
    // Parse the batch file passed in on the command line.
    XercesDOMParser* parser = XMLHelper<void>::getParser();
    DOMNode* root = XMLHelper<void>::parseXML( mBatchFileName, parser );

    // Private XML Parse
    XMLParse( root );
    
    return true;
}

/*! \brief Run the set of Scenarios as instructed by the batch configuration file.
* \details This is the main function of the BatchRunner which determines all permutations
* of the ComponentSets and runs a Scenario for each. 
* \param aTimer The timer used to print out the amount of time spent performing operations.
* \todo Handle duplicate names. Print a warning at least.
* \return Whether all model runs solved successfully.
*/
bool BatchRunner::runScenario( Timer& aTimer ){
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
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Running scenario " << aComponents.mName << "..." << endl;

    // Check if cost curve creation is needed.
    const Configuration* conf = Configuration::getInstance();
    if( conf->getBool( "createCostCurve" ) ){
        const string abatedGas = conf->getString( "AbatedGasForCostCurves", "CO2" );
        const unsigned int numPoints = conf->getInt( "numPointsForCO2CostCurve", 5 );
        mInternalRunner.reset( new MACGeneratorScenarioRunner( abatedGas, numPoints ) );
    }
    // Run a standard scenario.
    else {
        mInternalRunner.reset( new SingleScenarioRunner() );
    }
    // Setup the scenario.
    mInternalRunner->setupScenario( aTimer, aComponents.mName, components );

    // Run the scenario.
    bool success = mInternalRunner->runScenario( aTimer );
    
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
void BatchRunner::XMLParse( const DOMNode* aRoot ){
    // assume we were passed a valid node.
    assert( aRoot );
    
    // get the children of the node.
    DOMNodeList* nodeList = aRoot->getChildNodes();
    
    // loop through the children
    for ( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        // This is a three level XMLParse. Breaking up for now.
		else if ( nodeName == "ComponentSet" ){ // Changed internal struct to component, backwards compatable.
            XMLParseComponentSet( curr );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing BatchScenarioRunner." << endl;
        }
    }
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
    newComponent.mName = XMLHelper<string>::getAttrString( aNode, "name" );

    // get the children of the node.
    DOMNodeList* nodeList = aNode->getChildNodes();

    // loop through the children
    for ( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
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
    newFileSet.mName = XMLHelper<string>::getAttrString( aNode, "name" );

    // get the children of the node.
    DOMNodeList* nodeList = aNode->getChildNodes();

    // loop through the children
    for ( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        else if ( nodeName == "Value" ){
            // Create the new File
            File newFile;
            // Get the name of the file.
            newFile.mName = XMLHelper<string>::getAttrString( curr, "name" );
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
