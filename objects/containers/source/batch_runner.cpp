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
* \detailed Initialize the batch runner by parsing the XML batch configuration file. This function
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
* \detailed This is the main function of the BatchRunner which determines all permutations
* of the ComponentSets and runs a Scenario for each. 
* \param aTimer The timer used to print out the amount of time spent performing operations.
*/
void BatchRunner::runScenario( Timer& aTimer ){
    // Quick error checking for empty readin.
    if( mComponentSets.empty() ){
        cout << "Error: No scenario sets to run!" << endl;
        return;
    }

    // Make a map of ComponentSet name to iterator position.
    typedef map<string, ComponentSet::reverse_iterator> PositionMap;
    PositionMap currPositions;

    // Initialize the map with the first positions of each.
    for( ComponentSetStructure::iterator currSet = mComponentSets.begin(); currSet != mComponentSets.end(); ++currSet ){
        currPositions[ currSet->first ] = currSet->second.rbegin();
    }
    
    bool shouldExit = false;
    while( !shouldExit ){
        // Perform the currently setup run.
        // Create the list of addons.
        map<string, FileSet> currentComponents;

        // Loop through the iterators.
        for( PositionMap::const_iterator currPos = currPositions.begin(); currPos != currPositions.end(); ++currPos ){
            currentComponents[ currPos->second->first ] = currPos->second->second;
        }
        // Run it.
        runSingleScenario( currentComponents, aTimer );

        // Loop backwards to find a position to decrement.
        for( PositionMap::reverse_iterator revPos = currPositions.rbegin(); revPos != currPositions.rend(); ++revPos ){
            // Increment the position.
            revPos->second++;

            // Check if it reached the end.
            if( revPos->second == mComponentSets.find( revPos->first )->second.rend() ){
                revPos->second = mComponentSets.find( revPos->first )->second.rbegin();

                // If we just reset the first position, exit the loop.
                if( revPos == --currPositions.rend() ){
                    shouldExit = true;
                }
            }
            else {
                break;
            }
        }
    }
}

/*! \brief Print the BatchRunner output. This currently does nothing.
* \param aTimer The timer used to print out the amount of time spent performing operations.
* \param aCloseDB Whether to close the database and output variable IDs. Defaults to true.
*/
void BatchRunner::printOutput( Timer& aTimer, const bool aCloseDB ) const {
}

/*! \brief Helper function which runs a single scenario created by the BatchRunner.
* \detailed This function expans the list of FileSets into a list of scenario components files
* to parse. The scenario name is created by combining the names of all the filesets with the name
* read from the configruration file. The function selects the appropriate type of ScenarioRunner 
* from the configuration file, and initializes it with the list of scenario components. 
* It then runs the Scenario and prints its output.
* \param aComponents A map of FileSets which is expanded to create the list of scenario components to read in.
* \param aTimer The timer used to print out the amount of time spent performing operations.
*/
bool BatchRunner::runSingleScenario( const map<string, FileSet> aComponents, Timer& aTimer ){
    // Expand the file sets into a flat list and a scenario information string.
    list<string> components;

    // Scenario name.
    string scenName;
    for( map<string, FileSet>::const_iterator currFileSet = aComponents.begin(); currFileSet != aComponents.end(); ++currFileSet ){
        scenName += currFileSet->first;
        for( FileSet::const_iterator currFile = currFileSet->second.begin(); currFile != currFileSet->second.end(); ++currFile ){
            components.push_back( currFile->second );
        }
    }
    cout << "Running scenario " << scenName << endl;

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
    mInternalRunner->setupScenario( aTimer, scenName, components );

    // Run the scenario.
    mInternalRunner->runScenario( aTimer );

    // Print the output.
    mInternalRunner->printOutput( aTimer );

    return true;
}

/*! \brief Function which parses the XML BatchRunner configuration file.
* \detailed This function searches for base level XML tags and dispatches them
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
		else if ( nodeName == "ComponentSet" ){
            XMLParseComponentSet( curr );
        }
        else {
            cout << "Unrecognized text string: " << nodeName << " found while parsing BatchScenarioRunner." << endl;
        }
    }
}

/*! \brief Helper function to parse a single scenario set element.
* \detailed This function parses a single ComponentSet and adds it to the BatchRunner's list 
* of ComponentSets. It dispatches any FileSets it finds to the XMLParseFileSet helper function.
* \param aNode DOM node corresponding to the current ComponentSet.
*/
void BatchRunner::XMLParseComponentSet( const DOMNode* aNode ){
    // assume we were passed a valid node.
    assert( aNode );
    
    // Create the new scenario set and set the name.
    ComponentSet& currComponentSet = mComponentSets[ XMLHelper<string>::getAttrString( aNode, "name" ) ];
    
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
            XMLParseFileSet( curr, currComponentSet );
        }
        else {
            cout << "Unrecognized text string: " << nodeName << " found while parsing ComponentSet." << endl;
        }
    }
}

/*! \brief Helper function to parse a single FileSet element.
* \detailed This function parses a single FileSet and adds it to the passed in ComponentSet's
* list of FileSets. 
* \param aNode DOM node corresponding to the current FileSet.
* \param aCurrComponentSet The ComponentSet to add this FileSet to.
*/
void BatchRunner::XMLParseFileSet( const DOMNode* aNode, ComponentSet& aCurrComponentSet ){
    // assume we were passed a valid node.
    assert( aNode );
    
    // Create the new file set and set the name.
    FileSet& currFileSet = aCurrComponentSet[ XMLHelper<string>::getAttrString( aNode, "name" ) ];
    
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
            currFileSet[ XMLHelper<string>::getAttrString( curr, "name" ) ] = XMLHelper<string>::getValueString( curr );
        }
        else {
            cout << "Unrecognized text string: " << nodeName << " found while parsing FileSet." << endl;
        }
    }
}
