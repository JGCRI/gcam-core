/*! 
* \file merge_runner.cpp
* \ingroup objects
* \brief MergeRunner class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
#include <iostream>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/dom/DOMNode.hpp>
#include "containers/include/merge_runner.h"
#include "util/base/include/timer.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/configuration.h"
#include "containers/include/scenario.h"

using namespace std;
using namespace xercesc;

extern ofstream bugoutfile, logfile;
extern Scenario* scenario;

//! Constructor
MergeRunner::MergeRunner(){
}

//! Destructor
MergeRunner::~MergeRunner(){
    // Close All Text Files
    bugoutfile.close();
    logfile.close();
}

//! Setup the scenario.
bool MergeRunner::setupScenario( Timer& timer, const string aName, const list<string> aScenComponents ){
    // Open various files.
    const Configuration* conf = Configuration::getInstance();
    const string logFileName = conf->getFile( "logOutFileName" );
    logfile.open( logFileName.c_str(), ios::out );
    util::checkIsOpen( logfile, logFileName  );
    
    const string bugOutFileName = conf->getFile( "bugOutFileName" );
    bugoutfile.open( bugOutFileName.c_str(), ios::out );
    util::checkIsOpen( bugoutfile, bugOutFileName );
    
    // Parse the input file.
    XercesDOMParser* parser = XMLHelper<void>::getParser();
    DOMNode* root = XMLHelper<void>::parseXML( conf->getFile( "xmlInputFileName" ), parser );

    // Use a smart pointer for scenario so that if the main program exits before the end the memory is freed correctly. 
    mScenario.reset( new Scenario() );
    scenario = mScenario.get(); // Need to set the global pointer.
    mScenario->XMLParse( root );
    
    // Override scenario name from data file with that from configuration file
    string overrideName = conf->getString( "scenarioName" ) + aName;
    if ( overrideName != "") {
        mScenario->setName( overrideName );
    }

    // Fetch the listing of Scenario Components.
    const list<string> scenComponents = conf->getScenarioComponents();
    
    // Iterate over the vector.
    typedef list<string>::const_iterator ScenCompIter;
    for( ScenCompIter currComp = scenComponents.begin(); currComp != scenComponents.end(); ++currComp ) {
        if ( Configuration::getInstance()->getBool( "debugChecking" ) ) { 
            cout << "Parsing " << *currComp << " scenario component." << endl;
        }
        root = XMLHelper<void>::parseXML( *currComp, parser );
        mScenario->XMLParse( root );
    }

    if ( Configuration::getInstance()->getBool( "debugChecking" ) ) { 
        cout << "XML parsing complete." << endl;
    }
    logfile << "XML parsing complete." << endl;

    // Print data read in time.
    timer.save();
    if ( Configuration::getInstance()->getBool( "timestamp" ) ) { 
        timer.print( cout, "XML Readin Time:" );
    }
    timer.print( logfile, "XML Readin Time:" );

    return true;
}
/*! \brief Does nothing, needed for interface.
* \return Always returns true.
* \author Josh Lurz
*/

bool MergeRunner::runScenario( Timer& timer ){
    return true;
}

void MergeRunner::printOutput( Timer& timer, const bool aCloseDB ) const {
    // Print output xml file.
    const Configuration* conf = Configuration::getInstance();
    const string xmlOutFileName = conf->getFile( "xmlOutputFileName" );
    ofstream xmlOut;
    xmlOut.open( xmlOutFileName.c_str(), ios::out );
    util::checkIsOpen( xmlOut, xmlOutFileName );

    Tabs tabs;
    mScenario->toInputXML( xmlOut, &tabs );

    // Close the output file. 
    xmlOut.close();
}