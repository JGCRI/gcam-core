/*! 
* \file merge_runner.cpp
* \ingroup objects
* \brief MergeRunner class source file.
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <xercesc/dom/DOMNode.hpp>
#include "containers/include/merge_runner.h"
#include "util/base/include/timer.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/configuration.h"
#include "containers/include/scenario.h"
#include "util/logger/include/ilogger.h"

using namespace std;
using namespace xercesc;

//! Constructor
MergeRunner::MergeRunner(){
}

//! Destructor
MergeRunner::~MergeRunner(){
}

// IParsable interface
bool MergeRunner::XMLParse( const xercesc::DOMNode* aRoot ){
    // No data to parse.
    return true;
}

//! Setup the scenario.
bool MergeRunner::setupScenarios( Timer& timer, const string aName, const list<string> aScenComponents ){
    // Parse the input file.
    const Configuration* conf = Configuration::getInstance();
    bool success = XMLHelper<void>::parseXML( conf->getFile( "xmlInputFileName" ), mScenario.get() );

    // Parsing failed.
    if( !success ){
        return false;
    }

    // Need to ensure the Scenario is cleared and set.
    mScenario.reset( new Scenario );

    // Make sure the global scenario points is set.
    scenario = mScenario.get();

    // Override scenario name from data file with that from configuration file
    const string overrideName = conf->getString( "scenarioName" ) + aName;
    if ( !overrideName.empty() ) {
        mScenario->setName( overrideName );
    }

    // Fetch the listing of Scenario Components.
    const list<string> scenComponents = conf->getScenarioComponents();

    // Get the main log file.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );

    // Iterate over the vector.
    typedef list<string>::const_iterator ScenCompIter;
    for( ScenCompIter currComp = scenComponents.begin(); currComp != scenComponents.end(); ++currComp ) {
        mainLog << "Parsing " << *currComp << " scenario component." << endl;
        if( !XMLHelper<void>::parseXML( *currComp, mScenario.get() ) ){
            // Parsing failed.
            return false;
        }
    }
    
    mainLog << "XML parsing complete." << endl;

    // Print data read in time.
    timer.stop();
    mainLog.setLevel( ILogger::DEBUG );
    timer.print( mainLog, "XML Readin Time:" );
    return true;
}
/*! \brief Does nothing, needed for interface.
* \param aSinglePeriod This parameter is ignored.
* \param aTimer This parameter is ignored.
* \return Always returns true.
* \author Josh Lurz
*/
bool MergeRunner::runScenarios( const int aSinglePeriod,
                                const bool aPrintDebugging,
                                Timer& aTimer )
{
    return true;
}

void MergeRunner::printOutput( Timer& timer, const bool aCloseDB ) const {
    // Print output xml file.
    const Configuration* conf = Configuration::getInstance();
    const string xmlOutFileName = conf->getFile( "xmlOutputFileName", "output.xml" );
    ofstream xmlOut;
    xmlOut.open( xmlOutFileName.c_str(), ios::out );
    util::checkIsOpen( xmlOut, xmlOutFileName );

    Tabs tabs;
    mScenario->toInputXML( xmlOut, &tabs );

    // Close the output file. 
    xmlOut.close();
}

/*! \brief Get the internal scenario.
 \return The internal scenario.
*/
Scenario* MergeRunner::getInternalScenario(){
	return mScenario.get();
}

/*! \brief Get the internal scenario.
* \return Constant pointer to the internal scenario.
*/
const Scenario* MergeRunner::getInternalScenario() const {
	return mScenario.get();
}

/*! \brief Get the static name of the class.
* \return The static name of the class.
*/
const string& MergeRunner::getXMLNameStatic(){
	static const string XML_NAME = "merge-runner";
	return XML_NAME;
}
