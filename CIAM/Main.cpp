/*!
* \file Main.cpp															 
* \brief This is the Main program file which controls the initialization,
*  model looping over time steps, and outputs results  for the model.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"

// include standard libraries
#include <iostream>
#include <fstream>
#include <string>
#include <ctime>

// xerces xml headers
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include "xmlHelper.h"

// include custom headers
#include "Configuration.h"
#include "market.h" 
#include "world.h"
#include "modeltime.h"
#include "scenario.h"
#include "AgSector.h"
#include "Marketplace.h"
#include "LoggerFactory.h"
#include "Logger.h"

// Function Prototypes (for functions called by main())
extern void createDBout();
extern void openDB();
extern void createMCvarid();
extern void closeDB();

using namespace std; // enables elimination of std::

// define file (ofstream) objects for outputs, debugging and logs
ofstream bugoutfile,outfile,outfile2,dbout,logfile,sdcurvefile,sdfile;	

Scenario scenario; // model scenario info
time_t ltime;
int Tabs::numTabs = 0;

//******* Start of Main Program ********
int main() {	
	clock_t start, afterinit, intermediate, finish;
	Configuration* conf = Configuration::getInstance();	
	double duration = 0;
	ofstream xmlOutStream;
	const Modeltime* modeltime = scenario.getModeltime();
	World* world = scenario.getWorld();
	const Marketplace* marketplace = scenario.getMarketplace();

	// For some reason, the mac xerces parser wasn't properly using the relative path
#ifndef WIN32  
	const string configurationFileName = "/Configuration.xml";
#else
	const string configurationFileName = "Configuration.xml";
#endif
	start = clock(); // start of model run
	time(&ltime); // get time and date before model run
	
	// Initialize the Xerces parser
	try {
		XMLPlatformUtils::Initialize();
	} catch ( const XMLException& toCatch ) {
		string message = XMLHelper<string>::safeTranscode( toCatch.getMessage() );
		cout << "Error during initialization!"<< endl << message << endl;
		return -1;
	}
	
	DOMNode* root = 0;
	XercesDOMParser* parser = new XercesDOMParser();
	parser->setValidationScheme( XercesDOMParser::Val_Always );
	parser->setDoNamespaces( false );
	parser->setDoSchema( true );
	parser->setCreateCommentNodes( false ); // No comment nodes
	parser->setIncludeIgnorableWhitespace( false ); // No text nodes
	
	ErrorHandler* errHandler = ( ErrorHandler* ) new HandlerBase();
	parser->setErrorHandler( errHandler );
	// XML Parser initialized.

	// Initialize the LoggerFactory
	const string loggerFileName = "LoggerFactory.xml";
	root = XMLHelper<void>::parseXML( loggerFileName, parser );
	LoggerFactory::XMLParse( root );
	 
	// Parse configuration file.
	root = XMLHelper<void>::parseXML( configurationFileName, parser );
	conf->XMLParse( root );
	
	// Open various files.
	logfile.open( conf->getFile( "logOutFileName" ).c_str(), ios::out );
	assert( logfile );

	xmlOutStream.open( conf->getFile( "xmlOutputFileName" ).c_str(), ios::out );
	assert( xmlOutStream );

	bugoutfile.open( conf->getFile( "bugOutFileName" ).c_str(), ios::out );
	assert( bugoutfile );
	
	outfile.open( conf->getFile( "outFileName" ).c_str(), ios::out );
	assert( outfile );
	
	dbout.open( conf->getFile( "dbOutFileName" ).c_str(), ios::out );
	assert( dbout );


	root = XMLHelper<void>::parseXML( conf->getFile( "xmlInputFileName" ), parser );
	scenario.XMLParse( root );
	
	cout << "XML parsing complete." << endl;
	logfile << "XML parsing complete." << endl;
	
	// Cleanup Xerces.
	delete errHandler;
	delete parser;
	XMLPlatformUtils::Terminate();
	
	// Compute data read in time
	afterinit = clock();
	duration = (double)(afterinit-start) / CLOCKS_PER_SEC;
	cout << "XML Readin Time: " << duration << " Seconds" << endl;
	logfile << "XML Readin Time: " << duration << " Seconds" << endl;

    int t;
	outfile <<"Region,RegionName,Sector,Subsector,Technology,Variable,Units,";

	for (t=0;t<modeltime->getmaxper();t++) { 
		outfile << modeltime->getper_to_yr(t) <<",";
	}
	outfile <<"Date,Notes" << endl;
	
	// output file header
	dbout <<"RunID,Region,VarID,";
	for (t=1;t<modeltime->getmaxper();t++) { 
		dbout<<"y" << modeltime->getper_to_yr(t) <<",";
	}
	dbout << endl;
	// ******* end MiniCAM stype output *******

	scenario.run();
	
	// compute data read in time
	duration = (double)(afterinit-start) / CLOCKS_PER_SEC;
	cout << endl << "Data Readin Time: "<<duration<<" Seconds" << endl;
	
	// compute model run time
	intermediate = clock();
	duration = (double)(intermediate-start) / CLOCKS_PER_SEC;
	cout << "Data Readin & Model Run Time: "<<duration<<" Seconds" << endl;
	
	if ( conf->getBool( "timestamp" )  ) {
		bugoutfile << endl << "Model Run Time: ,"<< duration <<", Seconds";
	}

	// ***** Write results to database after last period
    openDB(); // open MS Access database
	createDBout(); // create main database output table before calling output routines
	
	world->createRegionMap(); // create map of region names
	
	// ***** Write to text file and database
	world->outputfile(); // write results to file
	world->MCoutput(); // MiniCAM style output to database
	marketplace->MCoutput(); // write global market info to database
	createMCvarid(); // create MC variable id's 
	// ***** end of writing to database
	
	finish = clock(); 
	duration = (double)(finish-start) / CLOCKS_PER_SEC;
	logfile << "Data Readin, Model Run & Write Time: "<< duration <<" Seconds" << endl;
	cout << endl<< "Date & Time: "<<ctime(&ltime)<< endl;
	logfile << endl<< "Date & Time: "<< ctime( &ltime ) << endl;
	
	if ( conf->getBool( "timestamp" ) ) { 
		bugoutfile << endl<< "Total Run & Write Time: ,"<< duration <<", Seconds";
	}
	
	if( conf->getBool( "agSectorActive" ) ){
		AgSector::internalOutput();
	}
	
	//******** Close All Text Files
	xmlOutStream.close();
	outfile.close();
	bugoutfile.close();
	logfile.close();
	sdcurvefile.close();
	sdfile.close();
	dbout.close();
    closeDB(); // close MS Access database
	delete conf;
	LoggerFactory::cleanUp();
	
	return 0;
}
