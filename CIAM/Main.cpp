/*! ******************************************************************
main.cpp															 
C++ Integrated Assessment Model								 
JGCRI

  This is the Main program file which controls the initialization,
  model looping over time steps, and outputs results  for the model.
  Initial Creation by Son Hwi Kim, March 27, 2000							
****************************************************************************/

#include "Definitions.h"

// include standard libraries
#include <iostream>
#include <fstream>
#include <string> // using "string.h" does not enable use of string class
#include <ctime> // to use clock and time functions

// xerces xml headers
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/sax/HandlerBase.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include "xmlHelper.h"
// end of xerces headers

// include customized custom headers
#include "Configuration.h"
#include "market.h" //contains market no.,supply,demand,price,and period.
#include "world.h" // generic region class
#include "modeltime.h" // model time period info
#include "scenario.h" // model scenario info
#include "AgSector.h"
#include "Marketplace.h"

using namespace std; // enables elimination of std::

// define file (ofstream) objects for outputs, debugging and logs
ofstream bugoutfile,outfile,outfile2,dbout,logfile,sdcurvefile,sdfile;	
ofstream gasfile; // text file for climate input

				  /*! Create global objects representing the Markets, the World container, Model Time, 
the Scenario, and the model execution time */

// really these should be children of the scenario class.
Marketplace marketplace; // global marketplace class, contains all individual markets
World world; // create world object as World class
//ghg_ind regGHGcoef; // indirect GHG emissions coefficient
Modeltime modeltime; // model time info 
Scenario scenario; // model scenario info
// time and clock objects
time_t ltime;


//Function Prototypes (for functions called by main())
void climat_data(void); // function to write data for climat
#ifdef WIN32
extern "C" { void _stdcall CLIMAT(void); };  // call external climate model
#endif

int Tabs::numTabs = 0;

//******* Start of Main Program ********
int main() 
{	
	clock_t start, afterinit, intermediate, finish;
	Configuration* conf = Configuration::getInstance();	
	int per = 0;
	double duration = 0;
	ofstream xmlOutStream;
	ofstream xmlDebugStream;
	
	// For some reason, the mac xerces parser wasn't properly using the relative path
#ifndef WIN32  
	const string configurationFileName = "/Configuration.xml";
#else
	const string configurationFileName = "Configuration.xml";
#endif
	start = clock(); // start of model run
	time(&ltime); // get time and date before model run
	
	
	// Open the xml input file and parse.
	try
	{
		XMLPlatformUtils::Initialize();
	} catch ( const XMLException& toCatch ) {
		string message = XMLString::transcode( toCatch.getMessage() );
		cout << "Error during initialization!"<< endl << message << endl;
		return -1;
	}
	
	DOMDocument* doc = 0;
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
	// Parse configuration file.
	try {
		const unsigned long startMillis = XMLPlatformUtils::getCurrentMillis();
		parser->parse( configurationFileName.c_str() );
		const unsigned long endMillis = XMLPlatformUtils::getCurrentMillis();
		long parseTime = endMillis - startMillis;
		cout << "Parsing took " << parseTime / float( 1000 ) << " seconds." << endl;
	} catch ( const XMLException& toCatch ) {
		string message = XMLString::transcode( toCatch.getMessage() );
		cout << "Exception message is:" << endl << message << endl;
		return -1;
	} catch ( const DOMException& toCatch ) {
		string message = XMLString::transcode( toCatch.msg );
		cout << "Exception message is:" << endl << message << endl;
		return -1;
	} catch ( const SAXException& toCatch ){
		string message = XMLString::transcode( toCatch.getMessage() );
		cout << "Exception message is:" << endl << message << endl;
		return -1;
	} catch (...) {
		cout << "Unexpected Exception." << endl;
		return -1;
	}
	
	doc = parser->getDocument();
	root = doc->getDocumentElement();
	conf->XMLParse( root );

        // Open log file
	logfile.open( conf->getFile( "logOutFileName" ).c_str(), ios::out );
	
#if( _DEBUG )
	conf->toDebugXML( cout );
	cout << "XML configuration parsing complete." << endl;
	logfile << "XML configuration parsing complete." << endl;
#endif
	
	//
	xmlOutStream.open( conf->getFile( "xmlOutputFileName" ).c_str(), ios::out );
	xmlDebugStream.open( conf->getFile( "xmlDebugFileName" ).c_str(), ios::out );
	
	// open output file and check if error
	outfile.open( conf->getFile( "outFileName" ).c_str(), ios::out );
	if (!outfile) {
		//open failed
		cerr<<"Cannot open file for output\n";
		exit(-1);
	}
	// output file header
    int t;
	outfile <<"Region,RegionName,Sector,Subsector,Technology,Variable,Units,";
	for (t=0;t<modeltime.getmaxper();t++) { 
		outfile << modeltime.getper_to_yr(t) <<",";
	}
	outfile <<"Date,Notes" << endl;
	
	// ******* MiniCAM stype output *******
	dbout.open( conf->getFile( "dbOutFileName" ).c_str(), ios::out );
	if (!dbout) {
		//open failed
		cerr<<"Cannot open dbout.csv for output\n";
		exit(-1);
	}
	// output file header
	dbout <<"RunID,Region,VarID,";
	for (t=1;t<modeltime.getmaxper();t++) { 
		dbout<<"y" << modeltime.getper_to_yr(t) <<",";
	}
	dbout << endl;
	// ******* end MiniCAM stype output *******
	bugoutfile.open( conf->getFile( "bugOutFileName" ).c_str(), ios::out );
	assert( bugoutfile );
	
	try {
		const unsigned long startMillis = XMLPlatformUtils::getCurrentMillis();
		parser->parse( conf->getFile( "xmlInputFileName" ).c_str() );
		const unsigned long endMillis = XMLPlatformUtils::getCurrentMillis();
		long parseTime = endMillis - startMillis;
		cout << "Parsing took " << parseTime / float( 1000 ) << " seconds." << endl;
	} catch ( const XMLException& toCatch ) {
		string message = XMLString::transcode( toCatch.getMessage() );
		cout << "Exception message is:" << endl << message << endl;
		return -1;
	} catch ( const DOMException& toCatch ) {
		string message = XMLString::transcode( toCatch.msg );
		cout << "Exception message is:" << endl << message << endl;
		return -1;
	} catch ( const SAXException& toCatch ){
		string message = XMLString::transcode( toCatch.getMessage() );
		cout << "Exception message is:" << endl << message << endl;
		return -1;
	} catch (...) {
		cout << "Unexpected Exception." << endl;
		return -1;
	}
	
	// because doc is owned by parser, we cannot delete parser until we are finished with the document. 
	// also as an effect of this I do not believe we need to explicitally delete the document.
	doc = parser->getDocument();
	root = doc->getDocumentElement();
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
	
	
	// set size of global arrays depending on MaxPer 
	// works fine with XMLParse, only calls maxper
	world.initper(); 
	
	//***** Start Model run for First Period
	per = 0; // set period to first period
	
	
	marketplace.initXMLPrices(); // initialize prices
	marketplace.nulldem(per); // null market demands
	marketplace.nullsup(per); // null market supply
	
	// Write scenario root element for the debugging.
	scenario.toDebugXMLOpen( per, xmlDebugStream );
	
	world.gnp(per); // call to calculate regional gnps
	world.calc(per); // call to calculate supply and demand
	world.sumpop(per); // call to calculate global population
	world.emiss_ind(per); // call to calculate global emissions
	world.sumrsc(per); // call to calculate global depletable resources
	cout << endl << "Period " << per <<": "<< modeltime.getper_to_yr(per) << endl;
	cout << "Period 0 not solved" << endl;
	logfile<< "Period:  " << per << endl;
	//***** End First Period
	
	// Loop over time steps and operate model
	for (per=1;per<modeltime.getmaxper();per++)
	// for ( per = 1; per == 1; per++ )
	{	
		cout << endl << "Period " << per <<": "<< modeltime.getper_to_yr(per) << endl;
		logfile<< "Period:  " << per << endl;
		// price, supply and demand curves for debugging
		sdcurvefile << "Period " << per <<": "<< modeltime.getper_to_yr(per) << endl;
		sdcurvefile << "Market,Name,Price,Supply,Demand,";
		sdcurvefile << "Market,Name,Price,Supply,Demand,";
		sdcurvefile << "Market,Name,Price,Supply,Demand,";
		sdcurvefile << "Market,Name,Price,Supply,Demand,";
		sdcurvefile << "Market,Name,Price,Supply,Demand," << endl;
		
		marketplace.nulldem(per); // initialize market demand to null
		marketplace.nullsup(per); // initialize market supply to null
		marketplace.storeto_last(per); // save last period's info to stored variables
		marketplace.init_to_last(per); // initialize to last period's info
		world.gnp(per); // call to calculate regional gnps
		world.calc(per); // call to calculate supply and demand
		marketplace.solve(per); // solution uses Bisect and NR routine to clear markets
		world.sumpop(per); // call to calculate global population
		world.emiss_ind(per); // call to calculate global emissions
		world.sumrsc(per); // call to calculate global depletable resources
		world.toDebugXML( per, xmlDebugStream );
	}
	
	scenario.toDebugXMLClose( per, xmlDebugStream ); // Close the xml debugging tag.
	
	// calling fortran subroutine climat/magicc
	world.emiss_all(); // read in all ghg gases except for CO2
	gasfile.open( conf->getFile( "climatFileName" ).c_str(), ios::out ); // open input file for climat
	// climat_data(); // writes the input text file
	gasfile.close(); // close input file for climat
#ifdef WIN32
	cout << endl << "Calling CLIMAT() "<< endl;
    //    CLIMAT();
  	cout << "Finished with CLIMAT()" << endl;
#endif
	
	// compute data read in time
	duration = (double)(afterinit-start) / CLOCKS_PER_SEC;
	cout << endl << "Data Readin Time: "<<duration<<" Seconds" << endl;
	// compute model run time
	intermediate = clock();
	duration = (double)(intermediate-start) / CLOCKS_PER_SEC;
	cout << "Data Readin & Model Run Time: "<<duration<<" Seconds" << endl;
	if ( conf->getBool( "timestamp" )  ) {
		bugoutfile << endl << "Model Run Time: ,"<<duration<<", Seconds";
	}
	// ***** Write results to database after last period
	extern void openDB(); // fuction protocol
    openDB(); // open MS Access database
	
	extern void createDBout(void); // function protocol
	createDBout(); // create main database output table before calling output routines
	
	world.createRegionMap(); // create map of region names
	
	// ***** Write to text file and database
	world.outputfile(); // write results to file
	world.MCoutput(); // MiniCAM style output to database
	marketplace.MCoutput(); // write global market info to database
	
	extern void createMCvarid(); // function protocol
	createMCvarid(); // create MC variable id's 
	// ***** end of writing to database
	
	finish = clock(); 
	duration = (double)(finish-start) / CLOCKS_PER_SEC;
	logfile << "Data Readin, Model Run & Write Time: "<<duration<<" Seconds" << endl;
	cout << "\nDate & Time: "<<ctime(&ltime)<< endl;
	logfile << "\nDate & Time: "<<ctime(&ltime)<< endl;
	
	if ( conf->getBool( "timestamp" ) ) { 
		bugoutfile << "\nTotal Run & Write Time: ,"<<duration<<", Seconds";
	}
	if( conf->getBool( "agSectorActive" ) ){
		AgSector::internalOutput();
	}

	xmlOutStream.close();
	xmlDebugStream.close();
	
	//******** Close All Text Files
	outfile.close(); // close output file
	bugoutfile.close();
	logfile.close();
	sdcurvefile.close();
	sdfile.close();
	dbout.close();
	delete conf;
	extern void closeDB(); // fuction protocol
    closeDB(); // close MS Access database
	//********
	
	return 0;
}
