/*! ******************************************************************
   main.cpp															 
   C++ Integrated Assessment Model								 
   JGCRI
   
	This is the Main program file which controls the initialization,
	model looping over time steps, and results output for the model.
	Initial Creation by Son Hwi Kim, March 27, 2000							
****************************************************************************/

// include standard libraries
#include <iostream>
#include <fstream>
#include <string> // using "string.h" does not enable use of string class
#include <time.h> // to use clock and time functions

using namespace std; // enables elimination of std::

// include customized custom headers
#include "market.h" //contains market no.,supply,demand,price,and period.
#include "str_indexname.h" // get index and name from database
#include "world.h" // generic region class
#include "modeltime.h" // model time period info
#include "scenario.h" // model scenario info

// define variables with global scope
//     define and initialize path and file names
//const char *file = "..\\input\\erbinput2.csv";
const char *file = "C:\\PNNL\\CIAM\\input\\erbinput2.csv";
const char *ofile = "outfile.csv";
const char *ofile2 = "outfile2.csv";
const char *ofile4 = "dbout.csv";
const char *bugofile = "bugoutfile.csv";
const char *logofile = "logfile.csv";
//const char *dbfile = "..\\input\\erbdb.mdb";
// full path required for profiling and for calling from Java
const char *dbfile = "C:\\PNNL\\CIAM\\input\\erbdb.mdb";
const char *climatfile = "gas.emk";

// define some switches
bool Minicam = true;  // run Minicam(true) or full CGE(false)
bool timestamp = true;  // print time stamp, yes(true) or no(false)

//  define file (ofstream) objects for outputs, debugging and logs
ofstream bugoutfile,outfile,outfile2,dbout,logfile,sdcurvefile,sdfile;	
ofstream gasfile; // text file for climate input


/*! Create global objects representing the Markets, the World container, Model Time, 
    the Scenario, and the model execution time */

Marketplace marketplace; // global marketplace class, contains all individual markets
World world; // create world object as World class
//ghg_ind regGHGcoef; // indirect GHG emissions coefficient
Modeltime modeltime; // model time info 
Scenario scenario; // model scenario info
// time and clock objects
time_t ltime;
clock_t start, afterinit, intermediate, finish, afterdata;

//Function Prototypes (for functions called by main())
void solution(int per); // function that solves markets
// function that counts the # of unique records in a database
int countdbrec(string fdname,const char *dbname,const char *dbtname);
void climat_data(void); // function to write data for climat
extern "C" { void _stdcall CLIMAT(void); };  // call external climate model
void createidtbl(void); 
void create_rst(void); //create recordsets
void close_rst1(void); //close counting and initializing recordsets
void close_rst2(void); //close addtional recordsets


//******* Start of Main Program ********
int main(void) 
{
	int is=0, id=0, per=0, i=0;
	int solved=0;
	double duration;

	start = clock(); // start of model run
	time(&ltime); // get time and date before model run
	
	//****** Open Text Files *******
	bugoutfile.open(bugofile,ios::out); // open debugging text file
	logfile.open(logofile,ios::out); // open log file
	sdcurvefile.open("sdcurve.csv",ios::out); // open supply & demand file
	sdfile.open("sdall.csv",ios::out); // open supply & demand file
	//***********

    create_rst();  // create recordset all database tables

	//********  read in model begin and end year and timestep
	modeltime.set(); // initialize time object
	//*********
	
	// open output file and check if error
	outfile.open(ofile,ios::out);
	if (!outfile) {
		//open failed
		cerr<<"Cannot open file for output\n";
		exit(-1);
	}
	// output file header
	outfile <<"Region,RegionName,Sector,Subsector,Technology,Variable,Units,";
	for (int t=0;t<modeltime.getmaxper();t++) { 
		outfile << modeltime.getper_to_yr(t) <<",";
	}
	outfile <<"Date,Notes\n";

	outfile2.open(ofile2,ios::out);
	if (!outfile2) {
		//open failed
		cerr<<"Cannot open file2 for output\n";
		exit(-1);
	}
	// output file header
	outfile2 <<"RegionName,Category,Subcat,Variable,Subvar,Units,";
	for (t=0;t<modeltime.getmaxper();t++) { 
		outfile2 << modeltime.getper_to_yr(t) <<",";
	}
	outfile2 <<"Date,Notes\n";

	// ******* MiniCAM stype output *******
	dbout.open(ofile4,ios::out);
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
	dbout <<"\n";
	// ******* end MiniCAM stype output *******

	// set size of global arrays depending on MaxPer 
	world.initper(); 
	// sets number of regions, sectors, subsectors, and technologies
	world.setregion();
	
	// write time after setting regions
	if (timestamp) {
		afterdata = clock();
		duration = (double)(afterdata-start) / CLOCKS_PER_SEC;
		bugoutfile << "\n\nData Read-in Time , after setregion: ,"<<duration<<", Seconds";
	}

	// sets number of markets, names and indeces
	// number of markets is resources plus supply sectors
	if (Minicam)
		marketplace.setmrks(); 
	else
		marketplace.setmrks2(); // extra secondary goods market

	// write time after setting markets
	if (timestamp) {
		afterdata = clock();
		duration = (double)(afterdata-start) / CLOCKS_PER_SEC;
		bugoutfile << "\nData Read-in Time , after setmarket: ,"<<duration<<", Seconds";
	}

	// initialize parameters for all regions, sectors, subsectors, and technologies
	world.initregion();

	// write time after initializing parameters
	if (timestamp) {
		afterdata = clock();
		duration = (double)(afterdata-start) / CLOCKS_PER_SEC;
		bugoutfile << "\nData Read-in Time , after initialization: ,"<<duration<<", Seconds";
	}

	close_rst1(); // close all recordsets

	afterinit = clock(); // time after reading all data

	// Model run for first period
	per = 0; // no need to solve for first period
	cout << "\nPeriod " << per <<": "<< modeltime.getper_to_yr(per);
	cout << "\nPeriod 0 not solved";
	logfile<< "Period:  " << per <<"\n";
	// copy prices to all periods for resources only
	marketplace.initprices(); // initialize market prices for base year
	marketplace.nulldem(per); // null market demands
	marketplace.nullsup(per); // null market supply
	
	world.gnp(per); // call to calculate regional gnps
	world.calc(per); // call to calculate supply and demand
	world.sumpop(per); // call to calculate global population
	world.emiss_ind(per); // call to calculate global emissions
	world.sumrsc(per); // call to calculate global depletable resources

	// Loop over time steps and operate model
	for (per=1;per<modeltime.getmaxper();per++)
	{	
		cout << "\nPeriod " << per <<": "<< modeltime.getper_to_yr(per) <<"\n";
		logfile<< "Period:  " << per <<"\n";
		// price, supply and demand curves for debugging
		sdcurvefile << "Period " << per <<": "<< modeltime.getper_to_yr(per) <<"\n";
		sdcurvefile << "Market,Name,Price,Supply,Demand,";
		sdcurvefile << "Market,Name,Price,Supply,Demand,";
		sdcurvefile << "Market,Name,Price,Supply,Demand,";
		sdcurvefile << "Market,Name,Price,Supply,Demand,";
		sdcurvefile << "Market,Name,Price,Supply,Demand,\n";
		//sdfile << "\nPeriod " << per <<": "<< modeltime.getper_to_yr(per) <<"\n"; //supply & demand info

		marketplace.nulldem(per); // initialize market demand to null
		marketplace.nullsup(per); // initialize market supply to null
		marketplace.storeto_last(per); // save last period's info to stored variables
		marketplace.init_to_last(per); // initialize to last period's info
		world.gnp(per); // call to calculate regional gnps
		world.calc(per); // call to calculate supply and demand
		solution(per); // solution uses Bisect and NR routine to clear markets
		world.sumpop(per); // call to calculate global population
		world.emiss_ind(per); // call to calculate global emissions
		world.sumrsc(per); // call to calculate global depletable resources
	}

	// calling fortran subroutine climat/magicc
	world.emiss_all(); // read in all ghg gases except for CO2
	gasfile.open(climatfile,ios::out); // open input file for climat
	climat_data(); // writes the input text file
	gasfile.close(); // close input file for climat
	cout << "\n\ncalling CLIMAT()\n";
	//CLIMAT();
	cout << "finished with CLIMAT()\n";

	// compute data read in time
	duration = (double)(afterinit-start) / CLOCKS_PER_SEC;
	cout << "\nData Read-in Time: "<<duration<<" Seconds\n";
	// compute model run time
	intermediate = clock();
	duration = (double)(intermediate-start) / CLOCKS_PER_SEC;
	cout << "Model Read & Run Time: "<<duration<<" Seconds\n";
	if (timestamp) 
		bugoutfile << "\nModel Run Time: ,"<<duration<<", Seconds";

	// ***** Write results to database after last period

	createidtbl(); // create ID tables
	scenario.setall(); // set scenario info

	// ***** Write to text file and insert into database
	world.outputfile(); // write all regional results to file
	world.MCoutput(); // MiniCAM style output to file
	marketplace.outputfile(); // write global market info to file
	marketplace.MCoutput(); // write global market info for dataviewer
	outfile.close(); // close output file

	extern void create_MCvarid(void);
	create_MCvarid(); // create MC variable id's 

    close_rst2(); // Close Recordsets and Database

	// end of writing to database

	finish = clock(); 
	duration = (double)(finish-start) / CLOCKS_PER_SEC;
	cout << "Total Read, Run & Write Time: "<<duration<<" Seconds\n";
	cout << "\nDate & Time: "<<ctime(&ltime)<<"\n";

	if (timestamp) 
		bugoutfile << "\nTotal Run & Write Time: ,"<<duration<<", Seconds";

	//******** Close All Text Files
	bugoutfile.close();
	logfile.close();
	sdcurvefile.close();
	sdfile.close();
	dbout.close();
	//********

	return 0;
}
