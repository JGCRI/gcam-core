/* *************************************************
   create_rst.cpp
   Sonny Kim and Marshall Wise
   August 20, 2002
   JGCRI

   Code to open database tables and create recordsets
   of tables using SQL.
   Recordsets are used for counting objects to be created
   and for initializing these objects.

******************************************************/

// standard libraries
#include <stdlib.h>
#include <afxdisp.h>
#include <dbdao.h>
#include <dbdaoerr.h>
#include <direct.h>
#include <iostream>
#include <fstream>
#include <map>
#include <string> // using "string.h" does not enable use of string class
#include <time.h> // to use clock and time functions

using namespace std; // enables elimination of std::

// global variables
// path and file names
extern const char *dbfile;

// database table names
const char *dbtghg = "ghg";
const char *dbtdrsc = "resource";
const char *dbtsupsec = "supsector";
const char *dbtdemsec = "demsector";
const char *dbtgen = "gen";
const char *dbtmod = "model";
const char *dbtdemog = "demographics";
const char *dbttechghg = "techghg";
const char *dbtout = "output";
const char *dbtoutf = "outputf";


// some switches
extern bool Minicam;  // run Minicam(true) or full CGE(false)
extern bool timestamp;  // print time stamp, yes(true) or no(false)

// define global DB engine and database
CdbDBEngine dben;
CdbDatabase db;
CdbRecordset outrst; // output recordset
// recordset for gen, resource, supply, and demand sector DB table
CdbRecordset modelrst,genrst,drscrst,drscrst_ct,suprst,suprst_ss,suprst_ct,
             demrst,demrst_ss,demrst_ct,demogrst,ghgrst,ghgrst_ct; 
CdbRecordset techghgrst;

extern ofstream bugoutfile,outfile,outfile2,dbout,logfile,sdcurvefile,sdfile;	
extern clock_t start, afterinit, intermediate, finish, afterdata;


void create_rst(void) {

	double duration; // temporary real to calculate time

	// Open a global Jet database in exclusive, read/write mode.
	try {
		db = dben.OpenDatabase(dbfile); }
	catch(...) {
		cout<<"\nError opening database: "<< dbfile <<"\n"; }
	// Open output recordset - dynasets are editable
	outrst = db.OpenRecordset(dbtout,dbOpenDynaset); 
	// Open greenhouse gas DB table recordset
	ghgrst = db.OpenRecordset(dbtghg,dbOpenDynaset);
	// Open model DB table recordset
	modelrst = db.OpenRecordset(dbtmod,dbOpenDynaset);
	// Open general DB table recordset
	genrst = db.OpenRecordset(dbtgen,dbOpenDynaset);
	// Open demograhpics DB table recordset
	demogrst = db.OpenRecordset(dbtdemog,dbOpenDynaset);
	// write time after opening recordsets
	if (timestamp) {
		afterdata = clock(); 
		duration = (double)(afterdata-start) / CLOCKS_PER_SEC;
		bugoutfile << "\nData Read-in Time, after opening genrst: ,"<<duration<<", Seconds";
	}

	// Open DEPLETABLE RESOURCE recordset, sort using order by 
	string sqltemp = "SELECT * FROM ";
	sqltemp = sqltemp + dbtdrsc + " ORDER BY Region,Sector,Subsector,Grade,Year";  
	drscrst = db.OpenRecordset(sqltemp.c_str()); //dynasets are editable
	// Open DEPLETABLE RESOURCE recordset for counting, sort using order by 
	sqltemp = "SELECT DISTINCT Region,Sector,SectorName,Subsector,SubsectorName,Grade,GradeName ";
	sqltemp = sqltemp + " FROM " + dbtdrsc + " ORDER BY Region,Sector,Subsector,Grade";  
	try {
		drscrst_ct = db.OpenRecordset(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError opening database: drscrst_ct\n"; }
	// write time to open database
	if (timestamp) {
		afterdata = clock();
		duration = (double)(afterdata-start) / CLOCKS_PER_SEC;
		bugoutfile << "\nData Read-in Time, after opening drscrst: ,"<<duration<<", Seconds";
	}

	// Open GHG recordset for counting, sort using order by 
	sqltemp = "SELECT DISTINCT Region,GHG,GHGName ";
	sqltemp = sqltemp + " FROM " + dbtghg + " ORDER BY Region,GHG";  
	try {
		ghgrst_ct = db.OpenRecordset(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError opening database: ghgrst_ct\n"; }


	// Open Supply Sector recordset, sort using order by 
	sqltemp = "SELECT * FROM ";
	sqltemp = sqltemp + dbtsupsec + " ORDER BY Region,Sector,Subsector,Technology,Year";  
	suprst = db.OpenRecordset(sqltemp.c_str()); //dynasets are editable
	// Open Supply Sector recordset for subsector info, sort using order by 
	sqltemp = "SELECT DISTINCT Region,Sector,SectorName,Subsector,SubsectorName,Year,CapLimit,Shrwts,RUI";
	sqltemp = sqltemp + " FROM " + dbtsupsec + " ORDER BY Region,Sector,Subsector,Year";  
	try {
		suprst_ss = db.OpenRecordset(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError opening database: suprst_ss\n"; }
	// Open Supply Sector recordset for counting, sort using order by 
	sqltemp = "SELECT DISTINCT Region,Sector,SectorName,Subsector,SubsectorName,Technology,TechnologyName ";
	sqltemp = sqltemp + " FROM " + dbtsupsec + " ORDER BY Region,Sector,Subsector,Technology";  
	try {
		suprst_ct = db.OpenRecordset(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError opening database: suprst_ct\n"; }
	// write time to open database
	if (timestamp) {
		afterdata = clock();
		duration = (double)(afterdata-start) / CLOCKS_PER_SEC;
		bugoutfile << "\nData Read-in Time , after opening suprst: ,"<<duration<<", Seconds";
	}

	// Open Demand Sector recordset, sort using order by 
	sqltemp = "SELECT * FROM ";
	sqltemp = sqltemp + dbtdemsec + " ORDER BY Region,Sector,Subsector,Technology,Year";  
	demrst = db.OpenRecordset(sqltemp.c_str());
	// Open Demand Sector recordset for subsector info, sort using order by 
	sqltemp = "SELECT DISTINCT Region,Sector,SectorName,Subsector,SubsectorName,Year,Shrwts,RUI";
	sqltemp = sqltemp + " FROM " + dbtdemsec + " ORDER BY Region,Sector,Subsector,Year";  
	try {
		demrst_ss = db.OpenRecordset(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError opening database: demrst_ss\n"; }
	// Open Demand Sector recordset for counting, sort using order by 
	sqltemp = "SELECT DISTINCT Region,Sector,SectorName,Subsector,SubsectorName,Technology,TechnologyName ";
	sqltemp = sqltemp + " FROM " + dbtdemsec + " ORDER BY Region,Sector,Subsector,Technology";  
	try {
		demrst_ct = db.OpenRecordset(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError opening database: demrst_ct\n"; }
	// write time to open database
	if (timestamp) {
		afterdata = clock();
		duration = (double)(afterdata-start) / CLOCKS_PER_SEC;
		bugoutfile << "\nData Read-in Time , after opening demrst: ,"<<duration<<", Seconds";
	}
	// Open technology ghg recordset, sort using order by 
	sqltemp = "SELECT * FROM ";
	sqltemp = sqltemp + dbttechghg + " ORDER BY Region,Sector,Subsector,Technology,Year,Ghg";  
	try {
		techghgrst = db.OpenRecordset(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError opening database: techghgrst\n"; }
}


void close_rst1(void) {

	//******* Close Recordsets *******
	drscrst.Close(); // depletable resource recordset
	drscrst_ct.Close(); // depletable resource recordset for counting
	ghgrst_ct.Close(); // ghg recordset for counting
	suprst.Close(); // supply sector recordset
	suprst_ct.Close(); // supply sector recordset for counting
	demrst.Close(); // demand sector recordset
	demrst_ct.Close(); // demand sector recordset for counting
	techghgrst.Close(); // technology ghg recordset
}

void close_rst2(void) {

	//******* Close Recordsets *******
	ghgrst.Close();			// carbon recordset
	outrst.Close();			// output recordset
	modelrst.Close();		// model recordset
	genrst.Close();			// gen recordset
	demogrst.Close();		// demographics recordset
	db.Close();				// close the database
}