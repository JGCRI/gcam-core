/* dbConnection.cpp															*
 * Function to open and close database connection and						*
 * to create database table for writing result.								*
 * This table is used to create DBmain table used by the Dataviewer.xls		*
 * DBout table is first deleted and recreated with the proper set of fiels.	*
 * Then create a recordset of this table for writing results.				*
 * SHK 2/27/03																*/
#include "Definitions.h"
#ifdef WIN32
// standard libraries
#include <cstdlib>
#include <afxdisp.h>
#include <dbdao.h>
#include <dbdaoerr.h>
#include <direct.h>
#include <iostream>
#include <string> // using "string.h" does not enable use of string class
// custom header
#include "modeltime.h"
#include "Configuration.h"

using namespace std; // enables elimination of std::

// define global DB engine and database
CdbDBEngine dben;
CdbDatabase db;
CdbTableDef DBoutTD; // table definitions for creating tables in the database
CdbField tfield; // tempory field for creating fields in tables
CdbRecordset DBoutrst; // recordset for writing results

const char *DBout = "DBout"; // name of the table for outputs compatible with dataviewer
extern Modeltime modeltime;


//! Open connection to the database
void openDB(void) {
	Configuration* conf = Configuration::getInstance();
	string dbFile = conf->getFile( "dbFileName" );
	// Open a global Jet database in exclusive, read/write mode.
	try {
		db = dben.OpenDatabase( dbFile.c_str()); }
	catch(...) {
		cout<<"Error opening database: "<< dbFile << endl; }
}

//! Close connection to the database
void closeDB(void) {

	db.Close();
}

//! Create and open the main database output table.
//  No data is appended from this method.
void createDBout(void)
{
	// ***** Create DBout table ******
	// first delete existing table
	try { db.TableDefs.Delete(DBout); } 
	catch (...) {cout<<"\nError deleting "<<DBout<<" table\n";}
	// create field names
	DBoutTD = db.CreateTableDef(DBout);
	tfield = DBoutTD.CreateField("RunID",dbLong);
	DBoutTD.Fields.Append(tfield);
	tfield = DBoutTD.CreateField("Region",dbLong);
	DBoutTD.Fields.Append(tfield);
	tfield = DBoutTD.CreateField("VarID",dbLong);
	DBoutTD.Fields.Append(tfield);
	tfield = DBoutTD.CreateField("Cat",dbText);
	DBoutTD.Fields.Append(tfield);
	tfield = DBoutTD.CreateField("SubCat",dbText);
	DBoutTD.Fields.Append(tfield);
	tfield = DBoutTD.CreateField("VarLabel",dbText);
	DBoutTD.Fields.Append(tfield);
	tfield = DBoutTD.CreateField("VarUnits",dbText);
	DBoutTD.Fields.Append(tfield);
	char buffer[4];
	string str;
	// add years as fields
	for (int t=0;t<modeltime.getmaxper();t++) { 
		str = "y";
		str += _itoa(modeltime.getper_to_yr(t),buffer,10); // convert int to char*
		tfield = DBoutTD.CreateField(str.c_str(),dbText);
		DBoutTD.Fields.Append(tfield);
	}
	// create the DBout table
	try {db.TableDefs.Append(DBoutTD);} 
	catch(...) { cout<<"\nError appending "<<DBout<<" table to database\n";}

	// open DBout table as a recordset (DBoutrst) for writing
	DBoutrst = db.OpenRecordset(DBout,dbOpenDynaset);
	// **** End DBout table *****	
}
#else
void closeDB(void) {
}
void openDB(void) {
}
void createDBout(void) {
}
#endif
