/*! 
* \file dbConnection.cpp
* \ingroup CIAM
* \brief Contains the functions to open and close database connection and to create database table for writing result.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#ifdef WIN32
// standard libraries
#include <cstdlib>
#include <afxdisp.h>
#include <dbdao.h>
#include <dbdaoerr.h>
#include <direct.h>
#include <iostream>
#include <string>
// custom header
#include "scenario.h"
#include "modeltime.h"
#include "Configuration.h"

using namespace std;

// define global DB engine and database
CdbDBEngine dben;
CdbDatabase db;
CdbTableDef DBoutTD; // table definitions for creating tables in the database
CdbField tfield; // tempory field for creating fields in tables
CdbRecordset DBoutrst; // recordset for writing results

extern Scenario* scenario;

const char *DBout = "DBout"; // name of the table for outputs compatible with dataviewer

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
void closeDB() {

	db.Close();
}

/*! \brief Create and open the main database output table.
* 
* No data is appended from this method. 
* This table is used to create DBmain table used by the Dataviewer.xls.
* DBout table is first deleted and recreated with the proper set of fields,
* then create a recordset of this table for writing results.
*/

void createDBout() {

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
	for (int t=0;t<scenario->getModeltime()->getmaxper();t++) { 
		str = "y";
		str += itoa(scenario->getModeltime()->getper_to_yr(t),buffer,10); // convert int to char*
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
