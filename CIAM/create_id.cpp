/* create_id.cpp															*
 * funtion to create id's for outputs *
 * shk 6/26/02	*/

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
// custom header
#include "modeltime.h"

using namespace std; // enables elimination of std::

// database table names
extern const char *dbtghg;
extern const char *dbtdrsc;
extern const char *dbtsupsec;
extern const char *dbtdemsec;
extern const char *dbtgen;
extern const char *dbtmod;
extern const char *dbtdemog;
extern const char *dbtout;

// id tables for all outputs 
const char *regid = "idregion"; // region id table
const char *secid = "idsector"; // sector id table
const char *subsecid = "idsubsec"; // subsector id table
const char *techid = "idtech"; // variable id table
const char *varid = "idvar"; // variable id table
const char *catid = "idcat"; // category id table
const char *tempid = "idtemp"; // tempory id table
const char *DBVarid = "DBVarLabels"; // variable ids
const char *DBVarOrg = "DBVarLabelOrg"; // Minicam variable ids
const char *DBout = "DBout"; // Minicam style output

extern Modeltime modeltime;
extern CdbDatabase db;
extern CdbRecordset outrst; // output recordset
// recordset for gen, resource, supply, and demand sector DB table
extern CdbRecordset modelrst,genrst,drscrst,suprst,demrst,demogrst,ghgrst; 
// recordset to add index and name for all id tables created
CdbRecordset regidrst, // region id 
			 secidrst, // sector id
			 subsecidrst, // subcsector id
			 techidrst, // technology id
			 varidrst, // variable id
			 catidrst, // category id
			 DBVaridrst, // variable id
			 DBVarOrgrst, // MiniCAM variable id
			 DBoutrst; // MiniCAM style output
// table defs for creating tables in the database
CdbTableDef outTD, 
			regidTD, // region id
			secidTD, // sector id
			subsecidTD, // subsector id
			techidTD, // technology id
			varidTD, // variable id
			catidTD, // category id
			tempidTD, // temp id table for ridding duplicates
			DBVaridTD, // MiniCAM variable id
			DBoutTD; // MiniCAM style output
// for creating fields in tables
CdbField tfield; // tempory field

// map objects for output id 's
map<string,int> mapreg,mapsec,mapsubsec,maptech,mapvar,mapcat;
map<string,int> mapVarID;


void createidtbl(void) 
{
	// ***** create index tables *******

	// ***** region id table ********
	try { db.TableDefs.Delete(regid); } // first delete existing table
	catch (...) {cout<<"\nError deleting "<<regid<<" table\n";}
	regidTD = db.CreateTableDef(regid);
	tfield = regidTD.CreateField("ID",dbInteger);
	regidTD.Fields.Append(tfield);
	tfield = regidTD.CreateField("Region",dbText);
	regidTD.Fields.Append(tfield);
	try {db.TableDefs.Append(regidTD);}  // create regid table
	catch(...) { cout<<"\nError appending "<<regid<<" table to database\n";}	
	// tempory id table
	tempidTD = db.CreateTableDef(tempid);
	tfield = tempidTD.CreateField("ID",dbInteger);
	tempidTD.Fields.Append(tfield);
	tfield = tempidTD.CreateField("Region",dbText);
	tempidTD.Fields.Append(tfield);
	try {db.TableDefs.Append(tempidTD);}
	catch(...) { cout<<"\nError appending tempid table to database\n";}	
	// add unique region and market region names to index from general table
	string sqltemp = "INSERT INTO ";
	sqltemp = sqltemp + tempid + " SELECT DISTINCT Region AS ID, RegionName AS Region";
	sqltemp = sqltemp + " FROM " + dbtgen;  
	try {
		db.Execute(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError executing sql\n"; }
	// add unique region and market region names to index from resource table
	sqltemp = "INSERT INTO ";
	sqltemp = sqltemp + tempid + " SELECT DISTINCT Market AS Region";
	sqltemp = sqltemp + " FROM " + dbtdrsc;
	try {
		db.Execute(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError executing sql\n"; }
	// add unique region and market region names to index from supply sector table
	sqltemp = "INSERT INTO " ;
	sqltemp = sqltemp + tempid + " SELECT DISTINCT Market AS Region";
	sqltemp = sqltemp + " FROM " + dbtsupsec;
	try {
		db.Execute(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError executing sql\n"; }
	// select unique region names from tempid and insert into regid table
	sqltemp = "INSERT INTO ";
	sqltemp = sqltemp + regid + " SELECT DISTINCT Region FROM ";
	sqltemp = sqltemp + tempid;  
	try {
		db.Execute(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError executing sql\n"; }
	try { db.TableDefs.Delete(tempid); } // delete temp table
	catch (...) {cout<<"\nError deleting tempid table\n";}

	// assign new unique id numbers
	regidrst = db.OpenRecordset(regid,dbOpenDynaset); // create region recordset
	short int id=0;
	regidrst.MoveFirst();
	while(!regidrst.GetEOF()) {
		regidrst.Edit();
		regidrst.SetField(0L, COleVariant(++id));
		regidrst.Update(); // save and write the record
		mapreg[regidrst.GetField(1L).pcVal] = id;
		regidrst.MoveNext(); // next record
	}
	//***** end region id table *****

	// ****** sector id table ******
	try { db.TableDefs.Delete(secid); } // first delete existing table
	catch (...) {cout<<"\nError deleting "<<secid<<" table\n";}
	secidTD = db.CreateTableDef(secid);
	tfield = secidTD.CreateField("ID",dbInteger);
	secidTD.Fields.Append(tfield);
	tfield = secidTD.CreateField("Sector",dbText);
	secidTD.Fields.Append(tfield);
	try {db.TableDefs.Append(secidTD);}  // creates secid table
	catch(...) { cout<<"\nError appending "<<secid<<" table to database\n";}	
	// add unique supply sector names to index
	sqltemp = "INSERT INTO ";
	sqltemp = sqltemp + secid + " SELECT DISTINCT Sector AS ID, SectorName AS Sector";
	sqltemp = sqltemp + " FROM " + dbtsupsec;  
	try {
		db.Execute(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError executing sql\n"; }
	// add unique demand sector names to index
	sqltemp = "INSERT INTO ";
	sqltemp = sqltemp + secid + " SELECT DISTINCT Sector AS ID, SectorName AS Sector";
	sqltemp = sqltemp + " FROM " + dbtdemsec;  
	try {
		db.Execute(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError executing sql\n"; }
	// add unique resource sector names to index	
	sqltemp = "INSERT INTO ";
	sqltemp = sqltemp + secid + " SELECT DISTINCT Sector AS ID, SectorName AS Sector";
	sqltemp = sqltemp + " FROM " + dbtdrsc;  
	try {
		db.Execute(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError executing sql\n"; }
/*
	sqltemp = "ALTER TABLE ";
	sqltemp = sqltemp + secid + " ADD PRIMARY KEY ID TAG ID";
	try {
		db.Execute(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError executing sql\n"; }
*/
 	// assign new unique id numbers
	secidrst = db.OpenRecordset(secid,dbOpenDynaset); // create sector recordset
	id=0;
	secidrst.MoveFirst();
	while(!secidrst.GetEOF()) {
		secidrst.Edit();
		secidrst.SetField(0L, COleVariant(++id));
		secidrst.Update(); // save and write the record
		mapsec[secidrst.GetField(1L).pcVal] = id;
		secidrst.MoveNext(); // next record
	}
	//  ****** end sector id table ******

	// ****** subsector id table ********
	try { db.TableDefs.Delete(subsecid); } // first delete existing table
	catch (...) {cout<<"\nError deleting "<<subsecid<<" table\n";}
	subsecidTD = db.CreateTableDef(subsecid);
	tfield = subsecidTD.CreateField("ID",dbInteger);
	subsecidTD.Fields.Append(tfield);
	tfield = subsecidTD.CreateField("Subsector",dbText);
	subsecidTD.Fields.Append(tfield);
	try {db.TableDefs.Append(subsecidTD);}  // create subsector id table
	catch(...) { cout<<"\nError appending "<<subsecid<<" table to database\n";}	
	// tempory id table
	tempidTD = db.CreateTableDef(tempid);
	tfield = tempidTD.CreateField("ID",dbInteger);
	tempidTD.Fields.Append(tfield);
	tfield = tempidTD.CreateField("Subsector",dbText);
	tempidTD.Fields.Append(tfield);
	try {db.TableDefs.Append(tempidTD);} // create temp id table
	catch(...) { cout<<"\nError appending tempid table to database\n";}	
	// add unique subsectors names from supply sector to index
	sqltemp = "INSERT INTO ";
	sqltemp = sqltemp + tempid + " SELECT DISTINCT Subsector AS ID, SubsectorName AS Subsector";
	sqltemp = sqltemp + " FROM " + dbtsupsec;  
	try {
		db.Execute(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError executing sql\n"; }
	// add unique subsector names from demand sector to index
	sqltemp = "INSERT INTO ";
	sqltemp = sqltemp + tempid + " SELECT DISTINCT Subsector AS ID, SubsectorName AS Subsector";
	sqltemp = sqltemp + " FROM " + dbtdemsec;  
	try {
		db.Execute(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError executing sql\n"; }
	// add unique subsector names from resource sector to index	
	sqltemp = "INSERT INTO ";
	sqltemp = sqltemp + tempid + " SELECT DISTINCT Subsector AS ID, SubsectorName AS Subsector";
	sqltemp = sqltemp + " FROM " + dbtdrsc;  
	try {
		db.Execute(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError executing sql\n"; }

	// select unique subsector names from tempid table and insert into subsecid table
	sqltemp = "INSERT INTO ";
	sqltemp = sqltemp + subsecid + " SELECT DISTINCT Subsector FROM ";
	sqltemp = sqltemp + tempid;  
	try {
		db.Execute(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError executing sql\n"; }
	try { db.TableDefs.Delete(tempid); } // delete tempid table
	catch (...) {cout<<"\nError deleting tempid table\n";}

	// assign new unique id numbers
	subsecidrst = db.OpenRecordset(subsecid,dbOpenDynaset); // create subsec recordset
	id=0;
	subsecidrst.MoveFirst();
	while(!subsecidrst.GetEOF()) {
		subsecidrst.Edit();
		subsecidrst.SetField(0L, COleVariant(++id));
		subsecidrst.Update(); // save and write the record
		mapsubsec[subsecidrst.GetField(1L).pcVal] = id;
		subsecidrst.MoveNext(); // next record
	}
	// ****** end of subsector id table *******

	// ***** technology id table ******
	try { db.TableDefs.Delete(techid); } // first delete existing table
	catch (...) {cout<<"\nError deleting "<<techid<<" table\n";}
	techidTD = db.CreateTableDef(techid);
	tfield = techidTD.CreateField("ID",dbInteger);
	techidTD.Fields.Append(tfield);
	tfield = techidTD.CreateField("Technology",dbText);
	techidTD.Fields.Append(tfield);
	try {db.TableDefs.Append(techidTD);} // create techid table
	catch(...) { cout<<"\nError appending "<<techid<<" table to database\n";}	
	// tempory id table
	tempidTD = db.CreateTableDef(tempid);
	tfield = tempidTD.CreateField("ID",dbInteger);
	tempidTD.Fields.Append(tfield);
	tfield = tempidTD.CreateField("Technology",dbText);
	tempidTD.Fields.Append(tfield);
	try {db.TableDefs.Append(tempidTD);} // create tempid table
	catch(...) { cout<<"\nError appending tempid table to database\n";}	
	// add unique supply technology names to index
	sqltemp = "INSERT INTO ";
	sqltemp = sqltemp + tempid + " SELECT DISTINCT Technology AS ID,TechnologyName AS Technology";
	sqltemp = sqltemp + " FROM " + dbtsupsec;  
	try {
		db.Execute(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError executing sql\n"; }
	// add unique demand technology names to index
	sqltemp = "INSERT INTO ";
	sqltemp = sqltemp + tempid + " SELECT DISTINCT Technology AS ID,TechnologyName AS Technology";
	sqltemp = sqltemp + " FROM " + dbtdemsec;  
	try {
		db.Execute(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError executing sql\n"; }
	// add unique resource grade names to index
	sqltemp = "INSERT INTO ";
	sqltemp = sqltemp + tempid + " SELECT DISTINCT Grade AS ID,GradeName AS Technology";
	sqltemp = sqltemp + " FROM " + dbtdrsc;  
	try {
		db.Execute(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError executing sql\n"; }
	// select unique technology names from tempid table and insert into techid table
	sqltemp = "INSERT INTO ";
	sqltemp = sqltemp + techid + " SELECT DISTINCT Technology FROM ";
	sqltemp = sqltemp + tempid;  
	try {
		db.Execute(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError executing sql\n"; }
	try { db.TableDefs.Delete(tempid); } // delete temp table
	catch (...) {cout<<"\nError deleting tempid table\n";}

	// assign new unique id numbers
	techidrst = db.OpenRecordset(techid,dbOpenDynaset); // create tech recordset
	id=0;
	techidrst.MoveFirst();
	while(!techidrst.GetEOF()) {
		techidrst.Edit();
		techidrst.SetField(0L, COleVariant(++id));
		techidrst.Update(); // save and write the record
		maptech[techidrst.GetField(1L).pcVal] = id;
		techidrst.MoveNext(); // next record
	}
	// ***** end technology id table *******

	// ***** variable id table ******
	try { db.TableDefs.Delete(varid); } // first delete existing table
	catch (...) {cout<<"\nError deleting "<<varid<<" table\n";}
	varidTD = db.CreateTableDef(varid);
	tfield = varidTD.CreateField("ID",dbInteger);
	varidTD.Fields.Append(tfield);
	tfield = varidTD.CreateField("Variable",dbText);
	varidTD.Fields.Append(tfield);
	try {db.TableDefs.Append(varidTD);} // create varid table
	catch(...) { cout<<"\nError appending "<<varid<<" table to database\n";}

	// add variables to varid table
	varidrst = db.OpenRecordset(varid,dbOpenDynaset);
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("population", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("CO2 emissions", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("primary energy production", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("consumption", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("generation", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("conv resource", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("unconv resource", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("all resources", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("labor productivity", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("gnp($)", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("gnp(norm)", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("gnp(energy adj)", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("carbon tax", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("carbon tax rev", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("production", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("available", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("energy input", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("ave cost", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("carbon taxes paid", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("share", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("cost", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("carbon tax($/TC)", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("carbon tax($/GJ)", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("fuel input", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("efficiency", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("non-energy cost", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("CO2 ind emissions", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("price", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("supply", VT_BSTRT));
	varidrst.Update(); // save and write the record
	varidrst.AddNew(); // now the current record is this empty new one
	varidrst.SetField(1L, COleVariant("demand", VT_BSTRT));
	varidrst.Update(); // save and write the record
	// assign new unique id numbers
	id=0;
	varidrst.MoveFirst();
	while(!varidrst.GetEOF()) {
		varidrst.Edit();
		varidrst.SetField(0L, COleVariant(++id));
		varidrst.Update(); // save and write the record
		mapvar[varidrst.GetField(1L).pcVal] = id;
		varidrst.MoveNext(); // next record
	}
	// ***** end variable id table *****

	// ***** category id table ******
	try { db.TableDefs.Delete(catid); } // first delete existing table
	catch (...) {cout<<"\nError deleting "<<catid<<" table\n";}
	catidTD = db.CreateTableDef(catid);
	//tfield = catidTD.CreateField("ID",dbInteger);
	tfield = catidTD.CreateField("ID",dbLong);
	catidTD.Fields.Append(tfield);
	tfield = catidTD.CreateField("Category",dbText);
	catidTD.Fields.Append(tfield);
	try {db.TableDefs.Append(catidTD);} // create catid table
	catch(...) { cout<<"\nError appending "<<catid<<" table to database\n";}

	// add Minicam category names to catid table
	catidrst = db.OpenRecordset(catid,dbOpenDynaset);
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(0L, COleVariant(long (100000), VT_I4));
	catidrst.SetField(1L, COleVariant("CO2 Emiss", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(0L, COleVariant(long (110000), VT_I4));
	catidrst.SetField(1L, COleVariant("Oth Emiss", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(0L, COleVariant(long (150000), VT_I4));
	catidrst.SetField(1L, COleVariant("General", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(0L, COleVariant(long (200000), VT_I4));
	catidrst.SetField(1L, COleVariant("Pri Energy", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(0L, COleVariant(long (210000), VT_I4));
	catidrst.SetField(1L, COleVariant("Final Energy", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(0L, COleVariant(long (250000), VT_I4));
	catidrst.SetField(1L, COleVariant("Electric Pwr", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(0L, COleVariant(long (300000), VT_I4));
	catidrst.SetField(1L, COleVariant("Refining", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(0L, COleVariant(long (400000), VT_I4));
	catidrst.SetField(1L, COleVariant("Prices", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(0L, COleVariant(long (450000), VT_I4));
	catidrst.SetField(1L, COleVariant("Elasticities", VT_BSTRT));
	catidrst.Update(); // save and write the record
/*
	// add category names to catid table
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(1L, COleVariant("population", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(1L, COleVariant("demographics", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(1L, COleVariant("economics", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(1L, COleVariant("energy", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(1L, COleVariant("emissions", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(1L, COleVariant("resource", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(1L, COleVariant("market", VT_BSTRT));
	catidrst.Update(); // save and write the record

	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(1L, COleVariant("CO2", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(1L, COleVariant("primary energy", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(1L, COleVariant("labor", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(1L, COleVariant("gnp", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(1L, COleVariant("tax", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(1L, COleVariant("all subresources", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(1L, COleVariant("all cattors", VT_BSTRT));
	catidrst.Update(); // save and write the record

	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(1L, COleVariant("total region(fuel)", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(1L, COleVariant("total region(sector)", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(1L, COleVariant("all grades", VT_BSTRT));
	catidrst.Update(); // save and write the record
	catidrst.AddNew(); // now the current record is this empty new one
	catidrst.SetField(1L, COleVariant("all catnologies", VT_BSTRT));
	catidrst.Update(); // save and write the record
*/
	// assign new unique id numbers
/*	id=0;
	catidrst.MoveFirst();
	while(!catidrst.GetEOF()) {
		catidrst.Edit();
		catidrst.SetField(0L, COleVariant(++id));
		catidrst.Update(); // save and write the record
		mapcat[catidrst.GetField(1L).pcVal] = id;
		catidrst.MoveNext(); // next record
	}
*/	// ***** end category id table *****

/*	// ***** MiniCAM DBVarLabels table ******
	try { db.TableDefs.Delete(DBVarid); } // first delete existing table
	catch (...) {cout<<"\nError deleting "<<DBVarid<<" table\n";}
	DBVaridTD = db.CreateTableDef(DBVarid);
	tfield = DBVaridTD.CreateField("VarID",dbLong);
	DBVaridTD.Fields.Append(tfield);
	tfield = DBVaridTD.CreateField("Cat",dbText);
	DBVaridTD.Fields.Append(tfield);
	tfield = DBVaridTD.CreateField("SubCat",dbText);
	DBVaridTD.Fields.Append(tfield);
	tfield = DBVaridTD.CreateField("VarLabel",dbText);
	DBVaridTD.Fields.Append(tfield);
	tfield = DBVaridTD.CreateField("VarUnits",dbText);
	DBVaridTD.Fields.Append(tfield);
	try {db.TableDefs.Append(DBVaridTD);} // create DBVarid table
	catch(...) { cout<<"\nError appending "<<DBVarid<<" table to database\n";}
	
	// map MiniCAM output variable ID
	DBVarOrgrst = db.OpenRecordset(DBVarOrg,dbOpenDynaset);
	string str;
	while(!DBVarOrgrst.GetEOF()) {
		// concatenate strings
		str = DBVarOrgrst.GetField(1L).pcVal;
		str += DBVarOrgrst.GetField(2L).pcVal;
		str += DBVarOrgrst.GetField(3L).pcVal;
		mapVarID[str] = DBVarOrgrst.GetField(0L).intVal;
		DBVarOrgrst.MoveNext(); // next record
	}

	DBVaridrst = db.OpenRecordset(DBVarid,dbOpenDynaset);
	DBVaridrst.AddNew(); // now the current record is this empty new one
	DBVaridrst.SetField(0L, COleVariant(long(mapVarID["CO2 Emissby FuelOil"]), VT_I4));
	DBVaridrst.SetField(1L, COleVariant("CO2 Emiss", VT_BSTRT));
	DBVaridrst.SetField(2L, COleVariant("by Fuel", VT_BSTRT));
	DBVaridrst.SetField(3L, COleVariant("Oil", VT_BSTRT));
	DBVaridrst.Update(); // save and write the record

	typedef map<string,int>:: const_iterator CI;
	for (CI p=mapVarID.begin(); p!=mapVarID.end(); ++p) {
		DBVaridrst.Edit();
		DBVaridrst.SetField(0L, COleVariant(p->first));
		DBVaridrst.SetField(1L, COleVariant(p->second, VT_BSTRT));
		DBVaridrst.Update(); // save and write the record
		DBVaridrst.MoveNext(); // next record
		cout <<p->second <<"    " <<p->first <<"\n";
	} */

	// ***** end MiniCAM DBVarLabels table ******

	// ***** MiniCAM DBout table ******
	try { db.TableDefs.Delete(DBout); } // first delete existing table
	catch (...) {cout<<"\nError deleting "<<DBout<<" table\n";}
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
	for (int t=0;t<modeltime.getmaxper();t++) { 
		str = "y"; //str is string object
		str += _itoa(modeltime.getper_to_yr(t),buffer,10); //convert to char*
		tfield = DBoutTD.CreateField(str.c_str(),dbText);
		DBoutTD.Fields.Append(tfield);
	}
	try {db.TableDefs.Append(DBoutTD);} // create DBout table
	catch(...) { cout<<"\nError appending "<<DBout<<" table to database\n";}
	// open dboutrst
	DBoutrst = db.OpenRecordset(DBout,dbOpenDynaset);
	// **** DBout table *****	
	
	regidrst.Close();		// region id recordset
	secidrst.Close();			// category id recordset
	subsecidrst.Close();	// subcategory id recordset
	techidrst.Close();		// variable id recordset
	varidrst.Close();	// subvariable id recordset
	//DBVaridrst.Close(); // Varid recordset
	//DBVarOrgrst.Close(); // MiniCAM Varid recordset

}
