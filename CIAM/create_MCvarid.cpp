/* create_MCvarid															*
 * funtion to create id's for outputs *
 * shk 6/26/02	*/

// standard libraries
#include <stdlib.h>
#include <afxdisp.h>
#include <dbdao.h>
#include <dbdaoerr.h>
#include <direct.h>
#include <iostream>
//#include <fstream>
#include <map>
#include <string> // using "string.h" does not enable use of string class
//#include <time.h> // to use clock and time functions
//#include "modeltime.h"

using namespace std; // enables elimination of std::


extern const char *DBout; // Minicam style output
//extern Modeltime modeltime;
extern CdbDatabase db;


void create_MCvarid(void) 
{
	// id tables for all outputs 
	const char *DBVarLabels = "DbVarLabels"; // variable ids
	// recordset to add index and name for all id tables created
	CdbRecordset temprst,DBVarLabelsrst,DBtemprst; // variable id and dbout
	// table defs for creating tables in the database
	CdbTableDef DBVarLabelsTD; // MiniCAM variable id
	// for creating fields in tables
	CdbField tfield2; // tempory field

	// map objects for output id 's
	map<string,int> mapMCcat,mapsubcat,mapvarlabel;
	map<string,int> mapVarID;

	string sqltemp,str; // temporary strings

	// ***** Create MiniCAM DBVarLabels table ******
	try { db.TableDefs.Delete(DBVarLabels); } // first delete existing table
	catch (...) {cout<<"\nError deleting "<<DBVarLabels<<" table\n";}
	DBVarLabelsTD = db.CreateTableDef(DBVarLabels);
	tfield2 = DBVarLabelsTD.CreateField("VarID",dbLong);
	DBVarLabelsTD.Fields.Append(tfield2);
	tfield2 = DBVarLabelsTD.CreateField("Cat",dbText);
	DBVarLabelsTD.Fields.Append(tfield2);
	tfield2 = DBVarLabelsTD.CreateField("SubCat",dbText);
	DBVarLabelsTD.Fields.Append(tfield2);
	tfield2 = DBVarLabelsTD.CreateField("VarLabel",dbText);
	DBVarLabelsTD.Fields.Append(tfield2);
	tfield2 = DBVarLabelsTD.CreateField("VarUnits",dbText);
	DBVarLabelsTD.Fields.Append(tfield2);
	try {db.TableDefs.Append(DBVarLabelsTD);} // create DBVarLabels table
	catch(...) { cout<<"\nError appending "<<DBVarLabels<<" table to database\n";}
	
	// map MiniCAM output category
	sqltemp = " SELECT DISTINCT Cat FROM ";
	sqltemp += DBout;  // MiniCAM style output  
	temprst = db.OpenRecordset(sqltemp.c_str());
	int catno = 10;
	while(!temprst.GetEOF()) {
		str = temprst.GetField(0L).pcVal;
		mapMCcat[str] = catno * 10000; // category starts from 100,000
		temprst.MoveNext(); // next record
		catno++;
	}
	// map MiniCAM output subcategory
	sqltemp = " SELECT DISTINCT SubCat FROM ";
	sqltemp += DBout;  // MiniCAM style output  
	temprst = db.OpenRecordset(sqltemp.c_str());
	int subcatno = 1;
	while(!temprst.GetEOF()) {
		str = temprst.GetField(0L).pcVal;
		mapsubcat[str] = subcatno * 100; // subcategory starts from 100
		temprst.MoveNext(); // next record
		subcatno++;
	}
	// map MiniCAM output varlabel
	sqltemp = " SELECT DISTINCT VarLabel FROM ";
	sqltemp += DBout;  // MiniCAM style output  
	temprst = db.OpenRecordset(sqltemp.c_str());
	int varlabelno = 1;
	while(!temprst.GetEOF()) {
		str = temprst.GetField(0L).pcVal;
		mapvarlabel[str] = varlabelno; // varlabel starts from 1
		temprst.MoveNext(); // next record
		varlabelno++;
	}
	// select unique varid names from DBout table and insert into VarLabel table
	sqltemp = "INSERT INTO ";
	sqltemp = sqltemp + DBVarLabels + " SELECT DISTINCT Cat,SubCat,VarLabel,VarUnits FROM ";
	sqltemp = sqltemp + DBout;
	try {
		db.Execute(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError executing sql for DBVarLabels\n"; }


	// map MiniCAM output varlabel
	sqltemp = " SELECT DISTINCT Cat, Subcat, VarLabel FROM ";
	sqltemp += DBout;  // MiniCAM style output  
	temprst = db.OpenRecordset(sqltemp.c_str());

	string tempcat = temprst.GetField(0L).pcVal;
	string tempsubcat = temprst.GetField(1L).pcVal;
	string tempvarlabel;
	catno = 10;
	while(!temprst.GetEOF()) {
		subcatno = 1; // restart from 1
		while(!temprst.GetEOF() && tempcat==temprst.GetField(0L).pcVal) {
			varlabelno = 1; // restart varlabel number from 1
			while(!temprst.GetEOF() && tempcat==temprst.GetField(0L).pcVal
				&& tempsubcat==temprst.GetField(1L).pcVal) {
				tempvarlabel = temprst.GetField(2L).pcVal;
				str = tempcat + tempsubcat + tempvarlabel;
				mapVarID[str] = catno*10000 + subcatno*100 + varlabelno;
				temprst.MoveNext(); // next record
				varlabelno++;
			}
			subcatno++;
			if(!temprst.GetEOF()) tempsubcat = temprst.GetField(1L).pcVal;
		}
		catno++;
		if(!temprst.GetEOF()) tempcat = temprst.GetField(0L).pcVal;
	}


	// add VarIDs to each record
	DBVarLabelsrst = db.OpenRecordset(DBVarLabels,dbOpenDynaset);
	long int varid;
	while(!DBVarLabelsrst.GetEOF()) {
		/*varid = mapMCcat[DBVarLabelsrst.GetField(_T("Cat")).pcVal]
				   + mapsubcat[DBVarLabelsrst.GetField(_T("SubCat")).pcVal]
			   	   + mapvarlabel[DBVarLabelsrst.GetField(_T("VarLabel")).pcVal];
				   */
		str = DBVarLabelsrst.GetField(_T("Cat")).pcVal;
		str += DBVarLabelsrst.GetField(_T("SubCat")).pcVal;
		str += DBVarLabelsrst.GetField(_T("VarLabel")).pcVal;
		varid = mapVarID[str];

		DBVarLabelsrst.Edit(); // edit current record
		DBVarLabelsrst.SetField(_T("VarID"), COleVariant(varid, VT_I4));
		DBVarLabelsrst.Update(); // save and write the record
		DBVarLabelsrst.MoveNext(); // next record
	}
/*
	// create VarID map
	DBVarLabelsrst.MoveFirst();
	while(!DBVarLabelsrst.GetEOF()) {
		// strings are concatenated
		str = DBVarLabelsrst.GetField(_T("Cat")).pcVal;
		str += DBVarLabelsrst.GetField(_T("SubCat")).pcVal;
		str += DBVarLabelsrst.GetField(_T("VarLabel")).pcVal;
		mapVarID[str] = DBVarLabelsrst.GetField(_T("VarID")).intVal;
		DBVarLabelsrst.MoveNext(); // next record
	}
	// ***** end MiniCAM DBVarLabels table ******
*/
	// use VarID map to fill id's in DBout
	DBtemprst = db.OpenRecordset(DBout,dbOpenDynaset);
	while(!DBtemprst.GetEOF()) {
		str = DBtemprst.GetField(_T("Cat")).pcVal;
		str += DBtemprst.GetField(_T("SubCat")).pcVal;
		str += DBtemprst.GetField(_T("VarLabel")).pcVal;
		varid = mapVarID[str];
		DBtemprst.Edit();
		//DBtemprst.SetField(_T("VarID"), COleVariant(long(mapVarID[str]), VT_I4));
		DBtemprst.SetField(_T("VarID"), COleVariant(varid, VT_I4));
		DBtemprst.Update(); // save and write the record
		DBtemprst.MoveNext(); // next record
	}

	// ***** Write to MiniCAM tables for dataviewer ******
	// database table names
	const char *dbtmain = "DbMain";
	const char *dbtrun = "DbRunLabels";
	// insert selected fields in DBout to DbMain 
	// this is for the dataviewer
	sqltemp = "INSERT INTO ";
	sqltemp += dbtmain;
	sqltemp += " SELECT RunID,Region,VarID,y1990,y2005,y2020,y2035,y2050,y2065,y2080,y2095";
	sqltemp = sqltemp + " FROM " + DBout;  
	try {
		db.Execute(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError executing sql\n"; }

	// insert into run labels table
	sqltemp = "INSERT INTO ";
	sqltemp += dbtrun;
	sqltemp += " SELECT DISTINCT RunID";
	sqltemp = sqltemp + " FROM " + DBout;
	try {
		db.Execute(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError executing sql\n"; }
	// ***** End write to MiniCAM tables for dataviewer ******

	
	DBtemprst.Close();
	DBVarLabelsrst.Close(); // Varid recordset
	temprst.Close(); // temporary recordset


}
