/*! 
* \file createMCvarid.cpp
* \ingroup CIAM
* \brief This file contains the function to create variable ids for outputs that are compatible with the dataviewer.xls. 
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#if(__HAVE_DB__)
#include <afxdisp.h>
#include <dbdao.h>
#include <iostream>
#include <map>
#include <string>

using namespace std; 

extern map<string,int> regionMap; // map of region names
extern const char *DBout; // Minicam style output
extern CdbDatabase db;

//! Create variable ids for outputs that are compatable with the dataviewer.xls.
void createMCvarid() {
	const char *DBVarLabels = "DbVarLabels"; // database table for variable ids
	CdbRecordset temprst,DBVarLabelsrst,DBtemprst; // recordsets for writing tables
	CdbTableDef DBVarLabelsTD; // table definitions for creating new tables
	CdbField tfield; // tempory field for creating fields in tables

	// map objects for creating variables id from output category, subcategory,
	// and variable labels.
	map<string,int> mapCat,mapSubCat,mapVarLabel,mapVarID;

	string sqltemp,str; // temporary strings

	// ***** Create DBVarLabels table ******
	// first delete existing table
	try { db.TableDefs.Delete(DBVarLabels); } 
	catch (...) {cout<<"\nError deleting "<<DBVarLabels<<" table\n";}
	// create fields
	DBVarLabelsTD = db.CreateTableDef(DBVarLabels);
	tfield = DBVarLabelsTD.CreateField("VarID",dbLong);
	DBVarLabelsTD.Fields.Append(tfield);
	tfield = DBVarLabelsTD.CreateField("Cat",dbText);
	DBVarLabelsTD.Fields.Append(tfield);
	tfield = DBVarLabelsTD.CreateField("SubCat",dbText);
	DBVarLabelsTD.Fields.Append(tfield);
	tfield = DBVarLabelsTD.CreateField("VarLabel",dbText);
	DBVarLabelsTD.Fields.Append(tfield);
	tfield = DBVarLabelsTD.CreateField("VarUnits",dbText);
	DBVarLabelsTD.Fields.Append(tfield);
	// create DBVarLabels table
	try {db.TableDefs.Append(DBVarLabelsTD);} 
	catch(...) { cout<<"\nError appending "<<DBVarLabels<<" table to database\n";}
	
	// map output category
	sqltemp = " SELECT DISTINCT Cat FROM ";
	sqltemp += DBout;   
	temprst = db.OpenRecordset(sqltemp.c_str());
	int catno = 10;
	while(!temprst.GetEOF()) {
		str = temprst.GetField(0L).pcVal;
		mapCat[str] = catno * 10000; // category starts from 100,000
		temprst.MoveNext(); // next record
		catno++;
	}

	// map output subcategory
	sqltemp = " SELECT DISTINCT SubCat FROM ";
	sqltemp += DBout; 
	temprst = db.OpenRecordset(sqltemp.c_str());
	int subcatno = 1;
	while(!temprst.GetEOF()) {
		str = temprst.GetField(0L).pcVal;
		mapSubCat[str] = subcatno * 100; // subcategory starts from 100
		temprst.MoveNext(); // next record
		subcatno++;
	}

	// map output variable label
	sqltemp = " SELECT DISTINCT VarLabel FROM ";
	sqltemp += DBout; 
	temprst = db.OpenRecordset(sqltemp.c_str());
	int varlabelno = 1;
	while(!temprst.GetEOF()) {
		str = temprst.GetField(0L).pcVal;
		mapVarLabel[str] = varlabelno; // varlabel starts from 1
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

	// map output varlabel
	sqltemp = " SELECT DISTINCT Cat, SubCat, VarLabel FROM ";
	sqltemp += DBout; 
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
		str = DBVarLabelsrst.GetField(_T("Cat")).pcVal;
		str += DBVarLabelsrst.GetField(_T("SubCat")).pcVal;
		str += DBVarLabelsrst.GetField(_T("VarLabel")).pcVal;
		varid = mapVarID[str];

		DBVarLabelsrst.Edit(); // edit current record
		DBVarLabelsrst.SetField(_T("VarID"), COleVariant(varid, VT_I4));
		DBVarLabelsrst.Update(); // save and write the record
		DBVarLabelsrst.MoveNext(); // next record
	}
	// ***** end DBVarLabels table ******

	// use VarID map to fill id's in DBout
	DBtemprst = db.OpenRecordset(DBout,dbOpenDynaset);
	while(!DBtemprst.GetEOF()) {
		str = DBtemprst.GetField(_T("Cat")).pcVal;
		str += DBtemprst.GetField(_T("SubCat")).pcVal;
		str += DBtemprst.GetField(_T("VarLabel")).pcVal;
		varid = mapVarID[str];
		DBtemprst.Edit();
		DBtemprst.SetField(_T("VarID"), COleVariant(varid, VT_I4));
		DBtemprst.Update(); // save and write the record
		DBtemprst.MoveNext(); // next record
	}

	// ***** Write tables for dataviewer ******
	// *** DbMain table ***
	// database table name
	const char *dbtmain = "DbMain";
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
	// *** end DbMain table ***

	// *** DbRunLabels table ***
	// database table names
	const char *dbtrun = "DbRunLabels";
	// insert into run labels table
	sqltemp = "INSERT INTO ";
	sqltemp += dbtrun;
	sqltemp += " SELECT DISTINCT RunID";
	sqltemp = sqltemp + " FROM " + DBout;
	try {
		db.Execute(sqltemp.c_str()); }
	catch(...) {
		cout<<"\nError executing sql\n"; }
	// *** end DbRunLabels table ***

	// *** RegionInfo table ***
	// database table names
	const char *dbtregion = "RegionInfo";
	// first delete existing table
	try { db.TableDefs.Delete(dbtregion); } 
	catch (...) {cout<<"\nError deleting "<<dbtregion<<" table\n";}
	// create new region info table
	CdbTableDef RegionInfoTD = db.CreateTableDef(dbtregion);
	tfield = RegionInfoTD.CreateField("RegionLabel",dbText);
	RegionInfoTD.Fields.Append(tfield);
	tfield = RegionInfoTD.CreateField("RegionID",dbLong);
	RegionInfoTD.Fields.Append(tfield);
	try {db.TableDefs.Append(RegionInfoTD);} // create DBVarLabels table
	catch(...) { cout<<"\nError appending "<<dbtregion<<" table to database\n";}

	CdbRecordset RegionInfoRst = db.OpenRecordset(dbtregion,dbOpenDynaset);
	typedef map<string,int>:: const_iterator CI;
	string regstr;
	for (CI rmap=regionMap.begin(); rmap!=regionMap.end(); ++rmap) {
		RegionInfoRst.AddNew(); // now the current record is this empty new one
		regstr = rmap->first;
		RegionInfoRst.SetField(_T("RegionLabel"), COleVariant(regstr.c_str(), VT_BSTRT));
		RegionInfoRst.SetField(_T("RegionID"), COleVariant(long(rmap->second), VT_I4));
		RegionInfoRst.Update(); // save and write the record
        if(rmap->second != 0) {
		// add Global region to sum all regional outputs
		RegionInfoRst.AddNew(); // now the current record is this empty new one
		RegionInfoRst.SetField(_T("RegionLabel"), COleVariant("zGlobal", VT_BSTRT));
		RegionInfoRst.SetField(_T("RegionID"), COleVariant(long(rmap->second), VT_I4));
		RegionInfoRst.Update(); // save and write the record
        }
	}
	// *** end RegionInfo table ***

	// ***** End write to MiniCAM tables for dataviewer ******

	// close all recordsets	
	RegionInfoRst.Close();
	DBtemprst.Close();
	DBVarLabelsrst.Close(); // Varid recordset
	temprst.Close(); // temporary recordset
}

#else
void createMCvarid(void) {
}
#endif
