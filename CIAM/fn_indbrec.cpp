/* fn_indbrec.cpp													*
 * Function to count the number of unique records in the database	*
 * with	search field specified as argument							*	
 * Arguments: string field name, dbname,dbtable						*
 * Returns: int count of unique records								*
 * Coded by Sonny Kim 7/27/00										*/

#include <string>
#include <iostream>
#include <afxdisp.h>
#include <dbdao.h>
#include <dbdaoerr.h>
#include "str_indexname.h" // get index and name from database

using namespace std; // enables elimination of std::

extern CdbDatabase db;

// Reads index and name at the sector level
void indbrec(str_indexname* str_temp,string index,string name,int ns,
					  const char *dbname,const char *dbtname)
{
	string str;
	CdbRecordset rst;

	// get index and first name (TName) from dataset
	// use group by on the index to get one for each index.
	str = "SELECT " + index + ", FIRST(" + name + ") AS TName FROM ";
	str = str + dbtname + " GROUP BY " + index;
	//openrecordset requires const char* or C style string
	try {
		rst = db.OpenRecordset(str.c_str());
	}
	catch(...) {
		cerr <<"\nproblem while calling indbrec()\n";
	}
	// check for null recordset, check query if exit
	if (rst.GetRecordCount() == 0) {
		cerr <<"Problem calling indbrec("<<index<<","<<name
			 <<","<<ns<<","<<dbtname<<"): null recordset\n";
		exit(1);
	}
	rst.MoveFirst();
	for (int i=0;i<ns;i++)
	{
		str_temp[i].index = rst.GetField(index.c_str()).iVal;
		strcpy(str_temp[i].name, rst.GetField("TName").pcVal);
		rst.MoveNext();
	}
	rst.Close(); // close the recordset
}

// Reads index and name at the sector level
void label_sec(str_indexname* str_temp,string region,string index,string name,int ns,
					  const char *dbtname)
{
	string str;
	CdbRecordset rst;

	// get index and first name (TName) from dataset
	// use group by on the index to get one for each index.
	str = "SELECT " + index + ", FIRST(" + name + ") AS TName FROM ";
	str = str + dbtname + " WHERE RegionName = '" + region + "'";
	str = str + " GROUP BY " + index;
	// openrecordset requires const char* or C style string
	// try catch finds errors only, does not catch null recordset
	try {
		rst = db.OpenRecordset(str.c_str());
	}
	catch(...) {
		cerr <<"\nproblem while calling label_sec()\n";
	}
	// check for null recordset, check query if exit
	if (rst.GetRecordCount() == 0) {
		cerr <<"Problem calling label_sec("<<region<<","<<index<<","<<name
			 <<","<<ns<<","<<dbtname<<"): null recordset\n";
		exit(1);
	}
	rst.MoveFirst();
	// update structure with DB records
	for (int i=0;i<ns;i++)
	{
		str_temp[i].index = rst.GetField(index.c_str()).iVal;
		strcpy(str_temp[i].name, rst.GetField("TName").pcVal);
		rst.MoveNext();
	}
	rst.Close(); // close the recordset
}

// Reads index and name at the subsector level for each sector
void label_subsec(str_indexname* str_temp,string region, string index,
					  string name,int is,int ns,const char *dbtname)
{
	char buffer[20];
	string str;
	CdbRecordset rst;
	// convert integer to string for sql (radix 10)
	// buffer contains converted string
	_itoa(is,buffer,10);

	// get index and first name (TName-new heading) from dataset
	// use group by on the index to get one for each index.
	str = "SELECT " + index + ", FIRST(" + name + ") AS TName FROM ";
	str += dbtname;
	str += " WHERE (Sector = ";
	str += buffer;
	str = str + " AND RegionName = '" + region + "')";
	str += " GROUP BY " + index;
	try {
		rst = db.OpenRecordset(str.c_str());
	}
	catch(...) {
		cerr <<"\nproblem while calling label_subsec()\n";
	}
	// check for null recordset, check query if exit
	if (rst.GetRecordCount() == 0) {
		cerr <<"Problem calling label_subsec("<<region<<","<<index<<","<<name
			 <<","<<is<<","<<ns<<","<<dbtname<<"): null recordset\n";
		exit(1);
	}
	rst.MoveFirst();
	// update structure with DB records
	for (int i=0;i<ns;i++)
	{
		str_temp[i].index = rst.GetField(index.c_str()).iVal;
		strcpy(str_temp[i].name, rst.GetField("TName").pcVal);
		rst.MoveNext();
	}
	rst.Close(); // close the recordset
}


// Reads market region and name
void indbrecmrk(str_regname* str_temp,string mrkt,string name,int ns,
					  const char *dbname,const char *dbtname)
{
	string str;
	CdbRecordset rst;

  // determine number of sectors in dataset
	str = "SELECT DISTINCT " +
		   mrkt + "," + name +" FROM ";
	str += dbtname;
	//openrecordset requires const char* or C style string
	try {
		rst = db.OpenRecordset(str.c_str());
	}
	catch(...) {
		cerr <<"\nproblem while calling indbrecmrk()\n";
	}
	// check for null recordset, check query if exit
	if (rst.GetRecordCount() == 0) {
		cerr <<"Problem calling indbrecmrk("<<mrkt<<","<<name
			 <<","<<ns<<","<<dbtname<<"): null recordset\n";
		exit(1);
	}
	rst.MoveFirst();
	for (int i=0;i<ns;i++)
	{
		strcpy(str_temp[i].region, rst.GetField(mrkt.c_str()).pcVal);
		strcpy(str_temp[i].name, rst.GetField(name.c_str()).pcVal);
		rst.MoveNext();
	}
	rst.Close(); // close the recordset
}


// Reads countries in market region
void indbmrkreg(str_regname* str_temp,string fdname1,string fdname2,int ns,
					  const char *dbname,const char *dbtname)
{
	string str;
	CdbRecordset rst;

  // determine number of sectors in dataset
	str = "SELECT DISTINCT RegionName,";
	str += "Market, SectorName FROM ";
	str += dbtname;
	str = str+" WHERE (Market = '"+fdname1+"' and SectorName = '"+fdname2+"')";
	//openrecordset requires const char* or C style string
	try {
		rst = db.OpenRecordset(str.c_str());
	}
	catch(...) {
		cerr <<"\nproblem while calling indbmrkreg()\n";
	}
	// check for null recordset, check query if exit
	if (rst.GetRecordCount() == 0) {
		cerr <<"Problem calling indbmrkreg("<<fdname1<<","<<fdname2
			 <<","<<ns<<","<<dbtname<<"): null recordset\n";
		exit(1);
	}
	rst.MoveFirst();
	for (int i=0;i<ns;i++)
	{
		strcpy(str_temp[i].region, rst.GetField("RegionName").pcVal);
		strcpy(str_temp[i].name, rst.GetField("Market").pcVal);
		rst.MoveNext();
	}
	rst.Close(); // close the recordset
}
