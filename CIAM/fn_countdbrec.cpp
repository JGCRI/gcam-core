/* fn_countdbrec.cpp													*
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
using namespace std; // enables elimination of std::

extern CdbDatabase db;

// counts the number of unique indices in a field
int countdbrec(string fdname,const char *dbname,const char *dbtname)
{
	int numrec=0; // model period
	string str;

	CdbRecordset rst;

  // determine number of sectors in dataset
	str = "SELECT COUNT(" + fdname + ") FROM (SELECT DISTINCT " +
		      fdname +" FROM ";
	str += dbtname;
	str += ")";
	try {
		rst = db.OpenRecordset(str.c_str());
	}
	catch(...) {
		cerr <<"\nproblem while calling countdbrec()\n";
	}
	numrec = rst.GetField(0L).iVal; 
	rst.Close(); // close the recordset

	return numrec;
}

// counts the number of unique records in a field with condition
int count_sec(string region,string fdname,const char *dbtname)
{
	int numrec=0;
	string str;

	CdbRecordset rst;

  // determine number of sectors in a region
	str = "SELECT COUNT(" + fdname + ") FROM (SELECT DISTINCT " +
		      fdname +" FROM ";
	str += dbtname;
	str = str + " WHERE RegionName = '" + region;
	str += "')";
	try {
		rst = db.OpenRecordset(str.c_str());
	}
	catch(...) {
		cerr <<"\nproblem while calling count_sec()\n";
	}
	numrec = rst.GetField(0L).iVal; 
	rst.Close(); // close the recordset

	return numrec;
}

// counts the number of unique indices from the second field
int countdbrec2(string fdname,int is,const char *dbname,const char *dbtname)
{
	char buffer[20];
	int numrec=0; // model period
	string str;

	CdbRecordset rst;

	// convert integer to string for sql (radix 10)
	// buffer contains converted string
	_itoa(is,buffer,10);

	// determine number of sectors in dataset
	str = "SELECT COUNT(" + fdname + ") FROM (SELECT DISTINCT " +
		      fdname +" FROM ";
	str += dbtname;
	str += " WHERE Sector = ";
	str += buffer;
	str += ")";
	try {
		rst = db.OpenRecordset(str.c_str());
	}
	catch(...) {
		cerr <<"\nproblem while calling countdbrec2()\n";
	}
	numrec = rst.GetField(0L).iVal; 
	rst.Close(); // close the recordset

	return numrec;
}

// counts the number of unique subsectors from a sector and region
int count_subsec(string region,string fdname,int is,const char *dbtname)
{
	char buffer[20];
	int numrec=0; // model period
	string str;

	CdbRecordset rst;

	// convert integer to string for sql (radix 10)
	// buffer contains converted string
	_itoa(is,buffer,10);

	// determine number of sectors in dataset
	str = "SELECT COUNT(" + fdname + ") FROM (SELECT DISTINCT " +
		      fdname +" FROM ";
	str += dbtname;
	str += " WHERE (Sector = ";
	str = str + buffer + " AND RegionName = '" + region;
	str += "'))";
	try {
		rst = db.OpenRecordset(str.c_str());
	}
	catch(...) {
		cerr <<"\nproblem while calling count_subsec()\n";
	}
	numrec = rst.GetField(0L).iVal; 
	rst.Close(); // close the recordset

	return numrec;
}

// counts the number of unique indices from the third field
int countdbrec3(string fdname,int is,int iss,const char *dbname,const char *dbtname)
{
	char bufis[20];
	char bufiss[20];
	int numrec=0; // model period
	string str;

	CdbRecordset rst;

	// convert integer to string for sql (radix 10)
	// buffer contains converted string
	_itoa(is,bufis,10);
	_itoa(iss,bufiss,10);

	// determine number of sectors in dataset
	str = "SELECT COUNT(" + fdname + ") FROM (SELECT DISTINCT " +
		      fdname +" FROM ";
	str += dbtname;
	str += " WHERE (Sector = ";
	str += bufis;
	str += " AND Subsector = ";
	str += bufiss;
	str += "))";
	try {
		rst = db.OpenRecordset(str.c_str());
	}
	catch(...) {
		cerr <<"\nproblem while calling countdbrec3()\n";
	}
	numrec = rst.GetField(0L).iVal; 
	rst.Close(); // close the recordset

	return numrec;
}

// counts the number of unique indices from the third field
int count_tech(string region,string fdname,int is,int iss,const char *dbtname)
{
	char bufis[20];
	char bufiss[20];
	int numrec=0; // model period
	string str;

	CdbRecordset rst;

	// convert integer to string for sql (radix 10)
	// buffer contains converted string
	_itoa(is,bufis,10);
	_itoa(iss,bufiss,10);

	// determine number of sectors in dataset
	str = "SELECT COUNT(" + fdname + ") FROM (SELECT DISTINCT " +
		      fdname +" FROM ";
	str += dbtname;
	str += " WHERE (RegionName = '";
	str += region;
	str += "' AND Sector = ";
	str += bufis;
	str += " AND Subsector = ";
	str += bufiss;
	str += "))";
	try {
		rst = db.OpenRecordset(str.c_str());
	}
	catch(...) {
		cerr <<"\nproblem while calling countdbrec3()\n";
	}
	numrec = rst.GetField(0L).iVal; 
	rst.Close(); // close the recordset

	return numrec;
}


// counts the number of unique indices in a field from local market
int countdbrecmrk(string fdname1,string fdname2,const char *dbname,const char *dbtname)
{
	int numrec=0; // model period
	string str;

	CdbRecordset rst;

  // determine number of sectors in dataset
	str = "SELECT COUNT(" + fdname1 + ") FROM (SELECT DISTINCT " +
		   fdname1 + "," + fdname2 +" FROM ";
	str += dbtname;
	str += " )";
	try {
		rst = db.OpenRecordset(str.c_str());
	}
	catch(...) {
		cerr <<"\nproblem while calling countdbrecmrk()\n";
	}
	numrec = rst.GetField(0L).iVal; 
	rst.Close(); // close the recordset

	return numrec;
}


// counts the number of countries in a particular market
// identified by market region and good.
int countdbmrkreg(string fdname1,string fdname2,const char *dbname,const char *dbtname)
{
	int numrec=0; // model period
	string str;

	CdbRecordset rst;

  // determine number of sectors in dataset
	str = "SELECT COUNT (RegionName) FROM (SELECT DISTINCT RegionName,";
	str += "Market, SectorName FROM ";
	str += dbtname;
	str = str+" WHERE (Market = '"+fdname1+"' and SectorName = '"+fdname2+"'))";
	try {
		rst = db.OpenRecordset(str.c_str());
	}
	catch(...) {
		cerr <<"\nproblem while calling countdbmrkreg()\n";
	}
	numrec = rst.GetField(0L).iVal; 
	rst.Close(); // close the recordset

	return numrec;
}

