/* dbgeneral.cpp														*
 * Function to read in single records from the general table in the		*
 * database.  Specify variable to be read in as string in the argument	*	
 * Arguments: string variable name1,string variable name1,dbname,dbtable*
 * Returns: array of doubles											*
 * Coded by Sonny Kim 8/31/00											*/

#include <string>
#include <iostream>
#include <afxdisp.h>
#include <dbdao.h>
#include <dbdaoerr.h>
#include <vector>

using namespace std; // enables elimination of std::

extern CdbDatabase db;
extern CdbRecordset modelrst,genrst,demogrst;

// function to read GEN database table
// the original temp variable in the argument gets changed
void dbgenread(double *temp,string region,string var1name,string var2name,int maxper)
{
	string str;

	// searching global genrst recordset for matching record
	str = "RegionName = '"+region+ "' AND Variable = '" + var1name
		 + "' AND Subvar = '" + var2name +"'";
	//genrst.MoveFirst();  // go to the first record
	try {
		genrst.FindFirst(str.c_str());
	}
	catch(...) {
		cout<<"\nError while calling dbgenread("<<region<<","<<var1name<<","
			<<var2name<<")\n";
		exit(1);
	}
	// assign values from recordset to temp variable
	for (int i=0;i<maxper;i++)
		temp[i] = genrst.GetField(i+4).dblVal;
}

// function to read GEN database table
// the original temp variable in the argument gets changed
void dbgenread2(vector<double>& temp,string region,string var1name,string var2name,int maxper)
{
	string str;

	// searching global genrst recordset for matching record
	str = "RegionName = '"+region+ "' AND Variable = '" + var1name
		 + "' AND Subvar = '" + var2name +"'";
	//genrst.MoveFirst();  // go to the first record
	try {
		genrst.FindFirst(str.c_str());
	}
	catch(...) {
		cout<<"\nError while calling dbgenread("<<region<<","<<var1name<<","
			<<var2name<<")\n";
		exit(1);
	}
	// assign values from recordset to temp variable
	for (int i=0;i<maxper;i++)
		temp[i] = genrst.GetField(i+4).dblVal;
}

// function to read MODEL database table
// the original temp variable in the argument gets changed
void dbmodelread(double* temp,string region,string var1name,string var2name)
{
	// reads in only one field
	string str;
	// searching global genrst recordset for matching record
	str = "RegionName = '"+region+ "' AND Variable = '" + var1name
		 + "' AND Subvar = '" + var2name +"'";
	//genrst.MoveFirst();  // go to the first record
	try {
		modelrst.FindFirst(str.c_str());
	}
	catch(...) {
		cout<<"\nError while calling dbmodelread("<<region<<","<<var1name<<","
			<<var2name<<")\n";
		exit(1);
	}
	// assign values from recordset to temp variable
	temp[0] = modelrst.GetField(4).dblVal;
}

// function to read MODEL database table
// the original temp variable in the argument gets changed
char* dbmodreadnote(string region,string var1name,string var2name)
{
	// reads in only one field
	string str;
	// searching global genrst recordset for matching record
	str = "RegionName = '"+region+ "' AND Variable = '" + var1name
		 + "' AND Subvar = '" + var2name +"'";
	//genrst.MoveFirst();  // go to the first record
	try {
		modelrst.FindFirst(str.c_str());
	}
	catch(...) {
		cout<<"\nError while calling dbmodreadnote("<<region<<","<<var1name<<","
			<<var2name<<")\n";
		exit(1);
	}
	// assign values from recordset to temp variable
	return modelrst.GetField(5).pcVal;
}

// function to read DEMOGRAPHICS database table
// the original temp variable in the argument gets changed
void dbdemogread(vector<double>& temp,string region,string var1name,string var2name,int maxper)
{
	string str;

	// searching global genrst recordset for matching record
	str = "RegionName = '"+region+ "' AND Variable = '" + var1name
		 + "' AND Subvar = '" + var2name +"'";
	//genrst.MoveFirst();  // go to the first record
	try {
		demogrst.FindFirst(str.c_str());
	}
	catch(...) {
		cout<<"\nError while calling dbdemogread("<<region<<","<<var1name<<","
			<<var2name<<")\n";
		exit(1);
	}
	// assign values from recordset to temp variable
	for (int i=0;i<maxper;i++)
		temp[i] = demogrst.GetField(i+4).dblVal;
}
