/* scenario.cpp										*
 * This file contains the methods definition for the 	*
 * the scenario class.								*
 * SHK  3/12/02										*/

#include <stdlib.h>
#include <string>
#include <afxdisp.h>
#include <dbdao.h>
#include <dbdaoerr.h>
#include "scenario.h"
using namespace std; // enables elimination of std::

extern CdbDatabase db;
CdbRecordset scenidrst;

// scenario class methods

// default construtor
Scenario::Scenario(void)
{
}

// default destrutor
Scenario::~Scenario(void)
{
}

// sets all scenario parameters
void Scenario::setall(void)
{
	// function protocol
	void dbmodelread(double *temp,string region,string var1name,string var2name);
	char* dbmodreadnote(string region,string var1name,string var2name);
	double tmpval[1];
	// reads in scenario data
	dbmodelread(tmpval,"model","scenario","scenario");
	id = int(tmpval[0]);
	const char* tname = dbmodreadnote("model","scenario","scenario");
	strcpy(name,tname);

	scenidrst = db.OpenRecordset("idscenario",dbOpenDynaset);
	scenidrst.AddNew(); // now the current record is this empty new one
	scenidrst.SetField(0L, COleVariant(short int(id)));
	scenidrst.SetField(1L, COleVariant(name, VT_BSTRT));
	scenidrst.Update(); // save and write the record
	scenidrst.Close();		// scenario id recordset
}

// ****** return name and id ******
// return scenario name
char* Scenario::getname(void)
{
	return name; 
}

// return fuel number
int Scenario::getid(void)
{
	return id; 
}
