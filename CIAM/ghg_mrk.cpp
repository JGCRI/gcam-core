/* ghg_ss.cpp										*
 * This header contains the methods for the		*
 * Greenhouse Gas class.						*
 *       										*
 * SHK  12/11/00								*/


//** Database Headers *****
#include <afxdisp.h>
#include <dbdao.h>
#include <dbdaoerr.h>
//** Other Headers ********
#include "ghg_mrk.h"
#include <iostream>
#include <string>
#include "modeltime.h" // model start, end, timestep and period info
using namespace std; // enables elimination of std::

extern CdbRecordset ghgrst;
extern Modeltime modeltime;

ghg_mrk::ghg_mrk(void) //default construtor
{
}
	
ghg_mrk::~ghg_mrk(void) // destructor
{
}

// resize vectors to max period
void ghg_mrk::initper(void)
{
	constraint.resize(modeltime.getmaxper()); // emissions constraint (tgC or MTC)
	emission.resize(modeltime.getmaxper()); // emissions (tgC or MTC)
}
// set emissions constraint from database
void ghg_mrk::setconstraint(int reg,int var1)
{
	char no1[10], no2[10];
	string str;

	_itoa(reg,no1,10);
	_itoa(var1,no2,10);

	// searching global genrst recordset for matching record
	str = "Region = ";
	str = str + no1 + " AND GHG = " + no2;
	try {
		ghgrst.FindFirst(str.c_str());
	}
	catch(...) {
		cout<<"\nError while calling setconstraint("<<reg<<","<<var1<<"\n";
		exit(1);
	}
	// assign values from recordset to constraint variable
	strcpy(name,ghgrst.GetField("GHGName").pcVal);
	no = var1;
	int k=0;
	for (int i=0;i<modeltime.getmaxdataper();i++) {
		int m = modeltime.getdata_to_mod(i); 
		constraint[m] = ghgrst.GetField(i+7).dblVal;
		int offset = modeltime.getdataoffset(i);
		// interpolate in between data periods
		for (int j=k; j<m; j++) {
			constraint[j] = constraint[k-1] + (constraint[m] - constraint[k-1])/
				          offset*(j-k+1);
		}
		k = m+1; // initialize for next time
	}
}

// set emissions
void ghg_mrk::setemission(double amount,int per)
{
	emission[per] = amount; // emissions (tgC or MTC)
}

// show emissions number
int ghg_mrk::showghgno(void)
{
	return no;
}

// show emission name
char* ghg_mrk::showname(void)
{
	return name;
}

// return emissions target
double ghg_mrk::showconstraint(int per)
{
	return constraint[per]; // emissions constraint (tgC or MTC)
}

// return emissions
double ghg_mrk::showemission(int per)
{
	return emission[per]; // emissions (tgC or MTC)

}

