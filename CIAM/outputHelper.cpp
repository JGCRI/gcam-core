/*! 
* \file outputHelper.cpp
* \ingroup CIAM
* \brief Contians the functions to write out single records to output file or table in the database.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"

#ifdef WIN32
#include <afxdisp.h>
#include <dbdao.h>
#include <dbdaoerr.h>
#endif

#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <map>
#include <ctime>
#include "scenario.h" 

using namespace std;

#ifdef WIN32
extern CdbDatabase db;
extern CdbRecordset DBoutrst;
#endif

extern time_t ltime;
extern ofstream outfile;
extern map<string,int> mapreg;
extern map<string,int> regionMap;
extern Scenario scenario;

/*! Output single records to file.
*
* Names of categories, subcategories, and variables are written
* as strings in the argument.  Values are also passed as arguments.
*
*/

void fileoutput3( string var1name,string var2name,string var3name,
			  string var4name,string var5name,string uname,vector<double> dout) {
	outfile <<var1name<<","<<var2name<<","<<var3name<<","
			<<var4name<<","<<var5name<<","<<uname<<",";
	for (int i=0;i<dout.size();i++) {
		outfile << dout[i]<<",";
	}
	outfile <<ctime(&ltime);
}

/*! Output single records MiniCAM style to the database. 
*
* Names of categories, subcategories, and variables are written
* as strings in the argument.  Values are also passed as arguments.
*
*/
void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout)
{
#ifndef WIN32
	fileoutput3( var1name,var2name,var3name,var4name,"",uname,dout);
#else
	int i=0,j;
	// COleVariant does not take int, must be short or long
	// now write to the database
	// MUST Update before adding another
	DBoutrst.AddNew(); // now the current record is this empty new one
	tm* lt = localtime(&ltime); // struct for time components
	// run id MiniCAM style
	long int RunID = lt->tm_sec + lt->tm_min * 100 + lt->tm_hour * 10000
                   + lt->tm_mday * 1000000 
				   + (lt->tm_mon * 100000000 + 100000000); // 0 is january
	DBoutrst.SetField(0L, COleVariant(RunID,VT_I4));
	DBoutrst.SetField(1L, COleVariant(short int(regionMap[var1name])));
	DBoutrst.SetField(3L, COleVariant(var2name.c_str(), VT_BSTRT));
	DBoutrst.SetField(4L, COleVariant(var3name.c_str(), VT_BSTRT));
	DBoutrst.SetField(5L, COleVariant(var4name.c_str(), VT_BSTRT));
	DBoutrst.SetField(6L, COleVariant(uname.c_str(), VT_BSTRT));
	for (i=0;i<dout.size();i++) {
		j = 7+i;
		DBoutrst.SetField(j, COleVariant(dout[i]));
	}
	DBoutrst.Update(); // save and write the record
#endif
}

