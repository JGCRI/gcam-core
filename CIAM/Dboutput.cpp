/* dboutput.cpp													*
 * Function to write out single records to the output table in 	*
 * database.  Specify variable name to be written as string in the argument	*	
 * Arguments: string variable name, dbname,dbtable						*
 * Return: Nothing										*
 * Coded by Sonny Kim 9/6/00										*/

#include <afxdisp.h>
#include <dbdao.h>
#include <dbdaoerr.h>
#include <string>
#include <vector>
#include <iostream>
#include <fstream>
#include <map>
#include <time.h> // to use clock and time functions
#include "scenario.h" 
using namespace std; // enables elimination of std::

extern CdbDatabase db;
extern CdbRecordset outrst,DBoutrst;
extern time_t ltime; // global time variable set in main
extern ofstream outfile,outfile2,dbout;
extern map<string,int> mapreg,mapsec,mapsubsec,maptech,mapvar,mapVarID;
extern Scenario scenario; // model scenario info

void dboutput2(string varreg,string var1name,string var2name,string var3name,
			  string var4name,vector<double> dout,string uname)
{
	// outputs to database
	int i=0,j;
	// COleVariant does not take int, must be short or long
	// now write to the database
	// MUST Update before adding another
	outrst.AddNew(); // now the current record is this empty new one
	outrst.SetField(0L, COleVariant(short int(scenario.getid())));
	outrst.SetField(1L, COleVariant(short int(mapreg[varreg])));
	outrst.SetField(2L, COleVariant(short int(mapsec[var1name])));
	outrst.SetField(3L, COleVariant(short int(mapsubsec[var2name])));
	outrst.SetField(4L, COleVariant(short int(maptech[var3name])));
	outrst.SetField(5L, COleVariant(short int(mapvar[var4name])));
	for (i=0;i<dout.size();i++) {
		j = 6+i;
		outrst.SetField(j, COleVariant(dout[i]));
	}
	outrst.SetField(++j, COleVariant(uname.c_str(), VT_BSTRT));
	outrst.SetField(++j, COleVariant(ctime(&ltime), VT_BSTRT));
	outrst.Update(); // save and write the record
	// write to text file
	outfile2 <<varreg<<","<<var1name<<","<<var2name<<","<<var3name<<","
			<<var4name<<",";
	for (i=0;i<dout.size();i++) {
		outfile2 << dout[i]<<",";
	}
	outfile2 <<uname<<","<<ctime(&ltime);
}

// outputs to file
void fileoutput2(int regno,string var1name,string var2name,string var3name,
			  string var4name,string var5name,vector<double> dout,string uname)
{
	outfile <<regno<<","<<var1name<<","<<var2name<<","<<var3name<<","
			<<var4name<<","<<var5name<<",";
	for (int i=0;i<dout.size();i++) {
		outfile << dout[i]<<",";
	}
	outfile <<uname<<","<<ctime(&ltime);
}

// outputs to file
void fileoutput3(int regno,string var1name,string var2name,string var3name,
			  string var4name,string var5name,string uname,vector<double> dout)
{
	outfile <<regno<<","<<var1name<<","<<var2name<<","<<var3name<<","
			<<var4name<<","<<var5name<<","<<uname<<",";
	for (int i=0;i<dout.size();i++) {
		outfile << dout[i]<<",";
	}
	outfile <<ctime(&ltime);
}

// MiniCAM style outputs to file
void fileoutput4(string var1name,string var2name,string var3name,string var4name,
			     vector<double> dout)
{
	tm* lt = localtime(&ltime); // struct for time components
	// run id MiniCAM style
	long int RunID = lt->tm_sec + lt->tm_min * 100 + lt->tm_hour * 10000
                   + lt->tm_mday * 1000000 
				   + (lt->tm_mon * 100000000 + 100000000); // 0 is january
	string str = var2name+var3name+var4name;
	//dbout <<RunID<<","<<mapreg[var1name]<<","<<mapVarID[str]<<",";
	dbout <<RunID<<","<<mapreg[var1name]<<","<<var2name<<","
	      <<var3name<<","<<var4name<<",";
	for (int i=1;i<dout.size();i++) {
		dbout << dout[i]<<",";
	}
	dbout <<"\n";  // skip line for each record
}

// MiniCAM style outputs to databasae
void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout)
{
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
	DBoutrst.SetField(1L, COleVariant(short int(mapreg[var1name])));
	DBoutrst.SetField(3L, COleVariant(var2name.c_str(), VT_BSTRT));
	DBoutrst.SetField(4L, COleVariant(var3name.c_str(), VT_BSTRT));
	DBoutrst.SetField(5L, COleVariant(var4name.c_str(), VT_BSTRT));
	DBoutrst.SetField(6L, COleVariant(uname.c_str(), VT_BSTRT));
	for (i=0;i<dout.size();i++) {
		j = 7+i;
		DBoutrst.SetField(j, COleVariant(dout[i]));
	}
	DBoutrst.Update(); // save and write the record
}

