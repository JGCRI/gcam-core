/* demographic.cpp											*
 * Method definition for demographic class					*
 * Coded by Sonny Kim 7/29/00								*/

#include <afxdisp.h>
#include <dbdao.h>
#include <dbdaoerr.h>
#include <iostream>
#include <fstream>
#include <string>
using namespace std; // enables elimination of std::

#include "demographic.h" // generic demographic class
#include "modeltime.h" // model start, end, timestep and period info

extern Modeltime modeltime;

// Per incremented by 1 because population reads in additional period, 1960.

// demographic class method definition
demographic::demographic(void) // default constructor
{
}

demographic::~demographic(void) // destructor
{
}

// set size of population and labor productivity variables to max period
void demographic::set(void)
{
	// pop has one more historical period
	int popmaxper = modeltime.getmaxper()+modeltime.getdataoffset(0); 

	malepop.resize(popmaxper); 
	femalepop.resize(popmaxper);
	totalpop.resize(popmaxper);
	laborprod.resize(popmaxper);	
	laborforce_p.resize(popmaxper);	
	laborforce.resize(popmaxper);	
}
	// initialize method definition
// reads in total population and labor productivity for all periods
void demographic::initialize(char* region)
{
	int i=0,j=0,k=0,m=0;
	int step;
	// function protocol
	void dbgenread(double *temp,string region,string var1name,string var2name,int maxper);
	void dbdemogread(vector<double>& temp,string region,string var1name,string var2name,int maxper);

	// one more historical data for population
	int dper = modeltime.getmaxpopdata();
	vector<double> dtemp(dper);
	// reads in data for population
	dbdemogread(dtemp,region,"population","total",dper);
	for (i=0; i<dper; i++) {
		// find model period
		if(i==0) step = modeltime.getdataoffset(0);
		else step = modeltime.getdataoffset(i-1);
		int test = modeltime.gettimestep(i);
		m = modeltime.getpopdata_popvar(i);
		totalpop[m] = dtemp[i]; // assign temp to total population
		// linearly interporate for timesteps that do not coincide with data
		for (j=k; j<m; j++) {
			totalpop[j] = totalpop[k-1] + (totalpop[m] - totalpop[k-1])/
				          step*(j-k+1);
		}
		k = m+1; // initialize for next time
	}

	// reads in data for labor productivity
	dbdemogread(dtemp,region,"laborproductivity","growthrate",dper);
	k=0; m=0;
	for (i=0; i<dper; i++) {
		// find model period
		if(i==0) step = modeltime.getdataoffset(0);
		else step = modeltime.getdataoffset(i-1);
		m = modeltime.getpopdata_popvar(i);
		laborprod[m] = dtemp[i]; // assign temp to labor productivity
		// linearly interporate for timesteps that do not coincide with data
		for (j=k; j<m; j++) {
			laborprod[j] = laborprod[k-1] + (laborprod[m] - laborprod[k-1])/
				           step*(j-k+1);
		}
		k = m+1; // initialize for next time
	}

	// reads in data for labor force participation
	dbdemogread(dtemp,region,"laborforce","percent",dper);
	k=0; m=0;
	for (i=0; i<dper; i++) {
		// find model period
		if(i==0) step = modeltime.getdataoffset(0);
		else step = modeltime.getdataoffset(i-1);
		m = modeltime.getpopdata_popvar(i);
		laborforce_p[m] = dtemp[i]; // assign temp to labor force 
		// linearly interporate for timesteps that do not coincide with data
		for (j=k; j<m; j++) {
			laborforce_p[j] = laborforce_p[k-1] + (laborforce_p[m] - laborforce_p[k-1])/
				           step*(j-k+1);
		}
		k = m+1; // initialize for next time
	}

	// initialize for periods greater than last data period
	// get model period of last data period
	dper = modeltime.getmaxdataper();
	int m1=modeltime.getdata_to_mod(dper-1); 
	if(modeltime.getendyr() > modeltime.getdataendyr()) {
		int m2=modeltime.getdataoffset(0);
		for (int j=m1+m2+1;j<modeltime.getmaxper()+m2;j++) {
			totalpop[j] = totalpop[j-1];
			laborprod[j] = laborprod[j-1];
			laborforce_p[j] = laborforce_p[j-1];
		}
	}

	// calculate actual labor force for all periods
	for (i=0;i<totalpop.size();i++) {
		laborforce[i] = totalpop[i]*laborforce_p[i];
	}
}

// return labor productivity
double demographic::labor(int per)
{
	return laborprod[modeltime.getmod_to_pop(per)];
}

// return total population
double demographic::total(int per)
{
	return totalpop[modeltime.getmod_to_pop(per)];
}

// return labor force (actual working)
double demographic::getlaborforce(int per)
{
	return laborforce[modeltime.getmod_to_pop(per)];
}

// show demographic information to screen
void demographic::show(int per) 
{
	int m = modeltime.getmod_to_pop(per);
	cout << "Male Population: " << malepop[m] << "\n";
	cout << "Female Population: " << femalepop[m] << "\n";
	cout << "Total Population: " << totalpop[m] << "\n";
}

// define method for outputing population info to database
void demographic::outputdb(const char *regname,int reg)
{
	int i=0;
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);

	// function protocol
	void dboutput2(string varreg,string var1name,string var2name,string var3name,
			  string var4name,vector<double> dout,string uname);
	
	// write population to temporary array since not all will be sent to output
	for (i=0;i<maxper;i++)
		temp[i] = totalpop[modeltime.getmod_to_pop(i)];
	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	dboutput2(regname,"demographics"," "," ","population",temp,"1000s");

	// labor productivity
	for (i=0;i<maxper;i++)
		temp[i] = laborprod[modeltime.getmod_to_pop(i)];
	dboutput2(regname,"economics","labor"," ","labor productivity",temp,"GR");	
}

// outputing population info to file
void demographic::outputfile(const char *regname,int reg)
{
	int i=0;
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);

	// function protocol
	void fileoutput3(int regno,string var1name,string var2name,string var3name,
				  string var4name,string var5name,string uname,vector<double> dout);
	
	// write population to temporary array since not all will be sent to output
	for (i=0;i<maxper;i++)
		temp[i] = totalpop[modeltime.getmod_to_pop(i)];
	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	fileoutput3(reg,regname," "," "," ","population","1000s",temp);

	// labor productivity
	for (i=0;i<maxper;i++)
		temp[i] = laborprod[modeltime.getmod_to_pop(i)];
	fileoutput3(reg,regname," "," "," ","labor prod","%/yr",temp);	
}

// MiniCAM output to file
void demographic::MCoutput(const char *regname,int reg)
{
	int i=0;
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);

	// function protocol
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout);
	
	// write population to temporary array since not all will be sent to output
	for (i=0;i<maxper;i++)
		temp[i] = totalpop[modeltime.getmod_to_pop(i)];
	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	dboutput4(regname,"General","Population","Total","thous",temp);

	// labor productivity
	for (i=0;i<maxper;i++)
		temp[i] = laborprod[modeltime.getmod_to_pop(i)];
	dboutput4(regname,"General","Labor Prod","GrowthRate","per yr",temp);	
}
