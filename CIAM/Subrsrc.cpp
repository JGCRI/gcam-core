/* subrsrc.cpp												*
 * Method definition for subrsrc class						*
 * Coded by Sonny Kim 9/13/00								*/

//** Database Headers *****
#include <afxdisp.h>
#include <dbdao.h>
#include <dbdaoerr.h>
//** Other Headers ********
#include <string>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <math.h>
#include <time.h> // to use clock and time functions
using namespace std; // enables elimination of std::

#include "modeltime.h"
#include "subrsrc.h"

extern CdbDBEngine dben;
extern CdbDatabase db;
extern CdbRecordset drscrst,drscrst_ct;
extern CdbRecordset regidrst,catidrst,subcatidrst,varidrst,subvaridrst;
extern ofstream bugoutfile, outfile;	

extern clock_t start, intermediate, finish, afterdata;
extern bool timestamp;
extern Modeltime modeltime;

int countdbrec3(string fdname,int is,int iss,const char *dbname,const char *dbtname);
int count_tech(string region,string fdname,int is,int iss,const char *dbtname);

// subrsrc class method definition
subrsrc::subrsrc(void) // default constructor
{
}

subrsrc::subrsrc(const char*nstr,int rno)
{
	//name = new char [strlen(nstr)+1];
	strcpy(name,nstr);
	no = rno;
}

subrsrc::~subrsrc(void)
{
}

// return subrsrc index
int subrsrc::index(void)
{
	return no;
}

// return subrsrc name
char* subrsrc::showname(void)
{
	return name;
}


void subrsrc::setlabel(const char *nstr, int rno)
{
	//name = new char [strlen(nstr+1)];
	strcpy(name, nstr);
	no = rno;
}

void subrsrc::setlabel2()
{
	strcpy(name, drscrst.GetField("SubsectorName").pcVal);
	no = drscrst.GetField("Subsector").intVal;
}

void subrsrc::setlabel3(int rno)
{
	no = rno;
}

void subrsrc::initper(void) //set vector size
{
	int maxper = modeltime.getmaxper();
	rscprc.resize(maxper); // subresource price
	available.resize(maxper); // total available resource
	annualprod.resize(maxper); // annual production of subrsrc
	cummprod.resize(maxper); // cummulative production of subrsrc
	gases.resize(maxper); // suite of greenhouse gas
}

void subrsrc::setgrades2(int igrade)
{
	int maxper = modeltime.getmaxper();
	nograde = igrade; // set private member
	depgrade.resize(nograde); // depgrade is 2-dim vector of grade obj
	for (int i=0;i<depgrade.size();i++) depgrade[i].resize(maxper);
}

void subrsrc::dbreadgrade(int treg,int tsec)
{
	char reg[20], sec[20], ssec[20];

	_itoa(treg,reg,10);
	_itoa(tsec,sec,10);
	_itoa(no,ssec,10);

	string str = "Region = ";
	str += reg;
	str += " AND Sector = ";
	str += sec;
	str += " AND Subsector = ";
	str += ssec;
	//drscrst.FindFirst(str.c_str()); // go to first record for region and sector

	no = drscrst.GetField("Subsector").intVal;
	strcpy(name,drscrst.GetField("SubsectorName").pcVal);

	// create an array of daorsetbing struct
	// table binding: (assign the field lengths and types)
	DAORSETBINDING	Bindings[] = 
	{
	//Index Type    Fld	Type	  Offset			 Size
	{dbBindIndexINT, 7, dbBindI2, offsetof(rsctech,rno), sizeof(long)},
	{dbBindIndexINT, 8, dbBindLPSTRING, offsetof(rsctech,rname), sizeof(TCHAR *)},
	{dbBindIndexINT, 9, dbBindI2, offsetof(rsctech,yr), sizeof(long)},
	{dbBindIndexINT, 10, dbBindR8, offsetof(rsctech,tavailable), sizeof(double)},
	{dbBindIndexINT, 11, dbBindR8, offsetof(rsctech,textcost), sizeof(double)},
	{dbBindIndexINT, 12, dbBindR8, offsetof(rsctech,tenvcost), sizeof(double)},
	{dbBindIndexINT, 13, dbBindR8, offsetof(rsctech,ttax), sizeof(double)},
	{dbBindIndexINT, 14, dbBindR8, offsetof(rsctech,ttechch), sizeof(double)},
	};

	// run C++ GetRowsEx 
	// number of rows is the number of grades times periods 
	int maxper = modeltime.getmaxper();
	int maxdataper = modeltime.getmaxdataper();
	int	maxrecords = nograde*maxdataper; 
	lprsctech prsctechRows = new rsctech[maxrecords];
	LONG lNumRecords, lYear;
	// cannot use maxrecords because not const int
	TCHAR pBuf[100 * 20]; // buffer for use with variable length text fields only
	
	// get yr rows for each technology
	lNumRecords = drscrst.GetRowsEx(prsctechRows, sizeof(rsctech),
		  &Bindings[0], sizeof(Bindings) / sizeof(DAORSETBINDING),
		  pBuf, sizeof(pBuf), maxrecords); 

	// initialize total resource available
	available[0] = 0;

	int k=0;
	for (int i=0;i<nograde;i++) {
		// Step through the returned rows and assign to technology
		for (lYear = 0; lYear < maxdataper; lYear++) {
			// find model period from data year
			int modelper = modeltime.getyr_to_per(prsctechRows[lYear].yr);
  			// prscgradeRows is a variant
			depgrade[i][modelper].setall(&prsctechRows[(i*maxdataper)+lYear]); 
			// fill periods not read in from data with previous per data
			for (int j=k; j<modelper; j++) {
				depgrade[i][j] = depgrade[i][j-1];
			}
			k = modelper+1; // initialize for next time
		}
	
		// initialize for periods greater than last data period
		// last model period for last data per
		int m1=modeltime.getdata_to_mod(maxdataper-1);
		if(modeltime.getendyr() > modeltime.getper_to_yr(m1)) {
			for (int j=m1+1;j<modeltime.getmaxper();j++)
				depgrade[i][j] = depgrade[i][j-1];
		}

		// set base year extraction cost to rest of the periods
		for (lYear = 1; lYear < maxper; lYear++) {
			depgrade[i][lYear].setextcost(depgrade[i][0].getextcost());
		}
		// add total available in each grade
		available[0] += depgrade[i][0].showavail();
	}
	// row is last record so move to next row
	// mainly done to get to EOF when last region resource is read in
	drscrst.MoveNext(); 
	
	for (lYear = 0; lYear < maxper; lYear++) {
		gases[lYear].setcoefs(); // initializes emissions coefficients
	}
	delete [] prsctechRows; // free memory
}

void subrsrc::dbreadgen(char* region,const char *nstr,const char *dbtname)
{
	// function protocol
	void dbmodelread(double *temp,string region,string var1name,string var2name);
	
	int i=0;
	double tmpval[1];

	// reads in data for resources
	dbmodelread(tmpval,region,"resource",nstr);
	annualprod[0] = tmpval[0];
	// reads in data for minimum annual production of resource
	dbmodelread(tmpval,region,"min_annualprod",nstr);
	min_annualprod = tmpval[0];
	
}

double subrsrc::price(int per)
{
	int i=0;
	rscprc[per] = 0.0;

	return rscprc[per] ;
}

int subrsrc::maxgrade(void) // returns total number of grades
{
	return nograde;
}

void subrsrc::cummsupply(double prc,int per)
{	
	int i=0,maxgrd;
	double slope=0;
	cummprod[per]=0.0;
	
	rscprc[per] = prc;
	// the index of the last grade is number of grades minus one
	// don't forget 0 as first
	maxgrd = nograde-1;

	// calculate total extraction cost for each grade
	for (int gr=0; gr<nograde; gr++) {
		depgrade[gr][per].cost(per);
	}

	if (per == 0) cummprod[per] = 0.0;
	else {
	// Case 1
	// if market price is less than cost of first grade, then zero cummulative 
	// production
	if (prc <= depgrade[0][per].getcost()) 
		cummprod[per] = cummprod[per-1];

	// Case 2
	// if market price is in between cost of first and last grade, then calculate 
	// cummulative production in between those grades
	if (prc > depgrade[0][per].getcost() && prc <= depgrade[maxgrd][per].getcost()) {
		int iL=0,iU=0;
		while (depgrade[i][per].getcost() < prc) {
			iL=i; i++; iU=i;
		}
		// add subrsrcs up to the lower grade
		for (i=0;i<=iL;i++) cummprod[per]+=depgrade[i][0].showavail();
		// price must reach upper grade cost to produce all of lower grade
		slope = depgrade[iL][0].showavail()
			  / (depgrade[iU][per].getcost() - depgrade[iL][per].getcost());
		cummprod[per] -= slope * (depgrade[iU][per].getcost() - prc);
	}

	// Case 3
	// if market price greater than the cost of the last grade, then
	// cummulative production is the amount in all grades
	if (prc > depgrade[maxgrd][per].getcost())
		for (i=0;i<nograde;i++) cummprod[per]+=depgrade[i][0].showavail();
	}
	//available[per]=available[0]-cummprod[per];
}

double subrsrc::showcummprod(int per)
{
	return cummprod[per];
}

void subrsrc::annualsupply(int per,double gnp1,double gnp2,double price1,double price2)
{	
	// for per = 0 use initial annual supply
	// cummulative production is 0 for per = 0
	if (per >= 1) {
		// 2 is for the average of the annual productions
		annualprod[per] = 2.0 * (cummprod[per] - cummprod[per-1])/modeltime.gettimestep(per)
			            - annualprod[per-1];
		if(annualprod[per] <= 0) {
			cummprod[per] = cummprod[per-1];
			annualprod[per] = 0.0;
		} 
		// incorporate short-term capacity limits after 1990
		if (per >= 2) {
			// min_annualprod is defined in object and read in from database
			double prc_elas = 1; //read in value set to 1
			double cur_annualprod = 0;
			double max_annualprod = annualprod[per-1]*gnp1/gnp2;
			if(min_annualprod < max_annualprod) cur_annualprod = max_annualprod;
			else cur_annualprod = min_annualprod;
			cur_annualprod*=pow((price1/price2),prc_elas);
			if(cur_annualprod < annualprod[per]) {
				cummprod[per] = cummprod[per-1]+(cur_annualprod+annualprod[per-1])
				            *modeltime.gettimestep(per)/2.0;
				annualprod[per] = cur_annualprod;
			}
		}
		// available is the total resource (stock)
		//available[per]=available[per-1]-(annualprod[per]*modeltime.gettimestep(per)/2);
		available[per]=available[per-1]-((annualprod[per]+annualprod[per-1])
					  /2*modeltime.gettimestep(per));
		if (available[per]<=0) available[per] = 0;
	}

}

double subrsrc::showannualprod(int per)
{
	return annualprod[per];
}

// show available resource for period
double subrsrc::showavailable(int per)
{
	return available[per];
}

// calculate GHG emissions from annual production of subresource
double subrsrc::emission(int per)
{
	gases[per].calc_allgases(annualprod[per], name);	
	return gases[per].showCO2();
}
/*
void subrsrc::show(void)
{
	int i=0;
	//write to file or database later
	cout << no <<"  " << name<<"\n";
	cout << "Number of Grades: " << nograde <<"\n";
	for (i=0;i<nograde;i++)
		cout<<"Annual Production: "<<annualprod(per)<<"\n";
}

*/

// write subrsrc output to database
void subrsrc::outputdb(const char *regname,int reg,const char *sname)
{
	// function protocol
	void dboutput2(string varreg,string var1name,string var2name,string var3name,
			  string var4name,vector<double> dout,string uname);
	
	int i=0, m=0;
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	
	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	// total subsector output
	dboutput2(regname,sname,name,"all grades","production",annualprod,"EJ");
	dboutput2(regname,sname,name,"all grades","available",available,"EJ");

	for (m=0;m<maxper;m++)
		temp[m] = gases[m].showCO2();
	dboutput2(regname,sname,name,"all grades","CO2 emissions",temp,"MTC");

/*	// do for all grades in the sector
	for (i=0;i<nograde;i++) {
		// output or demand for each grade
		for (m=0;m<maxper;m++)
			temp[m] = depgrade[i][m].showavail();
		dboutput2(regname,sname,name,"supply",depgrade[i][0].showname(),temp,"EJ");
		// grade cost
		for (m=0;m<maxper;m++)
			temp[m] = depgrade[i][m].getcost();
		dboutput2(regname,sname,name,"cost",depgrade[i][0].showname(),temp,"$/GJ");
		// grade efficiency
		for (m=0;m<maxper;m++)
			temp[m] = depgrade[i][m].getextcost();
		dboutput2(regname,sname,name,"ext cost",depgrade[i][0].showname(),temp,"$/GJ");
		// grade environmental cost
		for (m=0;m<maxper;m++)
			temp[m] = depgrade[i][m].getenvcost();
		dboutput2(regname,sname,name,"env cost",depgrade[i][0].showname(),temp,"$/GJ");
	}
  */
}

// write subrsrc output to database
void subrsrc::MCoutput(const char *regname,int reg,const char *sname)
{
	// function protocol
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout);
	
	int i=0, m=0;
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	
	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	// total subsector output
	dboutput4(regname,"Pri Energy Production",sname,name,"EJ",annualprod);
	dboutput4(regname,"Resource",sname,name,"EJ",available);

	for (m=0;m<maxper;m++)
		temp[m] = gases[m].showCO2();
	dboutput4(regname,"CO2 Emiss",sname,name,"MTC",temp);
	dboutput4(regname,"Price",sname,name,"$/GJ",rscprc);

	string tssname = name; // tempory subsector name
	string str; // tempory string

	// do for all grades in the sector
	for (i=0;i<nograde;i++) {
		str = tssname + " " + depgrade[i][0].showname();
		//str = depgrade[i][0].showname();
		// output or demand for each grade
		for (m=0;m<maxper;m++)
			temp[m] = depgrade[i][m].showavail();
		dboutput4(regname,"Resource",sname,str,"EJ",temp);
		// grade cost
		for (m=0;m<maxper;m++)
			temp[m] = depgrade[i][m].getcost();
		dboutput4(regname,"Price",sname,str,"$/GJ",temp);
		// grade extraction cost
		for (m=0;m<maxper;m++)
			temp[m] = depgrade[i][m].getextcost();
		dboutput4(regname,"Price ExtCost",sname,str,"$/GJ",temp);
		// grade environmental cost
		for (m=0;m<maxper;m++)
			temp[m] = depgrade[i][m].getenvcost();
		dboutput4(regname,"Price EnvCost",sname,str,"$/GJ",temp);
	}
}

// write subrsrc output to file
void subrsrc::outputfile(const char *regname,int reg,const char *sname)
{
	// function protocol
	void fileoutput3(int regno,string var1name,string var2name,string var3name,
				  string var4name,string var5name,string uname,vector<double> dout);
	
	int i=0, m=0;
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	
	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	// total subsector output
	fileoutput3(reg,regname,sname,name," ","production","EJ",annualprod);
	fileoutput3(reg,regname,sname,name," ","resource","EJ",available);

	for (m=0;m<maxper;m++)
		temp[m] = gases[m].showCO2();
	fileoutput3(reg,regname,sname,name," ","CO2 emiss","MTC",temp);

/*	// do for all grades in the sector
	for (i=0;i<nograde;i++) {
		// output or demand for each grade
		for (m=0;m<maxper;m++)
			temp[m] = depgrade[i][m].showavail();
		fileoutput2(reg,regname,sname,name,"supply",depgrade[i][0].showname(),temp,"EJ");
		// grade cost
		for (m=0;m<maxper;m++)
			temp[m] = depgrade[i][m].getcost();
		fileoutput2(reg,regname,sname,name,"cost",depgrade[i][0].showname(),temp,"$/GJ");
		// grade efficiency
		for (m=0;m<maxper;m++)
			temp[m] = depgrade[i][m].getextcost();
		fileoutput2(reg,regname,sname,name,"ext cost",depgrade[i][0].showname(),temp,"$/GJ");
		// grade environmental cost
		for (m=0;m<maxper;m++)
			temp[m] = depgrade[i][m].getenvcost();
		fileoutput2(reg,regname,sname,name,"env cost",depgrade[i][0].showname(),temp,"$/GJ");
	}
*/
}

