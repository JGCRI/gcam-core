/* resource.cpp												*
 * Method definition for resource class						*
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
#include <time.h> // to use clock and time functions
#include <vector>
using namespace std; // enables elimination of std::

#include "resource.h"
#include "str_indexname.h" // get index and name from database
#include "modeltime.h"

extern const char *dbfile;
extern const char *dbtdrsc;
extern const char *dbtgen;
extern const char *dbtout;
extern CdbRecordset drscrst,drscrst_ct;
extern CdbRecordset regidrst,catidrst,subcatidrst,varidrst,subvaridrst;
extern ofstream bugoutfile,outfile;	

extern CdbDBEngine dben;
extern CdbDatabase db;
extern clock_t start, intermediate, finish, afterdata;
extern bool timestamp;
extern Modeltime modeltime;

// function protocols
int countdbrec2(string fdname,int is,const char *dbname,const char *dbtname);
int count_subsec(string region,string fdname,int is,const char *dbtname);
void label_subsec(str_indexname* str_temp,string region,string index,string name,
					  int is,int ns,const char *dbtname);

// resource class method definition
resource::resource(void) // default constructor
{
}

resource::resource(const char* nstr,int rno)
{
	//name = new char [strlen(nstr)+1];
	strcpy(name,nstr);
	no = rno;
}

resource::~resource(void)
{
}

void resource::setlabel(const char *nstr, int rno)
{
	//name = new char [strlen(nstr+1)];
	strcpy(name, nstr);
	no = rno;
}

void resource::setlabel2(void)
{
	strcpy(name, drscrst.GetField("SectorName").pcVal);
	no = drscrst.GetField("Sector").intVal;
}

void resource::setlabel3(int rno)
{
	no = rno;
}

void resource::initper(void)
{
	int maxper = modeltime.getmaxper();
	rscprc.resize(maxper); // resource price
	available.resize(maxper); // total resource availabl
	annualprod.resize(maxper); // annual production rate of resource
	cummprod.resize(maxper); // cummulative production of resource
	ghgs.resize(maxper); // structure containing ghg emissions
}

// set number of subsecsectors for each depletable resource
void resource::setdepsubrsrc(int iss,int* tgrade) 
{

	nosubrsrc = iss; 
	depsubrsrc.resize(nosubrsrc);

	for (int i=0;i<nosubrsrc;i++) {
		depsubrsrc[i].initper(); // counts number of grades and creates objects
		int ig = *(tgrade+i); // gives number of grades in subresource
		depsubrsrc[i].setgrades2(ig); // counts number of grades and creates objects
	}	
}

// initializes subsector information from database
void resource::initialize(char* region,int tregno)
{
	int i=0;
	no = drscrst.GetField("Sector").intVal;
	strcpy(name,drscrst.GetField("SectorName").pcVal);

	for (i=0;i<nosubrsrc;i++) {
		depsubrsrc[i].dbreadgrade(tregno,no); // reads grade data
		depsubrsrc[i].dbreadgen(region,depsubrsrc[i].showname(),dbtgen);
	}
}

// return resource index
int resource::index(void)
{
	return no;
}

// return resource name
char * resource::showname(void)
{
	return name;
}

double resource::price(int per)
{
	int i=0;
	rscprc[per] = 0.0;

	return rscprc[per] ;
}

int resource::shownosubrsrc(void) // returns total number of subsectors
{
	return nosubrsrc;
}

void resource::cummsupply(double prc,int per)
{	
	int i=0;
	cummprod[per]=0.0;

	rscprc[per] = prc;
	// sum cummulative production of each subsector
	for (i=0;i<nosubrsrc;i++) {
		depsubrsrc[i].cummsupply(prc,per);
		cummprod[per] += depsubrsrc[i].showcummprod(per);
	}
}

double resource::showcummprod(int per)
{
	return cummprod[per];
}

// calculates annual production
void resource::annualsupply(int per,double gnp1,double gnp2,double price1,double price2)
{	
	int i=0;
	annualprod[per]=0.0;
	available[per]=0.0;

	// sum annual production of each subsector
	for (i=0;i<nosubrsrc;i++) {
		depsubrsrc[i].annualsupply(per,gnp1,gnp2,price1,price2);
		annualprod[per] += depsubrsrc[i].showannualprod(per);
		available[per] += depsubrsrc[i].showavailable(per);
	}
}

// returns annual production of resources
double resource::showannualprod(int per)
{
	return annualprod[per];
}

// returns resource available from all subsectors
double resource::showavailable(int per)
{
	return available[per];
}

// returns resource available from each subsectors
double resource::showsubavail(int subrscno,int per)
{
	for (int i=0;i<nosubrsrc;i++) {
		if (depsubrsrc[i].index() == subrscno)
			return depsubrsrc[i].showavailable(per);
	}
	return 0;
}

void resource::show(void)
{
	int i=0;
	//write to file or database later
	cout << no <<"  " << name<<"\n";
	cout << "Number of Subsectors: " << nosubrsrc <<"\n";
	for (i=0;i<nosubrsrc;i++)
		cout<<depsubrsrc[i].showname()<<"\n";
}

// calculate GHG emissions for each resource from subresources
void resource::emission(int per)
{
	// emission function calculate and returns CO2 only for now
	ghgs[per].CO2 = 0; // initialize
	for (int i=0;i<nosubrsrc;i++)
		ghgs[per].CO2 += depsubrsrc[i].emission(per);
}

// return GHG emissions for each resource
void resource::ghgoutputdb(const char *regname,int reg)
{
	int m=0;
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	// function protocol
	void dboutput2(string varreg,string var1name,string var2name,string var3name,
			  string var4name,vector<double> dout,string uname);
	
	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	// total sector output
	for (m=0;m<maxper;m++)
		temp[m] = ghgs[m].CO2;
	dboutput2(regname,"emissions","CO2",name,"CO2 emissions",temp,"MTC");

}

// write resource output to database
void resource::outputdb(const char *regname,int reg)
{
	// function protocol
	void dboutput2(string varreg,string var1name,string var2name,string var3name,
			  string var4name,vector<double> dout,string uname);

	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	// total sector output
	dboutput2(regname,name,"all subresources"," ","production",annualprod,"EJ");

	// do for all subsectors in the sector
	for (int i=0;i<nosubrsrc;i++)
		depsubrsrc[i].outputdb(regname,reg,name);
}

// return GHG emissions for each resource
void resource::ghgoutputfile(const char *regname,int reg)
{
	int m=0;
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	// function protocol
	void fileoutput3(int regno,string var1name,string var2name,string var3name,
				  string var4name,string var5name,string uname,vector<double> dout);
	
	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	// total sector output
	for (m=0;m<maxper;m++)
		temp[m] = ghgs[m].CO2;
	fileoutput3(reg,regname,name," "," ","CO2 emiss","MTC",temp);

}

// write resource output to file
void resource::outputfile(const char *regname,int reg)
{
	// function protocol
	void fileoutput3(int regno,string var1name,string var2name,string var3name,
				  string var4name,string var5name,string uname,vector<double> dout);
	
	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	// total sector output
	fileoutput3(reg,regname,name," "," ","production","EJ",annualprod);

	// do for all subsectors in the sector
	for (int i=0;i<nosubrsrc;i++)
		depsubrsrc[i].outputfile(regname,reg,name);
}

// write resource output to database
void resource::MCoutput(const char *regname,int reg)
{
	int m=0;
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	// function protocol
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout);

	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	// total sector output
	dboutput4(regname,"Pri Energy","Production by Sector",name,"EJ",annualprod);
	for (m=0;m<maxper;m++)
		temp[m] = ghgs[m].CO2;
	dboutput4(regname,"CO2 Emiss","by Fuel",name,"MTC",temp);
	dboutput4(regname,"CO2 Emiss",name,"zTotal","MTC",temp);
	// resource price
	dboutput4(regname,"Price","by Sector",name,"$/GJ",rscprc);

	// do for all subsectors in the sector
	for (int i=0;i<nosubrsrc;i++)
		depsubrsrc[i].MCoutput(regname,reg,name);
}

