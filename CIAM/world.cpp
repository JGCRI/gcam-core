/* World.cpp												*
 * Method definition for World class						*
 * Coded by Sonny Kim 2/21/01								*/

//** Database Headers *****
#include <afxdisp.h>
#include <dbdao.h>
#include <dbdaoerr.h>
//** Other Headers ********
#include <string>
#include <stdlib.h>
#include <iostream>
#include <fstream>

using namespace std; // enables elimination of std::

#include "world.h"
#include "str_indexname.h" // get index and name from database
#include "modeltime.h"
// global variables defined in main
extern const char* dbfile;
extern const char* dbtdrsc;
extern const char* dbtsupsec;
extern const char* dbtdemsec;
extern const char* dbtgen;
extern const char* dbtout;
extern ofstream bugoutfile,outfile, sdfile;	
extern clock_t start, intermediate, finish, afterdata;
extern bool timestamp;
extern bool Minicam;  // run Minicam(true) or full CGE(false)
extern Modeltime modeltime;
extern CdbRecordset regidrst,catidrst,subcatidrst,varidrst,subvaridrst;

// function protocols
int countdbrec(string fdname,const char* dbname,const char* dbtname);
void indbrec(str_indexname* str_temp,string index,string name,int ns,const char* dbname,
					  const char* dbtname);

// World class method definition
World::World(void) // default constructor
{
}

World::~World(void) // default constructor
{
	cout << "Deleting the World object...\n";
}

// set size of global arrays depending on MaxPer 
void World::initper(void)
{
	int maxper = modeltime.getmaxper();
	population.resize(maxper); // total global population
	crudeoilrsc.resize(maxper); // global crude oil resource
	unconvoilrsc.resize(maxper); // global crude oil resource
	natgasrsc.resize(maxper); // global natural gas resource
	coalrsc.resize(maxper); // global coal resource
	uranrsc.resize(maxper); // global uranium resource
	ghgs.resize(maxper+2); // structure containing ghg emissions
}

// World class method definition
void World::setregion(void) // set number of regions in World
{
	str_indexname* str_in; // structure with name and index
	
	noreg = countdbrec("Region",dbfile,dbtgen); // returns # of regions from gen table
	//noreg = 1; // just do USA for now
	region.resize(noreg); // create array of region objects
	str_in = new str_indexname[noreg]; // create array of index and name objects

	try {
		indbrec(str_in,"Region","RegionName",noreg,dbfile,dbtgen);
	}
	catch(...) {
		cerr <<"\nproblem while calling indbrec()\n";
	}
		
	for (int i=0;i<noreg;i++) {
		// set region name and index, create region method
		region[i].setlabel(str_in[i].name,str_in[i].index);
		// initialize size of arrays to max period
		region[i].initper();
		// set default CO2 emissions coefficients
		region[i].setCO2coef();
		// set size of population and labor productivity
		region[i].setpop();	
		// read in carbon tax from database for each region
		region[i].setcarbontax();	
		// set number ghg for market solution for each region
		region[i].setghgobj();	
		// sets number of depletable resources
		region[i].setdepresource(); 
		// sets number of supply sectors
		region[i].setsupsector(); 
		// sets number of demand sectors
		region[i].setdemsector(); 
	}	
	// delete structure for setting name and index
	delete [] str_in; 
}

// initialize all regions
// all data inputs are read in here
void World::initregion(void)
{
	for (int i=0;i<noreg;i++) {
		region[i].initpop();	// init population and labor productivity data
		region[i].economics();// initialize economic data for region
		region[i].rscinitialize();// initialize data for resources
		region[i].supinitialize();// initialize data for supply sectors
		region[i].deminitialize();// initialize data for demand sectors
		region[i].settechghg(); // sets number of ghgs in technology
	}	
}

// calculate regional gnps
void World::gnp(int per)
{
	for (int i=0;i<noreg;i++) {
		// calculate gnp
		region[i].calc_gnp(per);
	}
}

// calculate supply and demand and emissions for all regions
void World::calc(int per)
{
	for (int i=0;i<noreg;i++) {
		// apply carbon taxes to appropriate technologie
		region[i].applycarbontax(per);
		// set regional GHG constraint to market supply
		region[i].setghgsupply(per);
		// set regional GHG tax to individual technologies
		region[i].addghgtax(per);
		// determine supply of primary resources
		region[i].rscsupply(per);
		//sdfile<<"\n"; // supply & demand info.
		// determine prices of refined fuels and electricity
		region[i].finalsupplyprc(per);
		// calculate enduse service price
		region[i].calc_enduseprice(per);
		// adjust gnp for energy cost changes
		region[i].adjust_gnp(per);
		// determine end-use demand for energy and other goods
		region[i].endusedemand(per);
		//sdfile<<"\n"; // supply & demand info.
		// determine supply of final energy and other goods based on demand
		region[i].finalsupply(per);
		//sdfile<<"\n"; // supply & demand info.
		// for intermediate goods market set supply = cost and demand = solution price
		if(!Minicam)
			region[i].override_mrks(per);
		// calculate GHG emissions for region by technology
		region[i].emission(per);
		// set regional GHG emissions as market demand
		region[i].setghgdemand(per);
	}	
}

// sum population from each region for global total
void World::sumpop(int per)
{
	population[per] = 0.0;
	// divide by 1000 to get millions
	for (int i=0;i<noreg;i++)
		population[per] += region[i].showpop(per)/1000;
}

// sum regional resources for global total
void World::sumrsc(int per)
{
	crudeoilrsc[per] = 0.0;
	unconvoilrsc[per] = 0.0;
	natgasrsc[per] = 0.0;
	coalrsc[per] = 0.0;
	uranrsc[per] = 0.0;
	for (int i=0;i<noreg;i++) {
		crudeoilrsc[per] += region[i].showsubrsc(1,1,per);
		unconvoilrsc[per] += region[i].showsubrsc(1,2,per);
		natgasrsc[per] += region[i].showrsc(2,per);
		coalrsc[per] += region[i].showrsc(3,per);
		uranrsc[per] += region[i].showrsc(4,per);
	}
}

// calculate indirect emissions for each region
void World::emiss_ind(int per)
{
	for (int i=0;i<noreg;i++) {
		region[i].emiss_ind(per); // calculate indirect emissions
	}
}

// set global emissions for all GHG for climat
void World::emiss_all(void)
{
	int maxper = modeltime.getmaxdataper();
	ifstream gasfile2;
	//gasfile2.open("gas2.emk",ios::in); // open input file for reading
	gasfile2.open("gas2.emk"); // open input file for reading
	// read in all other gases except CO2 from fossil fuels
	// CO2 from fossil fuels comes from model
	int skiplines = 5;
	for (int i=0;i<skiplines;i++)
		gasfile2.ignore(80,'\n'); // skip lines
	for (int per=1;per<maxper;per++) {
		gasfile2.ignore(80,','); // skip year column
		gasfile2.ignore(80,','); // skip CO2 column
		gasfile2 >> ghgs[per].CO2ag;
		gasfile2.ignore(80,','); // skip comma
		gasfile2 >> ghgs[per].CH4;
		gasfile2.ignore(80,','); // skip comma
		gasfile2 >> ghgs[per].N2O;
		gasfile2.ignore(80,','); // skip comma
		gasfile2 >> ghgs[per].SOXreg1;
		gasfile2.ignore(80,','); // skip comma
		gasfile2>> ghgs[per].SOXreg2;
		gasfile2.ignore(80,','); // skip comma
		gasfile2>> ghgs[per].SOXreg3;
		gasfile2.ignore(80,','); // skip comma
		gasfile2>> ghgs[per].CF4;
		gasfile2.ignore(80,','); // skip comma
		gasfile2>> ghgs[per].C2F6;
		gasfile2.ignore(80,','); // skip comma
		gasfile2>> ghgs[per].HFC125;
		gasfile2.ignore(80,','); // skip comma
		gasfile2>> ghgs[per].HFC134a;
		gasfile2.ignore(80,','); // skip comma
		gasfile2>> ghgs[per].HFC143a;
		gasfile2.ignore(80,','); // skip comma
		gasfile2>> ghgs[per].HFC227ea;
		gasfile2.ignore(80,','); // skip comma
		gasfile2>> ghgs[per].HFC245ca;
		gasfile2.ignore(80,','); // skip comma
		gasfile2>> ghgs[per].SF6;
		gasfile2.ignore(80,'\n'); // next line
	}
	for (per=maxper;per<maxper+2;per++) {
		ghgs[per]=ghgs[per-1];
	}

	gasfile2.close();
}

// write results for all regions to database
void World::outputdb(void)
{
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	// function protocol
	void dboutput2(string varreg,string var1name,string var2name,string var3name,
			  string var4name,vector<double> dout,string uname);

	// write global population results to database
	dboutput2("zGlobal","demographics"," "," ","population",population,"Millions");

	// write total emissions for World
	for (int m=0;m<maxper;m++)
		temp[m] = ghgs[m].CO2;
	dboutput2("Global","CO2 Emiss","by fuel","zTotal","CO2 emissions",temp,"MTC");
	dboutput2("Global","resource","crude oil"," ","conv resource",crudeoilrsc,"EJ");
	dboutput2("Global","resource","crude oil"," ","unconv resource",unconvoilrsc,"EJ");
	dboutput2("Global","resource","natural gas"," ","all resources",natgasrsc,"EJ");
	dboutput2("Global","resource","coal"," ","all resources",coalrsc,"EJ");
	dboutput2("Global","resource","uranium"," ","all resources",uranrsc,"EJ");
	for (int i=0;i<noreg;i++)
		region[i].outputdb();
}

// write results for all regions to file
void World::outputfile(void)
{
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	// function protocol
	void fileoutput3(int regno,string var1name,string var2name,string var3name,
				  string var4name,string var5name,string uname,vector<double> dout);

	// write global population results to database
	fileoutput3(0,"global"," "," "," ","population","Millions",population);

	// write total emissions for World
	for (int m=0;m<maxper;m++)
		temp[m] = ghgs[m].CO2;
	fileoutput3(0,"global"," "," "," ","CO2 emiss","MTC",temp);
	fileoutput3(0,"global"," "," "," ","c.oil resource(conv)","EJ",crudeoilrsc);
	fileoutput3(0,"global"," "," "," ","c.oil resource(unconv)","EJ",unconvoilrsc);
	fileoutput3(0,"global"," "," "," ","n.gas resource","EJ",natgasrsc);
	fileoutput3(0,"global"," "," "," ","coal resource","EJ",coalrsc);
	fileoutput3(0,"global"," "," "," ","uran resource","EJ",uranrsc);
	for (int i=0;i<noreg;i++)
		region[i].outputfile();
}

// MiniCAM style output to file
void World::MCoutput(void)
{
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	// function protocol
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout);

	// write global population results to database
	//dboutput4("global","General","Population","zTotal","thous",population);

	for (int i=0;i<noreg;i++)
		region[i].MCoutput();
}

double World::showCO2(int per) // return global emissions for period
{
	return ghgs[per].CO2;
}

double World::showCO2ag(int per) // return global emissions for period
{
	return ghgs[per].CO2ag;
}

double World::showCH4(int per) // return global emissions for period
{
	return ghgs[per].CH4;
}

double World::showN2O(int per) // return global emissions for period
{
	return ghgs[per].N2O;
}

double World::showSOXreg1(int per) // return global emissions for period
{
	return ghgs[per].SOXreg1;
}

double World::showSOXreg2(int per) // return global emissions for period
{
	return ghgs[per].SOXreg2;
}

double World::showSOXreg3(int per) // return global emissions for period
{
	return ghgs[per].SOXreg3;
}

double World::showCF4(int per) // return global emissions for period
{
	return ghgs[per].CF4;
}

double World::showC2F6(int per) // return global emissions for period
{
	return ghgs[per].C2F6;
}

double World::showHFC125(int per) // return global emissions for period
{
	return ghgs[per].HFC125;
}

double World::showHFC134a(int per) // return global emissions for period
{
	return ghgs[per].HFC134a;
}

double World::showHFC143a(int per) // return global emissions for period
{
	return ghgs[per].HFC143a;
}

double World::showHFC227ea(int per) // return global emissions for period
{
	return ghgs[per].HFC227ea;
}

double World::showHFC245ca(int per) // return global emissions for period
{
	return ghgs[per].HFC245ca;
}

double World::showSF6(int per) // return global emissions for period
{
	return ghgs[per].SF6;
}
