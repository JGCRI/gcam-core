/* region.cpp											*
 * Method definition for region class						*
 * Coded by Sonny Kim 7/29/00								*/

#include <afxdisp.h>
#include <dbdao.h>
#include <dbdaoerr.h>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <math.h>

#include "market.h" //contains market no.,supply,demand,price,and period.
#include "Emcoef_ind.h" // indirect greenhouse gas emissions coefficient class
#include "str_indexname.h" // get index and name from database
#include "region.h" // generic region class
#include "modeltime.h" // model start, end, timestep and period info
using namespace std; // enables elimination of std::

extern const char* dbfile;
extern const char* dbtdrsc;
extern const char* dbtsupsec;
extern const char* dbtdemsec;
extern const char* dbtgen;
extern const char* dbtout;
extern Marketplace marketplace;
extern CdbRecordset drscrst,drscrst_ct,suprst,suprst_ct,demrst,demrst_ct;
extern CdbRecordset ghgrst,ghgrst_ct,techghgrst;
extern CdbRecordset regidrst,catidrst,subcatidrst,varidrst,subvaridrst;
extern bool Minicam;  // run Minicam(true) or full CGE(false)
extern ofstream outfile, sdfile;	
extern Modeltime modeltime;

// map of CO2 emissions coefficient for primary fuel only
map<string, double> co2coefpri;
// map of CO2 emissions coefficient for all fossil fuels
map<string, double> co2coefall;
// vector of objects containing indirect emissions coefficients
vector<Emcoef_ind> emcoef_ind;
// indirect emissions coefficients for secondary energy
map<string, double> co2coefind;
// test 1

int countdbrec(string fdname,const char* dbname,const char* dbtname);
int count_sec(string region,string fdname,const char* dbtname);
void label_sec(str_indexname* str_temp,string region,string index,string name,int ns,
					  const char* dbtname);

// region class method definition
region::region(void) // default constructor
{
}

region::~region(void) // destructor
{
}

void region::setlabel(const char* nstr, int rno)
{
	strcpy(name, nstr);
	no = rno;
}

// set array size to max period
void region::initper(void)
{
	int maxper = modeltime.getmaxper();
	i_elas.resize(maxper); // income elasticity
	gnp_dol.resize(maxper); // regional gross national product
	gnp.resize(maxper); // normalized regional gross national product
	gnp_adj.resize(maxper); // regional gross national product adjusted for energy
	input.resize(maxper); // total fuel and energy consumption
	price_ser.resize(maxper); // aggregate price for demand services
	carbontax.resize(maxper); // regional carbon tax
	carbontaxpaid.resize(maxper); // total regional carbon taxes paid
	summary.resize(maxper); // summary for reporting
}

// set default emissions coefficient for CO2
void region::setCO2coef(void)
{
	// initialize map (tgC/EJ) or (MTC/EJ)
	// apply carbon taxes to primary fuels
/*	co2coefpri["crude oil"] = 18.4; 
	co2coefpri["natural gas"] = 15.0;
	co2coefpri["coal"] = 25.3;
*/
	// apply carbon taxes to secondary fuels
	co2coefpri["refined oil"] = 18.4;
	co2coefpri["delivered gas"] = 15.0;
	co2coefpri["delivered coal"] = 25.3;

	// initialize map (tgC/EJ) or (MTC/EJ)
	co2coefall["crude oil"] = 18.4; 
	co2coefall["refined oil"] = 18.4;
	co2coefall["natural gas"] = 15.0;
	co2coefall["delivered gas"] = 15.0;
	co2coefall["coal"] = 25.3;
	co2coefall["delivered coal"] = 25.3;
}

// set size of population and labor productivity variable to max period
void region::setpop(void)
{
	population.set();
}

// read population and labor productivity info from database
void region::initpop(void)
{
	population.initialize(name); // region name
}

// read in carbon tax from database
void region::setcarbontax(void)
{
	// function protocol
	void dbgenread2(vector<double>& temp,string region,string var1name,string var2name,int maxper);
	int i=0,m=0,j=0,k=0;
	int dper = modeltime.getmaxdataper();
	vector<double> dtemp(dper);

	// reads in and sets carbon taxes for all period
	dbgenread2(dtemp,name,"price","carbon",dper);
	for (i=0; i<dper; i++) {
		// find model period
		m = modeltime.getdata_to_mod(i); 
		carbontax[m] = dtemp[i];
		int offset = modeltime.getdataoffset(i);
		// interpolate in between data periods
		for (j=k; j<m; j++) {
			carbontax[j] = carbontax[k-1] + (carbontax[m] - carbontax[k-1])/
				           offset*(j-k+1);
		}
		k = m+1; // initialize for next time
	}
}

// region class method definition
void region::setghgobj(void) // set number of greenhouse gases
{
	int ghgcount=0;
	int ghgno[10];

	// no is region number declared in region.h
	// loop through the whole recordset and count the number
	// of ghg in the region
	while (!ghgrst_ct.GetEOF() &&
		no == ghgrst_ct.GetField("Region").intVal) {
		ghgcount++;
		ghgno[ghgcount-1] = ghgrst_ct.GetField("GHG").intVal;
		ghgrst_ct.MoveNext();
	}
	noghg = ghgcount; // number of ghg for market solution
	ghgmarket.resize(noghg); // create array of ghg objects

	for (int i=0;i<noghg;i++) {
		ghgmarket[i].initper();
		// set emissions constraint for each ghg
		ghgmarket[i].setconstraint(no,ghgno[i]);
	}
}
	
// set regional ghg constraint to market supply
void region::setghgsupply(int per)
{
	int ghgno;
	double ghgtarget;

	for (int i=0;i<noghg;i++) {
		ghgno = ghgmarket[i].showghgno();
		ghgtarget = ghgmarket[i].showconstraint(per);
		marketplace.setsupply(ghgno,no,ghgtarget,per);		
	}
}
	
// set regional ghg tax to individual technologies
void region::addghgtax(int per)
{
	int ghgno;
	char* ghgname;

	for (int i=0;i<noghg;i++) {
		ghgno = ghgmarket[i].showghgno();
		ghgname = ghgmarket[i].showname();
		for (int j=0;j<nossec;j++)
			supplysector[j].addghgtax(ghgno,ghgname,no,per);
		for (j=0;j<nodsec;j++)
			demandsector[j].addghgtax(ghgno,ghgname,no,per);
	}
}
	
// region class method definition
void region::setdepresource(void) // set number of depletable resources
{
	int seccount=0,subseccount=0,gradecount=0;
	int secno,subsecno,gradeno;
	int countgrade[20][20][20];
	int countsubsec[20][20];
	int countsec[20];

	// loop through the whole recordset and count the number
	// of sectors in the region
	while (!drscrst_ct.GetEOF() &&
		no == drscrst_ct.GetField("Region").intVal) {
		seccount++;
		secno = drscrst_ct.GetField("Sector").intVal;
		subseccount=0;
		while (!drscrst_ct.GetEOF() &&
			no == drscrst_ct.GetField("Region").intVal &&
			secno == drscrst_ct.GetField("Sector").intVal) {
			subseccount++;
			subsecno = drscrst_ct.GetField("Subsector").intVal;
			gradecount=0;
			while (!drscrst_ct.GetEOF() &&
				no == drscrst_ct.GetField("Region").intVal &&
				secno == drscrst_ct.GetField("Sector").intVal &&
				subsecno == drscrst_ct.GetField("Subsector").intVal) {
				gradecount++;
				gradeno = drscrst_ct.GetField("Grade").intVal;
				while (!drscrst_ct.GetEOF() &&
					no == drscrst_ct.GetField("Region").intVal &&
					secno == drscrst_ct.GetField("Sector").intVal &&
					subsecno == drscrst_ct.GetField("Subsector").intVal &&
					gradeno == drscrst_ct.GetField("Grade").intVal) {
					drscrst_ct.MoveNext();
				}
			}
			countgrade[no][seccount-1][subseccount-1] = gradecount;
			int stop = 0;
		}
		countsubsec[no][seccount-1] = subseccount;
		int stop = 0;
	}
	countsec[no] = seccount;
	nodrsc = seccount; // number of resources
	depresource.resize(nodrsc); // resize resource objects

	for (int i=0;i<nodrsc;i++) {
		depresource[i].initper();
		// create subresource objects
		depresource[i].setdepsubrsrc(countsubsec[no][i],countgrade[no][i]);
	}	
}
	
// region class method definition
void region::setsupsector(void) // set number of supply sectors
{
	int seccount=0,subseccount=0,techcount=0;
	int secno,subsecno,techno;
	int counttech[20][20][20];
	int countsubsec[20][20];
	int countsec[20];
	vector<string> sectorname;

	// loop through the whole recordset and count the number
	// of sectors, subsectors and technologies in the region
	while (!suprst_ct.GetEOF() &&
		no == suprst_ct.GetField("Region").intVal) {
		seccount++;
		secno = suprst_ct.GetField("Sector").intVal;
		sectorname.resize(seccount);
		sectorname[seccount-1] = suprst_ct.GetField("SectorName").pcVal;
		subseccount=0;
		while (!suprst_ct.GetEOF() &&
			no == suprst_ct.GetField("Region").intVal &&
			secno == suprst_ct.GetField("Sector").intVal) {
			subseccount++;
			subsecno = suprst_ct.GetField("Subsector").intVal;
			techcount=0;
			while (!suprst_ct.GetEOF() &&
				no == suprst_ct.GetField("Region").intVal &&
				secno == suprst_ct.GetField("Sector").intVal &&
				subsecno == suprst_ct.GetField("Subsector").intVal) {
				techcount++;
				techno = suprst_ct.GetField("Technology").intVal;
				while (!suprst_ct.GetEOF() &&
					no == suprst_ct.GetField("Region").intVal &&
					secno == suprst_ct.GetField("Sector").intVal &&
					subsecno == suprst_ct.GetField("Subsector").intVal &&
					techno == suprst_ct.GetField("Technology").intVal) {
					suprst_ct.MoveNext();
				}
			}
			counttech[no][seccount-1][subseccount-1] = techcount;
			int stop = 0;
		}
		countsubsec[no][seccount-1] = subseccount;
		int stop = 0;
	}
	countsec[no] = seccount;
	nossec = seccount; // number of supply sectors
	supplysector.resize(nossec); // create array of suppley sector objects
	emcoef_ind.resize(nossec); // indirect GHG coef object for every supply sector

	for (int i=0;i<nossec;i++) {
		// set number of subsectors and technologies for each sector
		supplysector[i].set_subsec(countsubsec[no][i],counttech[no][i]);
		emcoef_ind[i].setname(sectorname[i]);
		supplysector[i].initper();
	}	
}
	
// region class method definition
void region::setdemsector(void) // set number of demand sectors
{
	int seccount=0,subseccount=0,techcount=0;
	int secno,subsecno,techno;
	int counttech[20][20][20];
	int countsubsec[20][20];
	int countsec[20];

	// loop through the whole recordset and count the number
	// of sectors in the region
	while (!demrst_ct.GetEOF() &&
		no == demrst_ct.GetField("Region").intVal) {
		seccount++;
		secno = demrst_ct.GetField("Sector").intVal;
		subseccount=0;
		while (!demrst_ct.GetEOF() &&
			no == demrst_ct.GetField("Region").intVal &&
			secno == demrst_ct.GetField("Sector").intVal) {
			subseccount++;
			subsecno = demrst_ct.GetField("Subsector").intVal;
			techcount=0;
			while (!demrst_ct.GetEOF() &&
				no == demrst_ct.GetField("Region").intVal &&
				secno == demrst_ct.GetField("Sector").intVal &&
				subsecno == demrst_ct.GetField("Subsector").intVal) {
				techcount++;
				techno = demrst_ct.GetField("Technology").intVal;
				while (!demrst_ct.GetEOF() &&
					no == demrst_ct.GetField("Region").intVal &&
					secno == demrst_ct.GetField("Sector").intVal &&
					subsecno == demrst_ct.GetField("Subsector").intVal &&
					techno == demrst_ct.GetField("Technology").intVal) {
					demrst_ct.MoveNext();
				}
			}
			counttech[no][seccount-1][subseccount-1] = techcount;
			int stop = 0;
		}
		countsubsec[no][seccount-1] = subseccount;
		int stop = 0;
	}
	countsec[no] = seccount;
	nodsec = seccount; // number of demand sectors
	demandsector.resize(nodsec); // create array of demand sector objects

	for (int i=0;i<nodsec;i++) {
		// set number of subsectors and technologies for each sector
		demandsector[i].set_subsec(countsubsec[no][i],counttech[no][i]);
		demandsector[i].initper();
		demandsector[i].initper_ds();
	}
}

// set number of ghg objects for supply and demand technologies
void region::settechghg(void) 
{
	// set number of ghg for supply technologies
	int regionno = no;
	for (int i=0;i<nossec;i++) {
		supplysector[i].settechghg(regionno);
	}
	// set number of ghg for demand technologies
	for (i=0;i<nodsec;i++) {
		demandsector[i].settechghg(regionno);
	}

}

// set economic data for each region
void region::economics(void)
{
	// function protocol
	void dbgenread2(vector<double>& temp,string region,string var1name,string var2name,int maxper);
	int i=0,m=0,j=0,k=0;
	int dper = modeltime.getmaxdataper();
	vector<double> dtemp(dper);

	// reads in income elasticity for region
	dbgenread2(dtemp,name,"gnp","actual",dper);
	for (i=0; i<dper; i++) {
		// find model period
		m = modeltime.getdata_to_mod(i); 
		gnp_dol[m] = dtemp[i];
		int offset = modeltime.getdataoffset(i);
		// interpolate in between data periods
		for (j=k; j<m; j++) {
			gnp_dol[j] = gnp_dol[k-1] + (gnp_dol[m] - gnp_dol[k-1])/
				          offset*(j-k+1);
		}
		k = m+1; // initialize for next time
	}
	// reads in income elasticity for region
	k=0;
	dbgenread2(dtemp,name,"elasticity","income",dper);
	for (i=0; i<dper; i++) {
		// find model period
		m = modeltime.getdata_to_mod(i); 
		i_elas[m] = dtemp[i];
		int offset = modeltime.getdataoffset(i);
		// interpolate in between data periods
		for (j=k; j<m; j++) {
			i_elas[j] = i_elas[k-1] + (i_elas[m] - i_elas[k-1])/
				          offset*(j-k+1);
		}
		k = m+1; // initialize for next time
	}
	// initialize for periods greater than last data period
	// last model period for last data per
	int m1=modeltime.getdata_to_mod(dper-1);
	if(modeltime.getendyr() > modeltime.getper_to_yr(m1)) {
		for (int j=m1+1;j<modeltime.getmaxper();j++)
			i_elas[j] = i_elas[j-1];
	}
}
	
// region class method definition
void region::rscinitialize(void) // initialize each resource
{
	// reads in data for all grades in sector and subsector
	for (int i=0;i<nodrsc;i++)
		depresource[i].initialize(name,no);
}

// region class method definition
void region::supinitialize(void) // initialize each supply sector
{
	int i=0;
	// reads in data for each technology in sector and subsector
	for (i=0;i<nossec;i++) {
		supplysector[i].init_Ssubsec(name,dbtsupsec);
		supplysector[i].init_stech(name,no);
	}
}

// region class method definition
void region::deminitialize(void) // initialize each demand sector
{
	// reads in data for each technology in sector and subsector
	for (int i=0;i<nodsec;i++) {
		demandsector[i].init_Dsubsec(name,dbtdemsec);
		demandsector[i].init_dtech(name,no);
	}
}

// calculates annual supply of primay resources
void region::rscsupply(int per) 
{
	char* mrkname;
	int rscno;
	double price=0;
	double price2=0;

	summary[per].clearpeprod();
	//for (int i=0;i<nodrsc-1;i++) {
	for (int i=0;i<nodrsc;i++) {
		mrkname = depresource[i].showname();
		rscno = depresource[i].index();
		// determine cummulative production for period
		// name is region or country name
		//price = marketplace.showprice(mrkname,name,per); // get market price
		price = marketplace.showprice(rscno,no,per); // get market price
		price2 = marketplace.showprice(rscno,no,per-1); // get market price
		// calculate annual supply from cummulative production
		depresource[i].cummsupply(price,per);
		//depresource[i].annualsupply(per,gnp_adj[per],gnp_adj[per-1],price,price2);
		depresource[i].annualsupply(per,gnp[per],gnp[per-1],price,price2);
		// set market supply of resources used for solution mechanism
		marketplace.setsupply(rscno,no,depresource[i].showannualprod(per),per);		
		// set market supply of resources 
		marketplace.setsupply_good(rscno,no,depresource[i].showannualprod(per),per);		
		summary[per].initpeprod(depresource[i].showname(),depresource[i].showannualprod(per));
	}
}

// calculate prices of refined fuels and electricity
void region::finalsupplyprc(int per)
{
	char* mrkname;
	int sectorid;
	double mrkprice;

	for (int i=0;i<nossec;i++) {
		mrkname = supplysector[i].showname();
		sectorid = supplysector[i].showno();
		// name is region or country name
		supplysector[i].calc_share(name,no,per);
		supplysector[i].price(per);
		mrkprice = supplysector[i].showprice(per);
		// set market price of intermediate goods
		// name is region or country name
		if (Minicam) // set market price equal to fixed prices
			marketplace.setprice(sectorid,no,mrkprice,per); 
		else // use market price determined in solution after period 0
			if (per == 0)
				marketplace.setprice(sectorid,no,mrkprice,per); 
	}
}

// calculates supply of final energy and other goods
void region::finalsupply(int per) 
{
	char* mrkname;
	int sectorid;
	int i=0, j=0;
	double mrksupply;

	// loop through all sectors once to get total output
	//marketplace.storeinfo(per); // market info from end-use demand
	for (i=0;i<nossec;i++) {
		// start with last supply sector
		// need demand for all intermediate and final energy to
		// determine need for primary energy
		j = nossec - (i+1);
		// name is country/region name
		supplysector[j].supply(name,no,per);
		supplysector[j].sumoutput(per);
		carbontaxpaid[per] += supplysector[j].showcarbontaxpaid(per);
	}
	//sdfile<<"\n"; //supply & demand info
	//marketplace.restoreinfo(per); // restore info from end-use demand

	for (i=0;i<nossec;i++) {
		j = nossec - (i+1);
		// name is country/region name
		//supplysector[j].supply(name,no,per);
		// supply and demand for intermediate and final good are set equal
		mrkname = supplysector[i].showname();
		sectorid = supplysector[i].showno();
		mrksupply = supplysector[i].getoutput(per);
		// set market supply of intermediate goods
		marketplace.setsupply(sectorid,no,mrksupply,per); 
		marketplace.setsupply_good(sectorid,no,mrksupply,per); 
		// update fuel consumption (primary and secondary) for supply sector
		summary[per].updatefuelcons(supplysector[i].getfuelcons(per)); 
		if (!Minicam) {// set market price equal to fixed prices
			if (per == 0) // actually trial demand 
				marketplace.setprice_d(sectorid,no,mrksupply,per); 
		}
	}
	// update primary energy trade from consumption and production amounts
	summary[per].updatepetrade(); 
}

// for intermediate goods override supply and demand with prices
void region::override_mrks(int per) 
{
	int sectorid;
	int i=0;
	double mrksupply=0,mrkdemand=0;
	double mrksupply_d=0,mrkdemand_d=0;

	for (i=0;i<nossec;i++) {
		sectorid = supplysector[i].showno();
		mrkdemand = supplysector[i].showprice(per);
		mrksupply = marketplace.showprice(sectorid,no,per);
		mrkdemand_d = marketplace.showdemand(sectorid,no,per);
		mrksupply_d = marketplace.showprice_d(sectorid,no,per);
		// set market supply equal to cost of good/service
		marketplace.override_supply_d(sectorid,no,mrksupply_d,per); 
		marketplace.override_demand_d(sectorid,no,mrkdemand_d,per); 
		marketplace.override_supply(sectorid,no,mrksupply,per); 
		marketplace.override_demand(sectorid,no,mrkdemand,per); 
	}
}

// calculates regional gnp
void region::calc_gnp(int per) 
{
	double labprd=0;

	if (per==modeltime.getyr_to_per(1975)) {
		gnp[per] = 1.0; // normalize to 1975
	}
	else {
		// 1 + labor productivity growth rate
		labprd = 1 + population.labor(per); // return labor productivity gr
		//double pop1 = population.total(per-1);
		//double pop2 = population.total(per-2);
		double pop1 = population.getlaborforce(per);
		double pop2 = population.getlaborforce(per-1);
		double tlab = pow(labprd,modeltime.gettimestep(per));
		//gnp[per] = gnp_adj[per-1]*pow(labprd,Years)
			//*population.total(per-1)/population.total(per-2);
		gnp[per] = gnp[per-1] * tlab * (pop1/pop2);
		if (gnp[per] == 0) {
			cerr << "error with GNP calculation:  pop1: " << pop1
			<< "  pop2: " << pop2 << "  lab: " << tlab << "\n";
		}
	}
}

// calculates demand sector aggregate price 
void region::calc_enduseprice(int per) 
{
	price_ser[per] = 0.0;
	for (int i=0;i<nodsec;i++) {
		if (per == 0) {
			// read in base year service
			demandsector[i].setbaseser(name,dbtgen);
			//demandsector[i].setbaseshr(name,dbtgen);
			//name is region or country name
			demandsector[i].calc_share(name,no,per);
		}
		else 
			demandsector[i].calc_share(name,no,per);

		// calculate service price for each demand sector
		demandsector[i].price(per);
		// calculate aggregate service price for region
		price_ser[per] += demandsector[i].getoutput(0)*demandsector[i].showprice(per);
	}
}

// adjusts regional gnp for energy
void region::adjust_gnp(int per) 
{
	double tempratio;
	double e_gnp_elas = -0.15;
	if (per<=modeltime.getyr_to_per(1990)) {
		gnp_adj[per] = gnp[per];
	}
	else {
		// adjust gnp using energy cost changes and 
		// energy to gnp feedback elasticity
		tempratio = price_ser[per]/price_ser[per-1];
		try {
			gnp_adj[per] = gnp[per]*pow(tempratio,e_gnp_elas);}
		catch(...) {
			cerr << "Error calculating gnp_adj in region.adjust_gnp()\n";
		}
	}
	// calculate dollar value gnp using 1990 value
	if (per>modeltime.getyr_to_per(1990)) gnp_dol[per] = 
		gnp_adj[per]*gnp_dol[modeltime.getyr_to_per(1990)];
}

// calculates regional demand for energy and other goods for all sectors
void region::endusedemand(int per) 
{
	double price_n=0;
	carbontaxpaid[per] = 0; // initialize total regional carbon taxes paid

	//double x = gnp_adj[per]*population.total(0)/population.total(per);
	double x = gnp_adj[per]*population.getlaborforce(0)/population.getlaborforce(per);

	summary[per].clearfuelcons();

	for (int i=0;i<nodsec;i++) {
		// normalized sector prices
		price_n = demandsector[i].showprice(per)/demandsector[i].showprice(0);

		// calculate aggregate demand for end-use sector services
		// set fuel demand from aggregate demand for services
		// name is region or country name
		demandsector[i].aggdemand(name,no,price_n,x,i_elas[per],per); 
		carbontaxpaid[per] += demandsector[i].showcarbontaxpaid(per);
		// update fuel consumption (primary and secondary) for demand sector
		summary[per].updatefuelcons(demandsector[i].getfuelcons(per)); 
	}
}

// show all supply sector information
void region::showsupsector(int per, const char *ofile) 
{
	for (int i=0;i<nossec;i++) {
		supplysector[i].showlabel(ofile);
		supplysector[i].showsubsec(per,ofile);
	}
}

// show all demand sector information
void region::showdemsector(int per, const char* ofile) 
{
	for (int i=0;i<nodsec;i++) {
		demandsector[i].showlabel(ofile);
		demandsector[i].showsubsec(per,ofile);
	}
}

// apply carbon taxes to appropriate sectors
void region::applycarbontax(int per)
{
	int i=0;
	// apply carbon taxes by period to primary fossil fuel user only
	for (i=0;i<nossec;i++)
		supplysector[i].applycarbontax(carbontax[per],per);
	for (i=0;i<nodsec;i++)
		demandsector[i].applycarbontax(carbontax[per],per);
}

// return regional population
double region::showpop(int per)
{
	return population.total(per);
}

// return regional available resource
double region::showrsc(int rscno,int per)
{
	for (int i=0;i<nodrsc;i++) {
		if (depresource[i].index() == rscno)
			return depresource[i].showavailable(per);
	}
	return 0;
}

// return regional available subresource
double region::showsubrsc(int rscno,int subrscno,int per)
{
	for (int i=0;i<nodrsc;i++) {
		if (depresource[i].index() == rscno)
			return depresource[i].showsubavail(subrscno,per);
	}
	return 0;
}

// calculate regional emissions from resources
void region::emission(int per)
{
	int i=0;
	map<string, double> fuelemiss; // tempory emissions by fuel

	fuelemiss["CO2oil"] = summary[per].get_pemap_second("crude oil")
					    * co2coefall["crude oil"];
	fuelemiss["CO2gas"] = summary[per].get_pemap_second("natural gas")
					    * co2coefall["natural gas"];
	fuelemiss["CO2coal"] = summary[per].get_pemap_second("coal")
				  	    * co2coefall["coal"];

	summary[per].clearemiss(); // clear emissions map
	summary[per].updateemiss(fuelemiss); // add CO2 emissions by fuel

	// need to call emissions function but sum is not needed
	for (i=0;i<nossec;i++) {
		supplysector[i].emission(per);
		summary[per].updateemiss(supplysector[i].getemission(per));
		emcoef_ind[i].setemcoef(supplysector[i].getemfuelmap(per), 
			supplysector[i].getoutput(per));
	}
	for (i=0;i<nodsec;i++) {
		demandsector[i].emission(per);
		summary[per].updateemiss(demandsector[i].getemission(per));
	}
}

// calculate regional indirect emissions from intermediate and final demand sectors
void region::emiss_ind(int per)
{
	// calculate indirect GHG emissions
	for (int i=0;i<nossec;i++)
		supplysector[i].indemission(per);
	for (i=0;i<nodsec;i++) 
		demandsector[i].indemission(per);
}

// set regional GHG emissions as market demand
void region::setghgdemand(int per)
{
	int ghgno;
	double ghgemiss;
	string ghgname;

	for (int i=0;i<noghg;i++) {
		ghgno = ghgmarket[i].showghgno();
		ghgname = ghgmarket[i].showname();
		if(ghgname == "CO2") {
			ghgemiss = summary[per].get_emissmap_second("CO2");
			ghgmarket[i].setemission(ghgemiss,per);
			marketplace.setdemand(ghgno,no,ghgemiss,per);		
		}
		else if(ghgname == "CH4") {
			ghgemiss = summary[per].get_emissmap_second("CH4");
			ghgmarket[i].setemission(ghgemiss,per);
			marketplace.setdemand(ghgno,no,ghgemiss,per);		
		}
	}
}	

// place all outputs to database here
void region::outputdb(void)
{
	int i=0;
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	// function protocol
	void dboutput2(string varreg,string var1name,string var2name,string var3name,
			  string var4name,vector<double> dout,string uname);

	// write population results to database
	population.outputdb(name,no);
	// write gnp and adjusted gnp for region
	dboutput2(name,"economics","gnp"," ","gnp($)",gnp_dol,"Bil90US$");
	dboutput2(name,"economics","gnp"," ","gnp(norm)",gnp,"normalized");
	dboutput2(name,"economics","gnp"," ","gnp(energy adj)",gnp_adj,"normalized");
	// regional carbon taxes
	dboutput2(name,"economics","tax"," ","carbon tax",carbontax,"90$/TC");
	// regional carbon taxes paid
	dboutput2(name,"economics","tax"," ","carbon tax rev",carbontaxpaid,"Mil90$");

	// write total emissions for region based on sector
	for (int m=0;m<maxper;m++)
		temp[m] = summary[m].get_emissmap_second("CO2");
	dboutput2(name,"emissions","CO2","total region","CO2 emissions",temp,"MTC");
	// write depletable resource results to database
	for (i=0;i<nodrsc;i++) 
		depresource[i].outputdb(name,no);
	// write supply sector results to database
	for (i=0;i<nossec;i++) 
		supplysector[i].outputdb(name,no);
	// write end-use sector demand results to database
	for (i=0;i<nodsec;i++) 
		demandsector[i].outputdb(name,no);	

}

// place all outputs to file here
void region::outputfile(void)
{
	int i=0;
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	// function protocol
	void fileoutput3(int regno,string var1name,string var2name,string var3name,
				  string var4name,string var5name,string uname,vector<double> dout);
	
	// write population results to database
	population.outputfile(name,no);
	// write gnp and adjusted gnp for region
	fileoutput3(no,name," "," "," ","GNP","Bil90US$",gnp_dol);
	fileoutput3(no,name," "," "," ","GNP","norm",gnp);
	fileoutput3(no,name," "," "," ","GNP","energy adj",gnp_adj);
	// regional carbon taxes
	fileoutput3(no,name," "," "," ","C tax (fixed)","90$/TC",carbontax);
	// regional total carbon taxes paid
	fileoutput3(no,name," "," "," ","C tax revenue","Mil90$",carbontaxpaid);

	// write total emissions for region
	for (int m=0;m<maxper;m++)
		temp[m] = summary[m].get_emissmap_second("CO2");
	fileoutput3(no,name," "," "," ","CO2 emiss","MTC",temp);
	// write depletable resource results to file
	for (i=0;i<nodrsc;i++) 
		depresource[i].outputfile(name,no);
	// write supply sector results to file
	for (i=0;i<nossec;i++) {
		supplysector[i].outputfile(name,no);
		supplysector[i].subsec_outfile(name,no);
	}
	// write end-use sector demand results to file
	for (i=0;i<nodsec;i++) {
		demandsector[i].outputfile(name,no);	
		demandsector[i].subsec_outfile(name,no);	
	}

}

// MiniCAM outputs to file
void region::MCoutput(void)
{
	int i=0, m=0;
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	// function protocol
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout);
	
	// write population results to database
	population.MCoutput(name,no);
	// write gnp and adjusted gnp for region
	dboutput4(name,"General","GDP 90$","GDP(90mer)","90US$",gnp_dol);
	dboutput4(name,"General","GDP","norm","unitless",gnp);
	dboutput4(name,"General","GDP","energy adj","unitless",gnp_adj);
	// regional carbon taxes
	dboutput4(name,"General","CarbonTax","Fos Fuel","90US$",carbontax);
	// regional total carbon taxes paid
	dboutput4(name,"General","CarbonTax","revenue","90US$",carbontaxpaid);


	// emissions by fuel for region crude oil
	for (m=0;m<maxper;m++)
		temp[m] = summary[m].get_emissmap_second("CO2oil");
	dboutput4(name,"CO2 Emiss","by Fuel","crude oil","MTC",temp);
	// emissions by fuel for region natural gas
	for (m=0;m<maxper;m++)
		temp[m] = summary[m].get_emissmap_second("CO2gas");
	dboutput4(name,"CO2 Emiss","by Fuel","natural gas","MTC",temp);
	// emissions by fuel for region coal
	for (m=0;m<maxper;m++)
		temp[m] = summary[m].get_emissmap_second("CO2coal");
	dboutput4(name,"CO2 Emiss","by Fuel","coal","MTC",temp);
	// total emission by fuel for region
	for (m=0;m<maxper;m++)
		temp[m] = summary[m].get_emissmap_second("CO2");
	dboutput4(name,"CO2 Emiss","by Fuel","zTotal","MTC",temp);
	dboutput4(name,"CO2 Emiss","by Sector","zTotal","MTC",temp);

	// regional emissions for all greenhouse gases
	typedef map<string,double>:: const_iterator CI;
	map<string,double> temissmap = summary[0].getemission(); // get gases for per 0
	for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
		for (int m=0;m<maxper;m++) {
			temp[m] = summary[m].get_emissmap_second(gmap->first);
		}
		dboutput4(name,"Emissions","by gas",gmap->first,"MTC",temp);
	}

	// region fuel consumption (primary and secondary) by fuel type
	map<string,double> tfuelmap = summary[0].getfuelcons();
	for (CI fmap=tfuelmap.begin(); fmap!=tfuelmap.end(); ++fmap) {
		for (int m=0;m<maxper;m++) {
			temp[m] = summary[m].get_fmap_second(fmap->first);
		}
		dboutput4(name,"Fuel Consumption","by fuel",fmap->first,"EJ",temp);
	}

	// region primary energy consumption by fuel type
	map<string,double> tpemap = summary[0].getpecons();
	for (CI pmap=tpemap.begin(); pmap!=tpemap.end(); ++pmap) {
		for (int m=0;m<maxper;m++) {
			temp[m] = summary[m].get_pemap_second(pmap->first);
		}
		dboutput4(name,"Pri Energy","Consumption by fuel",pmap->first,"EJ",temp);
	}

	// region primary energy trade by fuel type
	tpemap = summary[0].getpetrade();
	for (pmap=tpemap.begin(); pmap!=tpemap.end(); ++pmap) {
		for (int m=0;m<maxper;m++) {
			temp[m] = summary[m].get_petrmap_second(pmap->first);
		}
		dboutput4(name,"Pri Energy","Trade by fuel",pmap->first,"EJ",temp);
	}

	// regional Pri Energy Production Total
	for (m=0;m<maxper;m++) {
		temp[m] = summary[m].get_peprodmap_second("zTotal");
	}
	dboutput4(name,"Pri Energy","Production by Sector","zTotal","EJ",temp);

	// write depletable resource results to database
	for (i=0;i<nodrsc;i++) 
		depresource[i].MCoutput(name,no);
	// write supply sector results to database
	for (i=0;i<nossec;i++)
		supplysector[i].MCoutput(name,no);
	// write end-use sector demand results to database
	for (i=0;i<nodsec;i++)
		demandsector[i].MCoutput(name,no);
}

// return number of depletable resources
int region::shownodrsc(void)
{
	return nodrsc;
}

// return number of supply sectors
int region::shownossec(void)
{
	return nossec;
}

