/* subsector.cpp										*
 * Method definition for the Subsector class.			*
 * SHK 10/24/00											*/

//** Database Headers *****
#include <afxdisp.h>
#include <dbdao.h>
#include <dbdaoerr.h>
//** Other Headers ********
//#include <string>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <math.h>
//#include <vector>
#include "market.h"
#include "modeltime.h"
#include "subsector.h"

using namespace std; // enables elimination of std::

extern Marketplace marketplace;
extern CdbDBEngine dben;
extern CdbDatabase db;
extern CdbRecordset suprst,suprst_ct,demrst,demrst_ct;
extern CdbRecordset regidrst,catidrst,subcatidrst,varidrst,subvaridrst;
extern ofstream outfile;	
extern Modeltime modeltime;

// function protocol
int countdbrec3(string fdname,int is,int iss,const char *dbname,const char *dbtname);
int count_tech(string region,string fdname,int is,int iss,const char *dbtname);
void dbgenread(double *temp,string region,string var1name,string var2name,int maxper);
void dbmodelread(double *temp,string region,string var1name,string var2name);
	

// subsector class method definition
subsector::subsector(void) // default constructor
{
}

subsector::subsector(const char*nstr,int sno,double ttax)
{
	strcpy(name,nstr);
	no = sno;
	tax = ttax;
}

subsector::~subsector(void)
{
}

// return subsector index
int subsector::index(void)
{
	return no;
}

void subsector::setlabel(const char *nstr, int sno)
{
	strcpy(name, nstr);
	no = sno;
}

char * subsector::showname(void)
{
	return name;
}

void subsector::initper(void) //set vector size
{
	int maxper = modeltime.getmaxper();

	caplim.resize(maxper); // subsector capacity limit
	shrwts.resize(maxper); // subsector logit share weights
	lexp.resize(maxper); // subsector logit exponential
	share.resize(maxper); // subsector shares
	input.resize(maxper); // subsector energy input
	pe_cons.resize(maxper); // subsector primary energy consumption
	subsectorprice.resize(maxper); // subsector price for all periods
	output.resize(maxper); // total amount of final output from subsector
	carbontaxpaid.resize(maxper); // total subsector carbon taxes paid
	summary.resize(maxper); // object containing summaries
}

void subsector::setall(lpstr_ssec tempstr,int dataper) //set subsector info
{
	// set to model period
	int m = modeltime.getdata_to_mod(dataper);
	int offset = modeltime.getdataoffset(dataper);
	shrwts[m] = tempstr->tshrwts;
	lexp[m] = tempstr->tlexp;
	int k = 0;
	if (dataper>0) k = modeltime.getdata_to_mod(dataper-1);
	for (int j=k+1; j<m; j++) {
		shrwts[j] = shrwts[k]+(shrwts[m]-shrwts[k])/offset*(j-k);
		lexp[j] = lexp[k]+(lexp[m]-lexp[k])/offset*(j-k);
	}
}

void subsector::copytolast(int per) //set subsector info
{
	shrwts[per] = shrwts[per-1];
	lexp[per] = lexp[per-1];
}

void subsector::setall2(double tshrwts,double tlexp,int per) //set subsector info
{
	// read in data for subsector from database
	shrwts[per] = tshrwts;
	lexp[per] = tlexp;
}

// set number of technologies
void subsector::settech(int itech)
{
	notech = itech; // set private member
	techs.resize(notech); // techs is 2-dim vector of technology obj
	int maxper = modeltime.getmaxper();
	for (int i=0;i<techs.size();i++) techs[i].resize(maxper);
}

// set number of exogenously driven technologies
void subsector::set_hydrotech(int itech)
{
	// one hydro for each region
	int maxper = modeltime.getmaxper();
	hydro.resize(maxper);
}

// set number of and initialize ghgs for technologies
void subsector::settechghg(int regionno,int sectorno) 
{
	int subsectorno = no;
	// set Ghg object for period 0 only
	for (int i=0;i<notech;i++) {
		techs[i][0].settechghg(regionno,sectorno,subsectorno);
	}
	// set Ghg object for all other periods to period 0
	for (int per=1;per<modeltime.getmaxper();per++) {
		for (int i=0;i<notech;i++) {
			techs[i][per].setGhg(techs[i][0].getGhg());
		}
	}	

}

void subsector::dbreadstech(char* region,int is)
{
	char bufis[20],bufiss[20];
	int i=0;
	string str;

	// convert integer to string for sql (radix 10)
	// bufis,bufiss contains converted string
	_itoa(is,bufis,10);
	_itoa(no,bufiss,10);

	// get dataset for each subsector
	str = "RegionName = '";
	str = str + region + "'";
	str = str + " AND Sector = " + bufis;
	str = str + " AND Subsector = " + bufiss;
	//suprst.FindFirst(str.c_str());
	no = suprst.GetField("Subsector").intVal;
	strcpy(name,suprst.GetField("SubsectorName").pcVal);
/*
	// create an array of daorsetbing struct
	// table binding: (assign the field lengths and types)
	DAORSETBINDING	Bindings[] = {
	//Index Type    Fld	Type	  Offset			 Size
	{dbBindIndexINT, 7, dbBindI2, offsetof(strtech,yr), sizeof(long)},
	{dbBindIndexINT, 11, dbBindI2, offsetof(strtech,tno), sizeof(long)},
	{dbBindIndexINT, 12, dbBindLPSTRING, offsetof(strtech,tname), sizeof(TCHAR *)},
	{dbBindIndexINT, 13, dbBindI2, offsetof(strtech,ftype), sizeof(long)},
	{dbBindIndexINT, 14, dbBindLPSTRING, offsetof(strtech,fname), sizeof(TCHAR *)},
	{dbBindIndexINT, 15, dbBindR8, offsetof(strtech,shrs), sizeof(double)},
	{dbBindIndexINT, 16, dbBindR8, offsetof(strtech,teff), sizeof(double)},
	{dbBindIndexINT, 17, dbBindR8, offsetof(strtech,tnecost), sizeof(double)},
	{dbBindIndexINT, 18, dbBindR8, offsetof(strtech,ttax), sizeof(double)},
	{dbBindIndexINT, 19, dbBindR8, offsetof(strtech,tlexp), sizeof(double)},
	{dbBindIndexINT, 20, dbBindR8, offsetof(strtech,ttech), sizeof(double)}
	};
*/
	// create an array of daorsetbing struct
	// table binding: (assign the field lengths and types)
	DAORSETBINDING	Bindings[] = {
	//Index Type    Fld	Type	  Offset			 Size
	{dbBindIndexINT, 7, dbBindI2, offsetof(strtech2,yr), sizeof(long)},
	{dbBindIndexINT, 11, dbBindI2, offsetof(strtech2,tno), sizeof(long)},
	{dbBindIndexINT, 12, dbBindLPSTRING, offsetof(strtech2,tname), sizeof(TCHAR *)},
	{dbBindIndexINT, 13, dbBindI2, offsetof(strtech2,ftype), sizeof(long)},
	{dbBindIndexINT, 14, dbBindLPSTRING, offsetof(strtech2,fname), sizeof(TCHAR *)},
	{dbBindIndexINT, 15, dbBindR8, offsetof(strtech2,shrs), sizeof(double)},
	{dbBindIndexINT, 16, dbBindR8, offsetof(strtech2,teff), sizeof(double)},
	{dbBindIndexINT, 17, dbBindR8, offsetof(strtech2,tnecost), sizeof(double)},
	{dbBindIndexINT, 18, dbBindR8, offsetof(strtech2,ttax), sizeof(double)},
	{dbBindIndexINT, 19, dbBindR8, offsetof(strtech2,tlexp), sizeof(double)},
	{dbBindIndexINT, 20, dbBindR8, offsetof(strtech2,ttech), sizeof(double)},
	{dbBindIndexINT, 21, dbBindR8, offsetof(strtech2,tresource), sizeof(double)},
	{dbBindIndexINT, 22, dbBindR8, offsetof(strtech2,tA), sizeof(double)},
	{dbBindIndexINT, 23, dbBindR8, offsetof(strtech2,tB), sizeof(double)}
	};

	// run C++ GetRowsEx 
	int dper = modeltime.getmaxdataper();
	int	maxrecords = dper*notech; // number of rows is the number of periods
	lpstrtech2 pstrtechRows = new strtech2[maxrecords];
	LONG lNumRecords, lYear;
	// cannot use maxrecords because not const int
	TCHAR pBuf[100 * 20]; // buffer for use with variable length text fields only
	
	lNumRecords = suprst.GetRowsEx(pstrtechRows, sizeof(strtech2),
		  &Bindings[0], sizeof(Bindings) / sizeof(DAORSETBINDING),
		  pBuf, sizeof(pBuf), maxrecords); // get yr rows for each technology

	// Step through the returned rows and assign to technology
	int k=0;
	for (i=0;i<notech;i++) {
		for (lYear = 0; lYear<dper; lYear++) {
			// find model period from data year
			int modelper = modeltime.getyr_to_per(pstrtechRows[lYear].yr);
  		    // pstrtechRows is a variant
			techs[i][modelper].setall3(&pstrtechRows[(i*dper)+lYear]);
			// fill periods not read in from data with previous per data
			for (int j=k; j<modelper; j++) {
				techs[i][j] = techs[i][j-1];
			}
			k = modelper+1; // initialize for next time
		}
		// initialize for periods greater than last data period
		// last model period for last data per
		int m1=modeltime.getdata_to_mod(dper-1);
		if(modeltime.getendyr() > modeltime.getper_to_yr(m1)) {
			for (int j=m1+1;j<modeltime.getmaxper();j++)
				techs[i][j] = techs[i][j-1];
		}
	}
	suprst.MoveNext(); // row is last record so move to next row
	delete [] pstrtechRows; // free memory
}

void subsector::dbreaddtech(char* region,int is)
{
	char bufis[20],bufiss[20];
	int i=0;
	string str;

	// convert integer to string for sql (radix 10)
	// bufis,bufiss contains converted string
	_itoa(is,bufis,10);
	_itoa(no,bufiss,10);

	// get dataset for each subsector
	str = "RegionName = '";
	str = str + region + "'";
	str = str + " AND Sector = " + bufis;
	str = str + " AND Subsector = " + bufiss;
	//demrst.FindFirst(str.c_str());
	no = demrst.GetField("Subsector").intVal;
	strcpy(name,demrst.GetField("SubsectorName").pcVal);

	// create an array of daorsetbing struct
	// table binding: (assign the field lengths and types)
	DAORSETBINDING	Bindings[] = {
	//Index Type    Fld	Type	  Offset			 Size
	{dbBindIndexINT, 7, dbBindI2, offsetof(strtech,yr), sizeof(long)},
	{dbBindIndexINT, 10, dbBindI2, offsetof(strtech,tno), sizeof(long)},
	{dbBindIndexINT, 11, dbBindLPSTRING, offsetof(strtech,tname), sizeof(TCHAR *)},
	{dbBindIndexINT, 12, dbBindI2, offsetof(strtech,ftype), sizeof(long)},
	{dbBindIndexINT, 13, dbBindLPSTRING, offsetof(strtech,fname), sizeof(TCHAR *)},
	{dbBindIndexINT, 14, dbBindR8, offsetof(strtech,shrs), sizeof(double)},
	{dbBindIndexINT, 15, dbBindR8, offsetof(strtech,teff), sizeof(double)},
	{dbBindIndexINT, 16, dbBindR8, offsetof(strtech,tnecost), sizeof(double)},
	{dbBindIndexINT, 17, dbBindR8, offsetof(strtech,ttax), sizeof(double)},
	{dbBindIndexINT, 18, dbBindR8, offsetof(strtech,tlexp), sizeof(double)},
	{dbBindIndexINT, 19, dbBindR8, offsetof(strtech,ttech), sizeof(double)}
	};

	// run C++ GetRowsEx 
	int dper = modeltime.getmaxdataper();
	int	maxrecords = dper*notech; // number of rows is the number of periods
	lpstrtech pstrtechRows = new strtech[maxrecords];
	LONG lNumRecords, lYear;
	// cannot use maxrecords because not const int
	TCHAR pBuf[100 * 20]; // buffer for use with variable length text fields only
	
	lNumRecords = demrst.GetRowsEx(pstrtechRows, sizeof(strtech),
		  &Bindings[0], sizeof(Bindings) / sizeof(DAORSETBINDING),
		  pBuf, sizeof(pBuf), maxrecords); // get yr rows for each technology

	int k=0;
	// Step through the returned rows and assign to technology
	for (i=0;i<notech;i++) {
		for (lYear = 0; lYear<dper; lYear++) {
			// find model period from data year
			int modelper = modeltime.getyr_to_per(pstrtechRows[lYear].yr);
  		    // pstrtechRows is a variant
			techs[i][modelper].setall(&pstrtechRows[(i*dper)+lYear]);
			// fill periods not read in from data with previous per data
			for (int j=k; j<modelper; j++) {
				techs[i][j] = techs[i][j-1];
			}
			k = modelper+1; // initialize for next time
		}
		// initialize for periods greater than last data period
		// last model period for last data per
		int m1=modeltime.getdata_to_mod(dper-1);
		if(modeltime.getendyr() > modeltime.getper_to_yr(m1)) {
			for (int j=m1+1;j<modeltime.getmaxper();j++)
				techs[i][j] = techs[i][j-1];
		}
	}
	demrst.MoveNext(); // row is last record so move to next row
	delete [] pstrtechRows; // free memory
}

// price function called below in calc_share after technology shares 
// are determined.
// price function separated to allow different weighting for subsector price
double subsector::price(char* varcountry,int country_id,int per)
{
	int i=0;
	double fuelprice = 0;

	subsectorprice[per]=0.0;
	for (i=0;i<notech;i++) {
		// calculate weighted average price of electricity
		//fuelprice = marketplace.showprice(techs[i][per].showfuelno(),country_id,per);
		subsectorprice[per] += techs[i][per].showshare()*
			techs[i][per].cost(country_id,per);
	}
	return subsectorprice[per];
}

// returns subsector price 
double subsector::showprice(int per)
{
	return subsectorprice[per];
}

// passes carbon tax to technology
void subsector::applycarbontax(double tax,int per)
{
	for (int i=0;i<notech;i++) {
		techs[i][per].applycarbontax(tax);
	}
}

// sets ghg tax to technologies
void subsector::addghgtax(int ghgno,char* ghgname,int country_id,int per)
{
	for (int i=0;i<notech;i++) {
		techs[i][per].addghgtax(ghgno,ghgname,country_id,per);
	}
}

// calculate technology shares
void subsector::calc_share(char* varcountry,int country_id,int per)
{
	int i=0;
	double sum = 0;
	double fuelprice = 0;

	for (i=0;i<notech;i++) {
		//fuelprice = marketplace.showprice(techs[i][per].showfuelno(),country_id,per);
		// determine shares based on technology costs
		techs[i][per].calc_share(country_id,per);
		sum += techs[i][per].showshare();
	}
	// normalize technology shares to total 100 %
	for (i=0;i<notech;i++) {
		techs[i][per].norm_share(sum);
		// Logit exponential should not be zero or positive when more than
		// one technology
		if(notech>1 && techs[i][per].getlexp()>=0) cerr << "Tech Logit Exponential is invalid.\n";
	}
	// calculate and return subsector share; uses above price function
	// price() uses normalized shares calculated above
	// Logit exponential should not be zero
	if(lexp[per]==0) cerr << "SubSec Logit Exponential is 0.\n";
	if(price(varcountry,country_id,per)==0) 
		share[per] = 0;
	else
		share[per] = shrwts[per]*pow(price(varcountry,country_id,per),lexp[per]);
}

void subsector::norm_share(double sum, int per)
{
	if (sum==0)
		share[per]=0;
	else
		share[per] /= sum;
}

// call technology production, only exogenously driven technology
// gives output
double subsector::exog_supply(int per)
{
	double subsecdmd = 0; // no subsector demand
	double tprod=0;
	for (int i=0;i<notech;i++) {
		// calculate technology output and fuel input from subsector output
		techs[i][per].production(subsecdmd,per);
		tprod += techs[i][per].showoutput();
	}
	return tprod;
}

// sets demand to output and output
void subsector::setoutput(char* varcountry,int country_id,double dmd,int per)
{
	char *mrkname;
	int sectorid;
	int i=0;
	string tname; // temporary string
	double tinput=0;  // temporary input

	input[per] = 0; // initialize subsector total fuel input 
	carbontaxpaid[per] = 0; // initialize subsector total carbon taxes paid 
	fueltype = techs[0][0].getfname(); // all techs in subsec have same fuel type
	// output is in service unit when called from demand sectors
	double subsecdmd = share[per]*dmd; // share is subsector level
	for (i=0;i<notech;i++) {
		tname = techs[i][0].getfname();
		tinput = techs[i][per].showinput();
		// calculate technology output and fuel input from subsector output
		techs[i][per].production(subsecdmd,per);
		// total energy input into subsector, must call after tech production
		input[per] += techs[i][per].showinput();
		summary[per].initfuelcons(tname,techs[i][per].showinput());
		// sets subsector fuel demand
		mrkname = techs[i][0].getfname();
		sectorid = techs[i][0].showfuelno();
		marketplace.setdemand(sectorid,country_id,techs[i][per].showinput(),per);
		marketplace.setdemand_good(sectorid,country_id,techs[i][per].showinput(),per);
		carbontaxpaid[per] += techs[i][per].showcarbontaxpaid();
	}
}
  
// calculates fuel input and subsector output
void subsector::sumoutput(int per)
{
	output[per] = 0;
	for (int i=0;i<notech;i++) {
		output[per] += techs[i][per].showoutput();
	}
}

// set supply subsector total output and technology output
double subsector::supply(char* varcountry,int country_id,int per)
{	
	int i=0;
	double dmd=0;

	//dmd = marketplace.showdemand(no,country_id,per);
	dmd = marketplace.showdemand_good(no,country_id,per);
	// this demand does not account for simultaneity

	// this function pertains to the same subsector
	setoutput(varcountry,country_id,dmd,per);
	sumoutput(per);
	// supply and demand for intermediate and final good are set equal
	return dmd;
}

void subsector::show_subsec(void)
{
	int i=0;
	int m=0; // temp period
	//write to file or database later
	cout << no <<"  " << name<<"\n";
	cout << "Number of Technologies: " << notech <<"\n";
	for (i=0;i<notech;i++)
		cout<<"Share["<<i<<"] "<<techs[i][m].showshare()<<"\n";
	cout <<"Total subsector Output: " << output[m] <<"\n";
}

double subsector::showshare(int per)
{
	return share[per];
}

// shows all technologies in subsector by period
void subsector::showtechs(int per, const char *ofile)
{
	int i=0;

	for (i=0;i<notech;i++) {
		// use technology class method show_tech
		techs[i][per].show_techf(ofile);
	}
}

// shows subsector label (name and index)
void subsector::showlabel(const char *ofile)
{
	ofstream outfile;

	outfile.open(ofile, ios::app);

		if (!outfile) {
			//open failed
			cerr<<"Cannot open file for output\n";
			exit(-1);
		}

	outfile << "Subsector: " << no << " " << name << "\n";
	outfile << "Number of Technologies: " << notech << "\n";
	outfile.close();
}

void subsector::setbaseser(char* region,const char* dbtname)
{
	double tmpval[1];

	// reads in data for subsectors
	dbmodelread(tmpval,region,name,"base");
	output[0] = tmpval[0];
}
 
// set base year subsector shares
void subsector::setbaseshr(char* region,const char* secname,const char* dbtname)
{	
	double tmpval[1];

	// reads in data for subsectors
	// name is subsector name
	dbmodelread(tmpval,region,secname,name);
	share[0] = tmpval[0];
}

// write subsector output to database
void subsector::outputdb(const char *regname,int reg,const char *secname)
{
	// function protocol
	void dboutput2(string varreg,string var1name,string var2name,string var3name,
			  string var4name,vector<double> dout,string uname);
	
	int i=0, m=0;
	int mm=0; // temp period
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	
	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	// total subsector output
	dboutput2(regname,secname,name,"all technologies","production",output,"EJ");
	// subsector price
	dboutput2(regname,secname,name,"all technologies","ave cost",subsectorprice,"$/Service");
	for (m=0;m<maxper;m++)
		temp[m] = summary[m].get_emissmap_second("CO2");
	dboutput2(regname,secname,name,"all technologies","CO2 emissions",temp,"MTC");
	// subsector carbon taxes paid
	dboutput2(regname,secname,name,"all technologies","carbon taxes paid",carbontaxpaid,"Mil90$");
	
	// do for all technologies in the subsector
	for (i=0;i<notech;i++) {
		// output or demand for each technology
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].showoutput();
		dboutput2(regname,secname,name,techs[i][mm].showname(),"production",temp,"EJ");
		// technology share
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].showshare();
		dboutput2(regname,secname,name,techs[i][mm].showname(),"share",temp,"%");
		// technology cost
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].showtechcost();
		dboutput2(regname,secname,name,techs[i][mm].showname(),"cost",temp,"$/EJ");
		// ghg tax applied to technology
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].showcarbontax();
		dboutput2(regname,secname,name,techs[i][mm].showname(),"carbon tax($/TC)",temp,"$/TC");
		// ghg tax applied to technology
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].showcarbontaxgj();
		dboutput2(regname,secname,name,techs[i][mm].showname(),"carbon tax($/GJ)",temp,"$/GJ");
		// ghg tax paid
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].showcarbontaxpaid();
		dboutput2(regname,secname,name,techs[i][mm].showname(),"carbon taxes paid",temp,"90Mil$");
		// technology fuel input
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].showinput();
		dboutput2(regname,secname,name,techs[i][mm].showname(),"fuel input",temp,"EJ");
		// technology fficiency
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].showeff();
		dboutput2(regname,secname,name,techs[i][mm].showname(),"efficiency",temp,"eff");
		// technology non-energy cost
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].shownecost();
		dboutput2(regname,secname,name,techs[i][mm].showname(),"non-energy cost",temp,"$/EJ");
		// technology CO2 emission
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].get_emissmap_second("CO2");
		dboutput2(regname,secname,name,techs[i][mm].showname(),"CO2 emissions",temp,"MTC");
		// technology indirect CO2 emission
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].get_emissmap_second("CO2ind");
		dboutput2(regname,secname,name,techs[i][mm].showname(),"CO2 ind emissions",temp,"MTC");
	}
}

// write subsector output to database
void subsector::outputfile(const char *regname,int reg,const char *secname)
{
	// function protocol
	void fileoutput3(int regno,string var1name,string var2name,string var3name,
				  string var4name,string var5name,string uname,vector<double> dout);

	int i=0, m=0;
	int mm=0; // temp period
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	
	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	// total subsector output
	fileoutput3(reg,regname,secname,name," ","production","EJ",output);
	// subsector price
	fileoutput3(reg,regname,secname,name," ","price","$/GJ(ser)",subsectorprice);
	for (m=0;m<maxper;m++)
		temp[m] = summary[m].get_emissmap_second("CO2");
	fileoutput3(reg,regname,secname,name," ","CO2 emiss","MTC",temp);
	// subsector carbon taxes paid
	fileoutput3(reg,regname,secname,name," ","C tax paid","Mil90$",carbontaxpaid);
	
	// do for all technologies in the subsector
	for (i=0;i<notech;i++) {
		// output or demand for each technology
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].showoutput();
		fileoutput3(reg,regname,secname,name,techs[i][mm].showname(),"production","EJ",temp);
		// technology share
		if(notech>1) {
			for (m=0;m<maxper;m++)
				temp[m] = techs[i][m].showshare();
			fileoutput3(reg,regname,secname,name,techs[i][mm].showname(),"tech share","%",temp);
		}
		// technology cost
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].showtechcost();
		fileoutput3(reg,regname,secname,name,techs[i][mm].showname(),"price","$/GJ",temp);
		// ghg tax applied to technology
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].showcarbontax();
		fileoutput3(reg,regname,secname,name,techs[i][mm].showname(),"C tax","$/TC",temp);
		// ghg tax paid
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].showcarbontaxpaid();
		fileoutput3(reg,regname,secname,name,techs[i][mm].showname(),"C tax paid","90Mil$",temp);
		// technology fuel input
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].showinput();
		fileoutput3(reg,regname,secname,name,techs[i][mm].showname(),"fuel consump","EJ",temp);
		// technology efficiency
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].showeff();
		fileoutput3(reg,regname,secname,name,techs[i][mm].showname(),"efficiency","%",temp);
		// technology non-energy cost
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].shownecost();
		fileoutput3(reg,regname,secname,name,techs[i][mm].showname(),"non-energy cost","$/GJ",temp);
		// technology CO2 emission
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].get_emissmap_second("CO2");
		fileoutput3(reg,regname,secname,name,techs[i][mm].showname(),"CO2 emiss","MTC",temp);
		// technology indirect CO2 emission
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].get_emissmap_second("CO2ind");
		fileoutput3(reg,regname,secname,name,techs[i][mm].showname(),"CO2 emiss(ind)","MTC",temp);
	}
}

// write MiniCAM style subsector output to database
// Part A for supply sector, titles and units are different for Part B
void subsector::MCoutputA(const char *regname,int reg,const char *secname)
{
	// function protocol
	void fileoutput4(string var1name,string var2name,string var3name,string var4name,
			         vector<double> dout);
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout);

	int i=0, m=0;
	int mm=0; // temp period
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	
	// total subsector output
	dboutput4(regname,"Secondary Energy Prod",secname,name,"EJ",output);
	// subsector price
	dboutput4(regname,"Price",secname,name,"$/GJ",subsectorprice);

	string tssname = "tech_"; // tempory subsector name
	string str1, str2; // tempory string
	// do for all technologies in the subsector
	for (i=0;i<notech;i++) {
		str1 = secname;
		str1 += "_tech";
		str2 = techs[i][mm].showname();
		// technology non-energy cost
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].shownecost();
		dboutput4(regname,"Price NE Cost",secname,str2,"$/GJ",temp);
		// secondary energy and price output by tech
		// output or demand for each technology
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].showoutput();
		dboutput4(regname,"Secondary Energy Prod",str1,str2,"EJ",temp);
		// technology cost
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].showtechcost();
		dboutput4(regname,"Price",str1,str2,"$/GJ",temp);
	}
}

// write MiniCAM style subsector output to database
// Part B for demand sector, titles and units are different from Part A
void subsector::MCoutputB(const char *regname,int reg,const char *secname)
{
	// function protocol
	void fileoutput4(string var1name,string var2name,string var3name,string var4name,
			         vector<double> dout);
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout);

	int i=0, m=0;
	int mm=0; // temp period
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	
	// total subsector output
	dboutput4(regname,"End-Use Service",secname,name,"Ser Unit",output);
	// subsector price
	dboutput4(regname,"Price",secname,name,"$/Ser",subsectorprice);

	string tssname = "tech_"; // tempory subsector name
	string str; // tempory string
	// do for all technologies in the subsector
	for (i=0;i<notech;i++) {
		//str = tssname + techs[i][mm].showname();
		str = techs[i][mm].showname();
		if(notech>1) {  // write out if more than one technology
			// output or demand for each technology
			for (m=0;m<maxper;m++)
				temp[m] = techs[i][m].showoutput();
			dboutput4(regname,"End-Use Service",secname,str,"Ser Unit",temp);
			// technology cost
			for (m=0;m<maxper;m++)
				temp[m] = techs[i][m].showtechcost();
			dboutput4(regname,"Price",secname,str,"$/Ser",temp);
		}
		// technology non-energy cost
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].shownecost();
		dboutput4(regname,"Price NE Cost",secname,str,"$/Ser",temp);
	}
}


// write MiniCAM style subsector output to database
void subsector::MCoutputC(const char *regname,int reg,const char *secname)
{
	// function protocol
	void fileoutput4(string var1name,string var2name,string var3name,string var4name,
			         vector<double> dout);
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout);

	int i=0, m=0;
	int mm=0; // temp period
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	string str; // tempory string
	
	//for (m=0;m<maxper;m++)
	//	temp[m] = summary[m].get_emissmap_second("CO2");
	//dboutput4(regname,"CO2 Emiss",secname,name,"MTC",temp);
	// subsector carbon taxes paid
	dboutput4(regname,"General","CarbonTaxPaid",name,"$",carbontaxpaid);
	// subsector share 
	dboutput4(regname,"Subsec Share",secname,name,"100%",share);
	// subsector emissions for all greenhouse gases
	typedef map<string,double>:: const_iterator CI;
	map<string,double> temissmap = summary[0].getemission(); // get gases for per 0
	for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
		for (m=0;m<maxper;m++) {
			temp[m] = summary[m].get_emissmap_second(gmap->first);
		}
		str = "Subsec: "; // subsector heading
		str+= secname; // sector name
		str+= "_";
		str+= name; // subsector name
		dboutput4(regname,"Emissions",str,gmap->first,"MTC",temp);
	}

	//string tssname = name; // tempory subsector name
	string tssname = "tech_"; // tempory subsector name
	// do for all technologies in the subsector
	for (i=0;i<notech;i++) {
		//str = tssname + techs[i][mm].showname();
		str = techs[i][mm].showname();
//		if(notech>1) {  // write out if more than one technology
		if(notech>0) {  // write out if more than one technology
			// technology CO2 emission
			for (m=0;m<maxper;m++)
				temp[m] = summary[m].get_emissmap_second("CO2");
			dboutput4(regname,"CO2 Emiss",secname,str,"MTC",temp);
			// technology indirect CO2 emission
			for (m=0;m<maxper;m++)
				temp[m] = summary[m].get_emindmap_second("CO2");
			dboutput4(regname,"CO2 Emiss(ind)",secname,str,"MTC",temp);
			// technology ghg emissions, get gases for per 
			map<string,double> temissmap = techs[i][0].getemissmap();
			for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
				for (m=0;m<maxper;m++) {
					temp[m] = techs[i][m].get_emissmap_second(gmap->first);
				}
				str = "Tech: "; // subsector heading
				str += secname; // sector name
				str += "_";
				str += name; // subsector name
				str += "_";
				str += techs[i][mm].showname(); // technology name
				dboutput4(regname,"Emissions",str,gmap->first,"MTC",temp);
			}
			// technology share
			for (m=0;m<maxper;m++)
				temp[m] = techs[i][m].showshare();
			dboutput4(regname,"Tech Share",secname,str,"%",temp);
			// ghg tax applied to technology
			for (m=0;m<maxper;m++)
				temp[m] = techs[i][m].showcarbontax();
			dboutput4(regname,"C Tax",secname,str,"$/TC",temp);
			// ghg tax paid
			for (m=0;m<maxper;m++)
				temp[m] = techs[i][m].showcarbontaxpaid();
			dboutput4(regname,"C Tax Paid",secname,str,"90Mil$",temp);
			// technology fuel input
			for (m=0;m<maxper;m++)
				temp[m] = techs[i][m].showinput();
			dboutput4(regname,"Fuel Consumption",secname,techs[i][0].getfname(),"EJ",temp);
		}
		// for 1 or more technologies
		// technology efficiency
		for (m=0;m<maxper;m++)
			temp[m] = techs[i][m].showeff();
		dboutput4(regname,"Tech Efficiency",secname,str,"%",temp);
	}
}

int subsector::shownotech(void)
{
	return notech;
}

char* subsector::showtechname(int id)
{
	return techs[id][0].showname();
}

// calculate GHG emissions from annual production of subresource
void subsector::emission(int per, char* prodname)
{
	summary[per].clearemiss(); // clear emissions map
	summary[per].clearemfuelmap(); // clear emissions map
	for (int i=0;i<notech;i++) {
		techs[i][per].emission(prodname);
		summary[per].updateemiss(techs[i][per].getemissmap());
		summary[per].updateemfuelmap(techs[i][per].getemfuelmap());
	}
}

// calculate indirect GHG emissions from annual production of subresource
void subsector::indemission(int per)
{
	summary[per].clearemindmap(); // clear emissions map
	for (int i=0;i<notech;i++) {
		techs[i][per].indemission();
		summary[per].updateemindmap(techs[i][per].getemindmap());
	}
}

// returns subsector primary energy consumption
double subsector::showpe_cons(int per) 
{
	pe_cons[per] = 0;
	for (int i=0;i<notech;i++) {
		// depleatable resource indeces are less than 5
		// how should this condition be made generic?
		// shk 7/23/01
		if (techs[i][per].showfuelno() < 5) {
			int num = techs[i][per].showfuelno() ;
			pe_cons[per] += techs[i][per].showinput();
			double temp = techs[i][per].showinput();
			int stop = 1;
		}
	}
	return pe_cons[per];
}

// returns primary or final energy input
double subsector::showinput(int per) 
{
	return input[per];
}

// returns subsector output
double subsector::getoutput(int per) 
{
	return output[per];
}

// returns total subsector carbon taxes paid
double subsector::showcarbontaxpaid(int per) 
{
	return carbontaxpaid[per];
}

//  gets fuel consumption map in summary object
map<string, double> subsector::getfuelcons(int per) 
{
	map<string, double> test = summary[per].getfuelcons();
	return summary[per].getfuelcons();
}

// clears fuel consumption map in summary object
void subsector::clearfuelcons(int per) 
{
	summary[per].clearfuelcons();
}

//  get ghg emissions map in summary object
map<string, double> subsector::getemission(int per) 
{
	return summary[per].getemission();
}

//  get ghg emissions map in summary object
map<string, double> subsector::getemfuelmap(int per) 
{
	return summary[per].getemfuelmap();
}

//  get ghg emissions map in summary object
map<string, double> subsector::getemindmap(int per) 
{
	return summary[per].getemindmap();
}
