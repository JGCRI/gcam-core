/* sector.cpp												*
 * Method definition for sector class						*
 * Coded by Sonny Kim 7/13/00								*/

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
#include "market.h"
#include "str_indexname.h" // get index and name from database
using namespace std; // enables elimination of std::
#include "modeltime.h"
#include "sector.h"

extern Marketplace marketplace;
extern Modeltime modeltime;
extern const char *dbfile;
extern bool Minicam;  // run Minicam(true) or full CGE(false)

extern CdbDBEngine dben;
extern CdbDatabase db;
extern CdbRecordset suprst,suprst_ss,suprst_ct,demrst,demrst_ss,demrst_ct;
extern CdbRecordset regidrst,catidrst,subcatidrst,varidrst,subvaridrst;
extern ofstream outfile;	

// function protocols
int count_subsec(string region,string fdname,int is,const char* dbtname);
int countdbrec2(string fdname,int is,const char* dbname,const char* dbtname);
void label_subsec(str_indexname* str_temp,string region,string index,string name,
					  int is,int ns,const char* dbtname);

// sector class method definition
sector::sector(void) // default constructor
{
}

sector::sector(const char* nstr,int sno,double ttax)
{
	//name = new char [strlen(nstr)+1];
	strcpy(name,nstr);
	no = sno;
	tax = ttax;
}

sector::~sector(void)
{
}

// return sector index
int sector::index(void)
{
	return no;
}

void sector::setlabel(const char* nstr, int sno)
{
	strcpy(name, nstr);
	no = sno;
}

char* sector::showname(void)
{
	return name;
}

int sector::showno(void)
{
	return no;
}

// set vector size
void sector::initper(void)
{
	int maxper = modeltime.getmaxper();

	sectorprice.resize(maxper); // sector price for all periods
	pe_cons.resize(maxper); // sectoral primary energy consumption
	input.resize(maxper); // sector total energy consumption
	output.resize(maxper); // total amount of final output from sector
	carbontaxpaid.resize(maxper); // total sector carbon taxes paid
	summary.resize(maxper); // object containing summaries
}

// set number of subsectors and technologies for each sector and region
void sector::set_subsec(int iss,int* ttech) 
{
	nosubsec = iss; 
	subsec.resize(nosubsec);

	for (int i=0;i<nosubsec;i++) {
		subsec[i].initper();
		int it = *(ttech+i);
		subsec[i].settech(it);
	}	
}

// set number of ghg for technologies
void sector::settechghg(int regionno) 
{
	int sectorno = no;
	for (int i=0;i<nosubsec;i++) {
		subsec[i].settechghg(regionno,sectorno);
	}	
}

void sector::init_Ssubsec(char* region,const char *rstname)
{
	char bufis[20];
	int i=0;
	string str;

	// convert integer to string for sql (radix 10)
	// bufis,bufiss contains converted string
	_itoa(no,bufis,10);

	str = "RegionName = '";
	str = str + region + "' AND Sector = " + bufis ;
	//suprst.FindFirst(str.c_str());
	no = suprst_ss.GetField("Sector").intVal;
	strcpy(name,suprst_ss.GetField("SectorName").pcVal);

	// NOTE!!!!
	// if there are multiple technologies in the subsector, the share weights 
	// and logit exponential for the first technology are used for the subsector

	// create an array of daorsetbing struct
	// table binding: (assign the field lengths and types)
	// Using suprst_ss recordset which has different field index than 
	// original table
	DAORSETBINDING	Bindings[] = {
	//Index Type    Fld	Type	  Offset			 Size
	{dbBindIndexINT, 6, dbBindR8, offsetof(str_ssec,tcaplim), sizeof(double)},
	{dbBindIndexINT, 7, dbBindR8, offsetof(str_ssec,tshrwts), sizeof(double)},
	{dbBindIndexINT, 8, dbBindR8, offsetof(str_ssec,tlexp), sizeof(double)}
	};

	// run C++ GetRowsEx 
	int dper = modeltime.getmaxdataper();
	int	maxrecords = dper*nosubsec; // number of rows is the number of periods
	// these structures won't work; create subsector share weight and logit exponential 
	// structure
	lpstr_ssec pstr_ssecRows = new str_ssec[maxrecords];
	LONG lNumRecords, lYear;
	// cannot use maxrecords because not const int
	TCHAR pBuf[100 * 20]; // buffer for use with variable length text fields only
	
	lNumRecords = suprst_ss.GetRowsEx(pstr_ssecRows, sizeof(str_ssec),
		  &Bindings[0], sizeof(Bindings) / sizeof(DAORSETBINDING),
		  pBuf, sizeof(pBuf), maxrecords); // get year rows for each subsector
	
	// Step through the returned rows and assign to subsector
	for (i=0;i<nosubsec;i++) {
		for (lYear = 0; lYear < dper; lYear++) {
  		    // pstr_ssecRows is a variant
			subsec[i].setall(&pstr_ssecRows[(i*dper)+lYear],lYear);
		}
		// initialize for periods greater than last data period
		// last model period for last data per
		int m1=modeltime.getdata_to_mod(dper-1);
		if(modeltime.getendyr() > modeltime.getper_to_yr(m1)) {
			for (int j=m1+1;j<modeltime.getmaxper();j++)
				subsec[i].copytolast(j);
		}
	}
	suprst_ss.MoveNext(); // row is last record so move to next row
	delete [] pstr_ssecRows; // free memory
}

// same as above except for the demand recordset
void sector::init_Dsubsec(char* region,const char* rstname)
{
	char bufis[20];
	int i=0;
	string str;

	// convert integer to string for sql (radix 10)
	// bufis,bufiss contains converted string
	_itoa(no,bufis,10);

	str = "RegionName = '";
	str = str + region + "' AND Sector = " + bufis ;
	//demrst.FindFirst(str.c_str());
	no = demrst_ss.GetField("Sector").intVal;
	strcpy(name,demrst_ss.GetField("SectorName").pcVal);

	// NOTE!!!!
	// if there are multiple technologies in the subsector, the share weights 
	// and logit exponential for the first technology are used for the subsector

	// create an array of daorsetbing struct
	// table binding: (assign the field lengths and types)
	// Using recordset that has different field index than original table
	DAORSETBINDING	Bindings[] = {
	//Index Type    Fld	Type	  Offset			 Size
	{dbBindIndexINT, 6, dbBindR8, offsetof(str_ssec,tshrwts), sizeof(double)},
	{dbBindIndexINT, 7, dbBindR8, offsetof(str_ssec,tlexp), sizeof(double)}
	};

	// run C++ GetRowsEx 
	int dper = modeltime.getmaxdataper();
	int	maxrecords = dper*nosubsec; // number of rows is the number of periods
	// these structures won't work; create subsector share weight and logit exponential 
	// structure
	lpstr_ssec pstr_ssecRows = new str_ssec[maxrecords];
	LONG lNumRecords, lYear;
	// cannot use maxrecords because not const int
	TCHAR pBuf[100 * 20]; // buffer for use with variable length text fields only
	
	lNumRecords = demrst_ss.GetRowsEx(pstr_ssecRows, sizeof(str_ssec),
		  &Bindings[0], sizeof(Bindings) / sizeof(DAORSETBINDING),
		  pBuf, sizeof(pBuf), maxrecords); // get year rows for each subsector

	// Step through the returned rows and assign to subsector
	for (i=0;i<nosubsec;i++) {
		for (lYear = 0; lYear < dper; lYear++) {
  		    // pstr_ssecRows is a variant
			subsec[i].setall(&pstr_ssecRows[(i*dper)+lYear],lYear);
		}
		// initialize for periods greater than last data period
		// last model period for last data per
		int m1=modeltime.getdata_to_mod(dper-1);
		if(modeltime.getendyr() > modeltime.getper_to_yr(m1)) {
			for (int j=m1+1;j<modeltime.getmaxper();j++)
				subsec[i].copytolast(j);
		}
	}
	demrst_ss.MoveNext(); // row is last record so move to next row
	delete [] pstr_ssecRows; // free memory
}

void sector::init_stech(char* region,int tregno)
{
	// set number of technologies and initialize technology data
	for (int i=0;i<nosubsec;i++) {
		subsec[i].dbreadstech(region,no);
	}
}

void sector::init_dtech(char* region,int tregno)
{
	// set number of technologies and initialize technology data
	for (int i=0;i<nosubsec;i++) {
		subsec[i].dbreaddtech(region,no);
	}
}

// pass along carbon taxes
void sector::applycarbontax(double tax, int per)
{
	int i=0;
	for (i=0;i<nosubsec;i++) {
		subsec[i].applycarbontax(tax,per);
	}
}

// sets ghg tax to technologies
void sector::addghgtax(int ghgno,char* ghgname,int country_id,int per)
{
	for (int i=0;i<nosubsec;i++) {
		subsec[i].addghgtax(ghgno,ghgname,country_id,per);
	}
}

// calculate subsector shares
void sector::calc_share(char* varcountry,int country_id,int per)
{
	int i=0;
	double sum = 0.0;
	for (i=0;i<nosubsec;i++) {
		// determine subsector shares based on technology shares
		subsec[i].calc_share(varcountry,country_id,per);
		sum += subsec[i].showshare(per);
	}
	// normalize subsector shares to total 100 %
	for (i=0;i<nosubsec;i++)
		subsec[i].norm_share(sum, per);	
}

void sector::price(int per)
{
	sectorprice[per]=0.0;
	for (int i=0;i<nosubsec;i++)
		// calculate weighted average price of subsectors
		// price is calculated when calc_share() is called
		sectorprice[per] += subsec[i].showshare(per)*subsec[i].showprice(per);
}

double sector::showprice(int per)
{
	return sectorprice[per];
}

// sets demand to output and output
void sector::setoutput(char* varcountry,int country_id,double dmd, int per)
{
	carbontaxpaid[per] = 0; // initialize carbon taxes paid

	// clears subsector fuel consumption map
	for (int i=0;i<nosubsec;i++) 
		subsec[i].clearfuelcons(per);
	// clears sector fuel consumption map
	summary[per].clearfuelcons();

	for (i=0;i<nosubsec;i++) {
		// set subsector output from sector demand
		subsec[i].setoutput(varcountry,country_id,dmd,per);
		subsec[i].sumoutput(per);
		carbontaxpaid[per] += subsec[i].showcarbontaxpaid(per);
		// sum subsector fuel consumption for demand sector fuel consumption
		summary[per].updatefuelcons(subsec[i].getfuelcons(per)); 
	}
}

// sum subsector outputs
void sector::sumoutput(int per)
{
	output[per] = 0;
	for (int i=0;i<nosubsec;i++) {
		output[per] += subsec[i].getoutput(per);
	}
}

// set supply sector total output and technology output
void sector::supply(char* varcountry,int country_id,int per)
{	
	carbontaxpaid[per] = 0; // initialize carbon taxes paid
	double mrkprice = marketplace.showprice(no,country_id,per);
	//double mrkdmd = marketplace.showdemand(no,country_id,per);
	double mrkdmd = marketplace.showdemand_good(no,country_id,per);

	if(!Minicam) {  // solve marketplace for intermediate goods
		if (per==0)
			mrkdmd = marketplace.showdemand_good(no,country_id,per);
		else
			mrkdmd = marketplace.showprice_d(no,country_id,per); // actually trial demand
	}

	for (int i=0;i<nosubsec;i++) // clears subsector fuel consumption map
		subsec[i].clearfuelcons(per);

	summary[per].clearfuelcons(); // clears sector fuel consumption map

	// calculate output from technologies that are exogenously driven
	// such as hydro electricity
	double exog_prod = 0;
	for (i=0;i<nosubsec;i++) {
		exog_prod += subsec[i].exog_supply(per);
	}

	mrkdmd -= exog_prod; // subtract from total demand the exogenous portion

	for (i=0;i<nosubsec;i++) {
		// set subsector output from sector demand
		subsec[i].setoutput(varcountry,country_id,mrkdmd,per);
		subsec[i].sumoutput(per);
		carbontaxpaid[per] += subsec[i].showcarbontaxpaid(per);
		// sum subsector fuel consumption for supply sector fuel consumption
		summary[per].updatefuelcons(subsec[i].getfuelcons(per)); 
	}
}

void sector::show(void)
{
	int i=0;
	int m=0; // per = 0
	//write to file or database later
	cout << no <<"Sector: " << name<<"\n";
	cout << "Number of Subsectors: " << nosubsec <<"\n";
	for (i=0;i<nosubsec;i++)
		cout<<"Share["<<i<<"] "<<subsec[i].showshare(m)<<"\n";
	cout <<"Total Sector Output: " << output[m] <<"\n";

}

// shows all technologies in each subsector
void sector::showsubsec(int per, const char *ofile)
{
	int i=0;

	for (i=0;i<nosubsec;i++) {
		// shows subsector label (name and index)
		subsec[i].showlabel(ofile);
		// write technology info in each subsector to file
		subsec[i].showtechs(per, ofile);
	}
}

// shows sector label (name and index)
void sector::showlabel(const char* ofile)
{
	ofstream outfile;

	outfile.open(ofile, ios::app);

		if (!outfile) {
			//open failed
			cerr<<"Cannot open file for output\n";
			exit(-1);
		}

	outfile << "Sector: " << no << " " << name << "\n";
	outfile << "Number of Subsectors: " << nosubsec << "\n";
	outfile.close();
}

void sector::setbaseser(char* region,const char* dbtname)
{
	// function protocol
	void dbmodelread(double* temp,string region,string var1name,string var2name);
	double tmpval[1];

	// reads in data for sectors
	dbmodelread(tmpval,region,name,"base");
	output[0] = tmpval[0]; // base year is per=0
}

void sector::setbaseshr(char* region,const char* dbtname)
{	
	for (int i=0;i<nosubsec;i++)
		subsec[i].setbaseshr(region,name,dbtname);
}

int sector::shownosubsec(void)
{
	return nosubsec;
}

char* sector::showsubsecname(int iss)
{
	return subsec[iss].showname();
}

double sector::getoutput(int per)
{
	return output[per]; // returns sector output
}

// calculate GHG emissions for each sector from subsectors
void sector::emission(int per)
{
	summary[per].clearemiss(); // clear emissions map
	summary[per].clearemfuelmap(); // clear emissions map
	for (int i=0;i<nosubsec;i++) {
		subsec[i].emission(per,name);
		summary[per].updateemiss(subsec[i].getemission(per));
		summary[per].updateemfuelmap(subsec[i].getemfuelmap(per));
	}
}

// calculate indirect GHG emissions for each sector from subsectors
void sector::indemission(int per)
{
	summary[per].clearemindmap(); // clear emissions map
	for (int i=0;i<nosubsec;i++) {
		subsec[i].indemission(per);
		summary[per].updateemindmap(subsec[i].getemindmap(per));
	}
}

// returns sectoral primary energy consumption
double sector::showpe_cons(int per)
{
	pe_cons[per] = 0;
	for (int i=0;i<nosubsec;i++) {
		pe_cons[per] += subsec[i].showpe_cons(per);
	}
	return pe_cons[per];
}

// sums subsector primary and final energy consumption
void sector::suminput(int per)
{
	input[per] = 0;
	for (int i=0;i<nosubsec;i++)
		input[per] += subsec[i].showinput(per);
}

// returns sectoral energy consumption
double sector::showinput(int per)
{
	return input[per];
}

// write sector output to database
void sector::outputdb(const char* regname,int reg)
{
	// function protocol
	void dboutput2(string varreg,string var1name,string var2name,string var3name,
			  string var4name,vector<double> dout,string uname);

	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	// total sector output
	dboutput2(regname,name,"all subsectors"," ","production",output,"EJ");
	// total sector eneryg input
	dboutput2(regname,name,"all subsectors"," ","energy input",input,"EJ");
	// sector price
	dboutput2(regname,name,"all subsectors"," ","ave cost",sectorprice,"$/Service");
	// sector carbon taxes paid
	dboutput2(regname,name,"all subsectors"," ","carbon taxes paid",carbontaxpaid,"Mil90$");
	
	// do for all subsectors in the sector
	for (int i=0;i<nosubsec;i++) {
		// output or demand for each technology
		subsec[i].outputdb(regname,reg,name);
	}
}

// write sector output to database
void sector::outputfile(const char *regname,int reg)
{
	// function protocol
	void fileoutput3(int regno,string var1name,string var2name,string var3name,
				  string var4name,string var5name,string uname,vector<double> dout);
	
	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	// total sector output
	fileoutput3(reg,regname,name," "," ","production","EJ",output);
	// total sector eneryg input
	fileoutput3(reg,regname,name," "," ","consumption","EJ",input);
	// sector price
	fileoutput3(reg,regname,name," "," ","price","$/GJ",sectorprice);
	// sector carbon taxes paid
	fileoutput3(reg,regname,name," "," ","C tax paid","Mil90$",carbontaxpaid);
}

// for writing out subsector results from demand sector
void sector::MCoutput_subsec(const char *regname,int reg)	
{	// do for all subsectors in the sector
	for (int i=0;i<nosubsec;i++) {
		// output or demand for each technology
		subsec[i].MCoutputB(regname,reg,name);
		subsec[i].MCoutputC(regname,reg,name);
	}
}

// write MiniCAM style sector output to database
void sector::MCoutput(const char *regname,int reg)
{
	// function protocol
	void fileoutput4(string var1name,string var2name,string var3name,string var4name,
			         vector<double> dout);
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout);
	
	// total sector output
	dboutput4(regname,"Secondary Energy Prod","by Sector",name,"EJ",output);
	dboutput4(regname,"Secondary Energy Prod",name,"zTotal","EJ",output);
	
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	string str; // temporary string

	// sector fuel consumption by fuel type
	typedef map<string,double>:: const_iterator CI;
	map<string,double> tfuelmap = summary[0].getfuelcons();
	for (CI fmap=tfuelmap.begin(); fmap!=tfuelmap.end(); ++fmap) {
		for (int m=0;m<maxper;m++) {
			temp[m] = summary[m].get_fmap_second(fmap->first);
		}
		dboutput4(regname,"Fuel Consumption",name,fmap->first,"EJ",temp);
	}

	// sector emissions for all greenhouse gases
	map<string,double> temissmap = summary[0].getemission(); // get gases for per 0
	for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
		for (int m=0;m<maxper;m++) {
			temp[m] = summary[m].get_emissmap_second(gmap->first);
		}
		str = "Sec: "; // sector heading
		str+= name; // sector name
		dboutput4(regname,"Emissions",str,gmap->first,"MTC",temp);
	}
	// CO2 emissions by sector
	for (int m=0;m<maxper;m++) {
		temp[m] = summary[m].get_emissmap_second("CO2");
	}
	dboutput4(regname,"CO2 Emiss","by Sector",name,"MTC",temp);
	dboutput4(regname,"CO2 Emiss",name,"zTotal","MTC",temp);

	// CO2 indirect emissions by sector
	for (m=0;m<maxper;m++) {
		temp[m] = summary[m].get_emindmap_second("CO2");
	}
	dboutput4(regname,"CO2 Emiss(ind)",name,"zTotal","MTC",temp);

	// sector price
	dboutput4(regname,"Price",name,"zSectorAvg","$/GJ",sectorprice);
	// sector price
	dboutput4(regname,"Price","by Sector",name,"$/GJ",sectorprice);
	// sector carbon taxes paid
	dboutput4(regname,"General","CarbonTaxPaid",name,"$",carbontaxpaid);
	// do for all subsectors in the sector
	for (int i=0;i<nosubsec;i++) {
		// output or demand for each technology
		subsec[i].MCoutputA(regname,reg,name);
		subsec[i].MCoutputC(regname,reg,name);
	}
}
	
// write subsector output to database
void sector::subsec_outfile(const char *regname,int reg)
{
	// do for all subsectors in the sector
	for (int i=0;i<nosubsec;i++) {
		// output or demand for each technology
		subsec[i].outputfile(regname,reg,name);
	}
}

void sector::set_ser_dmd(double dmd, int per)
{
	output[per] = dmd;
}

// returns total sector carbon taxes paid
double sector::showcarbontaxpaid(int per)
{
	return carbontaxpaid[per];
}

//  gets fuel consumption map in summary object
map<string, double> sector::getfuelcons(int per) 
{
	return summary[per].getfuelcons();
}

//  gets second of fuel consumption map in summary object
double sector::getfuelcons_second(int per,string key) 
{
	return summary[per].get_fmap_second(key);
}

// clears fuel consumption map in summary object
void sector::clearfuelcons(int per) 
{
	summary[per].clearfuelcons();
}

//  get ghg emissions map in summary object
map<string, double> sector::getemission(int per) 
{
	return summary[per].getemission();
}

//  get ghg emissions map in summary object
map<string, double> sector::getemfuelmap(int per) 
{
	return summary[per].getemfuelmap();
}

//**********************************
// demand sector method definitions
//**********************************

// set vector size
void demsector::initper_ds(void)
{
	int maxper = modeltime.getmaxper();
	fe_cons.resize(maxper); // end-use sector final energy consumption
	service.resize(maxper); // total end-use sector service 
}

// aggrgate sector demand function
void demsector::aggdemand(char* varcountry,int country_id,double prc,double x,double ie,
						  int per)
{
	double ser_dmd, base, ser_dmd_adj;
	double pelasticity = -0.9;
	
	//ielasticity = 1.0;
	ielasticity = 0.5;
	//ielasticity = 0.7;
	//aeei = 0.1;
	aeei = 0.05;
	base = getoutput(0);

	// demand function for each sector
	// demand for service
	if (per == 0) 
		ser_dmd = base; // output is already initialized
	//	ser_dmd = base*pow(prc,pelasticity)*pow(x,ielasticity);
	else {
		//ser_dmd = base*pow(prc,pelasticity)*pow(x,ielasticity);
		ser_dmd = base*pow(prc,pelasticity)*pow(x,ie);
	}

	// demand sector output is total end-use sector demand for service
	service[per] = ser_dmd; 
	ser_dmd_adj = ser_dmd/pow(1+aeei,modeltime.gettimestep(per));

	set_ser_dmd(ser_dmd,per); // sets the output
	// sets subsector outputs, technology outputs, and market demands
	setoutput(varcountry,country_id,ser_dmd,per);
	sumoutput(per);
}

// write sector output to database
void demsector::outputfile(const char *regname,int reg)
{
	int maxper = modeltime.getmaxper();
	int m=0;
	vector<double> temp(maxper);
	// function protocol
	void fileoutput3(int regno,string var1name,string var2name,string var3name,
				  string var4name,string var5name,string uname,vector<double> dout);
	
	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	// total sector output
	for (m=0;m<maxper;m++)
		temp[m] = sector::getoutput(m);
	fileoutput3(reg,regname,showname()," "," ","prodution","SerUnit",temp);
	// total sector eneryg input
	for (m=0;m<maxper;m++)
		temp[m] = sector::showinput(m);
	fileoutput3(reg,regname,showname()," "," ","consumption","EJ",temp);
	// sector price
	for (m=0;m<maxper;m++)
		temp[m] = sector::showprice(m);
	fileoutput3(reg,regname,showname()," "," ","price","$/Service",temp);
	// sector carbon taxes paid
	for (m=0;m<maxper;m++)
		temp[m] = sector::showcarbontaxpaid(m);
	fileoutput3(reg,regname,showname()," "," ","C tax paid","Mil90$",temp);
	
}

// write MiniCAM style demand sector output to database
void demsector::MCoutput(const char *regname,int reg)
{
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);

	// function protocol
	void fileoutput4(string var1name,string var2name,string var3name,string var4name,
			         vector<double> dout);
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout);
	
	string secname = sector::showname();
	string str; // temporary string

	// total sector output
	for (int m=0;m<maxper;m++) {
		temp[m] = sector::getoutput(m);
	}
	dboutput4(regname,"End-Use Service","by Sector",secname,"Ser Unit",temp);
	dboutput4(regname,"End-Use Service",secname,"zTotal","Ser Unit",temp);
	
	// sector fuel consumption by fuel type
	typedef map<string,double>:: const_iterator CI;
	map<string,double> tfuelmap = sector::getfuelcons(m=0);
	for (CI fmap=tfuelmap.begin(); fmap!=tfuelmap.end(); ++fmap) {
		for (m=0;m<maxper;m++) {
			temp[m] = sector::getfuelcons_second(m,fmap->first);
		}
		dboutput4(regname,"Fuel Consumption",secname,fmap->first,"EJ",temp);
	}

	// sector emissions for all greenhouse gases
	map<string,double> temissmap = summary[0].getemission(); // get gases for per 0
	for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
		for (int m=0;m<maxper;m++) {
			temp[m] = summary[m].get_emissmap_second(gmap->first);
		}
		str = "Sec: "; // sector heading
		str+= secname; // sector name
		dboutput4(regname,"Emissions",str,gmap->first,"MTC",temp);
	}

	// CO2 emissions by sector
	for (m=0;m<maxper;m++) {
		temp[m] = summary[m].get_emissmap_second("CO2");
	}
	dboutput4(regname,"CO2 Emiss","by Sector",secname,"MTC",temp);
	dboutput4(regname,"CO2 Emiss",secname,"zTotal","MTC",temp);

	// CO2 indirect emissions by sector
	for (m=0;m<maxper;m++) {
		temp[m] = summary[m].get_emindmap_second("CO2");
	}
	dboutput4(regname,"CO2 Emiss(ind)",secname,"zTotal","MTC",temp);

	// sector price
	for (m=0;m<maxper;m++) {
		temp[m] = sector::showprice(m);
	}
	dboutput4(regname,"Price",secname,"zSectorAvg","$/Ser",temp);
	dboutput4(regname,"Price","by End-Use Sector",secname,"$/Ser",temp);
	// sector carbon taxes paid
	for (m=0;m<maxper;m++) {
		temp[m] = sector::showcarbontaxpaid(m);
	}
	dboutput4(regname,"General","CarbonTaxPaid",secname,"$",temp);
	// do for all subsectors in the sector
	MCoutput_subsec(regname,reg);
}
	
