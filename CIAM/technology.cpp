/* technology.cpp									*
 * Method definition for the technology class.		*
 * This technology class is based on the MiniCAM	*
 * description of technology.						*
 * SHK  6/12/00										*/

#include <stdlib.h>
#include <string>
#include <fstream>
#include <iostream>
#include <algorithm>
#include <functional>
#include <math.h>
//** Database Headers *****
#include <afxdisp.h>
#include <dbdao.h>
#include <dbdaoerr.h>

#include "market.h"
#include "technology.h"
#include "modeltime.h"

extern Marketplace marketplace;
extern Modeltime modeltime;
extern CdbDatabase db;
extern CdbRecordset techghgrst;

// Technology class method definition

// default constructor
technology::technology(void) 
{
}

// constructor
technology::technology(const char *nstr,const char *ustr,int yr,double ef)
{
	strcpy(name,nstr);
	unit = ustr;
	year = yr;
	eff = ef;
}

// destructor
technology::~technology(void)
{
}

// initialize technology parameters
void technology::setall(lpstrtech tempstr) 
{
	// read in data for technology from file or database
	no = tempstr->tno;
	strcpy(name,tempstr->tname);
	year = tempstr->yr;
	fueltype = tempstr->ftype;
	strcpy(fuelname,tempstr->fname);
	shrwts = tempstr->shrs;
	eff = tempstr->teff;
	necost = tempstr->tnecost;
	tax = tempstr->ttax;
	lexp = tempstr->tlexp;
	techchange = tempstr->ttech;
	technology::setghg();
}

// initialize technology parameters
void technology::setall3(lpstrtech2 tempstr) 
{
	// read in data for technology from file or database
	no = tempstr->tno;
	strcpy(name,tempstr->tname);
	year = tempstr->yr;
	fueltype = tempstr->ftype;
	strcpy(fuelname,tempstr->fname);
	shrwts = tempstr->shrs;
	eff = tempstr->teff;
	necost = tempstr->tnecost;
	tax = tempstr->ttax;
	lexp = tempstr->tlexp;
	techchange = tempstr->ttech;
	resource = tempstr->tresource;
	A = tempstr->tA;
	B = tempstr->tB;
	technology::setghg();
}

// initialize technology parameters (alternative)
void technology::setall2(strtech tempstr) 
{
	// read in data for technology from file or database
	no = tempstr.tno;
	strcpy(name,tempstr.tname);
	year = tempstr.yr;
	fueltype = tempstr.ftype;
	strcpy(fuelname,tempstr.fname);
	shrwts = tempstr.shrs;
	eff = tempstr.teff;
	necost = tempstr.tnecost;
	tax = tempstr.ttax;
	lexp = tempstr.tlexp;
}

// set number of greenhouse gases for each technology
void technology::setghg(void)
{
	ghg.resize(1); // at least one for CO2
	ghg[0].init("CO2","MTC",0,0,1); //gas,unit,rmfrac,emcoef,gwp
}

// set number of greenhouse gases for each technology
void technology::settechghg(int regionno,int sectorno,int subsecno)
{
	CdbRecordset rst;
	char bufreg[20]; // int to char for region
	char bufsec[20]; // int to char for sector
	char bufsubsec[20]; // int to char for subsector
	char buftech[20]; // int to char for technology
	string str; // temporary string

	int techno = no;
	int numghg=1; // default is 1 for CO2

	// convert integer to string for sql (radix 10)
	// buffer contains converted string
	_itoa(regionno,bufreg,10);
	_itoa(sectorno,bufsec,10);
	_itoa(subsecno,bufsubsec,10);
	_itoa(techno,buftech,10);

	// determine number of sectors in dataset
	//str = "SELECT * FROM (SELECT DISTINCT Ghg FROM techghg ";
	str = "SELECT * FROM techghg";
	str += " WHERE (Region = ";
	str += bufreg;
	str += " AND Sector = ";
	str += bufsec;
	str += " AND Subsector = ";
	str += bufsubsec;
	str += " AND Technology = ";
	str += buftech;
	str += ")";
	try {
		rst = db.OpenRecordset(str.c_str());
	}
	catch(...) {
		cerr <<"\nproblem while calling countdbrec3()\n";
	}

	while (!rst.GetEOF()) {
		str = rst.GetField("GhgName").pcVal;
		if (str == "CO2") {
			double test = rst.GetField("Rmfrac").dblVal;
			ghg[0].setrmfrac(rst.GetField("Rmfrac").dblVal);
			rst.MoveNext();
		}
		else {
			numghg++; // in addition to CO2
			ghg.resize(numghg); // at least one for CO2
			ghg[numghg-1].init(
				rst.GetField("GhgName").pcVal, // gas name
				"MTC",  // gas unit
				rst.GetField("Rmfrac").dblVal,
				rst.GetField("Em_Coef").dblVal,
				rst.GetField("GWP").dblVal);
			rst.MoveNext();
		}
	}
	rst.Close(); // close the recordset
}

// apply carbon tax to appropriate technology
void technology::applycarbontax(double tax)
{
	// convert tax from $/carbon unit to $/energy unit
	// if fuel does not contain carbon, emissions coefficient
	// is zero and so is the carbon tax
	// units: tax (90$/TC), CO2coef (MTC/EJ), carbontax (75$/GJ)
	carbontax = tax;

	// returns emissions coefficient only if fuels are primary fuels
	// crude oil, natural gas and coal
	// add to previous ghg tax if more than one ghg
	for(int i=0;i<ghg.size();i++) {
		carbontaxgj += carbontax*ghg[i].taxcnvrt(fuelname)*1e-3;
	}
}

// sets ghg tax to technologies
// does not get called if there are no markets for ghgs
void technology::addghgtax(int ghgno,char* ghgname,int country_id,int per)
{
	// returns coef for primary fuels only
	// carbontax has value for primary fuels only
	carbontax = marketplace.showprice(ghgno,country_id,per);
	// add to previous ghg tax if more than one ghg
	for(int i=0;i<ghg.size();i++) {
		carbontaxgj += carbontax*ghg[i].taxcnvrt(fuelname)*1e-3;
	}
	// need to add taxes from all ghgs
}

// define and return technology cost
double technology::cost(int country_id,int per) 
{
	double fuelprice = 0;

	fuelprice = marketplace.showprice(fueltype,country_id,per);
	//techcost = fprice/eff/pow(1+techchange,modeltime.gettimestep(per)) + necost;
	techcost = (fuelprice+carbontaxgj)/eff + necost;
	return techcost;
}

// calculate technology shares
void technology::calc_share(int country_id,int per)
{
	// use pow(x,y) == x**y in fortran, x and y are double, need math.h
	share = shrwts * pow(cost(country_id,per),lexp);
}

// normalize technology shares
void technology::norm_share(double sum) 
{
	if (sum==0)
		share = 0;
	else
		share /= sum;
}

// calculates fuel input and technology output
void technology::production(double dmd,int per) 
{
	string hydro = "hydro";
	// dmd is total subsector demand
	if(name != hydro) {
		output = share * dmd; // use share to get output for each technology
	}
	else { // do for hydroelectricity
		int T = per*modeltime.gettimestep(per);
		// resource and logit function 
		double fact = exp(A+B*T);
		output = resource*fact/(1+fact);
	}

	if (fueltype == 0) // fueltye=0 reserved for renewables
		input = output/eff;
	else {// demand for fossil, uranium and secondary energy
		//input = output/eff/pow(1+techchange,timestep);
		input = output/eff;
	}

	// total carbon taxes paid
	// carbontax is null for technologies that do not consume fossil fuels
	// input(EJ), carbontax(90$/GJ), carbontaxpaid(90$Mil)
	carbontaxpaid = input*carbontaxgj*1e+3;
}

// calculate GHG emissions from technology use
void technology::emission(char* prodname)
{
	// alternative ghg emissions calculation
	//for_each(ghg.begin(),ghg.end(),ghg.calc_emiss());
	emissmap.clear(); // clear emissions map
	emfuelmap.clear(); // clear emissions map
	for (int i=0; i<ghg.size(); i++) {
		ghg[i].calc_emiss(fuelname,input,prodname,output);
		emissmap[ghg[i].getname()] = ghg[i].getemission();
		emfuelmap[ghg[i].getname()] = ghg[i].getemiss_fuel();
	}
}

// calculate indirect GHG emissions from technology use
void technology::indemission(void)
{
	emindmap.clear(); // clear emissions map
	for (int i=0; i<ghg.size(); i++) {
		ghg[i].calc_emiss_ind(input,fuelname);
		emindmap[ghg[i].getname()] = ghg[i].getemiss_ind();
	}
}

// show technology info
void technology::show_tech(void) 
{
	//writes to screen
	cout << no <<"  " << name<<"\n";
	cout <<"Year: "<< year<<"\n";
	cout <<"Fuel Type: "<< fueltype<<"\n";
	cout <<"Share Weights: "<< shrwts<<"\n";
	cout <<"Energy Efficiency: "<<eff<<"\n";
	cout <<"Non-Energy Cost: "<<necost<<"\n";
	cout <<"Tax: "<<tax<<"\n";
	cout <<"Logit Exponential: "<<lexp<<"\n";
	cout <<"Technical Change: "<<techchange<<"\n\n";
}

// show technology info
void technology::show_techf(const char *ofile) 
{
	//writes to file
	ofstream outfile;
	outfile.open(ofile,ios::app);
	if (!outfile) {
		//open failed
		cerr<<"Cannot open file for output\n";
		exit(-1);
	}
	outfile << "Technology: "<<no <<"  " << name<<"\n";
	outfile <<"Year: "<< year<<"\n";
	outfile <<"Fuel Type: "<< fueltype<<"\n";
	outfile <<"Share Weights: "<< shrwts<<"\n";
	outfile <<"Energy Efficiency: "<<eff<<"\n";
	outfile <<"Non-Energy Cost: "<<necost<<"\n";
	outfile <<"Tax: "<<tax<<"\n";
	outfile <<"Logit Exponential: "<<lexp<<"\n";
	outfile <<"Technical Change: "<<techchange<<"\n\n";
	
	outfile.close();
}

// return technology name
char* technology::showname(void) 
{
	return name;
}

// return fuel name
char* technology::getfname(void) 
{
	return fuelname;
}

// return fuel type 
int technology::showfuelno(void) 
{
	return fueltype;
}

// return fuel efficiency
double technology::showeff(void)
{
	return eff;
}

// return technology share
double technology::showshare(void)
{
	return share;
}

// return fuel input for technology
double technology::showinput(void)
{
	return input;
}

// return output of technology
double technology::showoutput(void)
{
	return output;
}

// return the cost of technology
double technology::showtechcost(void)
{
	return techcost;
}

// return the non-energy cost of the technology
double technology::shownecost(void)
{
	return necost;
}

// return carbon taxes applied to technology
double technology::showcarbontax(void)
{
	// ($/TC)
	return carbontax;
}

// return carbon taxes applied to technology
double technology::showcarbontaxgj(void)
{
	// ($/GJ)
	return carbontaxgj;
}

// return carbon taxes paid by technology
double technology::showcarbontaxpaid(void)
{
	return carbontaxpaid;
}

// returns actual CO2 emissions from technology, alternate
double technology::getCO2(void) 
{
	return ghg[0].getemission(); // index 0 is for CO2
}

// return Ghg object for initialization
vector<Ghg> technology::getGhg(void) 
{
	return ghg;
}

// initialize Ghg object from argument 
void technology::setGhg(vector<Ghg> tempghg) 
{
	ghg = tempghg;
}

// return map of all ghg emissions
map<string,double> technology::getemissmap(void)
{
	return emissmap;
}

// return map of all ghg emissions
map<string,double> technology::getemfuelmap(void)
{
	return emfuelmap;
}

// return map of all ghg emissions
map<string,double> technology::getemindmap(void)
{
	return emindmap;
}

// return value for ghg
double technology::get_emissmap_second(string str)
{
	return emissmap[str];
}

// returns technology logit exponential
double technology::getlexp(void) 
{
	return lexp;
}

// set input from inherited objects
void technology::setinput(double in) 
{
	input = in;
}

// set output from inherited objects
void technology::setoutput(double out) 
{
	output = out;
}


//  ******* method definition for hydro_tech

// initialize technology parameters
void hydro_tech::setall3(lpstrtech2 tempstr) 
{
	// read in data for technology from file or database
	technology::setall3(tempstr) ;
	// plus additional stuff
}

// calculates hydroelectricity output based on logit function
void hydro_tech::production(double dmd,int per) 
{
	int T = per*modeltime.gettimestep(per);
	// resource and logit function 
	double fact = exp(A+B*T);
	technology::setoutput(resource*fact/(1+fact));
	technology::setinput(0); // no fuel input for hydro
}
