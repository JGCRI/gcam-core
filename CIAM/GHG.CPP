/*! ghg.cpp											*
 * This header contains the methods for the			*
 * Greenhouse Gas class.							*
 *       											*
 * Coded by: Sonny Kim and Marshall Wise 10/09/02	*/


#include <string>
#include <map>
#include <vector>
#include <math.h>
#include "ghg.h"
#include "Emcoef_ind.h" // indirect greenhouse gas emissions coefficient class
using namespace std; // enables elimination of std::

// map of CO2 emissions coefficient for primary fuel only
extern map<string, double> co2coefpri;
// map of CO2 emissions coefficient for all fossil fuels
extern map<string, double> co2coefall;
// indirect CO2 emissions coefficient
// coefficients for secondary energy only
extern map<string, double> co2coefind; 
extern vector<Emcoef_ind> emcoef_ind;

Ghg::Ghg(void) //default construtor
{
}
	
Ghg::~Ghg(void) // destructor
{
}

// set name, unit, remove fraction, gwp, and emiss coefficient of gas
void Ghg::init(string gasname,string gasunit,double frac,double emcoef,double tgwp) 
{
	name = gasname;
	unit = gasunit;
	rmfrac = frac;
	emiss_coef = emcoef;
	gwp = tgwp;
}
	
// initial all attributes from data
void Ghg::initialize(strGhg strghg) 
{
	// strghg is a temporary structure
	name = strghg.tname;
	unit = strghg.tunit;
	emiss_coef = strghg.temiss_coef;
}

// initial all attributes from data using pointers
void Ghg::initializeptr(lpstrGhg lpstrghg) 
{
	// strghg is a temporary structure
	name = lpstrghg->tname;
	unit = lpstrghg->tunit;
	emiss_coef = lpstrghg->temiss_coef;
}

// set emissions coefficient from data
void Ghg::setcoef(double em_coef)
{
	emiss_coef = em_coef; // set attribute
}

// set remove fraction from data
void Ghg::setrmfrac(double trmfrac)
{
	rmfrac = trmfrac;	
}

//convert carbon tax to energy tax
double Ghg::taxcnvrt(string fuelname)
{
	double conversion; 
	if (name == "CO2") {
		// use map object for coefficient
		conversion = (1.0 - rmfrac) * gwp * co2coefpri[fuelname];
	}
	// for all other gases used read-in emissions coefficient
	else {
		// apply carbon equivalent to emiss coefficienr
		//conversion = (1.0 - rmfrac) * gwp * emiss_coef;
		conversion = 0;
	}
	return conversion;
}
	//calculate Ghg emissions
void Ghg::calc_emiss(string fuelname,double input,string prodname,double output)
{
	// for CO2 use default emissions coefficient by fuel
	// remove fraction only applicable for CO2
	if (name == "CO2") {
		// use map object for coefficient
		// 100% efficiency and same coefficient, no emissions
		if (input==output && co2coefall[fuelname]==co2coefall[prodname]) {
			emission = 0;
			emiss_gwp = 0;
			seq_emiss = 0;
			emiss_fuel = (1.0-rmfrac)*input*co2coefall[fuelname];
		}
		else {
			// sequestered emissions
			seq_emiss = rmfrac * ((input*co2coefall[fuelname])
			                       - (output*co2coefall[prodname]));
			emission = (1.0 - rmfrac) * ((input*co2coefall[fuelname])
			                       - (output*co2coefall[prodname]));
			emiss_gwp = (1.0 - rmfrac) * gwp * ((input*co2coefall[fuelname])
			                              - (output*co2coefall[prodname]));
			emiss_fuel = (1.0-rmfrac)*input*co2coefall[fuelname];
		}
	}
	// for all other gases used read-in emissions coefficient
	else {
		seq_emiss = rmfrac * (input-output) * emiss_coef;
		emission = (1.0 - rmfrac) * (input-output) * emiss_coef;
		emiss_gwp = (1.0 - rmfrac) * gwp * (input-output) * emiss_coef;
		emiss_fuel = (1.0 - rmfrac) * input * emiss_coef;
	}
}

// calculate emissions alternative
void Ghg::calc_emiss2(double activity,double gamma,double* cntl) 
{
	emission = pow((emiss_coef*activity),gamma)
			  * (1 - cntl[1]) * (1 - cntl[2]) * (1 - cntl[3]);
}
	
// calculates emissions associated with the use of secondary
// energy, get indirect emissions coefficient from map object
void Ghg::calc_emiss_ind(double input,string fuelname) 
{
	emiss_ind = 0; // to initialize
	for (int i=0;i<emcoef_ind.size();i++) {
		if (emcoef_ind[i].getname() == fuelname) { // sector name
			double temp = emcoef_ind[i].getemcoef(name);
			emiss_ind = emcoef_ind[i].getemcoef(name) * input;
		}
	}
}

// return name of ghg
string Ghg::getname(void)
{
	return name;
}

// return unit for ghg
string Ghg::getunit(void)
{
	return unit;
}

// return ghg emissions
double Ghg::getemission(void)
{
	return emission;
}

// return sequestered ghg emissions
double Ghg::getseq_emiss(void)
{
	return seq_emiss;
}

// return ghg emissions inplicit in fuel
double Ghg::getemiss_fuel(void)
{
	return emiss_fuel;
}

// return indirect ghg emissions
double Ghg::getemiss_ind(void)
{
	return emiss_ind;
}

// return ghg emissions coefficient
double Ghg::getemiss_coef(void)
{
	return emiss_coef;
}
