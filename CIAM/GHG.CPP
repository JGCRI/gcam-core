/*! ghg.cpp											*
 * This header contains the methods for the			*
 * Greenhouse Gas class.							*
 *       											*
 * Coded by: Sonny Kim and Marshall Wise 10/09/02	*/


#include "ghg.h"
#include <string>
#include <map>
#include <math.h>
using namespace std; // enables elimination of std::

// map of CO2 emissions coefficient for primary fuel only
map<string, double> co2coefpri;
// map of CO2 emissions coefficient for all fossil fuels
map<string, double> co2coefall;
// indirect CO2 emissions coefficient
// coefficients for secondary energy only
map<string, double> co2coefind; 

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

// set default emissions coefficient for CO2
void Ghg::setCO2coef(void)
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
		double c1 = co2coefall[fuelname];
		double c2 = co2coefall[prodname];
		if (input==output && co2coefall[fuelname]==co2coefall[prodname]) {
			emission = 0;
			emiss_gwp = 0;
			seq_emiss = 0;
		}
		else {
			// sequestered emissions
			seq_emiss = rmfrac * ((input*co2coefall[fuelname])
			                       - (output*co2coefall[prodname]));
			emission = (1.0 - rmfrac) * ((input*co2coefall[fuelname])
			                       - (output*co2coefall[prodname]));
			emiss_gwp = (1.0 - rmfrac) * gwp * ((input*co2coefall[fuelname])
			                              - (output*co2coefall[prodname]));
		}
	}
	// for all other gases used read-in emissions coefficient
	else {
		seq_emiss = rmfrac * (input-output) * emiss_coef;
		emission = (1.0 - rmfrac) * (input-output) * emiss_coef;
		emiss_gwp = (1.0 - rmfrac) * gwp * (input-output) * emiss_coef;
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
void Ghg::calc_ind_emiss(double input,string fuelname) 
{
	if (co2coefind[fuelname] == 0)
		ind_co2emiss = 0;
	else
		ind_co2emiss = input * co2coefind[fuelname];
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

// return indirect CO2 emissions
double Ghg::getind_co2emiss(void)
{
	return ind_co2emiss;
}

// return ghg emissions coefficient
double Ghg::getemiss_coef(void)
{
	return emiss_coef;
}
