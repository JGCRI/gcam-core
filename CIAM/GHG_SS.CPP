/* ghg_ss.cpp										*
 * This header contains the methods for the		*
 * Greenhouse Gas class.						*
 *       										*
 * SHK  12/11/00								*/


#include "ghg_ss.h"
#include "ghg_ind.h"
#include <iostream>
#include <string>
using namespace std; // enables elimination of std::

extern ghg_ind regGHGcoef; // indirect GHG emissions coefficient

ghg_ss::ghg_ss(void) //default construtor
{
}
	
ghg_ss::ghg_ss(double tco2, double tch4, double tsox, double tn2o)//constructor
{
	CO2 = tco2;
	CH4 = tch4;
	SOX = tsox;
	N2O = tn2o;
}

ghg_ss::~ghg_ss(void) // destructor
{
}

void ghg_ss::setcoefs(void)
{
	// tgC/EJ or MTC/EJ
	co2oilcoef = 18.4;
	co2gascoef = 15.0;
	co2coalcoef = 25.3;
	ch4oilcoef = 1.0;
	ch4gascoef = 1.0;
	ch4coalcoef = 1.0;
}

void ghg_ss::calc_allgases(double fuel,double prod,char* fueltype,char* prodtype) 
{
	string fuelname = fueltype;
	string prodname = prodtype;
	double combusted = 0;
	enum Fueltype {oil,gas,coal,syngas,synliq,none};
	Fueltype FT;

	// All crude fuels are assumed to be refined,
	// while all refined fuels are assumed to be combusted
	// with exceptions for synfuels.
	// fuel >= prod due to efficiency losses, account for emissions from conversion
	// hydrogen production sector emits, hydrogen combustion emits no emission.

	if (fuelname == "crude oil") { 
		if (prodname == "hydrogen") {// all carbon released from H2 production
			combusted = fuel;
			CO2ind = combusted * co2oilcoef; 
		}
		else { // rest are refined
			combusted = fuel - prod;
			if (prodname == "refined oil")
				CO2ind = combusted * co2oilcoef; 
		}
		FT = oil; 
	}
	else if	(fuelname == "refined oil" ) { // all refined is combusted
		combusted = fuel;		
		FT = oil; 
	}
	else if (fuelname == "natural gas") {
		if (prodname == "hydrogen") { // all carbon released
			combusted = fuel;
			CO2ind = combusted * co2gascoef; 
		}
		else { // rest are refined
			combusted = fuel - prod;
			if (prodname == "delivered gas")
				CO2ind = combusted * co2gascoef; 
		}
		FT = gas; }
	else if (fuelname == "delivered gas") { // all refined is combusted
		combusted = fuel;
		FT = gas; 
	}
	else if (fuelname == "coal") {
		if (prodname == "hydrogen") { // all carbon released
			combusted = fuel;
			CO2ind = combusted * co2coalcoef; 
		}
		else if (prodname == "delivered gas") { // synfuel conversion to gas
			combusted = fuel - prod;
			FT = syngas; // special calculation for syngas
		}
		else if (prodname == "refined oil") {// synfuel conversion to liquid
			combusted = fuel - prod;
			FT = synliq; // special calculation for synliq
		}
		else { // rest are refined
			combusted = fuel - prod;
			if (prodname == "delivered coal")
				CO2ind = combusted * co2coalcoef; 
			FT = coal; 
		}
	}
	else if (fuelname == "delivered coal") {
		if (prodname == "delivered gas") { // synfuel conversion to gas
			combusted = fuel - prod;
			CO2ind = combusted * co2coalcoef; 
			FT = syngas; // special calculation for syngas
		}
		else if (prodname == "refined oil") {// synfuel conversion to liquid
			combusted = fuel - prod;
			FT = synliq; // special calculation for synliq
		}
		else { // all refined is combusted
			combusted = fuel;
			FT = coal; 
		}
	}
	else FT = none;

	switch (FT) {
	case oil:
		CO2 = combusted * co2oilcoef; 
		CH4 = combusted * ch4oilcoef; // include controls at a later time
		CO2fuel = fuel * co2oilcoef;
		break;
	case gas:
		CO2 = combusted * co2gascoef;
		CH4 = combusted * ch4gascoef;
		CO2fuel = fuel * co2gascoef;
		break;
	case coal:
		CO2 = combusted * co2coalcoef;
		CH4 = combusted * ch4coalcoef;
		CO2fuel = fuel * co2coalcoef; 
		break;
	case syngas:
		CO2 = (fuel-prod)*co2coalcoef+prod*(co2coalcoef-co2gascoef);
		CH4 = combusted * ch4coalcoef;
		CO2fuel = fuel * co2coalcoef; 
		break;
	case synliq:
		CO2 = (fuel-prod)*co2coalcoef+prod*(co2coalcoef-co2oilcoef);
		CH4 = combusted * ch4coalcoef;
		CO2fuel = fuel * co2coalcoef; 
		break;
	case none:
		CO2 = 0;
		CH4 = 0;
		CO2ind = 0;
		CO2fuel = 0;
		break;
	}
}

void ghg_ss::calc_indgases(double fuel,char* fueltype) 
{
	string fuelname = fueltype;
	double test = regGHGcoef.showco2coef(fuelname);
	if (regGHGcoef.showco2coef(fuelname)!=0)
		CO2ind = fuel * regGHGcoef.showco2coef(fuelname);
}


// return N2O emissions 
double ghg_ss::showN2O(void)
{
	return N2O;
}

// return SOX emissions 
double ghg_ss::showSOX(void)
{
	return SOX;
}

// return CH4 emissions 
double ghg_ss::showCH4(void)
{
	return CH4;
}

// return actual CO2 emissions 
double ghg_ss::showCO2(void)
{
	return CO2;
}

// return CO2 emissions equivalent to fuel combusted indirectly
double ghg_ss::showCO2ind(void)
{
	return CO2ind;
}

// return CO2 emissions equivalent to combusting all fuel input
double ghg_ss::showCO2fuel(void)
{
	return CO2fuel;
}

// return CO2 emissions coefficient if applicable
double ghg_ss::showghgcoef(char* fueltype, char* ghgname) 
{
	string fuelname = fueltype;
	string gasname = ghgname;

	// emissions coefficients are in tgC/EJ or MTC/EJ
	if (fuelname == "crude oil") {
		if (gasname == "CO2")
			return co2oilcoef; 
		else if(gasname == "CH4")
			return ch4oilcoef; 
		else
			return 0;
	}
	else if (fuelname == "natural gas") {
		if (gasname == "CO2")
			return co2gascoef; 
		else if(gasname == "CH4")
			return ch4gascoef; 
		else
			return 0;
	}
	else if (fuelname == "coal") {
		if (gasname == "CO2")
			return co2coalcoef; 
		else if(gasname == "CH4")
			return ch4coalcoef; 
		else
			return 0;
	}
	else 
		return 0;
}

