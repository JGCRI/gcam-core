/* ghg.cpp										*
 * This header contains the methods for the		*
 * Greenhouse Gas class.						*
 *       										*
 * SHK  12/11/00								*/


#include "ghgs.h"
#include <iostream>
#include <string>
using namespace std; // enables elimination of std::

ghgs::ghgs(void) //default construtor
{
}
	
ghgs::ghgs(double tco2, double tch4, double tsox, double tn2o)//constructor
{
	CO2 = tco2;
	CH4 = tch4;
	SOX = tsox;
	N2O = tn2o;
}

ghgs::~ghgs(void) // destructor
{
}

void ghgs::setcoefs(void)
{
	// tgC/EJ
	co2oilcoef = 18.4;
	co2gascoef = 15.0;
	co2coalcoef = 25.3;
	ch4oilcoef = 1.0;
	ch4gascoef = 1.0;
	ch4coalcoef = 1.0;
}

void ghgs::calc_allgases(double fuel, char* fueltype) 
{
	string fuelname = fueltype;
	enum Fueltype {oil,gas,coal,none};
	Fueltype FT;

	if (fuelname == "crude oil") FT = oil;
	else if (fuelname == "natural gas") FT = gas;
	else if (fuelname == "coal") FT = coal;
	else FT = none;

	switch (FT) {
	case oil:
		CO2 = co2oilcoef*fuel; 
		CH4 = ch4oilcoef*fuel; // include controls at a later time
		break;
	case gas:
		CO2 = co2gascoef*fuel;
		CH4 = ch4gascoef*fuel;
		break;
	case coal:
		CO2 = co2coalcoef*fuel;
		CH4 = ch4coalcoef*fuel;
		break;
	case none:
		CO2 = 0;
		CH4 = 0;
		break;
	}
}

double ghgs::showN2O(void)
{
	return N2O;
}

double ghgs::showSOX(void)
{
	return SOX;
}

double ghgs::showCH4(void)
{
	return CH4;
}

double ghgs::showCO2(void)
{
	return CO2;
}
