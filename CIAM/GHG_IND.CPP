/* ghg_ind.cpp									*
 * This header contains the methods for the		*
 * indirect Greenhouse Gas emissions class.		*
 *       										*
 * SHK  12/27/00								*/


#include "ghg_ind.h"
#include <iostream>
#include <string>
using namespace std; // enables elimination of std::

ghg_ind::ghg_ind(void) //default construtor
{
}
	
ghg_ind::~ghg_ind(void) // destructor
{	
}

// create array of objects for all intermediate goods
// create an emissions coef for each intermediat good
void ghg_ind::setcoefs(int nossect)
{
	noghgindcoef = nossect;
	ghgindcoef = new str_ghgcoef[nossect];
}

// assign name of intermediate good
void ghg_ind::setindex(int no, string varname)
{
	ghgindcoef[no].name = varname; // associate index with name
}

// calculate indirect CO2 emissions coefficient
void ghg_ind::setco2coef(string varname, double tco2, double toutput)
{
	for (int i=0; i<noghgindcoef; i++)
		if (varname == ghgindcoef[i].name)
			ghgindcoef[i].co2indcoef = tco2/toutput;// tgC/EJ
}

// calculate indirect CH4 emissions coefficient
void ghg_ind::setch4coef(string varname, double tch4, double toutput)
{
	for (int i=0; i<noghgindcoef; i++)
		if (varname == ghgindcoef[i].name)
			ghgindcoef[i].ch4indcoef = tch4/toutput;
}

// return indirect CH4 emissions coefficient
double ghg_ind::showch4coef(string varname)
{
	for (int i=0; i<noghgindcoef; i++)
		if (varname == ghgindcoef[i].name)
			return ghgindcoef[i].ch4indcoef;
	return 0;
}

// return indirect CO2 emissions coefficient
double ghg_ind::showco2coef(string varname)
{
	for (int i=0; i<noghgindcoef; i++)
		if (varname == ghgindcoef[i].name)
			return ghgindcoef[i].co2indcoef;
	return 0;
}

// return number of indirect emissions coefficients
int ghg_ind::shownoghgindcoef(void)
{
	return noghgindcoef;
}
