/* Emcoef_ind.cpp									*
 * This file contains methods for the indirect Greenhouse gas	*
 * emissions coefficients class.							*
 * Each class contains a map object of emissions	*
 * coefficients, and each emisscoef_ind object is	*
 * used to represent the secondary energy sector.	*
 * SHK  11/13/02	
								*/
#include "Definitions.h"
#include "Emcoef_ind.h"

Emcoef_ind::Emcoef_ind(void) //default construtor
{
}

Emcoef_ind::~Emcoef_ind(void) // destructor
{
}

// set the secondary good or sector name
void Emcoef_ind::setname(string secname)
{
	name = secname;
}

// set emissions coefficient for gas by providing emission and
// secondary output
void Emcoef_ind::setemcoef(map<string,double> eminfo, double toutput)
{
	typedef map<string,double>:: const_iterator CI;
	for (CI fmap=eminfo.begin(); fmap!=eminfo.end(); ++fmap) {	
		if (toutput!=0) emcoef[fmap->first] = fmap->second/toutput;
	}
}

// get the name of the secondary good or sector
string Emcoef_ind::getname(void)
{
	return name;
}

// return the emission coefficient for the gas
double Emcoef_ind::getemcoef(string gasname)
{
	return emcoef[gasname];
}
