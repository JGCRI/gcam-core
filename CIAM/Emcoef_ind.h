#ifndef _EMCOEF_IND_H_
#define _EMCOEF_IND_H_
#pragma once

/*! 
* \file Emcoef_ind.h
* \ingroup CIAM
* \brief The Emcoef_ind class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <map>
using namespace std;

/*! 
* \ingroup CIAM
* \brief This class contains the indirect Greenhouse gas emissions coefficients.
*
* Each object contains a map object of emissions
* coefficients, and each emisscoef_ind object is
* used to represent the secondary energy sector.
* \author Sonny Kim
*/

class Emcoef_ind
{
private:
	string name; //!< name of secondary good or sector
	map<string, double> emcoef; //!< contains all coefficients for all gases
public:
	Emcoef_ind();
	void setName( const string& secname );
	void setemcoef( const map<string,double>& eminfo, const double toutput );
	const string& getName() const;
	double getemcoef( const string& gasName ) const;
};

#endif // _EMCOEF_IND_H_