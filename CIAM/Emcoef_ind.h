#ifndef _EMCOEF_IND_H_
#define _EMCOEF_IND_H_
#pragma once

#include <string>
#include <map>
using namespace std; // enables elimination of std::

/*! 
* \ingroup CIAM
* \brief This class contains the indirect Greenhouse gas emissions coefficients.
*
* Each object contains a map object of emissions
* coefficients, and each emisscoef_ind object is
* used to represent the secondary energy sector.
* \author Sonny Kim
* \date $ Date $
* \version $ Revision $
*/

class Emcoef_ind
{
private:
	string name; //!< name of secondary good or sector
	map<string, double> emcoef; //!< contains all coefficients for all gases
public:
	Emcoef_ind(void); //default construtor
	~Emcoef_ind(void); // destructor
	// set the secondary good or sector name
	void setname(string secname);
	void setemcoef(map<string,double> eminfo, double toutput);
	string getname(void);
	double getemcoef(string gasname);
};

#endif // _EMCOEF_IND_H_