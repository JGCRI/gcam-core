#ifndef _GHG_H_
#define _GHG_H_
#pragma once

/*! 
* \file GHG.H
* \ingroup CIAM
* \brief The Ghg class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <string>

// xerces xml headers
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std;
using namespace xercesc;

// Forward declaration
class Emcoef_ind;

/*! 
* \ingroup CIAM
* \brief The Ghg class describes a single gas with
* attributes of gas name, unit, emissions coefficients,
* and the calculated emissions.
*
* Emissions emitted indirectly through use of technology are also calculated.
* \author Sonny Kim and Marshall Wise
*/

class Ghg
{
private:
	string name; //!< name of ghg gas
	string unit; //!< unit for ghg gas
	double rmfrac; //!< fraction of emissions removed
	double gwp; //!< global warming poential
	double emission; //!< emissions (calculated)
	double seq_emiss; //!< sequestered emissions (calculated)
	double emiss_gwp; //!< gwp emissions (calculated)
	double emiss_coef; //!< emissions coefficient
	double emiss_fuel; //!< implied emissions from total fuel consumption
	double emiss_ind; //!< indirect emissions
public:
	Ghg( const string& nameIn = "", const string& unitIn = "", const double rmfracIn = 0, const double gwpIn = 0, const double emissionIn = 0 );
	void clear();
	void setcoef( const double em_coef ); // set emissions coefficients from data
	void XMLParse( const DOMNode* tempnode ); // initialize ghg object with xml data
	void toXML( ostream& out ) const; // write out xml.
	void toDebugXML( const int period, ostream& out ) const;
	void setrmfrac( const double trmfrac ); // set remove fraction from data
	double taxcnvrt( const string& fuelname ) const; // conversion for carbon tax to energy equivalent
	void calc_emiss( const string& fuelname, const double input, const string& prodname, const double output ); // calculate emissions
	void calc_emiss2( const double activity, const double gamma, const double* cntl); // calculate emissions alternative
	void calc_emiss_ind( const double input, const string& fuelname, const vector<Emcoef_ind>& emcoef_ind  ); // calculate ind emissions
	string getname() const; // return name of ghg
	string getunit() const; // return unit for ghg
	double getemission() const; // return ghg emissions
	double getseq_emiss() const; // return sequestered emissions
	double getemiss_fuel() const; // return ghg emissions implicit in fuel
	double getemiss_ind() const; // return indirect ghg emissions
	double getemiss_coef() const; // return ghg emissions coefficient
};

#endif // _GHG_H_