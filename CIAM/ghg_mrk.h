/* ghg_mrk.h										*
 * This header contains the global				*
 * Greenhouse Gas Market class.						*
 * 										*
 * SHK  10/18/01								*/

#ifndef _GHG_MRK_H_
#define _GHG_MRK_H_
#pragma once

#include <vector>
#include <xercesc/dom/DOM.hpp>

using namespace std;
using namespace xercesc;

// ghg_mrk class (Greenhouse gas)
class ghg_mrk
{
private:
	string name; // ghg name
	string unit; // ghg unit
	string market; // added by jpl
	vector<double> constraint; // emissions constraint by year(tgC or MTC)
	vector<double> emission; // emissions by year(tgC or MTC)

public:
	ghg_mrk(); //default construtor
	void clear();
	void XMLParse( const DOMNode* node );
	void toXML( ostream& out ) const; // write out xml.
	void toDebugXML( const int period, ostream& out ) const;
	void setMarket( const string& regname ); // creates markets
	string getName() const; // show emissions name
	void setEmission( const double amount, const int per ); // set emissions
	double getConstraint( const int per ) const; // return emissions constraint
	double getEmission( const int per ) const; // return emissions
};

#endif // _GHG_MRK_H_