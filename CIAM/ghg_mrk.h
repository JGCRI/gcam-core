#ifndef _GHG_MRK_H_
#define _GHG_MRK_H_
#pragma once

#include <vector>
#include <xercesc/dom/DOM.hpp>

using namespace std;
using namespace xercesc;

/*! 
* \ingroup CIAM
* \brief Class which defines a market for a single greenhouse gas.
* \author Sonny Kim
* \date $ Date $
* \version $ Revision $
*/

class ghg_mrk
{
private:
	string name; //!< GHG name
	string unit; //!< GHG unit
	string market; //!< Name of the market
	vector<double> constraint; //!< Emissions constraint by year(tgC or MTC)
	vector<double> emission; //!< Emissions by year(tgC or MTC)

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