#ifndef _SUBRSRC_H_
#define _SUBRSRC_H_
#pragma once

/*! 
* \file Subrsrc.h
* \ingroup CIAM
* \brief The subrsrc class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <string>
#include "grade.h"

// xerces xml headers
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std; // enables elimination of std::
using namespace xercesc;

/*! 
* \ingroup CIAM
* \brief subrsrc is a class that contains grades.
* \author Sonny Kim
*/

class subrsrc
{
private:

	string name; //!< subrsrc name
	int nograde; //!< number of grades of each subrsrc
    double priceElas; //!< price elasticity for short-term supply limit
	double minShortTermSLimit; //!< short-term supply limit.
	vector<double> rscprc; //!< subresource price
	vector<double> techChange; //!< technical change
	vector<double> environCost; //!< Environmental costs
	vector<double> severanceTax; //!< Severence Tax (exogenous)
	vector<double> available; //!< total available resource
	vector<double> annualprod; //!< annual production of subrsrc
	vector<double> cumulprod; //!< cumulative production of subrsrc
	vector<double> gdpExpans; //!< short-term supply limit expansion elasticity w/ gdp
   vector<double> cumulativeTechChange; //!< Cumulative Technical Change for this sub-sector
            // Cumulative technical change needs to be in sub-resource sector 
	vector< vector<grade*> > depgrade; //!< amount of subrsrc for each grade

public:
	subrsrc(); //default construtor
	~subrsrc();
	void clear();
	string getName() const; // return subrsrc name
	void XMLParse( const DOMNode* tempnode ); // initialize with xml data
	void toXML( ostream& out ) const;
	void toDebugXML( const int period, ostream& out ) const;
	void cumulsupply(double prc,int per); // calculate cummulative production
	double getPrice(int per);
	double getCumulProd(int per); // return cummulative production
	// calculate annual production
	void annualsupply(int per,double gnp1,double gnp2,double price1,double price2);
	double getAnnualProd(int per); // return annual production
	double getAvailable(int per); // return available resource
	int getMaxGrade(void); // returns total number of grades
	// MiniCAM style output to database table
	void MCoutput( const string& regname, const string& secname); 
	// output to file
	void outputfile(const string &regname, const string& sname); 
	void updateAvailable( const int period );
};

#endif // _SUBRSRC_H_
