/* subrsrc.h							*
 * sub-resources for each region		*
 * subrsrc is a class					*
 * that contains grades 				*/

#ifndef _SUBRSRC_H_
#define _SUBRSRC_H_
#pragma once

#include <vector>
#include <string>
#include "grade.h"
// xerces xml headers
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std; // enables elimination of std::
using namespace xercesc;

// class definition for sub-resources
class subrsrc
{
private:
	string name; //! subrsrc name
	int nograde; //! number of grades of each subrsrc
	double min_annualprod; //! minimum annual production of subrsrc
	vector< vector<grade*> > depgrade; //! amount of subrsrc for each grade
	vector<double> rscprc; //! subresource price
	vector<double> available; //! total available resource
	vector<double> annualprod; //! annual production of subrsrc
	vector<double> cummprod; //! cummulative production of subrsrc
public:
	subrsrc(); //default construtor
	~subrsrc();
	void clear();
	string getName() const; // return subrsrc name
	void XMLParse( const DOMNode* tempnode ); // initialize with xml data
	void toXML( ostream& out ) const;
	void toDebugXML( const int period, ostream& out ) const;
	double price(int per);
	void cummsupply(double prc,int per); // calculate cummulative production
	double showcummprod(int per); // return cummulative production
	// calculate annual production
	void annualsupply(int per,double gnp1,double gnp2,double price1,double price2);
	double showannualprod(int per); // return annual production
	double showavailable(int per); // return available resource
	int maxgrade(void); // returns total number of grades
	// MiniCAM style output to database table
	void MCoutput( const string& regname, const string& secname); 
	// output to file
	void outputfile(const string &regname, const string& sname); 
	void updateAvailable( const int period );
};

#endif // _SUBRSRC_H_