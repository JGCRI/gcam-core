#ifndef _SUBSECTOR_H_
#define _SUBSECTOR_H_
#pragma once

#include <vector>
#include <string>
#include "technology.h"
#include "summary.h"
// xerces xml headers
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace xercesc;

/*! 
* \ingroup CIAM
* \brief A class which defines a single Subsector of the model.
* \author Sonny Kim
* \date $ Date $
* \version $ Revision $
*/

class subsector
{
private:
	string name; //!< subsector name
	string unit; //!< unit of final product from subsector
	string fueltype; //!< each subsector has one fueltype
	int notech; //!< number of technologies in each subsector
	double tax; //!< subsector tax or subsidy
	vector<vector<technology*> > techs; //!< array of pointers to technology objects for each period
	vector<hydro_tech> hydro; //!< array of hydroelectricity by period
	vector<double> caplim; //!< subsector capacity limit
	vector<double> shrwts; //!< subsector logit share weights
	vector<double> lexp; //!< subsector logit exponential
	vector<double> share; //!< subsector shares
	vector<double> input; //!< subsector energy input
	vector<double> pe_cons; //!< subsector primary energy consumption
	vector<double> subsectorprice; //!< subsector price for all periods
	vector<double> output; //!< total amount of final output from subsector
	vector<double> carbontaxpaid; //!< total subsector carbon taxes paid
	vector<Summary> summary; //!< summary for reporting
public:
	subsector(); // default construtor
	~subsector();
	void clear();
	string showname(void); // return name of subsector
	void initper(void); //set vector size
	void XMLParse( const DOMNode* tempNode ); // initialize subsector with xml data
	void toXML( ostream& out ) const;
	void toDebugXML( const int period, ostream& out ) const;
	void copytolast(int per); //set subsector info
	void settech(int itech); // sets number of technology objects
	void set_hydrotech(int itech); // sets number of exogenously driven tech
	// calculates and returns subsector price
	double price( const string regionName, const int per); 
	double showprice(int per); // returns subsector price
	void applycarbontax(double tax, int per); // passes along carbon tax
	// applies ghg tax to technologies
	void addghgtax( const string ghgname, const string regionName, const int per ); 
	// calculate shares and sum of shares
	void calc_share( const string regionName, const int per ); 
	void norm_share(double sum, int per); // normalizes shares to 100%
	// sets demand to output and output
	void setoutput( const string& regionName, const double dmd, const int per); 
	void sumoutput(int per); // sums technology output
	double supply( const string& regionName, const int per ); // calculates supply by technology
	double exog_supply(int per); // calculates exogenous supply
	void show_subsec(void); // write subsector info to screen
	double showshare(int per); // returns share for each subsector
	void showtechs(int per, const char* ofile); // shows all technologies in the subsector
	void showlabel(const char* ofile); // show subsector label
	// write out subsector result to file
	void outputfile( const string& regname, const string& secname); 
	// write MiniCAM subsector result to file
	void MCoutputA( const string & regname, const string& secname ); 
	void MCoutputB( const string & regname, const string& secname ); 
	void MCoutputC( const string & regname, const string& secname ); 
	int shownotech(void); // return number of technologies in subsector
	string showtechname(int id); // return technology name
	void emission(int per,string prodname); // calculate subsector emission
	void indemission(int per); // calculate subsector indirect emission
	double showCO2(int per); // returns CO2 emissions
	double showCO2ind(int per); // returns indirect CO2 emissions
	double showCO2fuel(int per); // returns equivalent CO2 emissions from fuel input
	double showpe_cons(int per); // returns subsector primary energy consumption
	double showinput(int per); // returns subsector primary or final energy consumption
	double getoutput(int per); // returns subsector output
	double showcarbontaxpaid(int per); // returns subsector total carbon taxes paid
	map<string, double> getfuelcons(int per); 
	void clearfuelcons(int per);  //  clears the fuelcons map in summary
	map<string, double> getemission(int per);// get ghg emissions map in summary object 
	map<string, double> getemfuelmap(int per);// get ghg emissions map in summary object 
	map<string, double> getemindmap(int per);// get ghg emissions map in summary object 
        void adjShares(double dmd, double varSectorSharesTot, double totalFixedSupply, int per);
};

#endif // _SUBSECTOR_H_