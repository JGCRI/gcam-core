#ifndef _SECTOR_H_
#define _SECTOR_H_
#pragma once

/*! 
* \file Sector.h
* \ingroup CIAM
* \brief The sector and demsector classes header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <xercesc/dom/DOM.hpp>
#include "subsector.h" // generic technology class

using namespace std; // enables elimination of std::
using namespace xercesc;

/*! 
* \ingroup CIAM
* \brief A class which defines a single supply sector.
* \author Sonny Kim
* \date $Date $
* \version $Revision $
*/

class sector
{
protected:
	string name; //!< sector name
	string unit; //!< unit of final product from sector
	string market; //!< regional market
	int nosubsec; //!< number of subsectors in each sector
	double tax; //!< sector tax or subsidy
	vector<subsector*> subsec; //!< subsector objects
	vector<double> sectorprice; //!< sector price in $/service
	vector<double> price_norm; //!< sector price normalized to base year
	vector<double> pe_cons; //!< sectoral primary energy consumption
	vector<double> input; //!< sector total energy consumption
	vector<double> output; //!< total amount of final output from sector
	vector<double> fixedOutput; //!< total amount of fixed output from sector
	vector<double> carbontaxpaid; //!< total sector carbon taxes paid
	vector<Summary> summary; //!< summary for reporting
	virtual void initElementalMembers();

public:
	sector();
	~sector();
	virtual void clear();
	string getName(); // return name of sector
	virtual void XMLParse( const DOMNode* node );
	virtual void toXML( ostream& out ) const;
	virtual void toDebugXML( const int period, ostream& out ) const;
	virtual void setMarket( const string& regname ); //create markets
	void applycarbontax(double tax,int per); // passes along regional carbon tax
	void addghgtax( const string ghgname, const string regionName, const int per); // sets ghg tax to technologies
	virtual void calc_share( const string regionName, const int per, const double gnp_cap = 1 ); // calculates and normalizes shares 
	void price(int per); // calculates sector price
	void production( const string& regionName,int per); // calculates production using mrk prices
	void setoutput(const string& regionName, double dmd, int per); // sets demand to totoutput and output
	void sumoutput(int per); // sum subsector outputs
	void set_ser_dmd(double dmd, int per); // sets end-use sector service demand
	void supply( const string regionName, const int per ); // calculates supply from each subsector
	void show();
	void showsubsec(int per, const char* ofile); // shows all subsectors in the sector
	void showlabel(const char* ofile); // show sector label
	int shownosubsec(void);
	double getoutput(int per); // returns sector output 
	double getFixedOutput(int per); // returns sector output 
	double showprice(int per); // returns sector aggregate price
	void emission(int per); // sum subsector emissions
	void indemission(int per); // sum subsector indirect emissions
	double showpe_cons(int per); // return sector primary energy consumption
	void suminput(int per); // sums subsector primary and final energy consumption
	double showinput(int per); // return sector energy consumption
	virtual void outputfile(const string& regname ); // write out sector result to file
	void MCoutput_subsec(const string& regname ); // calls write for subsector 
	virtual void MCoutput(const string& regname ); // write out sector result to file
	void subsec_outfile(const string& regname );  // call fn to write subsector output
	double showcarbontaxpaid(int per); // return total sector carbon taxes paid
	map<string, double> getfuelcons(int per); // get fuel consumption map
	double getfuelcons_second(int per,string key); // get second of fuel consumption map
	void clearfuelcons(int per);  //  clears the fuelcons map in summary
	map<string, double> getemission(int per);// get ghg emissions map in summary object 
	map<string, double> getemfuelmap(int per);// get ghg emissions map in summary object
	void updateSummary(const int per);  //  update summaries for reporting
};


/*! 
* \ingroup CIAM
* \brief A class which defines a single demand sector.
* \author Sonny Kim
* \date $ Date $
* \version $ Revision $
*/

class demsector : public sector
{
protected:
	int perCapitaBased; //!< demand equation based on per capita GNP, true or false
	double pElasticityBase; //!< base year energy price elasticity
	double priceRatio; // temp price ratio
	vector<double> sectorfuelprice; // temp vec
	vector<double> fe_cons; //!< end-use sector final energy consumption
	vector<double> service; //!< total end-use sector service 
	vector<double> iElasticity; //!< income elasticity 
	vector<double> pElasticity; //!< price elasticity.
	vector<double> aeei; //!< autonomous end-use energy intensity parameter
	vector<double> techChangeCumm; //!< cummulative technical change on end-use service

public:
	demsector();
	virtual void clear();
	virtual void XMLParse(const DOMNode* node);
	virtual void toXML( ostream& out ) const;
	virtual void toDebugXML( const int period, ostream& out ) const;
	virtual void setMarket( const string& regname );
	virtual void calc_share( const string regionName, const int per, const double gnp_cap = 1 );
	virtual void calc_pElasticity( const int per );
	virtual void aggdemand( const string& regionName, const double gnp_cap, const double gnp, const int per); 
	virtual void outputfile( const string& regionName );
	virtual void MCoutput( const string& regionName );
};

#endif // _SECTOR_H_