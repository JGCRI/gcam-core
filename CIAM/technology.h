#ifndef _TECHNOLOGY_H_
#define _TECHNOLOGY_H_
#pragma once

/*! 
* \file technology.h
* \ingroup CIAM
* \brief The technology class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

// Standard Library headers.
#include <vector>
#include <map>
#include <string>

// xerces xml headers
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std;
using namespace xercesc;

// Forward declaration
class Ghg;

/*! 
* \ingroup CIAM
* \brief This technology class is based on the MiniCAM description of technology.
* \author Sonny Kim
*/

class technology
{
protected:
	int fueltype; //!< fuel number
	int year; //!< period year or vintage
	double shrwts; //!< logit share weight
	double eff; //!< energy intensity
	double necost; //!< all non-fuel costs (levelized)
	double fuelcost; //!< fuel cost only
	double techcost; //!< total cost of technology
	double tax; //!< utility tax
	double fMultiplier; //!< multiplier on fuel cost or price
	double pMultiplier; //!< multiplier on total cost or price
	double carbontax; //!< carbon tax in $/TC
	double carbontaxgj; //!< carbon tax in $/GJ
	double carbontaxpaid; //!< total carbon taxes paid
	double lexp; //!< logit exponential
	double share; //!< technology shares
	double input; //!< total fuel input (fossil and uranium)
	double output; //!< technology output
	double techchange;  //!< technical change in %/year
	double fixedSupply; //!< amount of fixed supply (>0) for this tech, exclusive of constraints
	double fixedOutputVal; //!< The actual amount  of fixed output
	string name; //!< technology name
	string unit; //!< unit of final product from technology
	string fuelname; //!< name of fuel used
   bool doCalibration; // Flag set if calibration value is read-in
   double calInputValue; // Calibration value
	vector<Ghg*> ghg; //!< suite of greenhouse gases
	map<string,double> emissmap; //!< map of ghg emissions
	map<string,double> emfuelmap; //!< map of ghg emissions implicit in fuel
	map<string,double> emindmap; //!< map of indirect ghg emissions
        
	// attributes for hydroelectricity only!
	double resource; //!< available hydro resource in energy units
	double A; //!< logit function shape parameter
	double B; //!< logit function shape parameter
public:
	technology(); // default construtor
	virtual ~technology();
	virtual void clear();
	void initElementalMembers();
	virtual void XMLParse( const DOMNode* tempnode ); // initialize technology with xml data
	virtual void toXML( ostream& out ) const;
	virtual void toDebugXML( const int period, ostream& out ) const;
	void applycarbontax(double tax); // apply carbon tax to appropriate technology
	// sets ghg tax to technologies
	void addghgtax( const string ghgname, const string regionName, const int per ); 
	// calculates fuel and total cost of technology
	void cost( const string regionName, const int per); 
	// uses logit function to calculate technology share
	void calc_share( const string regionName, const int per); 
	void norm_share(double sum); // normalize technology share
   void calcFixedSupply(int per); // calculate fixed supply
   void resetFixedSupply(int per); // reset fixed supply to max value
   void adjShares(double subsecdmd, double totalFixedSupply, double varShareTot, int per);
	void scaleFixedSupply(const double scaleRatio); // scale fixed supply
	 // calculates fuel input and technology output
	virtual void production(const string& regionName,const string& prodName,double dmd,const int per);
	void emission( const string prodname); // calculates GHG emissions from technology
	void indemission(void); // calculates indirect GHG emissions from technology use
	void printTech( const string& outFile = "" ) const; // write technology information to file or screen
	// ****** return names and values ******
	string showname() const; // return technology name
	string getfname() const; // return fuel name
	int showfuelno() const; // return fuel number
	double showeff() const; // return fuel efficiency
	double getShare() const; // return normalized share
	bool getCalibrationStatus( ) const; // return true if technology has calibration value
	double showinput() const; // return fuel input amount
	double showoutput() const; // return technology output
	double getfuelcost() const; // return fuel cost only
	double gettechcost() const; // return total technology cost
	double getnecost() const; // return non-fuel cost
	double showcarbontax() const; // return carbon taxes in $/TC
	double showcarbontaxgj() const; // return carbon taxes in $/GJ
	double showcarbontaxpaid() const; // return carbon taxes paid
	double getCO2() const; // return actual CO2 emissions from technology
	map<string,double> getemissmap() const; // return map of all ghg emissions
	map<string,double> getemfuelmap() const; // return map of all ghg emissions
	map<string,double> getemindmap() const; // return map of all ghg emissions
	double get_emissmap_second( const string& str ) const; // return value for ghg
	double getlexp() const; // return logit exponential for the technology
    double getFixedSupply() const; // return fixed supply

};

/*! 
* \ingroup CIAM
* \brief Hydroelectricity technology class derived from the technology class.
* \author Sonny Kim
* \date $ Date $
* \version $ Revision $
*/
class hydro_tech : public technology
{
private:
	double resource; //!< available hydro resource in energy units
	double A; //!< logit function shape parameter
	double B; //!< logit function shape parameter

public:
	hydro_tech();
	void clear();
	void production(double dmd,int per); // calculates fuel input and technology output
};

#endif // _TECHNOLOGY_H_