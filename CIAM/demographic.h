#ifndef _DEMOGRAPHIC_H_
#define _DEMOGRAPHIC_H_
#pragma once

#include <vector>
#include <xercesc/dom/DOM.hpp>

using namespace std;
using namespace xercesc;

/*! 
* \ingroup CIAM
* \brief An object which contains the demographic information for a region.
* \author Sonny Kim
* \date $ Date $
* \version $ Revision $
*/
class demographic
{
private:
	// vector of time period 
	// population has 1 more historical period 
	vector<double> malepop; //!< Total male population
	vector<double> femalepop; //!< Total female population.
	vector<double> totalpop; //!< Total population
	vector<double> laborprod; //!< labor productivity growth rate
	vector<double> laborforce_p; //!< labor force participation percent
	vector<double> laborforce; //!< actual labor force
public:
	demographic(); //default construtor
	demographic( int per, double mpop, double fpop ); //constructor
	void clear();
	void XMLParse( const DOMNode* node );
	void toXML( ostream& out ) const;
	void toDebugXML( const int period, ostream& out ) const;
	void initData();
	// set size of population and labor productivity variables to max period
	double labor( const int per ) const; // return labor productivity growthrate
	double total( const int per ) const; // return total population
	const vector<double>& getTotalPopVec() const; 
	// return labor force (actual working)
	double getlaborforce( const int per ) const;
	void show(int per);
	// outputs to file
	void outputfile(const string& regname ); 
	// MiniCAM outputs to file
	void MCoutput(const string& regname ); 
};

#endif // _DEMOGRAPHIC_H_
