#ifndef _WORLD_H_
#define _WORLD_H_
#pragma once

/*! 
* \file world.h
* \ingroup CIAM
* \brief The World class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <map>
#include <vector>
#include <xercesc/dom/DOM.hpp>

using namespace xercesc;

// Forward declarations
class Region;
struct str_ghgss;

/*! 
* \ingroup CIAM
* \brief A class which contains all the model's regions.
* \author Sonny Kim
*/

class World
{
private:
	int noreg; //!< number of regions
	map<string, int> regionNamesToNumbers;
	vector<Region*> region; //!< array of pointers to Region objects
	// **** sum of regional values ****
	vector<double> population; //!< total global population
	vector<double> crudeoilrsc; //!< global conventional crude oil resource
	vector<double> unconvoilrsc; //!< global unconventional crude oil resource
	vector<double> natgasrsc; //!< global natural gas resource
	vector<double> coalrsc; //!< global coal resource
	vector<double> uranrsc; //!< global uranium resource
	vector<str_ghgss> ghgs; //!< structure containing ghg emissions
	void initAgLu(); 

public:
	void setupCalibrationMarkets();
	World(); // default construtor
	~World();
	void clear();
	void XMLParse( const DOMNode* node );
	void toXML( ostream& out ) const;
	void toDebugXML( const int period, ostream& out ) const;
	void calc( const int per, const vector<string>& regionsToSolve = vector<string>( 0 ) ); // model calculation for each region
	void updateSummary(int per); // update summaries for reporting
	void sumpop(int per); // sum global population
	void sumrsc(int per); // sum regional resources for global total
	void emiss_ind(int per); // calculate indirect emissions
	void emiss_all(void); // set global emissions for all GHG for climat
	void outputfile(void); // write output to file
	void MCoutput(void); // write MiniCAM output to file
	double showCO2(int per); // return global emissions for period
	double showCO2ag(int per); // return global emissions for period
	double showCH4(int per); // return global emissions for period
	double showN2O(int per); // return global emissions for period
	double showSOXreg1(int per); // return global emissions for period
	double showSOXreg2(int per); // return global emissions for period
	double showSOXreg3(int per); // return global emissions for period
	double showCF4(int per); // return global emissions for period
	double showC2F6(int per); // return global emissions for period
	double showHFC125(int per); // return global emissions for period
	double showHFC134a(int per); // return global emissions for period
	double showHFC143a(int per); // return global emissions for period
	double showHFC227ea(int per); // return global emissions for period
	double showHFC245ca(int per); // return global emissions for period
	double showSF6(int per); // return global emissions for period
	void createRegionMap(void); // create map of region names
};

#endif