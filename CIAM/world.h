#ifndef _WORLD_H_
#define _WORLD_H_
#if defined(_MSC_VER)
#pragma once
#endif

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

// Forward declarations
class Region;
class Logger;

/*! 
* \ingroup CIAM
* \brief A class which contains all the model's regions.
* \author Sonny Kim
*/

class World
{
private:
	int noreg; //!< number of regions
   std::map<std::string, int> regionNamesToNumbers;
	std::vector<Region*> region; //!< array of pointers to Region objects
	std::vector<double> population; //!< total global population
	std::vector<double> crudeoilrsc; //!< global conventional crude oil resource
	std::vector<double> unconvoilrsc; //!< global unconventional crude oil resource
	std::vector<double> natgasrsc; //!< global natural gas resource
	std::vector<double> coalrsc; //!< global coal resource
	std::vector<double> uranrsc; //!< global uranium resource
   std::vector<std::map<std::string,double> >ghgs; //!< maps containing ghg emissions
   std::vector<std::string> primaryFuelList; //!< vector of names of primary fuels. 
	void initAgLu(); 
   bool doCalibrations; //!< turn on or off calibration routines
   
public:
	void setupCalibrationMarkets();
	World(); // default construtor
	~World();
	void clear();
   void XMLParse( const xercesc::DOMNode* node );
   void completeInit();
   void toXML( std::ostream& out ) const;
	void toDebugXML( const int period, std::ostream& out ) const;
   void initCalc( const int per ); // initializations
	void calc( const int per, const std::vector<std::string>& regionsToSolve = std::vector<std::string>( 0 ) ); // model calculation for each region
	void updateSummary(int per); // update summaries for reporting
	void sumpop(int per); // sum global population
	void sumrsc(int per); // sum regional resources for global total
	void emiss_ind(int per); // calculate indirect emissions
	void emiss_all(void); // set global emissions for all GHG for climat
	void outputfile(void); // write output to file
	void MCoutput(void); // write MiniCAM output to file
   double getGHGEmissions( const std::string& ghgName, const int per ) const;
	void createRegionMap(void); // create map of region names
   std::vector<std::string> getRegionVector() const;
	void turnCalibrationsOn(); // turn on calibrations
	void turnCalibrationsOff(); // turn off calibrations
	bool getCalibrationSetting() const; // return calibration setting
   void printGraphs( std::ostream& outStream, const int period ) const;
   const std::vector<std::string> getPrimaryFuelList() const;
   double getPrimaryFuelCO2Coef( const std::string& regionName, const std::string& fuelName ) const;
   double getCarbonTaxCoef( const std::string& regionName, const std::string& fuelName ) const;
   void printSectorDependencies( Logger* logger ) const;
};

#endif // _WORLD_H_

