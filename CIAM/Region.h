#ifndef _REGION_H_
#define _REGION_H_
#pragma once

/*! 
* \file Region.h
* \ingroup CIAM
* \brief The Region class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <map>

using namespace std;
using namespace xercesc;

// Forward declarations.
class demographic;
class Resource;
class sector;
class demsector;
class AgSector;
class ghg_mrk;
// class TransSector;
class Summary;
class Emcoef_ind;

/*! 
* \ingroup CIAM
* \brief A class which defines a single region of the model.
* \author Sonny Kim
*/

class Region
{
private:
	string name; //!< Region name
	int noghg; //!< number of ghg for market solution in each region
	int numResources; //!< number of depletable resources in each region
	int nossec; //!< number of supply sectors in each region
	int nodsec; //!< number of demand sectors in each region
	int noregmrks; //!< number of markets in each region
	double EnergyGNPElas; //!< elasticity for energy price feedback on GNP
	demographic* population; //!< demographic object
	vector<Resource*> resources; //!< vector of resource objects
	vector<sector*> supplysector; //!< array of pointers to supply sector objects
	vector<demsector*> demandsector; //!< array of pointers to demand sector objects
	AgSector* agSector; //!< Agricultural sector.
	vector<ghg_mrk*> ghgmarket; //!< array of pointers to ghg market objects, container for constraints and emissions
	vector<double> i_elas; //!< income elasticity
	vector<double> gnp_dol; //!< regional gross national product
	vector<double> calibrationGNPs; //!< GNPs to calibrate to.
	vector<double> gnp; //!< normalized regional gross national product
	vector<double> gnp_adj; //!< regional gross national product adjusted for energy
	vector<double> gnp_cap; //!< regional gross national product per capita
	vector<double> input; //!< total fuel and energy consumption
	vector<double> price_ser; //!< aggregate price for demand services
	vector<double> carbontax; //!< regional carbon tax
	vector<double> carbontaxpaid; //!< total regional carbon taxes paid
   vector<double> TFEcalb;  //!< TFE Calibration value (can't be set = 0)
	vector<Summary> summary; //!< summary for reporting
   map<string,int> resourceNameMap; //!< Map of resource name to integer position in vector. 
   map<string,int> supplySectorNameMap; //!< Map of supplysector name to integer position in vector. 
   map<string,int> demandSectorNameMap; //!< Map of demandsector name to integer position in vector. 
   map<string,int> ghgMarketNameMap; //!< Map of ghgmarket name to integer position in vector. 
   vector<Emcoef_ind> emcoef_ind; //!< vector of objects containing indirect emissions coefficients
   map<string, double> primaryFuelCO2Coef; //!< map of CO2 emissions coefficient for primary fuel only
   map<string, double> carbonTaxFuelCoef; //!< map of CO2 emissions coefficient for all fossil fuels

public:
	
	Region(); // default construtor
	~Region(); 
	void clear();
	void initElementalMembers();
	void XMLParse( const DOMNode* node );
   void completeInit();
	void toXML( ostream& out ) const;
	void toDebugXML( const int period, ostream& out ) const;
	string getName() const;
   void writeBackCalibratedValues( const int period );
   void setupCalibrationMarkets();
   void calibrateRegion( const bool doCalibrations, const int per );
   bool demandAllCalibrated( const int per );
   void calibrateTFE( const int per ); 
   void initCalc( const int per ); // Call sectors to consistantly adjust share weights
	void setghgsupply(int per); // sets ghg constraint to market supply
	void setghgdemand(int per); // sets ghg emissions to market demand
	void addghgtax(int per); // sets ghg tax to technologies
	void rscsupply(int per); // calculates annual supply of primary resources
	void finalsupplyprc(int per); // calculates prices of refined fuels and electricity
	void calc_gnp(int per); // calculates gnp
	const vector<double> calcFutureGNP() const;
	void calcGNPlfp(int per); // Calculate regional GNP using laborforce participation and labor productivity.
	void calc_enduseprice(int per); // calculates end use demand sector aggregate price
	void calcEndUsePrice( const int period ); // new function which avoids db call
	void adjust_gnp(int per); // adjust gnp for energy
	void endusedemand(int per);  // calculates end use demand for energy and other goods
	void finalsupply(int per); // calculates supply of final energy and other goods after getting demand
	void emission(int per); // calculates GHG emissions by sector
	void calcEmissFuel(int per); // calculates GHG emissions by fuel
	void emiss_ind(int per); // calculates indirect GHG emissions from demand sectors
	void showsupsector(int per, const char* ofile); // write supply sector info to text file
	void showdemsector(int per, const char* ofile); // write demand sector info to text file
	void applycarbontax(int per); // apply carbon taxes to appropriate sectors
	double showpop(int per); // return regional population
	void outputfile(void); // write output to file
	void MCoutput(void); // MiniCAM output to file
	int shownodrsc(void); // show number of depletable resources
	int shownossec(void); // show number of supply sectors
    void findSimul( const int per ) const;
	void calcAgSector( const int period );
	void initializeAgMarketPrices( const vector<double>& pricesIn );
	double showrsc( const string resourceName, const int per );
	double showsubrsc( const string resourceName, const string& subResourceName, const int per );
	void updateSummary( const int period ); // update regional summary for reporting
   void printGraphs( ostream& outStream, const int period ) const;
   static void replaceSpaces( string& stringIn );
   double getPrimaryFuelCO2Coef( const string& fuelName ) const;
   double getCarbonTaxCoef( const string& fuelName ) const;
};

#endif // _REGION_H_