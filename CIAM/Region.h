#ifndef _REGION_H_
#define _REGION_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file Region.h
* \ingroup CIAM
* \brief The Region class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <map>

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
	std::string name; //!< Region name
	int noghg; //!< number of ghg for market solution in each region
	int numResources; //!< number of depletable resources in each region
	int nossec; //!< number of supply sectors in each region
	int nodsec; //!< number of demand sectors in each region
	int noregmrks; //!< number of markets in each region
	double EnergyGNPElas; //!< elasticity for energy price feedback on GNP
	demographic* population; //!< demographic object
	std::vector<Resource*> resources; //!< vector of resource objects
	std::vector<sector*> supplysector; //!< array of pointers to supply sector objects
	std::vector<demsector*> demandsector; //!< array of pointers to demand sector objects
	AgSector* agSector; //!< Agricultural sector.
	std::vector<ghg_mrk*> ghgmarket; //!< array of pointers to ghg market objects, container for constraints and emissions
	std::vector<double> i_elas; //!< income elasticity
	std::vector<double> gnp_dol; //!< regional gross national product
	std::vector<double> calibrationGNPs; //!< GNPs to calibrate to.
	std::vector<double> gnp; //!< normalized regional gross national product
	std::vector<double> gnp_adj; //!< regional gross national product adjusted for energy
	std::vector<double> gnp_cap; //!< regional gross national product per capita
	std::vector<double> input; //!< total fuel and energy consumption
	std::vector<double> price_ser; //!< aggregate price for demand services
	std::vector<double> carbontax; //!< regional carbon tax
	std::vector<double> carbontaxpaid; //!< total regional carbon taxes paid
   std::vector<double> TFEcalb;  //!< TFE Calibration value (can't be set = 0)
	std::vector<Summary> summary; //!< summary for reporting
   std::map<std::string,int> resourceNameMap; //!< Map of resource name to integer position in vector. 
   std::map<std::string,int> supplySectorNameMap; //!< Map of supplysector name to integer position in vector. 
   std::map<std::string,int> demandSectorNameMap; //!< Map of demandsector name to integer position in vector. 
   std::map<std::string,int> ghgMarketNameMap; //!< Map of ghgmarket name to integer position in vector. 
   std::vector<Emcoef_ind> emcoef_ind; //!< vector of objects containing indirect emissions coefficients
   std::map<std::string, double> primaryFuelCO2Coef; //!< map of CO2 emissions coefficient for primary fuel only
   std::map<std::string, double> carbonTaxFuelCoef; //!< map of CO2 emissions coefficient for all fossil fuels

public:
	
	Region();
	~Region(); 
	void clear();
	void initElementalMembers();
   void XMLParse( const xercesc::DOMNode* node );
   void completeInit();
	void toXML( std::ostream& out ) const;
	void toDebugXML( const int period, std::ostream& out ) const;
	std::string getName() const;
   void writeBackCalibratedValues( const int period );
   void setupCalibrationMarkets();
   void calibrateRegion( const bool doCalibrations, const int per );
   bool demandAllCalibrated( const int per );
   void calibrateTFE( const int per ); 
   void initCalc( const int per );
	void setghgsupply(int per);
	void setghgdemand(int per);
	void addghgtax(int per);
	void rscsupply(int per);
	void finalsupplyprc(int per);
	void calc_gnp(int per);
	const std::vector<double> calcFutureGNP() const;
	void calcGNPlfp(int per);
	void calc_enduseprice(int per);
	void calcEndUsePrice( const int period );
	void adjust_gnp(int per);
	void endusedemand(int per);
	void finalsupply(int per);
	void emission(int per);
	void calcEmissFuel(int per);
	void emiss_ind(int per);
	void showsupsector(int per, const char* ofile);
	void showdemsector(int per, const char* ofile); 
	void applycarbontax(int per);
	double showpop(int per);
	void outputfile(void);
	void MCoutput(void);
	int shownodrsc(void);
	int shownossec(void);
    void findSimul( const int per ) const;
	void calcAgSector( const int period );
	void initializeAgMarketPrices( const std::vector<double>& pricesIn );
	double showrsc( const std::string resourceName, const int per );
	double showsubrsc( const std::string resourceName, const std::string& subResourceName, const int per );
	void updateSummary( const int period );
   void printGraphs( std::ostream& outStream, const int period ) const;
   static void replaceSpaces( std::string& stringIn );
   double getPrimaryFuelCO2Coef( const std::string& fuelName ) const;
   double getCarbonTaxCoef( const std::string& fuelName ) const;
};

#endif // _REGION_H_

