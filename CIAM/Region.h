#ifndef _REGION_H_
#define _REGION_H_
#pragma once

#include "demographic.h"
#include "resource.h"
#include "sector.h"
#include "AgSector.h"
#include "ghg_mrk.h"
// #include "TransSector.h" // Transportation sector class class  maw

#include <string>

using namespace std;
using namespace xercesc;

/*! 
* \ingroup CIAM
* \brief A class which defines a single region of the model.
* \author Sonny Kim
* \date $ Date $
* \version $ Revision $
*/

class Region
{
private:
	string name; //! Region name
	int noghg; //! number of ghg for market solution in each region
	int numResources; //! number of depletable resources in each region
	int nossec; //! number of supply sectors in each region
	int nodsec; //! number of demand sectors in each region
	int noregmrks; //! number of markets in each region
	double EnergyGNPElas; //! elasticity for energy price feedback on GNP
	demographic population; //! demographic object
	vector<Resource*> resources; //! vector of resource objects
	vector<sector*> supplysector; //! array of pointers to supply sector objects
	vector<demsector*> demandsector; //! array of pointers to demand sector objects
	AgSector* agSector; //! Agricultural sector.
	vector<ghg_mrk*> ghgmarket; //! array of pointers to ghg market objects
	vector<double> i_elas; //! income elasticity
	vector<double> gnp_dol; //! regional gross national product
	vector<double> gnp; //! normalized regional gross national product
	vector<double> gnp_adj; //! regional gross national product adjusted for energy
	vector<double> gnp_cap; //! regional gross national product per capita
	vector<double> input; //! total fuel and energy consumption
	vector<double> price_ser; //! aggregate price for demand services
	vector<double> carbontax; //! regional carbon tax
	vector<double> carbontaxpaid; //! total regional carbon taxes paid
	vector<Summary> summary; //! summary for reporting
public:
	Region(); // default construtor
	~Region(); 
	void clear();
	void initElementalMembers();
	void XMLParse( const DOMNode* node );
	void toXML( ostream& out ) const;
	void toDebugXML( const int period, ostream& out ) const;
	string getName() const;
	void initperXML(void); // set array size to max period for variables not set by XMLParse
	void setCO2coef(void); // set default CO2 emissions coefficients
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
	void emission(int per); // calculates GHG emissions by fuel and sector
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
};

#endif // _REGION_H_