/* market.h                                         *
 * This header contains the global                  *
 * market information for all markets.              *
 * Market class contains number of markets          *
 * and collection of individual market information. *
 * SHK  9/21/00                                     */

#include <string>
#include <vector>
#include <map>

//#include <valarray>
// including valarray gives errors when afxdisp.h is also included. why????
using namespace std; // enables elimination of std::
// important to include namespace otherwise will not recognize vector

// struct for individual market information
// market for primary and secondary energy.
// not a market for service goods.

//struct str_mrk
class str_mrk
{
public:
	//char *name;  // name of market (fuel or good)
	//char *region;  // market region
	char name[20];  // name of market (fuel or good)
	char region[20];  // market region
	int nocountry; // number of countries in market region
	int type; // market type: fuel, good, or service
	int year; // period
	double price;  // market price
	double tprice;  // store market price
	double demand; // demand for market solution
	double tdemand; // store previous demand
	double demand_good; // demand of goods/service
	double supply; // supply for market solution
	double tsupply; // store previous supply
	double supply_good; // supply of goods/service
	double exdmd; // excess demand for each market
	double lexdmd; // log of excess demand for each market
	double texdmd; // store excess demand
	double dexdmd; // derivative of excess demand
	double ldem; // log of demand for each market
	double lsup; // log of supply for each market
	vector<int> countryid; // vector of country indeces
};


class Marketplace
{
private:
	int nomrks;  // number of markets
	int nomrks_t; // number of markets that require solving
	int nomrks_t_NR; // number of markets for Newton-Rhapson
	int nodrscmrks;  // number of depletable resource markets
	int nossecmrks;  // number of supply sector markets
	int noghgmrks;  // number of GHG markets
	vector< vector<str_mrk> > mrk; // no of market objects by period
	vector<int> mrk_isol; // index look up for markets that require solving
	vector<int> mrk_isol_NR; // index look up for markets for Newton-Rhapson
	//vector< vector<int> > mrk_index; // index look up for market definition
	int mrk_index[20][20]; // dimensions: product, regions
	//map<string,int> mrkmap; // map of good and region to market id

public:		
	Marketplace(void); //default construtor
	~Marketplace(void); // destructor
	void setmrks(void); // sets number of markets
	void setmrks2(void); // sets number of markets
	void initprices(void); // read in initial prices from DB
	void nullprc(int per); // initialize all market prices to 0
	void nulldem(int per); // initialize all market demands to 0
	void nullsup(int per); // initialize all market supplies to 0
	void nulldem_imrk(int good_id,int country_id,int per); // set one market demand to 0
	void nullsup_imrk(int good_id,int country_id,int per); // set one market supply to 0
	int shownoghgmrks(void); // show number of ghg markets
	void showmrks(int per); // show market information
	// set market price
	void setprice(int good_id,int country_id,double value,int per);
	void setprice_d(int good_id,int country_id,double value,int per);
	// set market supply
	void setsupply(int good_id,int country_id,double value,int per); 
	void setsupply_good(int good_id,int country_id,double value,int per); 
	// replace supply with new value
	void override_supply(int good_id,int country_id,double value,int per); 
	void override_supply_d(int good_id,int country_id,double value,int per); 
	// set market demand
	void setdemand(int good_id,int country_id,double value,int per); 
	void setdemand_good(int good_id,int country_id,double value,int per); 
	// replace demand with new value
	void override_demand(int good_id,int country_id,double value,int per); 
	void override_demand_d(int good_id,int country_id,double value,int per); 
	// return market price
	double showprice(int good_id,int country_id,int per); 
	double showprice_d(int good_id,int country_id,int per); 
	// return market supply
	double showsupply(int good_id,int country_id,int per); 
	double showsupply_good(int good_id,int country_id,int per); 
	// return market demand
	double showdemand(int good_id,int country_id,int per); 
	double showdemand_good(int good_id,int country_id,int per); 
	void excessdemand(int per); // calculates excess demand for all markets
	void logED(int per); // calculates log of excess demand for all markets
	void logDem(int per); // calculates log of demand for all markets
	void logSup(int per); // calculates log of supply for all markets
	int setmrks_sol(int per,double tol); // set number of markets and index for solving
	int setmrks_sol_NR(int per,double tol); // set number of markets for Newton-Rhapson
	int showmrk_sol(int id); // returns market index that requires solving
	bool checkprod(int per); // checks if supply or demand = 0 for all markets
	int worstED(int per); // returns index of market that has the largest excess demand
	double maxED(int per); // returns the largest excess demand
	void setPRC(vector<double> prices,int per); // sets solution prices for all markets
	void setPRC_NR(vector<double> prices,int per); // sets solution prices for all markets
	void init_to_last(int per); // initialize current market info to last period
	void storeto_last(int per); // initialize current market t info to last period
	void storeinfo(int per); // stores original market information
	void restoreinfo(int per); // restores original market information
	void restoreprc(int per);
	void restoreprc_NR(int per);
	vector<double> showPRC(int per); // returns vector of market prices
	vector<double> showPRC_NR(int per); // returns vector of market prices
	vector<double> showED(int per); // returns vector of market excess demands
	vector<double> showED_NR(int per); // returns vector of market excess demands
	vector<double> showlogED(int per); // returns vector of log of market excess demands
	vector<double> showlogED_NR(int per); // returns vector of log of market excess demands
	vector<double> showlogDem(int per); // returns vector of log of demand
	vector<double> showlogDem_NR(int per); // returns vector of log of demand
	vector<double> showlogSup(int per); // returns vector of log of supply
	vector<double> showlogSup_NR(int per); // returns vector of log of supply
	vector<double> jacobian(int k,int per); // calculate the derivative or Jacobian
	vector<double> dem_elas(int k,int per); // calculate the demand elasticity
	vector<double> dem_elas_NR(int k,int per); // calculate the demand elasticity
	vector<double> sup_elas(int k,int per); // calculate the supply elasticity
	vector<double> sup_elas_NR(int k,int per); // calculate the supply elasticity
	// write out market info to database
	void outputdb(void); 
	// write out market info to database for dataviewer
	void MCoutput(void); 
	// write out market info to file
	void outputfile(void); 
	// for debugging markets
	void bugout(int per,int iter);
	// supply and demand curves for debugging markets
	void sdcurves(int per,int iter);
};

