/* World.h									*
 * This header contains the	World class		*
 * which contains all region objects.		*
 *											*
 * SHK  2/20/01								*/



// World class includes the following headers
#include "region.h" // generic region class

class World
{
private:
	int noreg; // number of regions
	vector<region> country; // array of pointers to region objects
	// **** sum of regional values ****
	vector<double> population; // total global population
	vector<double> crudeoilrsc; // global conventional crude oil resource
	vector<double> unconvoilrsc; // global unconventional crude oil resource
	vector<double> natgasrsc; // global natural gas resource
	vector<double> coalrsc; // global coal resource
	vector<double> uranrsc; // global uranium resource
	vector<str_ghgss> ghgs; // structure containing ghg emissions
public:
	World(void); // default construtor
	~World(void); // destructor
	void initper(void); // sets number of periods in World
	void setregion(void); // sets number of regions in World
	void initregion(void); // initialize each region
	void gnp(int per); // gnp calculation for each region
	void calc(int per); // model calculation for each region
	void sumpop(int per); // sum global population
	void sumrsc(int per); // sum regional resources for global total
	void emission(int per); // calculate global emissions
	void emiss_all(void); // set global emissions for all GHG for climat
	void outputdb(void); // write output to database table
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
};
