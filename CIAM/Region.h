/* region.h									*
 * This header contains the					*
 * region class.							*
 *											*
 * SHK  7/28/00								*/

// region class includes the following headers
#include "demographic.h" // generic demographic class
#include "resource.h" // generic resource class, includes str_ghgs
#include "sector.h" // generic supply sector class
#include "ghg_mrk.h" // generic ghg market class

class region
{
private:
	char name[20]; // region name
	int no; // region number
	int noghg; // number of ghg for market solution in each region
	int nodrsc; // number of depletable resources in each region
	int nossec; // number of supply sectors in each region
	int nodsec; // number of demand sectors in each region
	int noregmrks; // number of markets in each region
	demographic population; // demographic object
	vector<resource> depresource; // array of depletable resource objects
	vector<sector> supplysector; // array of pointers to supply sector objects
	vector<demsector> demandsector; // array of pointers to demand sector objects
	vector<ghg_mrk> ghgmarket; // array of pointers to ghg market objects
	vector<double> i_elas; // income elasticity
	vector<double> gnp_dol; // regional gross national product
	vector<double> gnp; // normalized regional gross national product
	vector<double> gnp_adj; // regional gross national product adjusted for energy
	vector<double> input; // total fuel and energy consumption
	vector<double> price_ser; // aggregate price for demand services
	vector<double> carbontax; // regional carbon tax
	vector<double> carbontaxpaid; // total regional carbon taxes paid
	vector<Summary> summary; // summary for reporting
public:
	region(void); // default construtor
	~region(void); // destructor
	void setlabel(const char* nstr,int sno); // set region name and index
	void initper(void); // set array size to max period
	void setCO2coef(void); // set default CO2 emissions coefficients
	void setpop(void); // set vector size
	void initpop(void); // read population information
	void setcarbontax(void); // read in carbon tax from database
	void setghgobj(void); // sets number of ghg objects need for ghg market
	void setghgsupply(int per); // sets ghg constraint to market supply
	void setghgdemand(int per); // sets ghg emissions to market demand
	void addghgtax(int per); // sets ghg tax to technologies
	void setdepresource(void); // sets number of depletable resources
	void setsupsector(void); // sets number of intermediate and final supply sectors
	void setdemsector(void); // sets number of demand sectors
	void settechghg(void); // sets number of ghg in technology
	void economics(void); // sets economic data for region
	void rscinitialize(void); // sets resource technologies
	void supinitialize(void); // sets supply technologies
	void deminitialize(void); // sets demand technologies
	void rscsupply(int per); // calculates annual supply of primary resources
	void finalsupplyprc(int per); // calculates prices of refined fuels and electricity
	void calc_gnp(int per); // calculates gnp
	void calc_enduseprice(int per); // calculates end use demand sector aggregate price
	void adjust_gnp(int per); // adjust gnp for energy
	void endusedemand(int per);  // calculates end use demand for energy and other goods
	void finalsupply(int per); // calculates supply of final energy and other goods 
							   // after getting demand
	void override_mrks(int per); // intermediate goods supply=cost demand=solution price 
	void emission(int per); // calculates GHG emissions by fuel and sector
	void emiss_ind(int per); // calculates indirect GHG emissions from demand sectors
	void showsupsector(int per, const char* ofile); // write supply sector info to text file
	void showdemsector(int per, const char* ofile); // write demand sector info to text file
	void applycarbontax(int per); // apply carbon taxes to appropriate sectors
	double showpop(int per); // return regional population
	double showrsc(int rscno,int per); // return regional available resource
	double showsubrsc(int rscno,int subrscno,int per); // return regional available subresource
	void outputdb(void); // write output to database table
	void outputfile(void); // write output to file
	void MCoutput(void); // MiniCAM output to file
	int shownodrsc(void); // show number of depletable resources
	int shownossec(void); // show number of supply sectors
};
