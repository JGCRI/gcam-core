/* subsector.h									*
 * This header contains the Subsector class.	*
 * The Subsector object is contained within the	*
 * the Sector object.  The Subsector class		*
 * has as its private member the Technology		*
 * object.										*
 * SHK  10/24/00								*/


// subsector class
#include <vector>
#include <string>
#include "technology.h" // generic technology class
#include "summary.h" // summary class for reporting
#include "str_ghgss.h"

// struct to read database for subsectors
typedef struct
{
	double tcaplim; // capacity limit
	double tshrwts; // logit share weights
	double tlexp; // logit exponential
} str_ssec, *lpstr_ssec;

class subsector
{
private:
	char name[20]; // subsector name
	char *unit; // unit of final product from subsector
	int no; // subsector number
	int notech; // number of technologies in each subsector
	string fueltype; // each subsector has one fueltype
	vector<vector<technology> > techs; // array of pointers to technology objects for each period
	vector<hydro_tech> hydro; // array of hydroelectricity by period
	vector<double> caplim; // subsector capacity limit
	vector<double> shrwts; // subsector logit share weights
	vector<double> lexp; // subsector logit exponential
	vector<double> share; // subsector shares
	vector<double> input; // subsector energy input
	vector<double> pe_cons; // subsector primary energy consumption
	vector<Summary> summary; // summary for reporting
	vector<double> subsectorprice; // subsector price for all periods
	vector<double> output; // total amount of final output from subsector
	double tax; // subsector tax or subsidy
	vector<double> carbontaxpaid; // total subsector carbon taxes paid
	vector<str_ghgss> ghgs; // struct containing ghg emissions
public:
	subsector(void); //default construtor
	subsector(const char* nstr,int sno,double ttax); //constructor
	~subsector(void); // destructor
	int index(void); // return subsector index
	void setlabel(const char* nstr,int sno); // reads in subsectors from database
	char* showname(void); // return name of subsector
	void initper(void); //set vector size
	void setall(lpstr_ssec temp,int dataper); //set subsector info
	void setall2(double tshrwts,double tlexp,int per); //set subsector info
	void copytolast(int per); //set subsector info
	void settech(int itech); // sets number of technology objects
	void set_hydrotech(int itech); // sets number of exogenously driven tech
	// sets the number of ghg for technologies
	void settechghg(int regionno, int sectorno);
	void dbreadstech(char* region,int is); // reads in supply tech info
	void dbreaddtech(char* region,int is); // reads in demand tech info
	// calculates and returns subsector price
	double price(char* varcountry,int country_id,int per); 
	double showprice(int per); // returns subsector price
	void applycarbontax(double tax, int per); // passes along carbon tax
	// applies ghg tax to technologies
	void addghgtax(int ghgno,char* ghgname,int country_id,int per); 
	// calculate shares and sum of shares
	void calc_share(char* varcountry,int country_id,int per); 
	void norm_share(double sum, int per); // normalizes shares to 100%
	// sets demand to output and output
	void setoutput(char* varcountry,int country_id,double dmd,int per); 
	void sumoutput(int per); // sums technology output
	double supply(char* varcountry,int country_id,int per); // calculates supply by technology
	double exog_supply(int per); // calculates exogenous supply
	void show_subsec(void); // write subsector info to screen
	double showshare(int per); // returns share for each subsector
	void showtechs(int per, const char* ofile); // shows all technologies in the subsector
	void showlabel(const char* ofile); // show subsector label
	// reads in subsector base weight from database
	void setbaseser(char* region,const char* dbtname); 
	// set base year subsector shares
	void setbaseshr(char* region,const char* secname,const char* dbtname); 
	// write out subsector result to database
	void outputdb(const char* regname,int reg,const char* secname);
	// write out subsector result to file
	void outputfile(const char* regname,int reg,const char* secname); 
	// write MiniCAM subsector result to file
	void MCoutputA(const char* regname,int reg,const char* secname); 
	void MCoutputB(const char* regname,int reg,const char* secname); 
	void MCoutputC(const char* regname,int reg,const char* secname); 
	int shownotech(void); // return number of technologies in subsector
	char* showtechname(int id); // return technology name
	void emission(int per, char* prodname); // calculate subsector emission
	void indemission(int per); // calculate subsector indirect emission
	double showCO2(int per); // returns CO2 emissions
	double showCO2ind(int per); // returns indirect CO2 emissions
	double showCO2fuel(int per); // returns equivalent CO2 emissions from fuel input
	double showpe_cons(int per); // returns subsector primary energy consumption
	double showinput(int per); // returns subsector primary or final energy consumption
	double getoutput(int per); // returns subsector output
	double showcarbontaxpaid(int per); // returns subsector total carbon taxes paid
	map<string, double> getfuelcons(int per); 
	void clearfuelcons(int per);  //  clears the fuelcons map in summary
	map<string, double> getemission(int per);// get ghg emissions map in summary object 
};

