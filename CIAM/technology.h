/* technology.h										*
 * This header contains the definition for	the 	*
 * the technology class.  The technology object is	*
 * contained in the subsector object.  This			*
 * technology class is based on the MiniCAM			*
 * description of technology.						*
 * SHK  5/17/00										*/


#include <vector>
#include <map>
#include <string>
// GHG object for supply sector technologies
#include "ghg_ss.h"
// new ghg object
#include "ghg.h"

// struct for reading in inputs from database
typedef struct
{
	const char *tname;  // technology name
	long tno; // technology number
	int yr; // year or vintage
	int ftype; // fuel type
	const char *fname;  // fuel name
	double shrs; // logit share weight
	double teff;  // energy intensity
	double tnecost;  // all non-energy cost (levelized)
	double ttax; // utility tax
	double tlexp; // logit exponential
	double ttech; // technical change
} strtech, *lpstrtech;

// struct for reading in hydroelectricity inputs from database
typedef struct
{
	const char *tname;  // technology name
	long tno; // technology number
	int yr; // year or vintage
	int ftype; // fuel type
	const char *fname;  // fuel name
	double shrs; // logit share weight
	double teff;  // energy intensity
	double tnecost;  // all non-energy cost (levelized)
	double ttax; // utility tax
	double tlexp; // logit exponential
	double ttech; // technical change
	double tresource; // available hydro resource in energy units
	double tA; // logit function shape parameter
	double tB; // logit function shape parameter
} strtech2, *lpstrtech2;

// technology class
class technology
{
private:
	char name[20]; // technology name
	int no; // technology number
	const char* unit; // unit of final product from technology
	char fuelname[20]; // name of fuel used
	int fueltype; // fuel number
	int year; // period year or vintage
	double shrwts; //logit share weight
	double eff; // energy intensity
	double necost; // all non-fuel costs (levelized)
	double techcost; // total cost of technology
	double tax; // utility tax
	double carbontax; // carbon tax in $/TC
	double carbontaxgj; // carbon tax in $/GJ
	double carbontaxpaid; // total carbon taxes paid
	double lexp; // logit exponential
	double share; // technology shares
	double input; // total fuel input (fossil and uranium)
	double output; // technology output
	double techchange;  // technical change in %/year
	ghg_ss gases; // suite of greenhouse gases
	vector<Ghg> ghg; // suite of greenhouse gases
	map<string,double> emissmap; // map of ghg emissions
	// attributes for hydroelectricity only!
	double resource; // available hydro resource in energy units
	double A; // logit function shape parameter
	double B; // logit function shape parameter
public:
	technology(void); // default construtor
	technology(const char* nstr,const char* ustr,int yr,double ef);// constructor
	~technology(void); // destructor
	void setall(lpstrtech temp); // sets all technology parameters
	virtual void setall3(lpstrtech2 temp); // sets all technology parameters
	void setall2(strtech temp); // sets all technology parameters (alternative)
	void setghg(void); // set number of greenhouse gases for each technology
	// set number of greenhouse gases for each technology
	void settechghg(int regionno,int sectorno,int subsecno); 
	void applycarbontax(double tax); // apply carbon tax to appropriate technology
	// sets ghg tax to technologies
	void addghgtax(int ghgno,char* ghgname,int country_id,int per); 
	void addghgtax2(int ghgno,char* ghgname,int country_id,int per); 
	double cost(int country_id,int per); // calculates and returns cost of technology
	// uses logit function to calculate technology share
	void calc_share(int country_id,int per); 
	void norm_share(double sum); // normalize technology share
	//void production(double dmd); // calculates fuel input and technology output
	virtual void production(double dmd,int per); // calculates fuel input and technology output
	void emission(char* prodname); // calculates GHG emissions from technology
	void indemission(void); // calculates indirect GHG emissions from technology use
	// write information on technology to screen or file
	void show_tech(void); // write technology information to screen
	void show_techf(const char* ofile); // write technology information to file
	// ****** return names and values ******
	char* showname(void); // return technology name
	char* getfname(void); // return fuel name
	int showfuelno(void); // return fuel number
	double showeff(void); // return fuel efficiency
	double showshare(void); // return normalized share
	double showinput(void); // return fuel input amount
	double showoutput(void); // return technology output
	double showtechcost(void); // return total technology cost
	double shownecost(void); // return non-fuel cost
	double showcarbontax(void); // return carbon taxes in $/TC
	double showcarbontaxgj(void); // return carbon taxes in $/GJ
	double showcarbontaxpaid(void); // return carbon taxes paid
	double getCO2(void); // return actual CO2 emissions from technology
	map<string,double> getemission(void); // return map of all ghg emissions
	vector<Ghg> getGhg(void); // return Ghg object for initialization
	double get_emissmap_second(string str); // return value for ghg
	void setGhg(vector<Ghg> tempghg); // initialize Ghg object from argument 
	double showCO2ind(void); // return indirect CO2 emissions from technology use
	double showCO2fuel(void); // return equivalent CO2 emissions from total fuel input
	double getlexp(void); // return logit exponential for the technology
	void setinput(double in); // set input exogenously
	void setoutput(double out); // set output exogenously
};

// hydroelectricity class inherited from technology class
class hydro_tech : public technology
{
private:
	double resource; // available hydro resource in energy units
	double A; // logit function shape parameter
	double B; // logit function shape parameter

public:
	void setall3(lpstrtech2 temp); // sets all technology parameters
	void production(double dmd,int per); // calculates fuel input and technology output
};
