/* sector.h									*
 * This header contains the global			*
 * sector class.							*
 *											*
 * SHK  7/7/00								*/


// sector class
/* get errors if this is included
//** Database Headers *****
#include <afxdisp.h>
#include <dbdao.h>
#include <dbdaoerr.h>
*/
//#include <vector>

#include "subsector.h" // generic technology class

using namespace std; // enables elimination of std::

class sector
{
private:
	char name[20]; // sector name
	char* unit; // unit of final product from sector
	int no; // sector number
	int nosubsec; // number of subsectors in each sector
	vector<subsector> subsec; // subsector objects
	vector<double> sectorprice; // sector price for all periods
	vector<double> pe_cons; // sectoral primary energy consumption
	vector<double> input; // sector total energy consumption
	vector<double> output; // total amount of final output from sector
	double tax; // sector tax or subsidy
	vector<double> carbontaxpaid; // total sector carbon taxes paid
protected:
	vector<Summary> summary; // summary for reporting
public:
	sector(void); //default construtor
	sector(const char* nstr,int sno,double ttax);//constructor
	~sector(void); // destructor
	int index(void); // return sector number (secno)
	void setlabel(const char* nstr,int sno); // sets sector name and index
	char* showname(void); // return name of sector
	int showno(void); // return sector no
	void initper(void); //set vector size
	// sets the number of subsectors for each sector and region
	void set_subsec(int iss,int* ttech); // sets number of subsectors 
	// sets the number of ghg for technologies
	void settechghg(int regionno);
	// reads in subsector info from supply recordset
	void init_Ssubsec(char* region,const char* rstname);
	// reads in subsector info from demand recordset
	void init_Dsubsec(char* region,const char* rstname);
	// calls technology initialization
	void init_stech(char* region,int tregno); 
	void init_dtech(char* region,int tregno); 
	void applycarbontax(double tax,int per); // passes along regional carbon tax
	void addghgtax(int ghgno,char* ghgname,int country_id,int per); // sets ghg tax to technologies
	void calc_share(char* varcountry,int country_id,int per); // calculates and noralizes shares
	void price(int per); // calculates sector price
	void production(char* varcountry,int country_id,int per); // calculates production using mrk prices
	void setoutput(char* varcountry,int country_id,double dmd, int per); // sets demand to totoutput and output
	void sumoutput(int per); // sum subsector outputs
	void set_ser_dmd(double dmd, int per); // sets end-use sector service demand
	void supply(char* varcountry,int country_id,int per); // calculates supply from each subsector
	void show(void);
	void showsubsec(int per, const char* ofile); // shows all subsectors in the sector
	void showlabel(const char* ofile); // show sector label
	void setbaseser(char* region,const char* dbtname); // reads in sector base service from database
	void setbaseshr(char* region,const char* dbtname); // reads in sector base share from database
	int shownosubsec(void);
	char* showsubsecname(int iss);
	double getoutput(int per); // returns sector output 
	double showprice(int per); // returns sector aggregate price
	void emission(int per); // sum subsector emissions
	void indemission(int per); // sum subsector indirect emissions
	double showpe_cons(int per); // return sector primary energy consumption
	void suminput(int per); // sums subsector primary and final energy consumption
	double showinput(int per); // return sector energy consumption
	void outputdb(const char* regname,int reg); // write out sector result to database
	virtual void outputfile(const char* regname,int reg); // write out sector result to file
	void MCoutput_subsec(const char* regname,int reg); // calls write for subsector 
	virtual void MCoutput(const char* regname,int reg); // write out sector result to file
	void subsec_outfile(const char* regname,int reg);  // call fn to write subsector output
	double showcarbontaxpaid(int per); // return total sector carbon taxes paid
	map<string, double> getfuelcons(int per); // get fuel consumption map
	double getfuelcons_second(int per,string key); // get second of fuel consumption map
	void clearfuelcons(int per);  //  clears the fuelcons map in summary
	map<string, double> getemission(int per);// get ghg emissions map in summary object 
	map<string, double> getemfuelmap(int per);// get ghg emissions map in summary object 
};


// demand sector class derived from base sector class
class demsector : public sector
{
private:
	vector<double> fe_cons; // end-use sector final energy consumption
	vector<double> service; // total end-use sector service 
	double ielasticity; // income elasticity 
	double aeei; // autonomous end-use energy intensity parameter
public:
	void initper_ds(void); //set vector size
	// aggregate demand for service
	void aggdemand(char* varcountry,int country_id,double prc,double x,double ie,
		int per); 
	void outputfile(const char* regname,int reg); // write out sector result to file
	void MCoutput(const char* regname,int reg); // write out sector result to file
};
