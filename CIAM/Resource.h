/* resource.h								*
 * resources for each region				*
 * resource is a class						*
 * that contains resource subsectors 		*/

#include "subrsrc.h"
#include "str_ghgff.h"

// struct for index and name

class resource
{
private:
	//char *name; // resource name
	char name[20]; // resource name
	int no; // resource number
	int nosubrsrc; // number of subsectors for each resource
	vector<subrsrc> depsubrsrc; // subsector objects for each resource
	vector<double> rscprc; // resource price
	vector<double> available; // total resource availabl
	vector<double> annualprod; // annual production rate of resource
	vector<double> cummprod; // cummulative production of resource
	vector<str_ghgff> ghgs; // structure containing ghg emissions
public:
	resource(void); // default construtor
	resource(const char* nstr,int rno); // constructor
	~resource(void); // destructor
	int index(void); // return resource no
	char *showname(void); // return resource name
	void setlabel(const char *nstr,int rno); // reads in sectors from database
	void setlabel2(void); // reads in sectors from database
	void setlabel3(int rno);
	void initper(void); //set vector size
	void setdepsubrsrc(int iss,int* tgrade); // sets number of subsectors
	void initialize(char* region,int tregno); // reads in subsector information
	double price(int per); 
	void cummsupply(double prc,int per); // calculative cummulative supply from supply curve
	double showcummprod(int per); // returns cummulative supply
	// calculates annual supply or production
	void annualsupply(int per,double gnp1,double gnp2,double price1,double price2);
	double showannualprod(int per); // returns annnual production of resource
	double showavailable(int per); // returns total available resource
	double showsubavail(int subrscno,int per); // returns total available subresource
	int shownosubrsrc(void); // returns total number of subsectors
	void emission(int per); // sum subsector emissions
	void show(void); // shows resource name and subresources
	// emissions output to database table
	void ghgoutputdb(const char *regname,int reg); 
	// output to database table
	void outputdb(const char *regname,int reg); 
	// MiniCAM style output to database table
	void MCoutput(const char *regname,int reg); 
	// emissions output to file
	void ghgoutputfile(const char *regname,int reg); 
	// output to file
	void outputfile(const char *regname,int reg); 
};

