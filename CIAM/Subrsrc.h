/* subrsrc.h							*
 * sub-resources for each region		*
 * subrsrc is a class					*
 * that contains grades 				*/

#include <vector>
#include "grade.h"
#include "ghg_ff.h"
using namespace std; // enables elimination of std::

// class definition for sub-resources
class subrsrc
{
private:
	//char *name; // sub-subrsrc name
	char name[20]; // sub-subrsrc name
	int no; // sub-subrsrc number
	int nograde; // number of grades of each subrsrc
	//***** don't forget to skip space > >
	vector< vector<grade> > depgrade; // amount of subrsrc for each grade
	vector<double> rscprc; // subresource price
	vector<double> available; // total available resource
	vector<double> annualprod; // annual production of subrsrc
	double min_annualprod; // minimum annual production of subrsrc
	vector<double> cummprod; // cummulative production of subrsrc
	vector<ghg_ff> gases; // suite of greenhouse gas
public:
	subrsrc(void); //default construtor
	subrsrc(const char *nstr,int rno);//constructor
	~subrsrc(void); // destructor
	int index(void); // return subrsrc no
	char *showname(void); // return subrsrc name
	void setlabel(const char *nstr,int rno); // reads in sectors from database
	void setlabel2(void); // reads in sectors from database
	void setlabel3(int rno); // reads in sectors from database
	void initper(void); //set vector size
	void setgrades2(int igrade); // sets number of grades for each subsec
	void dbreadgrade(int reg,int is); // reads in data for each grade
	void dbreadgen(char* region,const char *nstr,const char *dbtname);
	double price(int per);
	void cummsupply(double prc,int per); // calculate cummulative production
	double showcummprod(int per); // return cummulative production
	// calculate annual production
	void annualsupply(int per,double gnp1,double gnp2,double price1,double price2);
	double showannualprod(int per); // return annual production
	double showavailable(int per); // return available resource
	double emission(int per); // returns CO2 emissions
	int maxgrade(void); // returns total number of grades
	// output to database table
	void outputdb(const char *regname,int reg,const char *sname); 
	// MiniCAM style output to database table
	void MCoutput(const char *regname,int reg,const char *sname); 
	// output to file
	void outputfile(const char *regname,int reg,const char *sname); 
};

