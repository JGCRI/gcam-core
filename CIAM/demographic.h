/* demographic.h							*
 * This header contains the					*
 * demographics class.						*
 *											*
 * SHK  8/21/00								*/

#include <vector>
using namespace std; // enables elimination of std::

// demographic class
class demographic
{
private:
	// vector of time period 
	// population has 1 more historical period 
	vector<double> malepop; 
	vector<double> femalepop;
	vector<double> totalpop;
	vector<double> laborprod; // labor productivity growth rate
	vector<double> laborforce_p; // labor force participation percent
	vector<double> laborforce; // actual labor force
public:
	demographic(void); //default construtor
	demographic(int per,double mpop,double fpop);//constructor
	~demographic(void); // destructor
	// set size of population and labor productivity variables to max period
	void set(void);
	void initialize(char* region); // reads in database
	double labor(int per); // return labor productivity growthrate
	double total(int per); // return total population
	// return labor force (actual working)
	double getlaborforce(int per);
	void show(int per);
	// outputs to database
	void outputdb(const char *regname,int reg); 
	// outputs to file
	void outputfile(const char *regname,int reg); 
	// MiniCAM outputs to file
	void MCoutput(const char *regname,int reg); 
};
