/* Emcoef_ind.h										*
 * This header contains the indirect Greenhouse gas	*
 * emissions coefficients.							*
 * Each class contains a map object of emissions	*
 * coefficients, and each emisscoef_ind object is	*
 * used to represent the secondary energy sector.	*
 * SHK  11/13/02									*/
 
#include <string>
#include <map>
using namespace std; // enables elimination of std::

class Emcoef_ind
{
private:
	string name; // name of secondary good or sector
	map<string, double> emcoef; // contains all coefficients for all gases
public:
	Emcoef_ind(void); //default construtor
	~Emcoef_ind(void); // destructor
	// set the secondary good or sector name
	void setname(string secname);
	void setemcoef(map<string,double> eminfo, double toutput);
	string getname(void);
	double getemcoef(string gasname);
};
