/* ghg_ind.h									*
 * This header contains the global				*
 * indirect Greenhouse gas emissions class.		*
 * This is an entity							*
 * class.										*
 * SHK  12/27/00								*/

#include <string>
using namespace std; // enables elimination of std::

struct str_ghgcoef
{
public:
	string name;  // name of sector
	double co2indcoef; // indirect CO2 emissions coefficient (tgC/EJ)
	double ch4indcoef; // indirect CH4 emissions coefficient (tgC/EJ)
};

// ghg_ind class (indirect Greenhouse gas emissions)
class ghg_ind
{
private:
	int noghgindcoef; // number of indirect ghg coefficients
	str_ghgcoef *ghgindcoef; // structure for each intermediate good
public:
	ghg_ind(void); //default construtor
	~ghg_ind(void); // destructor
	void setcoefs(int nossect);
	void setindex(int no, string varname);
	void setco2coef(string varname, double tco2, double toutput);
	void setch4coef(string varname, double tch4, double toutput);
	double showch4coef(string varname);
	double showco2coef(string varname);
	int shownoghgindcoef(void);
};
