/*!ghg.h												*
 * This header contains the	Greenhouse Gas class.		*
 * The Ghg class describes a single gas with			*
 * attributes of gas name, unit, emissions coefficients,*
 * and the calculated emissions.  Emissions emitted		*
 * indirectly through use of technology is also calculated *
 * Coded by: Sonny Kim and Marshall Wise  10/09/02		*/

#include <string>
using namespace std; // enables elimination of std::

// struct for reading in inputs from database
typedef struct
{
	string tname;  // name of ghg
	string tunit; // unit for ghg emissions
	double temiss_coef; // emissions coefficient
} strGhg, *lpstrGhg;

// Ghg class (Greenhouse gas)
class Ghg
{
private:
	string name; // name of ghg gas
	string unit; // unit for ghg gas
	double rmfrac; // fraction of emissions removed
	double gwp; // global warming poential
	double emission; // emissions (calculated)
	double seq_emiss; // sequestered emissions (calculated)
	double emiss_gwp; // gwp emissions (calculated)
	double emiss_coef; // emissions coefficient
	double emiss_fuel; // implied emissions from total fuel consumption
	double emiss_ind; // indirect emissions
public:
	Ghg(void); //default construtor
	~Ghg(void); // destructor
	// set name, unit, rmfrac, gwp, and emiss coefficient for gas
	void init(string gasname,string gasunit,double frac,double emcoef,double tgwp); 
	void initialize(strGhg strghg); // initialize all attributes from data
	void initializeptr(lpstrGhg lpstrghg); // initialize all attributes from data using pointers
	void setcoef(double em_coef); // set emissions coefficients from data
	void setrmfrac(double trmfrac); // set remove fraction from data
	double taxcnvrt(string fuelname); // conversion for carbon tax to energy equivalent
	void calc_emiss(string fuelname,double input,string prodname,double output); // calculate emissions
	void calc_emiss2(double activity,double gamma,double* cntl); // calculate emissions alternative
	void calc_emiss_ind(double input,string fuelname); // calculate ind emissions
	string getname(void); // return name of ghg
	string getunit(void); // return unit for ghg
	double getemission(void); // return ghg emissions
	double getseq_emiss(void); // return sequestered emissions
	double getemiss_fuel(void); // return ghg emissions implicit in fuel
	double getemiss_ind(void); // return indirect ghg emissions
	double getemiss_coef(void); // return ghg emissions coefficient
};
