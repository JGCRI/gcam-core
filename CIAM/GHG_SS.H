/* ghg_ss.h										*
 * This header contains the global				*
 * Greenhouse Gas class.  This is an entity		*
 * class.										*
 * SHK  12/11/00								*/



// ghg_ss class (Greenhouse gas)
class ghg_ss
{
private:
	double CO2; // CO2 emissions (tgC or MTC)
	double CH4; // CH4 emissions (tgC or MTC)
	double SOX; // SOX emissions (tgC or MTC)
	double N2O; // N2O emissions (tgC or MTC)
	double co2oilcoef; // CO2 emissions coefficient for oil (tgC/EJ)
	double co2gascoef; // CO2 emissions coefficient for gas (tgC/EJ)
	double co2coalcoef; // CO2 emissions coefficient for coal (tgC/EJ)
	double ch4oilcoef; // CH4 emissions coefficient for oil (tgC/EJ)
	double ch4gascoef; // CH4 emissions coefficient for gas (tgC/EJ)
	double ch4coalcoef; // CH4 emissions coefficient for coal (tgC/EJ)
	double CO2ind; // indirect CO2 emissions (tgC or MTC)
	double CO2fuel; // equivalent CO2 emissions from fuel input (tgC or MTC)
public:
	ghg_ss(void); //default construtor
	ghg_ss(double tco2, double tch4, double tsox, double tn2o);//constructor
	~ghg_ss(void); // destructor
	void setcoefs(void); // set emissions coefficients for all gases
	void calc_allgases(double fuel,double prod,char* fueltype,char* prodtype);
	void calc_indgases(double fuel,char* fueltype);
	double showN2O(void); // return N2O emissions from technology
	double showSOX(void); // return N2O emissions from technology
	double showCH4(void); // return N2O emissions from technology
	double showCO2(void); // return N2O emissions from technology
	double showCO2ind(void); // return N2O emissions from technology
	double showCO2fuel(void); // return N2O emissions from technology
	double showghgcoef(char* fueltype, char* ghgname); // return ghg emissions coefficient
};
