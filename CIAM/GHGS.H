/* ghg.h										*
 * This header contains the global				*
 * Greenhouse Gas class.  This is an entity		*
 * class.										*
 * SHK  12/11/00								*/



// ghgs class (Greenhouse gas)
class ghgs
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
public:
	ghgs(void); //default construtor
	ghgs(double tco2, double tch4, double tsox, double tn2o);//constructor
	~ghgs(void); // destructor
	void setcoefs(void);
	void calc_allgases(double fuel, char* fueltype);
	double showN2O(void);
	double showSOX(void);
	double showCH4(void);
	double showCO2(void);
};
