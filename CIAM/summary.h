/* summary.h									*
 * This header contains the	summary class		*
 * which contains variables for reporting.		*
 *												*
 * SHK  7/25/02									*/

#include <map>
#include <string> 

using namespace std; // enables elimination of std::

class Summary
{
private:
	map<string, double> fuelcons;  // map of fuel name and amount consumed
	map<string, double> pecons;  // map of primary energy consumption
	map<string, double> peprod;  // map of primary energy production
	map<string, double> petrade;  // map of primary energy trade
	map<string, double> emission;  // map of ghg emissions
public:
	Summary(void); // default construtor
	~Summary(void); // destructor
	void initfuelcons(string fname, double value);
	void initpeprod(string fname, double value);
	void initemission(string ghgname, double value);
	map<string, double> getfuelcons(void);
	map<string, double> getpecons(void);
	map<string, double> getpetrade(void);
	map<string, double> getemission(void);
	void updatefuelcons(map<string, double> fuelinfo);
	void updatepetrade(void);
	void updateemiss(map<string, double> ghginfo);
	void clearfuelcons(void);
	void clearpeprod(void);
	void clearemiss(void);
	double get_fmap_second(string str);
	double get_pemap_second(string str);
	double get_petrmap_second(string str);
	double get_peprodmap_second(string str);
	double get_emissmap_second(string str);
};
