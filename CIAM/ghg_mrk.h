/* ghg_mrk.h										*
 * This header contains the global				*
 * Greenhouse Gas Market class.						*
 * 										*
 * SHK  10/18/01								*/

#include <vector>
using namespace std;

// ghg_mrk class (Greenhouse gas)
class ghg_mrk
{
private:
	char name[20]; // ghg name
	char* unit; // ghg unit
	int no; // ghg number
	vector<double> constraint; // emissions constraint by year(tgC or MTC)
	vector<double> emission; // emissions by year(tgC or MTC)

public:
	ghg_mrk(void); //default construtor
	~ghg_mrk(void); // destructor
	void initper(void); // resize vector to max period
	void setconstraint(int reg,int var1); // set emissions constraint from database
	char* showname(void); // show emissions name
	int showghgno(void); // show emissions number
	void setemission(double amount,int per); // set emissions
	double showconstraint(int per); // return emissions constraint
	double showemission(int per); // return emissions
};
