/* scenario.h										*
 * This header contains the definition for	the 	*
 * the scenario class.								*
 * SHK  3/12/02										*/


// scenario class
class Scenario
{
private:
	int id; // scenario id
	char name[20]; // scenario name
public:
	Scenario(void); // default construtor
	~Scenario(void); // destructor
	void setall(void); // sets all scenario parameters
	// ****** return name and id ******
	char* getname(void); // return scenario name
	int getid(void); // return fuel number
};
