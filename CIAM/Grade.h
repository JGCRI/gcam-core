/* grade.h												*
 * technologies representing grade for each resource	*
 * grade is a class										*
 * that contains technologies that characterized each grade	*/

// struct for database
typedef struct
{
	const char *rname;  // technology name
	long rno; // technology number
	int yr; // year
	double tavailable; // amount of grade for each grade
	double textcost; // extraction cost of each grade
	double tenvcost; // environmental cost of each grade
	double ttechch; // technical change for all grades
	double ttax; // severance tax
} rsctech, *lprsctech;


class grade
{
private:
	//char* name; // grade name
	char name[20]; // grade name
	int no; // grade number
	int year; // year
	double available; // amount of grade for each grade
	double extcost; // extraction cost of each grade
	double envcost; // environmental cost of each grade
	double techch; // technical change for all grades
	double tax; // severance tax
	double tcost; // total cost
public:
	grade(void); //default construtor
	grade(const char* nstr,int rno);//constructor
	~grade(void); // destructor
	void setall(lprsctech tempstr); // creates recordset each time
	void setall2(rsctech tempstr); // read from global recordset
	void show_grade(void);
	void cost(int per);
	double showavail(void);
	double getcost(void);
	double getextcost(void);
	double getenvcost(void);
	void setextcost(double ecost);
	char* showname(void);
};

