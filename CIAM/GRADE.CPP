/* grade.cpp												*
 * Method definition for grade class						*
 * Coded by Sonny Kim 9/13/00								*/

#include <string>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <math.h>
using namespace std; // enables elimination of std::

#include "grade.h"
#include "modeltime.h"

extern Modeltime modeltime;

// grade class method definition
grade::grade(void) // default constructor
{
}

grade::grade(const char* nstr,int rno)
{
	//name = new char [strlen(nstr)+1];
	strcpy(name,nstr);
	no = rno;
}

grade::~grade(void)
{
}

void grade::setall(lprsctech tempstr) //define and initialize technology
{
	// read in data for technology from file or database
	no = tempstr->rno;
	//name = new char [strlen(tempstr->rname)+1];
	strcpy(name,tempstr->rname);
	year = tempstr->yr;
	available = tempstr->tavailable;
	extcost = tempstr->textcost;
	envcost = tempstr->tenvcost;
	techch = tempstr->ttechch;
	tax = tempstr->ttax;
}

void grade::setall2(rsctech tempstr) //define and initialize technology
{
	// read in data for technology from file or database
	no = tempstr.rno;
	strcpy(name,tempstr.rname);
	year = tempstr.yr;
	available = tempstr.tavailable;
	extcost = tempstr.textcost;
	envcost = tempstr.tenvcost;
	techch = tempstr.ttechch;
	tax = tempstr.ttax;
}

void grade::show_grade(void) //show technology info
{
	//write to file or database later
	cout << no <<"  " << name<<"\n";
	cout <<"Year: "<< year<<"\n";
	cout <<"Available: "<< available<<"\n";
	cout <<"Extraction Cost: "<< extcost<<"\n";
	cout <<"Environmental Cost: "<<envcost<<"\n";
	cout <<"Technical Change: "<<techch<<"\n";
	cout <<"Severance Tax: "<<tax<<"\n\n";
}

void grade::cost(int per) //total cost of each grade
{
	double tech;

	tech = pow((1.0 + techch),modeltime.gettimestep(per));
	tcost = (extcost + envcost)/tech + tax;
}

double grade::showavail(void) // return available amount in each grade
{
	return available;
}

double grade::getcost(void) // return available amount in each grade
{
	return tcost;
}

double grade::getextcost(void) // return available amount in each grade
{
	return extcost;
}

double grade::getenvcost(void) // return available amount in each grade
{
	return envcost;
}

void grade::setextcost(double ecost) // return available amount in each grade
{
	extcost = ecost;
}

char* grade::showname(void) // return available amount in each grade
{
	return name;
}

