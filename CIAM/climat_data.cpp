/* climat_data.cpp												*
 * This function writes the input text file for the				*
 * CLIMAT model													*
 * Coded by Sonny Kim 1/20/02									*/

// standard library
#include "Definitions.h"
#include <iostream>
#include <iomanip>
#include <fstream>
#include <ctime> // to use clock and time functions
// user defined headers
#include "world.h"
#include "modeltime.h"

using namespace std; // enables elimination of std::

// global variables
extern World world;
extern ofstream gasfile;
extern Modeltime modeltime;

void climat_data(void)
{
	int maxper = modeltime.getmaxdataper();
        int per;
    
	gasfile << "10\n Scenario B2-550\n\n\n\n";
	gasfile.setf(ios::right, ios::adjustfield);
	gasfile.setf(ios::fixed, ios::floatfield);
	gasfile.setf(ios::showpoint);
	for(per=1;per<maxper;per++) {
		gasfile << setw(4)
				<< 1975+(per*15) <<","
				<< setw(7) << setprecision(2) << world.showCO2(per)/1000 <<","
				<< setw(7) << setprecision(2) << world.showCO2ag(per) <<","
				<< setw(7) << setprecision(2) << world.showCH4(per) <<","
				<< setw(7) << setprecision(2) << world.showN2O(per) <<","
				<< setw(7) << setprecision(2) << world.showSOXreg1(per) <<","
				<< setw(7) << setprecision(2) << world.showSOXreg2(per) <<","
				<< setw(7) << setprecision(2) << world.showSOXreg3(per) <<","
				<< setw(7) << setprecision(2) << world.showCF4(per) <<","
				<< setw(7) << setprecision(2) << world.showC2F6(per) <<","
				<< setw(7) << setprecision(2) << world.showHFC125(per) <<","
				<< setw(7) << setprecision(2) << world.showHFC134a(per) <<","
				<< setw(7) << setprecision(2) << world.showHFC143a(per) <<","
				<< setw(7) << setprecision(2) << world.showHFC227ea(per) <<","
				<< setw(7) << setprecision(2) << world.showHFC245ca(per) <<","
				<< setw(7) << setprecision(2) << world.showSF6(per) <<",\n";
	}
	for(per=maxper;per<maxper+2;per++) {
		gasfile << setw(4)
				<< 2150+((per-maxper)*140) <<","
				<< setw(7) << setprecision(2) << -3.18 <<","
				<< setw(7) << setprecision(2) << world.showCO2ag(per) <<","
				<< setw(7) << setprecision(2) << world.showCH4(per) <<","
				<< setw(7) << setprecision(2) << world.showN2O(per) <<","
				<< setw(7) << setprecision(2) << world.showSOXreg1(per) <<","
				<< setw(7) << setprecision(2) << world.showSOXreg2(per) <<","
				<< setw(7) << setprecision(2) << world.showSOXreg3(per) <<","
				<< setw(7) << setprecision(2) << world.showCF4(per) <<","
				<< setw(7) << setprecision(2) << world.showC2F6(per) <<","
				<< setw(7) << setprecision(2) << world.showHFC125(per) <<","
				<< setw(7) << setprecision(2) << world.showHFC134a(per) <<","
				<< setw(7) << setprecision(2) << world.showHFC143a(per) <<","
				<< setw(7) << setprecision(2) << world.showHFC227ea(per) <<","
				<< setw(7) << setprecision(2) << world.showHFC245ca(per) <<","
				<< setw(7) << setprecision(2) << world.showSF6(per) <<",\n";
	}
}
