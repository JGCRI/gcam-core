/*! 
* \file climat_data.cpp
* \ingroup CIAM
* \brief This file contains the function that writes the input text file for the CLIMAT model.	
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

// standard library
#include "Definitions.h"
#include <iostream>
#include <iomanip>
#include <fstream>
#include <ctime>
// user defined headers
#include "scenario.h"
#include "world.h"
#include "modeltime.h"

using namespace std;

// global variables
extern Scenario* scenario;
extern ofstream gasfile;

//! Write the input text file for the climate model.
void climat_data() {
	const Modeltime* modeltime = scenario->getModeltime();
	World* world = scenario->getWorld();

	int maxper = modeltime->getmaxdataper();
        int per;
    
	gasfile << "10\n Scenario B2-550\n\n\n\n";
	gasfile.setf(ios::right, ios::adjustfield);
	gasfile.setf(ios::fixed, ios::floatfield);
	gasfile.setf(ios::showpoint);
	for(per=1;per<maxper;per++) {
		gasfile << setw(4)
				<< 1975+(per*15) <<","
				<< setw(7) << setprecision(2) << world->showCO2(per)/1000 <<","
				<< setw(7) << setprecision(2) << world->showCO2ag(per) <<","
				<< setw(7) << setprecision(2) << world->showCH4(per) <<","
				<< setw(7) << setprecision(2) << world->showN2O(per) <<","
				<< setw(7) << setprecision(2) << world->showSOXreg1(per) <<","
				<< setw(7) << setprecision(2) << world->showSOXreg2(per) <<","
				<< setw(7) << setprecision(2) << world->showSOXreg3(per) <<","
				<< setw(7) << setprecision(2) << world->showCF4(per) <<","
				<< setw(7) << setprecision(2) << world->showC2F6(per) <<","
				<< setw(7) << setprecision(2) << world->showHFC125(per) <<","
				<< setw(7) << setprecision(2) << world->showHFC134a(per) <<","
				<< setw(7) << setprecision(2) << world->showHFC143a(per) <<","
				<< setw(7) << setprecision(2) << world->showHFC227ea(per) <<","
				<< setw(7) << setprecision(2) << world->showHFC245ca(per) <<","
				<< setw(7) << setprecision(2) << world->showSF6(per) <<",\n";
	}
	for(per=maxper;per<maxper+2;per++) {
		gasfile << setw(4)
				<< 2150+((per-maxper)*140) <<","
				<< setw(7) << setprecision(2) << -3.18 <<","
				<< setw(7) << setprecision(2) << world->showCO2ag(per) <<","
				<< setw(7) << setprecision(2) << world->showCH4(per) <<","
				<< setw(7) << setprecision(2) << world->showN2O(per) <<","
				<< setw(7) << setprecision(2) << world->showSOXreg1(per) <<","
				<< setw(7) << setprecision(2) << world->showSOXreg2(per) <<","
				<< setw(7) << setprecision(2) << world->showSOXreg3(per) <<","
				<< setw(7) << setprecision(2) << world->showCF4(per) <<","
				<< setw(7) << setprecision(2) << world->showC2F6(per) <<","
				<< setw(7) << setprecision(2) << world->showHFC125(per) <<","
				<< setw(7) << setprecision(2) << world->showHFC134a(per) <<","
				<< setw(7) << setprecision(2) << world->showHFC143a(per) <<","
				<< setw(7) << setprecision(2) << world->showHFC227ea(per) <<","
				<< setw(7) << setprecision(2) << world->showHFC245ca(per) <<","
				<< setw(7) << setprecision(2) << world->showSF6(per) <<",\n";
	}
}
