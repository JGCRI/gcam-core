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
#include <string>

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
   const int maxper = modeltime->getmaxdataper();
   const string scenarioString = "10\n Scenario B2-550\n\n\n";
   
   gasfile << scenarioString << endl;
   gasfile.setf( ios::right, ios::adjustfield );
   gasfile.setf( ios::fixed, ios::floatfield );
   gasfile.setf( ios::showpoint );
   
   for( int per = 1; per < maxper + 2; per++ ) {
      if( per < maxper ) {
         gasfile << setw(4) << 1975 + ( per * 15 ) << ","
            << setw(7) << setprecision(2) << world->getGHGEmissions( "CO2", per ) / 1000 << ",";
      }
      else {
         gasfile << setw(4) << 2150 + ( ( per - maxper ) * 140) << ","
            << setw(7) << setprecision(2) << -3.18 <<",";
      }
      
      gasfile << setw(7) << setprecision(2) << world->getGHGEmissions( "CO2ag", per ) <<","
         << setw(7) << setprecision(2) << world->getGHGEmissions( "CH4", per) <<","
         << setw(7) << setprecision(2) << world->getGHGEmissions( "N2O", per) <<","
         << setw(7) << setprecision(2) << world->getGHGEmissions( "SOXreg1", per) <<","
         << setw(7) << setprecision(2) << world->getGHGEmissions( "SOXreg2", per) <<","
         << setw(7) << setprecision(2) << world->getGHGEmissions( "SOXreg3", per) <<","
         << setw(7) << setprecision(2) << world->getGHGEmissions( "CF4", per) <<","
         << setw(7) << setprecision(2) << world->getGHGEmissions( "C2F6", per) <<","
         << setw(7) << setprecision(2) << world->getGHGEmissions( "HFC125", per) <<","
         << setw(7) << setprecision(2) << world->getGHGEmissions( "HFC134a", per) <<","
         << setw(7) << setprecision(2) << world->getGHGEmissions( "HFC143a", per) <<","
         << setw(7) << setprecision(2) << world->getGHGEmissions( "HFC227ea", per) <<","
         << setw(7) << setprecision(2) << world->getGHGEmissions( "HFC245ca", per) <<","
         << setw(7) << setprecision(2) << world->getGHGEmissions( "SF6", per) << endl;
   }
}
