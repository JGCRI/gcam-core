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
#include "Configuration.h"
#include "Util.h"

using namespace std;

// global variables
extern Scenario* scenario;
extern ofstream gasfile;

//! Write the input text file for the climate model.
void writeClimatData() {

    const int maxPeriod = scenario->getModeltime()->getmaxdataper();
    int period;
    
    // First open up the file with all the rest of the gas data and read it in.
    vector<map<string,double> > ghgs( maxPeriod + 2 );
    ifstream gasfile2;
    const string gasFileName = Configuration::getInstance()->getFile( "GHGInputFileName" );
    gasfile2.open( gasFileName.c_str(), ios::in ); // open input file for reading

    util::checkIsOpen( gasfile2, gasFileName );

    // read in all other gases except CO2 from fossil fuels
    // CO2 from fossil fuels comes from model
    const int skiplines = 5;
    for ( int i = 0; i < skiplines; i++ ){
        gasfile2.ignore(80,'\n'); // skip lines
    }
    
    // Now read in all the gases. 
    for ( period=1; period < maxPeriod; period++ ) {
        gasfile2.ignore(80,','); // skip year column
        gasfile2.ignore(80,','); // skip CO2 column
        gasfile2 >> ghgs[period][ "CO2ag" ];
        gasfile2.ignore(80,','); // skip comma
        gasfile2 >> ghgs[period][ "CH4" ];
        gasfile2.ignore(80,','); // skip comma
        gasfile2 >> ghgs[period][ "N2O" ];
        gasfile2.ignore(80,','); // skip comma
        gasfile2 >> ghgs[period][ "SOXreg1" ];
        gasfile2.ignore(80,','); // skip comma
        gasfile2>> ghgs[period][ "SOXreg2" ];
        gasfile2.ignore(80,','); // skip comma
        gasfile2>> ghgs[period][ "SOXreg3" ];
        gasfile2.ignore(80,','); // skip comma
        gasfile2>> ghgs[period][ "CF4" ];
        gasfile2.ignore(80,','); // skip comma
        gasfile2>> ghgs[period][ "C2F6" ];
        gasfile2.ignore(80,','); // skip comma
        gasfile2>> ghgs[period][ "HFC125" ];
        gasfile2.ignore(80,','); // skip comma
        gasfile2>> ghgs[period][ "HFC134a" ];
        gasfile2.ignore(80,','); // skip comma
        gasfile2>> ghgs[period][ "HFC143a" ];
        gasfile2.ignore(80,','); // skip comma
        gasfile2>> ghgs[period][ "HFC227ea" ];
        gasfile2.ignore(80,','); // skip comma
        gasfile2>> ghgs[period][ "HFC245ca" ];
        gasfile2.ignore(80,','); // skip comma
        gasfile2>> ghgs[period][ "SF6" ];
        gasfile2.ignore(80,'\n'); // next line
    }
    
    gasfile2.close();

    // Add on extra periods MAGICC needs. 
    for ( period = maxPeriod; period < maxPeriod + 2; period++ ) {
        ghgs[period]=ghgs[period-1];
    }

    World* world = scenario->getWorld();
    const string scenarioString = "10\n Scenario B2-550\n\n\n";
    
    ofstream gasfile;
    const Configuration* conf = Configuration::getInstance();
    gasfile.open( conf->getFile( "climatFileName" ).c_str(), ios::out );
    gasfile << scenarioString << endl;
    gasfile.setf( ios::right, ios::adjustfield );
    gasfile.setf( ios::fixed, ios::floatfield );
    gasfile.setf( ios::showpoint );

    for( int per = 1; per < maxPeriod + 2; per++ ) {
        if( per < maxPeriod ) {
            gasfile << setw(4) << 1975 + ( per * 15 ) << ","
                << setw(7) << setprecision(2) << world->getGHGEmissions( "CO2", per ) / 1000 << ",";
        }
        else {
            gasfile << setw(4) << 2150 + ( ( per - maxPeriod ) * 140) << ","
                << setw(7) << setprecision(2) << -3.18 <<",";
        }

        gasfile << setw(7) << setprecision(2) << ghgs[ per ][ "CO2ag" ] <<","
            << setw(7) << setprecision(2) << ghgs[ per ][ "CH4" ] <<","
            << setw(7) << setprecision(2) << ghgs[ per ][ "N2O" ] <<","
            << setw(7) << setprecision(2) << ghgs[ per ][ "SOXreg1" ] <<","
            << setw(7) << setprecision(2) << ghgs[ per ][ "SOXreg2" ] <<","
            << setw(7) << setprecision(2) << ghgs[ per ][ "SOXreg3" ] <<","
            << setw(7) << setprecision(2) << ghgs[ per ][ "CF4" ] <<","
            << setw(7) << setprecision(2) << ghgs[ per ][ "C2F6" ] <<","
            << setw(7) << setprecision(2) << ghgs[ per ][ "HFC125" ] <<","
            << setw(7) << setprecision(2) << ghgs[ per ][ "HFC134a" ] <<","
            << setw(7) << setprecision(2) << ghgs[ per ][ "HFC143a" ] <<","
            << setw(7) << setprecision(2) << ghgs[ per ][ "HFC227ea" ] <<","
            << setw(7) << setprecision(2) << ghgs[ per ][ "HFC245ca" ] <<","
            << setw(7) << setprecision(2) << ghgs[ per ][ "SF6" ] << endl;
    }
    gasfile.close();
}
