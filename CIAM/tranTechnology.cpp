/*! 
* \file tranTechnology.cpp
* \ingroup CIAM
* \brief transporation technology class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

// Standard Library headers
#include "Definitions.h"
#include <string>
#include <iostream>
#include <fstream>
#include <cassert>

// User headers
#include "tranTechnology.h"
#include "GHG.H"
#include "scenario.h"
#include "xmlHelper.h"
#include "modeltime.h"
#include "Marketplace.h"

using namespace std;

extern Scenario* scenario;

// tranTechnology class method definition

//! Default constructor.
tranTechnology::tranTechnology() {
    intensity = 1;
}


//! Clear member variables.
void tranTechnology::clear(){
    technology::clear();
    intensity = 1;
}


//! initialize tranTechnology with xml data
void tranTechnology::XMLDerivedClassParse( const string nodeName, const DOMNode* curr ) {
    // additional read in for transportation
    if( nodeName == "intensity" ){
        intensity = XMLHelper<double>::getValue( curr );
    }
    else {
        cout << "Unrecognized text string: " << nodeName << " found while parsing tranTechnology." << endl;
    }
}


//! Calculates fuel input and tranTechnology output.
/*! Adds demands for fuels and ghg emissions to markets in the marketplace
*/
void tranTechnology::production(const string& regionName,const string& prodName,
                                double dmd,const int per) {
    string hydro = "hydro";
    Marketplace* marketplace = scenario->getMarketplace();
    
    // dmd is total subsector demand
    if(name != hydro) {
        output = share * dmd; // use share to get output for each tranTechnology
    }
    else { // do for hydroelectricity
        //output = fixedOutputVal;
        output = fixedOutputVal = dmd;
    }
    
    // eliminated renewable branch for input calc, since code was the same. sjs
    // non renewable technologies previously had
    //input = output/eff/pow(1+techchange,timestep);
    // for transportation technology use intensity instead of efficiency
    // convert from million Btu to EJ
    const double ECONV = 1.055e-9;
    input = output*intensity*ECONV;
	   
    if (input < 0) {
        cerr << "ERROR: Output value < 0 for tranTechnology " << name << endl;
    }
    
    // set demand for fuel in marketplace
    marketplace->setdemand(fuelname,regionName,input,per);
    
    // total carbon taxes paid for reporting only
    // carbontax and carbontaxpaid is null for technologies that do not consume fossil fuels
    // input(EJ), carbontax(90$/GJ), carbontaxpaid(90$Mil)
    carbontaxpaid = input*carbontaxgj*1e+3;
    
    // calculate emissions for each gas after setting input and output amounts
    for (int i=0; i< static_cast<int>( ghg.size() ); i++) {
        ghg[i]->calc_emiss(regionName, fuelname,input,prodName,output);
        // set emissions as demand side of gas market
        marketplace->setdemand(ghg[i]->getname(),regionName,ghg[i]->getemission(),per);		
    }
}

