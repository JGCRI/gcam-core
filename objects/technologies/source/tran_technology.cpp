/*! 
* \file tran_technology.cpp
* \ingroup CIAM
* \brief transporation technology class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

// Standard Library headers
#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <cmath>

// User headers
#include "technologies/include/tran_technology.h"
#include "emissions/include/ghg.h"
#include "containers/include/scenario.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

// tranTechnology class method definition

//! Default constructor.
tranTechnology::tranTechnology() {
    intensity = 1;
    techChangeCumm = 1;
    loadFactor = 1;
    vehicleOutput = 0;
    serviceOutput = 0;
    baseScaler = 0;
}


//! Clear member variables.
void tranTechnology::clear(){
    technology::clear();
    intensity = 1;
    techChangeCumm = 1;
    loadFactor = 1;
    vehicleOutput = 0;
    serviceOutput = 0;
    baseScaler = 0;
}


//! initialize tranTechnology with xml data
void tranTechnology::XMLDerivedClassParse( const string nodeName, const DOMNode* curr ) {
    // additional read in for transportation
    if( nodeName == "intensity" ){
        intensity = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "loadFactor" ){
        loadFactor = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "serviceoutput" ){
        serviceOutput = XMLHelper<double>::getValue( curr );
    }
    else {
        cout << "Unrecognized text string: " << nodeName << " found while parsing tranTechnology." << endl;
    }
}

//! define technology fuel cost and total cost
void tranTechnology::calcCost( const string regionName, const int per ) 
{
    Marketplace* marketplace = scenario->getMarketplace();
    const Modeltime* modeltime = scenario->getModeltime();
    const int timestep = modeltime->gettimestep(per);

    double fuelprice = marketplace->getPrice(fuelname,regionName,per);
    
    if(per>=2) {
        techChangeCumm = pow(1+techchange,timestep*(per-1));
    }
    // fMultiplier and pMultiplier are initialized to 1 for those not read in
    // 75$/GJ 
    const double CVRT90 = 2.212; // 1975 $ to 1990 $
    const double JperBTU = 1055.0; // 1055 Joules per BTU
    
    fuelcost = ( (fuelprice * fMultiplier) + carbonValue ) * intensity/techChangeCumm
             * JperBTU/(1.0E9)*CVRT90;
    techcost = ( fuelcost + necost ) * pMultiplier;
}


//! calculate technology shares
void tranTechnology::calcShare( const string regionName, const int per)
{
    // original technology share calculation
    share = shrwts * pow(techcost,lexp);

    // problem with this code because baseScalar is recalculated every period
/*    if(per==0 || per==1) {
        baseScaler = serviceOutput / shrwts * pow(techcost, -lexp);
    }
    // for base period share = serviceOutput
    share = baseScaler * shrwts * pow(techcost,lexp);
*/
}

//! Calculates fuel input and tranTechnology output.
/*! Adds demands for fuels and ghg emissions to markets in the marketplace
*/
void tranTechnology::production(const string& regionName,const string& prodName,
                                double dmd, const int per) {
    string hydro = "hydro";
    Marketplace* marketplace = scenario->getMarketplace();
    const Modeltime* modeltime = scenario->getModeltime();
    const int timestep = modeltime->gettimestep(per);
    
    // dmd is total subsector demand
    if(name != hydro) {
        output = share * dmd; // use share to get output for each tranTechnology
    }
    else { // do for hydroelectricity
        output = fixedOutputVal = dmd;
    }
    
    // eliminated renewable branch for input calc, since code was the same. sjs
    // for transportation technology use intensity instead of efficiency
    // convert from million Btu to EJ
    vehicleOutput = output/loadFactor;
    const double ECONV = 1.055e-9;

    //intensity /= pow(1+techchange,timestep*per);
    input = vehicleOutput*intensity*ECONV/techChangeCumm;
    //input = vehicleOutput*intensity*ECONV;
   
    if (input < 0) {
        cerr << "ERROR: Output value < 0 for tranTechnology " << name << endl;
    }
    
    // set demand for fuel in marketplace
    marketplace->addToDemand(fuelname,regionName,input,per);
    
    // total carbon taxes paid for reporting only
    // carbontax and carbontaxpaid is null for technologies that do not consume fossil fuels
    // input(EJ), carbonValue(90$/GJ), carbontaxpaid(90$Mil)
    carbontaxpaid = input*carbonValue*1e+3;
    
    // calculate emissions for each gas after setting input and output amounts
    for (int i=0; i< static_cast<int>( ghg.size() ); i++) {
        ghg[i]->calc_emiss(regionName, fuelname,input,prodName,output);
        // set emissions as demand side of gas market
        marketplace->addToDemand(ghg[i]->getname(),regionName,ghg[i]->getemission(),per);		
    }
}


//! return fuel intensity
double tranTechnology::getIntensity(const int per) const {
    return intensity/techChangeCumm;
}

