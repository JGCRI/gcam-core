/* TransSector.cpp									*
* Method definition for Transportation sector      *
* and Subsector classes					        *
* Initiated by MAW  3/14/2003                      *
* Revised to work with latest code                 *
* SHK 6/30/03
*/

#include "Definitions.h"
#include <string>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <cmath>
#include <cassert>
#include <vector>

#include "tranSubsector.h"
#include "technology.h"
#include "scenario.h"
#include "modeltime.h"
#include "xmlHelper.h"
#include "marketplace.h"
#include "summary.h"


using namespace std;

extern ofstream outfile;	
extern Scenario* scenario;


/*  Begin tranSubsector Method Definitions */

//! Default constructor
tranSubsector::tranSubsector() {
    // resize vectors
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    techChange.resize( maxper ); // technical change
    speed.resize( maxper ); // average speed of mode
    popDenseElasticity.resize( maxper );
    adjPrice.resize( maxper ); // price adjusted by time value
    loadFactor.resize( maxper ); // persons or tons per vehicle
    popDensity = 1; // initialize to 1 for now
}


//! Clear member variables.
void tranSubsector::clear()
{
    // call super clear
    subsector::clear();
    
    // now clear own data.
    techChange.clear(); 
    speed.clear();
    popDenseElasticity.clear();
    adjPrice.clear(); 
    loadFactor.clear(); 
}

//! Parses any input variables specific to derived classes
void tranSubsector::XMLDerivedClassParse( const string nodeName, const DOMNode* curr ) {
    
    const Modeltime* modeltime = scenario->getModeltime();
    
    // additional read in for transportation
    if( nodeName == "techChange" ){
        XMLHelper<double>::insertValueIntoVector( curr, techChange, modeltime );
    }
    else if( nodeName == "speed" ){
        XMLHelper<double>::insertValueIntoVector( curr, speed, modeltime );
    }
    else if( nodeName == "popDenseElasticity" ){
        XMLHelper<double>::insertValueIntoVector( curr, popDenseElasticity, modeltime );
    }
    else if( nodeName == "loadFactor" ){
        XMLHelper<double>::insertValueIntoVector( curr, loadFactor, modeltime );
    }
    else if( nodeName == "serviceoutput" ){
        XMLHelper<double>::insertValueIntoVector( curr, output, modeltime );
    }
    
    // completed parsing.
}


//! calculate subsector share numerator
void tranSubsector::calcShare( const string& regionName, const int per, const double gnp_cap )
{
    
    // call function to compute technology shares
    calcTechShares(regionName, per);
    
    // calculate and return subsector share; uses calcPrice function
    // calcPrice() uses normalized technology shares calculated above
    // Logit exponential should not be zero
    
    //compute subsector weighted average price of technologies
    calcPrice( regionName,per);
    
    if(lexp[per]==0) cerr << "TranSubSec Logit Exponential is 0." << endl;
    
    //Adjust price to consider time value 
    const double daysPerYear = 365.0;
    const double hoursPerDay = 24.0;
    
    // convert $/vehicle-mi into $/ser-mi 
    // add cost of time spent on travel by converting gnp/cap into
    // an hourly wage and multipling by average speed
    
    adjPrice[per] = subsectorprice[per]/loadFactor[per] 
        + gnp_cap*1000.0/(daysPerYear*hoursPerDay)/speed[per] ;
    
    /*!  Compute calibrating scaler if first period, otherwise use computed
    scaler in subsecquent periods */
    
    if(per==0) {
        shrwts[0] = output[0] * pow(subsectorprice[per], -lexp[per])
            * pow(gnp_cap, -fuelPrefElasticity[per])
            * pow(popDensity, -popDenseElasticity[per]);
    }
    else {
        share[per]  = shrwts[0] * pow(subsectorprice[per], lexp[per])
            * pow(gnp_cap, fuelPrefElasticity[per])
            * pow(popDensity, popDenseElasticity[per]);
    }
    
}

