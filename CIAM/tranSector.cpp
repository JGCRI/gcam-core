/* tranSector.cpp										*
* Method definition for Transportation sector.         *
* Initiated by MAW  3/14/2003                          *
* Revised to work with latest code                     *
* SHK 6/30/03                                          *
*/

#include "Definitions.h"
#include <string>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <cmath>
#include <cassert>

#include "marketplace.h"
#include "modeltime.h"
#include "scenario.h"
#include "tranSector.h"
#include "tranSubsector.h"

// xml headers
#include "xmlHelper.h"
#include <xercesc/util/XMLString.hpp>
#include <xercesc/dom/DOM.hpp>

using namespace std; // enables elimination of std::

extern Scenario* scenario;
extern ofstream outfile;


//! Default constructor
tranSector::tranSector() {
    // resize vectors
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    percentLicensed.resize( maxper ); // percentage of population licensed
}

//! Clear member variables.
void tranSector::clear() {
    
    // call super clear
    demsector::clear();
    
    // now clear own data.
    percentLicensed.clear();
}


//! Parses any input variables specific to derived classes
void tranSector::XMLDerivedClassParse( const string nodeName, const DOMNode* curr ) {
    
    const Modeltime* modeltime = scenario->getModeltime();
    tranSubsector* tempSubSector = 0;
    
    // call the demand sector XML parse to fill demand sector attributes
    demsector::XMLDerivedClassParse( nodeName, curr );
    
    if( nodeName == "percentLicensed" ) {
        XMLHelper<double>::insertValueIntoVector( curr, percentLicensed,modeltime );
    } 
    else if( nodeName == "tranSubsector" ){
        tempSubSector = new tranSubsector();
        tempSubSector->XMLParse( curr );
        subsec.push_back( tempSubSector );
    }	
    
}


//! Aggrgate sector energy service demand function.
void tranSector::aggdemand( const string& regionName, const double gnp_cap, const double gnp, const int per) { 
    
    const Modeltime* modeltime = scenario->getModeltime();
    double ser_dmd;
    
    /*!  Compute calibrating scaler if first period, otherwise use computed
    scaler in subsequent periods */
    
    // demand for service
    if (per == 0) {
        priceRatio=1.0;
        priceRatioNotLic=1.0;
        
        // calculate base year scalers
        if (perCapitaBased) { // demand based on per capita GNP
            scaler = service[0]* percentLicensed[per] * pow(priceRatio,-pElasticity[per])
                * pow(gnp_cap,-iElasticity[per]);
            scalerNotLic = service[0]* (1 - percentLicensed[per]) * pow(priceRatioNotLic,-pElasticity[per])
                * pow(gnp_cap,-iElasticity[per]);
        }
        else {
            scaler = service[0]* percentLicensed[per] * pow(priceRatio,-pElasticity[per])
                * pow(gnp,-iElasticity[per]);
            scalerNotLic = service[0]* (1 - percentLicensed[per]) * pow(priceRatioNotLic,-pElasticity[per])
                * pow(gnp,-iElasticity[per]);
        }
        // base output is initialized by data
        ser_dmd = service[0]; 
    }
    else {
        // for non-base year
        // note normalized to previous year not base year
        // has implications for how technical change is applied
        //   priceRatio = sectorprice[per]/sectorprice[per-1];
        priceRatio = 1;
        //    priceRatioNotLic = sectorprice[per]/sectorprice[per-1];
        priceRatioNotLic = 1;
        // perCapitaBased is true or false
        if (perCapitaBased) { // demand based on per capita GNP
            ser_dmd = scaler*pow(priceRatio,pElasticity[per])*pow(gnp_cap,iElasticity[per])
                + scalerNotLic*pow(priceRatioNotLic,pElasticity[per])*pow(gnp_cap,iElasticity[per]);
            // need to multiply above by population ratio (current population/base year
            // population).  The gnp ratio provides the population ratio.
            ser_dmd *= gnp/gnp_cap;
        }
        else { // demand based on scale of GNP
            ser_dmd = scaler*pow(priceRatio,pElasticity[per])*pow(gnp,iElasticity[per]);
        }
    }
    
    // adjust demand for AEEI, autonomous end-use energy intensity
    // note: not using cummulative technical change
    service[per] = ser_dmd/pow(1+aeei[per],modeltime->gettimestep(per));
    output[per] = service[per];
    // sets subsector outputs, technology outputs, and market demands
    setoutput( regionName,service[per],per);
    sumoutput(per);
}
