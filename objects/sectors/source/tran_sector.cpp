/* tran_sector.cpp										*
* Method definition for Transportation sector.         *
* Initiated by MAW  3/14/2003                          *
* Revised to work with latest code                     *
* SHK 6/30/03                                          *
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>

#include "marketplace/include/marketplace.h"
#include "util/base/include/model_time.h"
#include "containers/include/scenario.h"
#include "sectors/include/tran_sector.h"
#include "sectors/include/tran_subsector.h"

// xml headers
#include "util/base/include/xml_helper.h"
#include <xercesc/dom/DOM.hpp>

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
extern ofstream outfile;


//! Default constructor
tranSector::tranSector( const string regionName ): DemandSector( regionName ) {
    // resize vectors
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    percentLicensed.resize( maxper ); // percentage of population licensed
}

//! Destructor.
tranSector::~tranSector() {
}

//! Clear member variables.
void tranSector::clear() {
    
    // call super clear
    DemandSector::clear();
    
    // now clear own data.
    percentLicensed.clear();
}


//! Parses any input variables specific to derived classes
void tranSector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    
    const Modeltime* modeltime = scenario->getModeltime();
    tranSubsector* tempSubSector = 0;
    
    // call the demand sector XML parse to fill demand sector attributes
    DemandSector::XMLDerivedClassParse( nodeName, curr );
    
    if( nodeName == "percentLicensed" ) {
        XMLHelper<double>::insertValueIntoVector( curr, percentLicensed,modeltime );
    } 
    else if( nodeName == "tranSubsector" ){
        tempSubSector = new tranSubsector( regionName, name );
        tempSubSector->XMLParse( curr );
        subsec.push_back( tempSubSector );
    }	
    
}


//! Aggrgate sector energy service demand function.
void tranSector::aggdemand( const double gnp_cap, const double gnp, const int period) { 
    
    const Modeltime* modeltime = scenario->getModeltime();
    double ser_dmd;
    
    /*!  Compute calibrating scaler if first period, otherwise use computed
    scaler in subsequent periods */
    
    // demand for service
    // reading in period 1 data so calibrate scaler to same for both periods
    if (period == 0 || period == 1) {
        priceRatio=1.0;
        priceRatioNotLic=1.0;
        
        // calculate base year scalers
        if (perCapitaBased) { // demand based on per capita GNP
            baseScaler = service[0]* percentLicensed[period] * pow(priceRatio,-pElasticity[period])
                * pow(gnp_cap,-iElasticity[period]);
            baseScalerNotLic = service[0]* (1 - percentLicensed[period]) * pow(priceRatioNotLic,-pElasticity[period])
                * pow(gnp_cap,-iElasticity[period]);
        }
        else {
            baseScaler = service[0]* percentLicensed[period] * pow(priceRatio,-pElasticity[period])
                * pow(gnp,-iElasticity[period]);
            baseScalerNotLic = service[0]* (1 - percentLicensed[period]) * pow(priceRatioNotLic,-pElasticity[period])
                * pow(gnp,-iElasticity[period]);
        }
        // base output is initialized by data
        ser_dmd = service[0]; 

        // Save the service demand without technical change applied for comparison with miniCAM.
        servicePreTechChange[ period ] = ser_dmd;
        service[period] = service[0];
    }
    else {
        // for non-base year
        // note normalized to previous year not base year
        // has implications for how technical change is applied
           priceRatio = sectorprice[period]/sectorprice[period-1];
           priceRatioNotLic = sectorprice[period]/sectorprice[period-1];
        // perCapitaBased is true or false
        if (perCapitaBased) { // demand based on per capita GNP
            ser_dmd = baseScaler*pow(priceRatio,pElasticity[period])*pow(gnp_cap,iElasticity[period])
                + baseScalerNotLic*pow(priceRatioNotLic,pElasticity[period])*pow(gnp_cap,iElasticity[period]);
            // need to multiply above by population ratio (current population/base year
            // population).  The gnp ratio provides the population ratio.
            ser_dmd *= gnp/gnp_cap;
        }
        else { // demand based on scale of GNP
            ser_dmd = baseScaler*pow(priceRatio,pElasticity[period])*pow(gnp,iElasticity[period]);
        }
        // Save the service demand without technical change applied for comparison with miniCAM.
        servicePreTechChange[ period ] = ser_dmd;

        // adjust demand for AEEI, autonomous end-use energy intensity
        // note: not using cummulative technical change
        service[period] = ser_dmd/pow(1+aeei[period],modeltime->gettimestep(period));
    }
    
    output[period] = service[period];
    // sets subsector outputs, technology outputs, and market demands
    setoutput( service[ period ], period );
    sumOutput(period);
}
