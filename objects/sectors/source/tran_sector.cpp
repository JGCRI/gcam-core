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
#include "containers/include/gdp.h"

// xml headers
#include "util/base/include/xml_helper.h"
#include <xercesc/dom/DOMNode.hpp>

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string TranSector::XML_NAME = "tranSector";

//! Default constructor
TranSector::TranSector( const string regionName ): DemandSector( regionName ) {
    // resize vectors
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    percentLicensed.resize( maxper ); // percentage of population licensed
}

//! Destructor.
TranSector::~TranSector() {
}

//! Clear member variables.
void TranSector::clear() {
    
    // call super clear
    DemandSector::clear();
    
    // now clear own data.
    percentLicensed.clear();
}


//! Parses any input variables specific to derived classes
void TranSector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    
    const Modeltime* modeltime = scenario->getModeltime();
    
    // call the demand sector XML parse to fill demand sector attributes
    DemandSector::XMLDerivedClassParse( nodeName, curr );
    
    if( nodeName == "percentLicensed" ) {
        XMLHelper<double>::insertValueIntoVector( curr, percentLicensed,modeltime );
    } 
    else if( nodeName == "tranSubsector" ){
        parseContainerNode( curr, subsec, subSectorNameMap, new TranSubsector( regionName, name ) );
    }	
}

/*! \brief Perform any sector level calibration data consistancy checks
*
* Check to make sure that total calibrated outputs are equal to sector demand in base period.
* 
* \author Steve Smith
* \param period Model period
*/
void TranSector::checkSectorCalData( const int period ) {
// Since period 1 is calibrated, must make sure that read-in calibration is equal to demand. 

   // Adjust aggregate demand to match calibrated outputs of all inputs to this sector are calibrated
  if ( inputsAllFixed( period, "allInputs" ) ) {
      double scaleFactor = getCalOutput( period ) / service[0];
      service[0] = scaleFactor * service[0];
      logfile << "Calibrated Demand Scaled by " << scaleFactor << " in Region " << regionName << " sector: " << name << endl;
   }
}

//! Aggrgate sector energy service demand function.
void TranSector::aggdemand( const GDP* gdp, const int period ) { 
      
    double gdp_cap = gdp->getBestScaledGDPperCap(period); 
			 
	double gdp1 = gdp->getApproxScaledGDP(period); //gdp->getGDP(period); 
	
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
        if (perCapitaBased) { // demand based on per capita GDP
            baseScaler = service[0]* percentLicensed[period] * pow(priceRatio,-pElasticity[period])
                * pow(gdp_cap,-iElasticity[period]);
            baseScalerNotLic = service[0]* (1 - percentLicensed[period]) * pow(priceRatioNotLic,-pElasticity[period])
                * pow(gdp_cap,-iElasticity[period]);
        }
        else {
            baseScaler = service[0]* percentLicensed[period] * pow(priceRatio,-pElasticity[period])
                * pow(gdp1,-iElasticity[period]);
            baseScalerNotLic = service[0]* (1 - percentLicensed[period]) * pow(priceRatioNotLic,-pElasticity[period])
                * pow(gdp1,-iElasticity[period]);
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
        if (perCapitaBased) { // demand based on per capita GDP
            ser_dmd = baseScaler*pow(priceRatio,pElasticity[period])*pow(gdp_cap,iElasticity[period])
                + baseScalerNotLic*pow(priceRatioNotLic,pElasticity[period])*pow(gdp_cap,iElasticity[period]);
            // need to multiply above by population ratio (current population/base year
            // population).  The gdp ratio provides the population ratio.
            ser_dmd *= gdp1/gdp_cap;
        }
        else { // demand based on scale of GDP
            ser_dmd = baseScaler*pow(priceRatio,pElasticity[period])*pow(gdp1,iElasticity[period]);
        }
        // Save the service demand without technical change applied for comparison with miniCAM.
        servicePreTechChange[ period ] = ser_dmd;

        // adjust demand for AEEI, autonomous end-use energy intensity
        // note: not using cummulative technical change
        service[period] = ser_dmd/pow(1+aeei[period],modeltime->gettimestep(period));
    }
    
    output[period] = service[period];
    // sets subsector outputs, technology outputs, and market demands
    setoutput( service[ period ], period, gdp );
    sumOutput(period);
    
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& TranSector::getXMLName() const {
	return XML_NAME;
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const std::string& TranSector::getXMLNameStatic() {
	return XML_NAME;
}
