/* TransSector.cpp									*
* Method definition for Transportation sector      *
* and Subsector classes					        *
* Initiated by MAW  3/14/2003                      *
* Revised to work with latest code                 *
* SHK 6/30/03
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "sectors/include/tran_subsector.h"
#include "technologies/include/tran_technology.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/summary.h"
#include "containers/include/gdp.h"

using namespace std;
using namespace xercesc;
	
extern Scenario* scenario;


/*  Begin TranSubsector Method Definitions */

//! Default constructor
TranSubsector::TranSubsector( const string regionName, const string sectorName ): Subsector( regionName, sectorName ) {
    // resize vectors
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    speed.resize( maxper ); // average speed of mode
    popDenseElasticity.resize( maxper );
    servicePrice.resize( maxper ); // price converted by loadfactor
    timeValue.resize( maxper ); // time value of mode
    generalizedCost.resize( maxper ); // price adjusted by time value
    loadFactor.resize( maxper ); // persons or tons per vehicle
    popDensity = 1; // initialize to 1 for now
    baseScaler = 0;
}


//! Clear member variables.
void TranSubsector::clear()
{
    // call super clear
    Subsector::clear();
    
    // now clear own data.
    speed.clear();
    popDenseElasticity.clear();
    servicePrice.clear(); 
    timeValue.clear(); 
    generalizedCost.clear(); 
    loadFactor.clear(); 
}

//! Parses any input variables specific to derived classes
void TranSubsector::XMLDerivedClassParse( const string nodeName, const DOMNode* curr ) {
    
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxperiod = modeltime->getmaxper();
    DOMNodeList* childNodeList = 0;
    DOMNode* currChild = 0;
    string childNodeName;
    vector<technology*> techVec( modeltime->getmaxper() );
    tranTechnology* tempTech = 0;
    
    // additional read in for transportation
    if( nodeName == "speed" ){
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
    else if( nodeName == "tranTechnology" ){
        map<string,int>::const_iterator techMapIter = techNameMap.find( XMLHelper<string>::getAttrString( curr, "name" ) );
        if( techMapIter != techNameMap.end() ) {
            // technology already exists.
            childNodeList = curr->getChildNodes();
            
            // loop through technologies children.
            for( int j = 0; j < static_cast<int>( childNodeList->getLength() ); j++ ){
                
                currChild = childNodeList->item( j );
                childNodeName = XMLHelper<string>::safeTranscode( currChild->getNodeName() );
                
                if( childNodeName == "#text" ){
                    continue;
                }
                else if( childNodeName == technology::getXMLNameStatic2D() ){
                    int thisPeriod = XMLHelper<int>::getNodePeriod( currChild, modeltime );
                    techs[ techMapIter->second ][ thisPeriod ]->XMLParse( currChild );
                }
            }
        }
        
        else {
            // create a new vector of techs.
            childNodeList = curr->getChildNodes();
            
            // loop through technologies children.
            for( int j = 0; j < static_cast<int>( childNodeList->getLength() ); j++ ){
                
                currChild = childNodeList->item( j );
                childNodeName = XMLHelper<string>::safeTranscode( currChild->getNodeName() );
                
                if( childNodeName == technology::getXMLNameStatic2D() ){
                    tempTech = new tranTechnology();
                    tempTech->XMLParse( currChild );
                    int thisPeriod = XMLHelper<int>::getNodePeriod( currChild, modeltime );
                    techVec[ thisPeriod ] = tempTech;
                    
                    // boolean to fill out the readin value to all the periods
                    const bool fillout = XMLHelper<bool>::getAttr( currChild, "fillout" );
                    
                    // copy technology object for one period to all the periods
                    if (fillout) {
                        // will not do if period is already last period or maxperiod
                        for (int i = thisPeriod+1; i < maxperiod; i++) {
                            techVec[ i ] = new tranTechnology( *tempTech );
                            techVec[ i ]->setYear( modeltime->getper_to_yr( i ) );
                        }
                    }
                    
                }
            }
            techs.push_back( techVec );
            techNameMap[ techVec[ 0 ]->getName() ] = static_cast<int>( techs.size() ) - 1;
            techVec.clear();
            techVec.resize( modeltime->getmaxper(), 0 );
        }
    }
    // completed parsing.
}


//! calculate subsector share numerator
void TranSubsector::calcShare( const int period, const GDP* gdp )
{
    const double gdp_cap = gdp->getBestScaledGDPperCap(period);

    // call function to compute technology shares
    calcTechShares( period );
    
    // calculate and return subsector share; uses calcPrice function
    // calcPrice() uses normalized technology shares calculated above
    // Logit exponential should not be zero
    
    //compute subsector weighted average price of technologies
    calcPrice( period );
    
    if(lexp[period]==0) cerr << "TranSubSec Logit Exponential is 0." << endl;
    
    //Adjust price to consider time value 
    const double daysPerYear = 365.0;
    const double hoursPerDay = 24.0;
    const double weeksPerYear = 50.0;
    const double hoursPerWeek = 40.0;
    
    // convert $/vehicle-mi into $/pass-mi or $/ton-mi 
    // add cost of time spent on travel by converting gdp/cap into
    // an hourly wage and multipling by average speed
    servicePrice[period] = subsectorprice[period]/loadFactor[period] ;
    // calculate time value based on hours worked per year
    // gdp_cap is normalized, need GDP per capita in $/person, fix when available
    const double dollarGDP75 = 3.46985e+12;
    const double population75 = 2.16067e+8;
	double tempGDP = gdp_cap*(dollarGDP75/population75);
	double tempGDP2 = gdp->getApproxGDP(period);
    timeValue[period] = gdp_cap*(dollarGDP75/population75)/(hoursPerWeek*weeksPerYear)/speed[period] ;

    generalizedCost[period] = servicePrice[period] + timeValue[period] ;
    
    /*!  Compute calibrating scaler if first period, otherwise use computed
    scaler in subsequent periods */
    
    if(period==0) {
        baseScaler = output[0] / shrwts[period] * pow(generalizedCost[period], -lexp[period])
            * pow(gdp_cap, -fuelPrefElasticity[period])
            * pow(popDensity, -popDenseElasticity[period]);
    }

    share[period]  = baseScaler * shrwts[period] * pow(generalizedCost[period], lexp[period])
        * pow(gdp_cap, fuelPrefElasticity[period])
        * pow(popDensity, popDenseElasticity[period]);
    
}

//! sets demand to output and output
/* Demand from the "dmd" parameter (could be energy or energy service) is passed to technologies.
*  This is then shared out at the technology level.
*  See explanation for sector::setoutput. 
*/
void TranSubsector::setoutput( const double demand, const int period) {

    int i=0;
    input[period] = 0; // initialize subsector total fuel input 
    carbontaxpaid[period] = 0; // initialize subsector total carbon taxes paid 
    
    // output is in service unit when called from demand sectors
    double subsecdmd = share[period]* demand; // share is subsector level
    //subsecdmd /= loadFactor[period]; // convert to per veh-mi
    
    for ( i=0; i<notech; i++ ) {
        // calculate technology output and fuel input from subsector output
        techs[i][period]->production( regionName, sectorName, subsecdmd, period );
        
        // total energy input into subsector, must call after tech production
        input[period] += techs[i][period]->getInput();
        // sum total carbon tax paid for subsector
        carbontaxpaid[period] += techs[i][period]->getCarbontaxpaid();
    }
    
}

