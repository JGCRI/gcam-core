/* TransSector.cpp									*
* Method definition for Transportation sector      *
* and Subsector classes					        *
* Initiated by MAW  3/14/2003                      *
* Revised to work with latest code                 *
* SHK 6/30/03
*/

#include "Definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <vector>

#include "tranSubsector.h"
#include "tranTechnology.h"
#include "scenario.h"
#include "modeltime.h"
#include "xmlHelper.h"
#include "marketplace.h"
#include "summary.h"


using namespace std;
using namespace xercesc;

extern ofstream outfile;	
extern Scenario* scenario;


/*  Begin tranSubsector Method Definitions */

//! Default constructor
tranSubsector::tranSubsector() {
    // resize vectors
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    speed.resize( maxper ); // average speed of mode
    popDenseElasticity.resize( maxper );
    servicePrice.resize( maxper ); // price converted by loadfactor
    timeValue.resize( maxper ); // time value of mode
    adjPrice.resize( maxper ); // price adjusted by time value
    loadFactor.resize( maxper ); // persons or tons per vehicle
    popDensity = 1; // initialize to 1 for now
    baseScaler = 0;
}


//! Clear member variables.
void tranSubsector::clear()
{
    // call super clear
    subsector::clear();
    
    // now clear own data.
    speed.clear();
    popDenseElasticity.clear();
    servicePrice.clear(); 
    timeValue.clear(); 
    adjPrice.clear(); 
    loadFactor.clear(); 
}

//! Parses any input variables specific to derived classes
void tranSubsector::XMLDerivedClassParse( const string nodeName, const DOMNode* curr ) {
    
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
            for( int j = 0; j < childNodeList->getLength(); j++ ){
                
                currChild = childNodeList->item( j );
                childNodeName = XMLHelper<string>::safeTranscode( currChild->getNodeName() );
                
                if( childNodeName == "#text" ){
                    continue;
                }
                else if( childNodeName == "period" ){
                    int thisPeriod = XMLHelper<int>::getNodePeriod( currChild, modeltime );
                    techs[ techMapIter->second ][ thisPeriod ]->XMLParse( currChild );
                }
            }
        }
        
        else {
            // create a new vector of techs.
            childNodeList = curr->getChildNodes();
            
            // loop through technologies children.
            for( int j = 0; j < childNodeList->getLength(); j++ ){
                
                currChild = childNodeList->item( j );
                childNodeName = XMLHelper<string>::safeTranscode( currChild->getNodeName() );
                
                if( childNodeName == "period" ){
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
            techNameMap[ techVec[ 0 ]->getName() ] = techs.size() - 1;
            techVec.clear();
            techVec.resize( modeltime->getmaxper(), 0 );
        }
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
    const double weeksPerYear = 50.0;
    const double hoursPerWeek = 40.0;
    
    // convert $/vehicle-mi into $/pass-mi or $/ton-mi 
    // add cost of time spent on travel by converting gnp/cap into
    // an hourly wage and multipling by average speed
    servicePrice[per] = subsectorprice[per]/loadFactor[per] ;
    //timeValue[per] = 10*gnp_cap*1000.0/(hoursPerDay*daysPerYear)/speed[per] ;
    //timeValue[per] = gnp_cap*1000.0/(hoursPerDay*daysPerYear)/speed[per] ;
    // calculate time value based on hours worked per year
    timeValue[per] = gnp_cap*1000.0/(hoursPerWeek*weeksPerYear)/speed[per] ;

    adjPrice[per] = servicePrice[per] + timeValue[per] ;
    
    /*!  Compute calibrating scaler if first period, otherwise use computed
    scaler in subsequent periods */
    
    if(per==0) {
        baseScaler = output[0] / shrwts[per] * pow(adjPrice[per], -lexp[per])
            * pow(gnp_cap, -fuelPrefElasticity[per])
            * pow(popDensity, -popDenseElasticity[per]);
    }

    share[per]  = baseScaler * shrwts[per] * pow(adjPrice[per], lexp[per])
        * pow(gnp_cap, fuelPrefElasticity[per])
        * pow(popDensity, popDenseElasticity[per]);
    
}

//! sets demand to output and output
/* Demand from the "dmd" parameter (could be energy or energy service) is passed to technologies.
*  This is then shared out at the technology level.
*  See explanation for sector::setoutput. 
*/
void tranSubsector::setoutput( const string& regionName, const string& prodName, const double dmd, const int per) {
    int i=0;
    input[per] = 0; // initialize subsector total fuel input 
    carbontaxpaid[per] = 0; // initialize subsector total carbon taxes paid 
    
    // output is in service unit when called from demand sectors
    double subsecdmd = share[per]*dmd; // share is subsector level
    //subsecdmd /= loadFactor[per]; // convert to per veh-mi
    
    for ( i=0; i<notech; i++ ) {
        // calculate technology output and fuel input from subsector output
        techs[i][per]->production( regionName, prodName, subsecdmd, per );
        
        // total energy input into subsector, must call after tech production
        input[per] += techs[i][per]->getInput();
        // sum total carbon tax paid for subsector
        carbontaxpaid[per] += techs[i][per]->getCarbontaxpaid();
    }
    
}

