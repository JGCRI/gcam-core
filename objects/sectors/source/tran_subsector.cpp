/*! 
* \file tran_subsector.cpp
* \ingroup Objects
* \brief transporation technology class source file.
* \author Marshall Wise, Sonny Kim, Josh Lurz
* \date $Date$
* \version $Revision$
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
const string TranSubsector::XML_NAME = "tranSubsector";

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

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& TranSubsector::getXMLName() const {
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
const std::string& TranSubsector::getXMLNameStatic() {
	return XML_NAME;
}

//! Parses any input variables specific to derived classes
void TranSubsector::XMLDerivedClassParse( const string nodeName, const DOMNode* curr ) {    
    // additional read in for transportation
    const Modeltime* modeltime = scenario->getModeltime();
    if( nodeName == "speed" ){
        XMLHelper<double>::insertValueIntoVector( curr, speed, modeltime );
    }
    else if( nodeName == "popDenseElasticity" ){
        XMLHelper<double>::insertValueIntoVector( curr, popDenseElasticity, modeltime );
    }
    else if( nodeName == "loadFactor" ){
        XMLHelper<double>::insertValueIntoVector( curr, loadFactor, modeltime );
    }
    // Is this going to conflict with parsing output? 
    else if( nodeName == "serviceoutput" ){
        XMLHelper<double>::insertValueIntoVector( curr, output, modeltime );
    }
    // Todo: Rework this to use the subsector parsing.
    else if( nodeName == TranTechnology::getXMLNameStatic1D() ){
        map<string,int>::const_iterator techMapIter = techNameMap.find( XMLHelper<string>::getAttrString( curr, "name" ) );
        if( techMapIter != techNameMap.end() ) {
            // technology already exists.
            DOMNodeList* childNodeList = curr->getChildNodes();
            
            // loop through technologies children.
            for( unsigned int j = 0; j < childNodeList->getLength(); j++ ){
                
                DOMNode* currChild = childNodeList->item( j );
                string childNodeName = XMLHelper<string>::safeTranscode( currChild->getNodeName() );
                
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
            vector<technology*> techVec( modeltime->getmaxper() );
            // create a new vector of techs.
            DOMNodeList* childNodeList = curr->getChildNodes();
            
            // loop through technologies children.
            for( unsigned int j = 0; j < childNodeList->getLength(); j++ ){
                
                DOMNode* currChild = childNodeList->item( j );
                string childNodeName = XMLHelper<string>::safeTranscode( currChild->getNodeName() );
                
                if( childNodeName == technology::getXMLNameStatic2D() ){ // this is "period"
                    TranTechnology* tempTech = new TranTechnology();
                    tempTech->XMLParse( currChild );
                    int thisPeriod = XMLHelper<int>::getNodePeriod( currChild, modeltime );
                    techVec[ thisPeriod ] = tempTech;
                    
                    // boolean to fill out the readin value to all the periods
                    const bool fillout = XMLHelper<bool>::getAttr( currChild, "fillout" );
                    
                    // copy technology object for one period to all the periods
                    if (fillout) {
                        // will not do if period is already last period or maxperiod
                        for (int i = thisPeriod+1; i < modeltime->getmaxper(); i++) {
                            // Check that a technology does not already exist.
                            if( techVec[ i ] ){
                                cout << "Warning: Removing duplicate technology." << endl;
                                delete techVec[ i ];
                            }
                            techVec[ i ] = new TranTechnology( *tempTech );
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

/*! \brief XML output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML
* \author Josh Lurz
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void TranSubsector::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    for( unsigned int i = 0; i < speed.size(); ++i ){
        XMLWriteElementCheckDefault( speed[ i ], "speed", out, tabs, 0.0, modeltime->getper_to_yr( i ) );
    }
    for( unsigned int i = 0; i < popDenseElasticity.size(); ++i ){
        XMLWriteElementCheckDefault( popDenseElasticity[ i ], "popDenseElasticity", out, tabs, 0.0, modeltime->getper_to_yr( i ) );
    }
    for( unsigned int i = 0; i < loadFactor.size(); ++i ){
        XMLWriteElementCheckDefault( loadFactor[ i ], "loadFactor", out, tabs, 0.0, modeltime->getper_to_yr( i ) );
    }
    for( unsigned int i = 0; i < output.size(); ++i ){
        XMLWriteElementCheckDefault( output[ i ], "serviceoutput", out, tabs, 0.0, modeltime->getper_to_yr( i ) );
    }
}	

//! XML output for viewing.
void TranSubsector::toOutputXMLDerived( ostream& out, Tabs* tabs ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    for( unsigned int i = 0; i < speed.size(); ++i ){
        XMLWriteElementCheckDefault( speed[ i ], "speed", out, tabs, 0.0, modeltime->getper_to_yr( i ) );
    }
    for( unsigned int i = 0; i < popDenseElasticity.size(); ++i ){
        XMLWriteElementCheckDefault( popDenseElasticity[ i ], "popDenseElasticity", out, tabs, 0.0, modeltime->getper_to_yr( i ) );
    }
    for( unsigned int i = 0; i < loadFactor.size(); ++i ){
        XMLWriteElementCheckDefault( loadFactor[ i ], "loadFactor", out, tabs, 0.0, modeltime->getper_to_yr( i ) );
    }
    for( unsigned int i = 0; i < output.size(); ++i ){
        XMLWriteElementCheckDefault( output[ i ], "serviceoutput", out, tabs, 0.0, modeltime->getper_to_yr( i ) );
    }
}

//! Write object to debugging xml output stream.
void TranSubsector::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    XMLWriteElement( speed[ period ], "speed", out, tabs );
    XMLWriteElement( popDenseElasticity[ period ], "popDenseElasticity", out, tabs );
    XMLWriteElement( loadFactor[ period ], "loadFactor", out, tabs );
    XMLWriteElement( output[ period ], "serviceoutput", out, tabs );
    XMLWriteElement( servicePrice[ period ], "servicePrice", out, tabs );
    XMLWriteElement( timeValue[ period ], "timeValue", out, tabs );
    XMLWriteElement( generalizedCost[ period ], "generalizedCost", out, tabs );
    XMLWriteElement( popDensity, "popDensity", out, tabs );
    XMLWriteElement( baseScaler, "baseScaler", out, tabs );
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
    const double weeksPerYear = 50.0;
    const double hoursPerWeek = 40.0;
    
    // convert $/vehicle-mi into $/pass-mi or $/ton-mi 
    // add cost of time spent on travel by converting gdp/cap into
    // an hourly wage and multipling by average speed
    servicePrice[period] = subsectorprice[period]/loadFactor[period] ;

    // calculate time value based on hours worked per year
 	 // Convert GDPperCap into dollars (instead of 1000's of $'s)
    // GDP value at this point in the code does not include energy feedback calculation for this year, so is, therefore, approximate
    timeValue[period] = gdp->getApproxGDPperCap( period ) * 1000 /(hoursPerWeek*weeksPerYear)/speed[period];
	 
    generalizedCost[period] = servicePrice[period] + timeValue[period] ;
    
    /*  Compute calibrating scaler if first period, otherwise use computed
    scaler in subsequent periods */
    
    if(period==0) {
        baseScaler = output[0] / shrwts[period] * pow(generalizedCost[period], -lexp[period])
            * pow(gdp_cap, -fuelPrefElasticity[period]) * pow(popDensity, -popDenseElasticity[period]);
    }

    share[period] = baseScaler * shrwts[period] * pow(generalizedCost[period], lexp[period])
        * pow(gdp_cap, fuelPrefElasticity[period]) * pow(popDensity, popDenseElasticity[period]);
    
}

//! sets demand to output and output
/* Demand from the "dmd" parameter (could be energy or energy service) is passed to technologies.
*  This is then shared out at the technology level.
*  See explanation for sector::setoutput. 
*/
void TranSubsector::setoutput( const double demand, const int period, const GDP* gdp ) {

    int i=0;
    input[period] = 0; // initialize subsector total fuel input 
    carbontaxpaid[period] = 0; // initialize subsector total carbon taxes paid 
    
    // output is in service unit when called from demand sectors
    double subsecdmd = share[period]* demand; // share is subsector level
    //subsecdmd /= loadFactor[period]; // convert to per veh-mi

    for ( i=0; i<notech; i++ ) {
        // calculate technology output and fuel input from subsector output
        techs[i][period]->production( regionName, sectorName, subsecdmd, gdp, period );
        
        // total energy input into subsector, must call after tech production
        input[period] += techs[i][period]->getInput();
        // sum total carbon tax paid for subsector
        carbontaxpaid[period] += techs[i][period]->getCarbontaxpaid();
    }
    
}

/*! \brief Writes variables specific to transportation class to database.
*
* \author Steve Smith
*/
void TranSubsector::MCDerivedClassOutput() const {
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);
    
    int i=0, m=0;
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    string str; // tempory string

    // Subsector service price
    dboutput4(regionName,"General","ServicePrice",name," $/pass(ton)-mi",servicePrice);
    // Subsector timeValue price
    dboutput4(regionName,"General","TimeValue",name," $/pass(ton)-mi",timeValue);
    // Subsector speed
    dboutput4(regionName,"General","Speed",name,"Miles/hr",speed);
    // Subsector speed
    dboutput4(regionName,"General","LoadFactor",name,"persons(tons) per vehicle",loadFactor);
}

