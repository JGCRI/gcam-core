/*! 
* \file demographic.cpp
* \ingroup Objects-SGM
* \brief Demographic class source file.
* \author Sonny Kim
* \author Katherine Chung
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <map>
#include <fstream>
#include <iostream>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "demographics/include/demographic.h"
#include "demographics/include/population.h"
#include "demographics/include/population_sgm_fixed.h"
#include "demographics/include/population_sgm_rate.h"
#include "demographics/include/population_mini_cam.h"
#include "util/base/include/util.h"
#include "util/logger/include/ilogger.h"
// class for reporting
#include "reporting/include/output_container.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

// static initialize.
const string Demographic::XML_NAME = "demographics";

//! Default constructor.
Demographic::Demographic() {
}

//! Demographic destructor. 
Demographic::~Demographic(){
	clear();
}

//! Helper member function for the destructor. Performs memory deallocation. 
void Demographic::clear(){
	for( PopulationIterator popIter = population.begin(); popIter != population.end(); ++popIter ){
		delete *popIter;
	}
}

//! parses Demographics xml object
void Demographic::XMLParse( const xercesc::DOMNode* node ){
    // make sure we were passed a valid node.
    assert( node );

    // get all child nodes.
    DOMNodeList* nodeList = node->getChildNodes();

    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
		else if( nodeName == PopulationMiniCAM::getXMLNameStatic() ) {
            parseContainerNode( curr, population, yearToMapIndex, new PopulationMiniCAM(), "year" );
        }
		else if( nodeName == PopulationSGMFixed::getXMLNameStatic() ){
            parseContainerNode( curr, population, yearToMapIndex, new PopulationSGMFixed(), "year" );
		}
		else if( nodeName == PopulationSGMRate::getXMLNameStatic() ){
            parseContainerNode( curr, population, yearToMapIndex, new PopulationSGMRate(), "year" );
		}
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing demographics." << endl;
        }
    }
}

//! Write out datamembers to XML output stream.
void Demographic::toInputXML( ostream& out, Tabs* tabs ) const{
	XMLWriteOpeningTag ( getXMLName(), out, tabs );
	
	for( vector<Population*>::const_iterator i = population.begin(); i != population.end(); i++){
		( *i )->toInputXML( out, tabs );
	}

    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Write out XML for debugging purposes.
void Demographic::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
	XMLWriteOpeningTag ( getXMLName(), out, tabs );
	population[ convertPeriodToPopulationIndex( period ) ]->toDebugXML( out, tabs ); 
	XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Complete the initialization.
void Demographic::completeInit(){
	for( vector<Population*>::iterator popIter = population.begin(); popIter != population.end(); ++popIter ) {
		if( popIter == population.begin() ){
			( *popIter )->completeInit();
		}
		else {
            ( *popIter )->completeInit( (*(popIter - 1))->getSurvFemalePop(), (*( popIter - 1 ))->getSurvMalePop()  );
		}
    }
}

//! initialize anything that won't change during the calcuation
void Demographic::initCalc(){
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& Demographic::getXMLName() const{
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
const string& Demographic::getXMLNameStatic(){
	return XML_NAME;
}

//! return total population
double Demographic::getTotal( const int per ) const {
	return population[ convertPeriodToPopulationIndex( per ) ]->getTotal();
}

//! return the male working age population
double Demographic::getWorkingAgePopulationMales( const int per ) const {
	return population[ convertPeriodToPopulationIndex( per ) ]->getWorkingAgePopMale();
}

//! return the female working age population
double Demographic::getWorkingAgePopulationFemales( const int per ) const {
	return population[ convertPeriodToPopulationIndex( per ) ]->getWorkingAgePopFemale();
}

//! return total working age population (male and female)
double Demographic::getWorkingAgePopulation( const int per ) const {
    return population[ convertPeriodToPopulationIndex( per ) ]->getWorkingAgePop();
}

//! Translate a period into the index within the demographic object of the population.
int Demographic::convertPeriodToPopulationIndex( int aPeriod ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    assert( aPeriod >= 0 && aPeriod < modeltime->getmaxper() );

	int year = modeltime->getper_to_yr( aPeriod ); // get year from model period

	// print out error if year doesn't exist
	CYearMapIterator iter = yearToMapIndex.find( util::toString( year ) );
    if( iter == yearToMapIndex.end() ){
		cout << "Error: In convertPeriodToPopulationIndex, Year " << year 
             << " corresponding to period " << aPeriod << " doesn't exist." << endl;
		return 0;
	}
    return iter->second;
}

//! return a vector of total population 
const vector<double> Demographic::getTotalPopVec() const{
	vector<double> newTotalVector;
	newTotalVector.resize( population.size() );
	for ( unsigned int i = 0; i < population.size(); i++ ){
		newTotalVector[ i ] = population[ i ]->getTotal();
	}
	return newTotalVector;
}


//! MiniCAM output to file
void Demographic::dbOutput( const string& regionName ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxPeriod = modeltime->getmaxper();
    vector<double> temp( maxPeriod );

    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);

    // write population to temporary array since not all will be sent to output
    for ( int i = 0; i < maxPeriod; i++ ){
		temp[ i ] = population[ convertPeriodToPopulationIndex( i )  ]->getTotal();
    }
    // function arguments are variable name, double array, db name, table name
    // the function writes all years
    dboutput4( regionName, "General", "Population", "Total", "thous", temp );
}

//! outputing population info to file
void Demographic::csvOutputFile( const string& regionName ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxPeriod = modeltime->getmaxper();
    vector<double> temp( maxPeriod );

    // function protocol
    void fileoutput3( string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);

    // write population to temporary array since not all will be sent to output
    for ( int i = 0; i < maxPeriod; i++ ){
		temp[i] = population[ convertPeriodToPopulationIndex( i ) ]->getTotal();
    }

    // function arguments are variable name, double array, db name, table name
    // the function writes all years
    fileoutput3( regionName," "," "," ","population","1000s",temp);
}

void Demographic::csvSGMOutputFile( ostream& aFile, const int period ) const {
	 aFile << "Demographic Data for Labor Force and Government Transfers" << endl << endl;
	 aFile << getTotal( period ) << ',' << "Total Population" << endl;
	 aFile << getWorkingAgePopulationMales( period ) << ',' << "Working Age Pop. Male" << endl;
	 aFile << getWorkingAgePopulationFemales( period ) << ',' << "Working Age Pop. Females" << endl;
	 aFile << endl;
     const string year = util::toString( scenario->getModeltime()->getper_to_yr( period ) );
	 population[ util::searchForValue( yearToMapIndex, year ) ]->csvSGMOutputFile( aFile, period );
}

// for reporting
void Demographic::updateOutputContainer( OutputContainer* outputContainer, const int aPeriod ) const{
	outputContainer->updateDemographic( this, aPeriod );
}


