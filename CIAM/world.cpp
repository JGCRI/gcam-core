/*! 
* \file world.cpp
* \ingroup CIAM
* \brief world class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <vector>
#include <map>
#include <algorithm>
#include <xercesc/dom/DOM.hpp>
#include "xmlHelper.h"
#include "world.h"
#include "Region.h"
#include "AgSector.h"
#include "scenario.h"
#include "modeltime.h"
#include "Marketplace.h"
#include "Configuration.h"
#include "Util.h"
#include "Summary.h"

using namespace std;
using namespace xercesc;

extern "C" { void _stdcall AG2INITC( double[14][12] ); };
extern Scenario* scenario;

//! Default constructor.
World::World() {
    // initialize elemental datamembers.
    doCalibrations = true;
    // We can resize all the arrays because we are garunteed by the schema that the modeltime object is parsed first.
    ghgs.resize( scenario->getModeltime()->getmaxper() ); // structure containing ghg emissions
}

//! World destructor. 
World::~World(){
    clear();
}

//! Helper member function for the destructor. Clears all clears all datastructures and deallocates memory. 
void World::clear(){

    ghgs.clear();
    for ( vector<Region*>::iterator regionIter = regions.begin(); regionIter != regions.end(); regionIter++ ) {
        delete *regionIter;
    }

    regions.clear();
}

//! parses World xml object
void World::XMLParse( const DOMNode* node ){

    string nodeName;
    DOMNode* curr = 0;
    Region* tempRegion = 0; // tempory region object

    // assume we are passed a valid node.
    assert( node );

    // get all the children.
    DOMNodeList* nodeList = node->getChildNodes();

    for( int i = 0;  i < static_cast<int>( nodeList->getLength() ); i++ ){
        curr = nodeList->item( i );
        nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }

        else if( nodeName == "region" ){
            // Check if the region already exists.
            map<string,int>::const_iterator regionIter = regionNamesToNumbers.find( XMLHelper<string>::getAttrString( curr, "name" ) );
            if( regionIter != regionNamesToNumbers.end() ) {
                // Region exists.
                regions[ regionIter->second ]->XMLParse( curr );
            }
            else {
                tempRegion = new Region();
                tempRegion->XMLParse( curr );
                regions.push_back( tempRegion ); // resizes vector of region objects
                regionNamesToNumbers[ tempRegion->getName() ] = static_cast<int>( regions.size() ) - 1;
            }
        }
        else if( nodeName == "primaryFuelName" ) {
            // Get the fuel name.
            const string primaryFuelName = XMLHelper<string>::getValueString( curr );

            // Check if it already exists.
            if( std::find( primaryFuelList.begin(), primaryFuelList.end(), primaryFuelName ) == primaryFuelList.end() ) {
                primaryFuelList.push_back( primaryFuelName );
            }
        }
        else {
            cout << "Unrecognized text string: " << nodeName << " found while parsing World." << endl;
        }
    }
}

//! Complete the initialization.
void World::completeInit() {

    // Finish initializing all the regions.
    for( vector<Region*>::iterator regionIter = regions.begin(); regionIter != regions.end(); regionIter++ ) {
        ( *regionIter )->completeInit();
    }

    // Initialize AgLU
    Configuration* conf = Configuration::getInstance();
    if( conf->getBool( "agSectorActive" ) ) {
        initAgLu();
    }
}

//! Initialize the AgLu model.
void World::initAgLu() {

    cout << "Initializing agLU" << endl;
    double prices[ 14 ][ 12 ]; 

    vector<double> tempVec( 12 );

#if(__HAVE_FORTRAN__)		
    AG2INITC( prices ); // not implimented for non-PC's at this time
#endif

    for ( int j = 0; j < static_cast<int>( regions.size() ); j++ ) {
        for ( int k = 0; k < AgSector::getNumAgMarkets(); k++ ) {
            tempVec[ k ] = prices[ j ][ k ];
        }
        regions[ j ]->initializeAgMarketPrices( tempVec );
    }
}

//! Write out datamembers to XML output stream.
void World::toXML( ostream& out ) const {

    // write the beginning tag.
    Tabs::writeTabs( out );
    out << "<world>" << endl;

    // increase the indent.
    Tabs::increaseIndent();

    // write the xml for the class members.
    // for_each( region.begin(), region.end(), bind1st( mem_fun_ref( &Region::toXML ), out ) );
    // won't work with VC 6.0. Forgot to implement const mem_fun_ref helper. whoops.
    for( vector<string>::const_iterator fuelIter = primaryFuelList.begin(); fuelIter != primaryFuelList.end(); fuelIter++ ) {
        XMLWriteElement( *fuelIter, "primaryFuelName", out );
    }

    for( vector<Region*>::const_iterator i = regions.begin(); i != regions.end(); i++ ){
        ( *i )->toXML( out );
    }
    // finished writing xml for the class members.

    // decrease the indent.
    Tabs::decreaseIndent();

    // write the closing tag.
    Tabs::writeTabs( out );
    out << "</world>" << endl;

}

//! Write out XML for debugging purposes.
/*! \warning This only call Region::toXML for the US. */
void World::toDebugXML( const int period, ostream& out ) const {

    // write the beginning tag.
    Tabs::writeTabs( out );
    out << "<world period=\"" << period << "\">" << endl;

    // increase the indent.
    Tabs::increaseIndent();

    // write the xml for the class members.
    for( vector<string>::const_iterator fuelIter = primaryFuelList.begin(); fuelIter != primaryFuelList.end(); fuelIter++ ) {
        XMLWriteElement( *fuelIter, "primaryFuelName", out );
    }

    // for_each( region.begin(), region.end(), bind1st( mem_fun_ref( &Region::toXML ), out ) );
    // won't work with VC 6.0. Forgot to implement const mem_fun_ref helper. whoops.
    scenario->getMarketplace()->toDebugXML( period, out );

    for( vector<Region*>::const_iterator i = regions.begin(); i == regions.begin(); i++ ) { 
        // for( vector<Region*>::const_iterator i = region.begin(); i != region.end(); i++ ) {
        ( *i )->toDebugXML( period, out );
    }

    for( vector<map<string,double> >::const_iterator j = ghgs.begin(); j != ghgs.end(); j++ ) {
        // j->toDebugXML( out ); // not yet implemented.
    }
    // finished writing xml for the class members.

    // decrease the indent.
    Tabs::decreaseIndent();

    // write the closing tag.
    Tabs::writeTabs( out );
    out << "</world>" << endl;
}

//! initialize anything that won't change during the calcuation
/*! Examples: share weight scaling due to previous calibration, 
* cumulative technology change, etc.
*/
void World::initCalc( const int period ) {	
    for( vector<Region*>::iterator i = regions.begin(); i != regions.end(); i++ ){
        ( *i )->initCalc( period );
    }
}

//! calculate supply and demand and emissions for all regions
/*! This is the main action loop for the model. 
Uses "MiniCAM" style logic where primary costs are calculated, 
then prices of refined fuels, end-use costs, end-use, etc. */
void World::calc( const int period, const vector<string>& regionsToSolve ) {	

    vector<int> regionNumbersToSolve;

    Configuration* conf = Configuration::getInstance();

    if ( regionsToSolve.size() == 0 ) {
        regionNumbersToSolve.resize( regions.size() );

        for( int regionNumber = 0; regionNumber < static_cast<int> ( regionNumbersToSolve.size() ); regionNumber++ ) {
            regionNumbersToSolve[ regionNumber ] = regionNumber;
        }
    }
    else {
        for( vector<string>::const_iterator regionName = regionsToSolve.begin(); regionName != regionsToSolve.end(); regionName++ ) {
            map<string,int>::const_iterator foundName = regionNamesToNumbers.find( *regionName );
            if ( foundName != regionNamesToNumbers.end() ) {
                const int regionNumber = foundName->second; 
                regionNumbersToSolve.push_back( regionNumber );
            }
            else {
                cout << "Error: Region " << *regionName << " not found." << endl;
            }
        }
    }

    for ( vector<int>::iterator i = regionNumbersToSolve.begin(); i != regionNumbersToSolve.end(); i++ ) {
        // Write back calibrated values to the member variables.
        // These are still trial values.
        if( conf->getBool( "CalibrationActive" ) ) {
            regions[ *i ]->writeBackCalibratedValues( period );
        }
        // calculate regional GNP
        regions[ *i ]->calcGnp( period );
        // apply carbon taxes to appropriate technologie
        regions[ *i ]->applycarbontax(period);
        // set regional GHG constraint to market supply
        regions[ *i ]->setGhgSupply(period);
        // set regional GHG tax to individual technologies
        regions[ *i ]->addGhgTax(period);
        // determine supply of primary resources
        regions[ *i ]->rscSupply(period);
        // determine prices of refined fuels and electricity
        regions[ *i ]->finalSupplyPrc(period);
        // calculate enduse service price
        regions[ *i ]->calcEndUsePrice( period );
        // adjust gnp for energy cost changes
        regions[ *i ]->adjustGnp(period);

        // determine end-use demand for energy and other goods
        regions[ *i ]->enduseDemand(period);

        // determine supply of final energy and other goods based on demand
        regions[ *i ]->finalSupply(period);

        if( conf->getBool( "agSectorActive" ) ){
            regions[ *i ]->calcAgSector(period);
        }

        // Perform calibrations
        if( conf->getBool( "CalibrationActive" ) ) {
            regions[ *i ]->calibrateRegion( doCalibrations, period );
        }

    }

}

//! Update all summary information for reporting
// Orginally in world.calc, removed to call only once after solved
void World::updateSummary( const int period ) {
    for( vector<Region*>::iterator i = regions.begin(); i != regions.end(); i++ ){
        ( *i )->emission( period );
        ( *i )->updateSummary( period );
        ( *i )->calcEmissFuel( period );
    }
}

//! calculate indirect emissions for each region
void World::emiss_ind( const int period ) {
    for( vector<Region*>::iterator i = regions.begin(); i != regions.end(); i++ ){
        ( *i )->emissionInd( period ); // calculate indirect emissions
    }
}

//! Calculates the global emissions. Currently only CO2 exists.
void World::calculateEmissionsTotals() {
    for( vector<Region*>::iterator iter = regions.begin(); iter != regions.end(); iter++ ) {
        for( int i = 0; i < scenario->getModeltime()->getmaxper(); i++ ){
            Summary tempSummary = ( *iter )->getSummary( i );
            ghgs[ i ][ "CO2" ] += tempSummary.get_emissmap_second( "CO2" );
        }
    }
}

//! write results for all regions to file
void World::outputfile() const {
    const int maxper = scenario->getModeltime()->getmaxper();
    vector<double> temp(maxper);
    // function protocol
    void fileoutput3(string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);

    // write global population results to database
    // fileoutput3("global"," "," "," ","population","Millions",population);

    // write total emissions for World
    for ( int m = 0; m < maxper; m++ ){
        map<string,double>::const_iterator ghgValue = ghgs[m].find( "CO2" );
        if( ghgValue != ghgs[ m ].end() ){
            temp[m] = ghgValue->second;
        } else {
            temp[m] = 0;
        }
    }
    fileoutput3( "global"," "," "," ","CO2 emiss","MTC",temp);

    for( vector<Region*>::const_iterator i = regions.begin(); i != regions.end(); i++ ){
        ( *i )->outputFile();
    }
}

//! MiniCAM style output to database
void World::MCoutput() const {
    // call regional output
    for( vector<Region*>::const_iterator i = regions.begin(); i != regions.end(); i++ ){
        ( *i )->MCoutput();
    }
}

//! turn on calibrations
void World::turnCalibrationsOn() {
    doCalibrations = true;
}

//! turn off calibrations
void World::turnCalibrationsOff(){
    doCalibrations = false;
}

//! return calibration setting
bool World::getCalibrationSetting() const {
    return doCalibrations;
}

//! Return the amount of the given GHG in the given period.
double World::getGHGEmissions( const std::string& ghgName, const int period ) const {
    assert( period < static_cast<int>( ghgs.size() ) );

    map<string,double>::const_iterator iter = ghgs[ period ].find( ghgName );

    if( iter != ghgs[ period ].end() ) {
        return iter->second;
    }
    else {
        cout << "GHG: " << ghgName << " was not found for period " << period << "." << endl;
        return 0;
    }
}
/*! \brief This function returns a special mapping of strings to ints for use in the outputs. 
* \details This map is created such that global maps to zero, region 0 maps to 1, etc
* It is similiar to the regionNamesToNumbers map but has the global element and each region number in the 
* regionMap is 1 + the number in the regionNamesToNumbers map.
* \warning This function should only be used by the database output functions. 
* \return The map of region names to numbers.
*/
const map<string,int> World::getOutputRegionMap() const {
    map<string,int> regionMap;

    for ( int i = 0; i < static_cast<int>( regions.size() ); i++ ) {
        regionMap[regions[i]->getName()] = i+1; // start index from 1
    }
    // hardcode for now
    regionMap["global"] = 0;
    return regionMap;
}

void World::setupCalibrationMarkets() {
    for( vector<Region*>::iterator i = regions.begin(); i != regions.end(); i++ ) {
        ( *i )->setupCalibrationMarkets();
    }
}

/* \brief This function returns a vector of all region names which exist in the world.
* \detailed This function creates a vector of region names in the same order as they exist 
* in the world.
* \todo There are still several functions returning region names.
* \return A constant vector of region names.
*/
const vector<string> World::getRegionVector() const {
    vector<string> regionNames;

    for( vector<Region*>::const_iterator i = regions.begin(); i != regions.end(); i++ ) {
        regionNames.push_back( ( *i )->getName() );
    }
    return regionNames;
}

/*! \brief A function which print dependency graphs showing fuel usage by sector.
*
* This function is called by Scenario::printGraphs to iterate through the regions and call
* Region::printGraphs, which does the actual printing.
*
* \param outStream An output stream to write to which was previously created.
* \param period The period to print graphs for.
* \return void
* \warning Currently only the U.S. has graphs printed for it.
*/
void World::printGraphs( ostream& outStream, const int period ) const {
    assert( outStream );

    // Only do the US for now.
    for ( vector<Region*>::const_iterator regionIter = regions.begin(); regionIter == regions.begin(); regionIter++ ) {
        ( *regionIter )->printGraphs( outStream, period );
    }
}

//! Return the list of primary fuels. 
const vector<string> World::getPrimaryFuelList() const {
    return primaryFuelList;
}

//! Return the primaryFuelCO2Coef for a specific region and fuel.
double World::getPrimaryFuelCO2Coef( const string& regionName, const string& fuelName ) const {

    // Determine the correct region.
    double coef = 0;
    map<string,int>::const_iterator regionIter = regionNamesToNumbers.find( regionName );
    if( regionIter != regionNamesToNumbers.end() ) {
        coef = regions[ regionIter->second ]->getPrimaryFuelCO2Coef( fuelName );
    }

    return coef;
}

//! Return the carbonTaxCoef for a specific region and fuel.
double World::getCarbonTaxCoef( const string& regionName, const string& fuelName ) const {

    // Determine the correct region.
    double coef = 0;
    map<string,int>::const_iterator regionIter = regionNamesToNumbers.find( regionName );
    if( regionIter != regionNamesToNumbers.end() ) {
        coef = regions[ regionIter->second ]->getCarbonTaxCoef( fuelName );
    }

    return coef;
}

/*! \brief A function to print a csv file including the list of all regions their sector dependencies.
* 
* \author Josh Lurz
* \param logger The to which to print the dependencies. 
*/
void World::printSectorDependencies( Logger* logger ) const {
    for( vector<Region*>::const_iterator regionIter = regions.begin(); regionIter != regions.end(); regionIter++ ) {
        ( *regionIter )->printSectorDependencies( logger );
    }
}