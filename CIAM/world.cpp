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

using namespace std;
using namespace xercesc;

extern "C" { void _stdcall AG2INITC( double[14][12] ); };
extern Scenario* scenario;

// global map of region names
map<string,int> regionMap;

//! Default constructor.
World::World() {
	// initialize elemental datamembers.
	noreg = 0;

   doCalibrations = true;
   
   // We can resize all the arrays because we are garunteed by the schema that the modeltime object is parsed first.
   const int maxper = scenario->getModeltime()->getmaxper();
	population.resize(maxper); // total global population
	crudeoilrsc.resize(maxper); // global crude oil resource
	unconvoilrsc.resize(maxper); // global crude oil resource
	natgasrsc.resize(maxper); // global natural gas resource
	coalrsc.resize(maxper); // global coal resource
	uranrsc.resize(maxper); // global uranium resource
	ghgs.resize(maxper+2); // structure containing ghg emissions
}

World::~World(){
	for ( vector<Region*>::iterator regionIter = region.begin(); regionIter != region.end(); regionIter++ ) {
		delete *regionIter;
	}
}

//! Initialize member variables.
void World::clear(){
	noreg = 0;
	region.clear();
	population.clear();
	crudeoilrsc.clear();
	unconvoilrsc.clear();
	natgasrsc.clear();
	coalrsc.clear();
	uranrsc.clear();
	ghgs.clear();
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
	
	for( int i = 0; static_cast<int>( i < nodeList->getLength() ); i++ ){
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
            region[ regionIter->second ]->XMLParse( curr );
         }
         else {
			   tempRegion = new Region();
			   tempRegion->XMLParse( curr );
			   region.push_back( tempRegion ); // resizes vector of region objects
			   regionNamesToNumbers[ tempRegion->getName() ] = region.size() - 1;
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
   
   // Set the number of regions.
   noreg = region.size();

   // Finish initializing all the regions.
   for( vector<Region*>::iterator regionIter = region.begin(); regionIter != region.end(); regionIter++ ) {
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
	
	for ( int j = 0; j < noreg; j++ ) {
		for ( int k = 0; k < AgSector::getNumAgMarkets(); k++ ) {
			tempVec[ k ] = prices[ j ][ k ];
		}
		region[ j ]->initializeAgMarketPrices( tempVec );
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

	for( vector<Region*>::const_iterator i = region.begin(); i != region.end(); i++ ){
		//for( vector<Region>::const_iterator i = region.begin(); i <= region.begin(); i++ ){
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

	XMLWriteElement( noreg, "numberOfRegions", out );
	XMLWriteElement( population[ period ], "globalPopulation", out );
	XMLWriteElement( crudeoilrsc[ period ], "globalCrudeOil", out );
	XMLWriteElement( unconvoilrsc[ period ], "globalUnconventionalOil", out );
	XMLWriteElement( natgasrsc[ period ], "globalNaturalGas", out );
	XMLWriteElement( coalrsc[ period ], "globalCoal", out );
	XMLWriteElement( uranrsc[ period ], "globalUranium", out );
	
	// for_each( region.begin(), region.end(), bind1st( mem_fun_ref( &Region::toXML ), out ) );
	// won't work with VC 6.0. Forgot to implement const mem_fun_ref helper. whoops.
	scenario->getMarketplace()->toDebugXML( period, out );
	
	for( vector<Region*>::const_iterator i = region.begin(); i == region.begin(); i++ ) { 
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
void World::initCalc( const int per ) {	
	
   for ( int i=0 ;i<noreg; i++ ) {
		region[ i ]->initCalc( per );
	}
   
}

//! calculate supply and demand and emissions for all regions
/*! This is the main action loop for the model. 
Uses "MiniCAM" style logic where primary costs are calculated, 
then prices of refined fuels, end-use costs, end-use, etc. */
void World::calc( const int per, const vector<string>& regionsToSolve ) {	
	
	vector<int> regionNumbersToSolve;
	
	Configuration* conf = Configuration::getInstance();
	
	if ( regionsToSolve.size() == 0 ) {
		regionNumbersToSolve.resize( region.size() );
		
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
         region[ *i ]->writeBackCalibratedValues( per );
      }
		// calculate regional GNP
		region[ *i ]->calc_gnp( per );
		// apply carbon taxes to appropriate technologie
		region[ *i ]->applycarbontax(per);
		// set regional GHG constraint to market supply
		region[ *i ]->setghgsupply(per);
		// set regional GHG tax to individual technologies
		region[ *i ]->addghgtax(per);
		// determine supply of primary resources
		region[ *i ]->rscsupply(per);
		// determine prices of refined fuels and electricity
		region[ *i ]->finalsupplyprc(per);
		// calculate enduse service price
		region[ *i ]->calcEndUsePrice( per );
		// adjust gnp for energy cost changes
		region[ *i ]->adjust_gnp(per);

		// determine end-use demand for energy and other goods
		region[ *i ]->endusedemand(per);

		// determine supply of final energy and other goods based on demand
		region[ *i ]->finalsupply(per);
		
		if( conf->getBool( "agSectorActive" ) ){
			region[ *i ]->calcAgSector(per);
		}

      // Perform calibrations
      if( conf->getBool( "CalibrationActive" ) ) {
         region[ *i ]->calibrateRegion( doCalibrations, per );
      }

	}

   Marketplace* marketplace = scenario->getMarketplace();
}

//! Update all summary information for reporting
// Orginally in world.calc, removed to call only once after solved
void World::updateSummary( int per )
{
	for (int i=0;i<noreg;i++) {
		region[i]->emission(per);
		region[i]->updateSummary(per);
		region[i]->calcEmissFuel(per);
	}
}

//! sum population from each region for global total
void World::sumpop( int per )
{
	population[per] = 0.0;
	// divide by 1000 to get millions
	for ( int i = 0; i < noreg; i++ ) {
		population[per] += region[i]->showpop(per)/1000;
	}
}

//! sum regional resources for global total
void World::sumrsc( int per )
{
	crudeoilrsc[per] = 0.0;
	unconvoilrsc[per] = 0.0;
	natgasrsc[per] = 0.0;
	coalrsc[per] = 0.0;
	uranrsc[per] = 0.0;
	
	for ( int i = 0; i < noreg; i++ ) {
		crudeoilrsc[per] += region[i]->showsubrsc( "crude oil", "crude oil", per );
		unconvoilrsc[per] += region[i]->showsubrsc( "crude oil", "unconventional oil", per );
		natgasrsc[per] += region[i]->showrsc( "natural gas",per );
		coalrsc[per] += region[i]->showrsc( "coal" ,per );
		uranrsc[per] += region[i]->showrsc( "uranium", per );
	}
}

//! calculate indirect emissions for each region
void World::emiss_ind(int per)
{
	for (int i=0;i<noreg;i++) {
		region[i]->emiss_ind(per); // calculate indirect emissions
	}
}

//! set global emissions for all GHG for climat
void World::emiss_all() {
	const int maxper = scenario->getModeltime()->getmaxdataper();
	int  per;

	ifstream gasfile2;
	//gasfile2.open("gas2.emk",ios::in); // open input file for reading
	gasfile2.open("gas2.emk"); // open input file for reading
   util::checkIsOpen( gasfile2 );
	// read in all other gases except CO2 from fossil fuels
	// CO2 from fossil fuels comes from model
	int skiplines = 5;
	for (int i=0;i<skiplines;i++)
		gasfile2.ignore(80,'\n'); // skip lines
	for (per=1;per<maxper;per++) {
		gasfile2.ignore(80,','); // skip year column
		gasfile2.ignore(80,','); // skip CO2 column
		gasfile2 >> ghgs[per][ "CO2ag" ];
      gasfile2.ignore(80,','); // skip comma
		gasfile2 >> ghgs[per][ "CH4" ];
		gasfile2.ignore(80,','); // skip comma
      gasfile2 >> ghgs[per][ "N2O" ];
		gasfile2.ignore(80,','); // skip comma
		gasfile2 >> ghgs[per][ "SOXreg1" ];
		gasfile2.ignore(80,','); // skip comma
		gasfile2>> ghgs[per][ "SOXreg2" ];
		gasfile2.ignore(80,','); // skip comma
		gasfile2>> ghgs[per][ "SOXreg3" ];
		gasfile2.ignore(80,','); // skip comma
		gasfile2>> ghgs[per][ "CF4" ];
		gasfile2.ignore(80,','); // skip comma
		gasfile2>> ghgs[per][ "C2F6" ];
		gasfile2.ignore(80,','); // skip comma
		gasfile2>> ghgs[per][ "HFC125" ];
		gasfile2.ignore(80,','); // skip comma
		gasfile2>> ghgs[per][ "HFC134a" ];
		gasfile2.ignore(80,','); // skip comma
		gasfile2>> ghgs[per][ "HFC143a" ];
		gasfile2.ignore(80,','); // skip comma
		gasfile2>> ghgs[per][ "HFC227ea" ];
		gasfile2.ignore(80,','); // skip comma
		gasfile2>> ghgs[per][ "HFC245ca" ];
		gasfile2.ignore(80,','); // skip comma
		gasfile2>> ghgs[per][ "SF6" ];
		gasfile2.ignore(80,'\n'); // next line
	}
	for (per=maxper;per<maxper+2;per++) {
		ghgs[per]=ghgs[per-1];
	}
	
	gasfile2.close();
}

//! write results for all regions to file
void World::outputfile() {
	const int maxper = scenario->getModeltime()->getmaxper();
	vector<double> temp(maxper);
	// function protocol
	void fileoutput3(string var1name,string var2name,string var3name,
		string var4name,string var5name,string uname,vector<double> dout);
	
	// write global population results to database
	fileoutput3("global"," "," "," ","population","Millions",population);
	
	// write total emissions for World
	for (int m=0;m<maxper;m++)
		temp[m] = ghgs[m][ "CO2" ];
	fileoutput3( "global"," "," "," ","CO2 emiss","MTC",temp);
	fileoutput3( "global"," "," "," ","c.oil resource(conv)","EJ",crudeoilrsc);
	fileoutput3( "global"," "," "," ","c.oil resource(unconv)","EJ",unconvoilrsc);
	fileoutput3( "global"," "," "," ","n.gas resource","EJ",natgasrsc);
	fileoutput3( "global"," "," "," ","coal resource","EJ",coalrsc);
	fileoutput3( "global"," "," "," ","uran resource","EJ",uranrsc);
	for (int i=0;i<noreg;i++)
		region[i]->outputfile();
}

//! MiniCAM style output to database
void World::MCoutput() {
	const int maxper = scenario->getModeltime()->getmaxper();
	vector<double> temp(maxper);
	// function protocol
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
		string uname,vector<double> dout);
	
	// write global population results to database
	//dboutput4("global","General","Population","zTotal","thous",population);
	
	// call regional output
	for (int i=0;i<noreg;i++) {
		region[i]->MCoutput();
	}
}

//! turn on calibrations
void World::turnCalibrationsOn()
{
	doCalibrations = true;
}

//! turn off calibrations
void World::turnCalibrationsOff()
{
	doCalibrations = false;
}

//! return calibration setting
bool World::getCalibrationSetting() const 
{
	return doCalibrations;
}

//! Return the amount of the given GHG in the given period.
double World::getGHGEmissions( const std::string& ghgName, const int per ) const {
   assert( per < static_cast<int>( ghgs.size() ) );

   map<string,double>::const_iterator iter = ghgs[ per ].find( ghgName );
   
   if( iter != ghgs[ per ].end() ) {
      return iter->second;
   }
   else {
      cout << "GHG: " << ghgName << " was not found for period " << per << "." << endl;
      return 0;
   }
}

void World::createRegionMap(void) // create map of region names
{
	for (int i=0;i<noreg;i++) {
		regionMap[region[i]->getName()] = i+1; // start index from 1
	}
    // hardcode for now
    regionMap["global"] = 0;
}

void World::setupCalibrationMarkets() {
   for( vector<Region*>::iterator i = region.begin(); i != region.end(); i++ ) {
      ( *i )->setupCalibrationMarkets();
   }
}

vector<string> World::getRegionVector() const {
   vector<string> regionNames;

   for( vector<Region*>::const_iterator i = region.begin(); i != region.end(); i++ ) {
      regionNames.push_back( ( *i )->getName() );
   }
   return regionNames;
}

/*! A function which print dependency graphs showing fuel usage by sector.
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
   for ( vector<Region*>::const_iterator regionIter = region.begin(); regionIter == region.begin(); regionIter++ ) {
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
      coef = region[ regionIter->second ]->getPrimaryFuelCO2Coef( fuelName );
   }

   return coef;
}

//! Return the carbonTaxCoef for a specific region and fuel.
double World::getCarbonTaxCoef( const string& regionName, const string& fuelName ) const {
   
   // Determine the correct region.
   double coef = 0;
   map<string,int>::const_iterator regionIter = regionNamesToNumbers.find( regionName );
   if( regionIter != regionNamesToNumbers.end() ) {
      coef = region[ regionIter->second ]->getCarbonTaxCoef( fuelName );
   }

   return coef;
}
