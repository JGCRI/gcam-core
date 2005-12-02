/*! 
* \file region_cge.cpp
* \ingroup Objects-SGM
* \brief The RegionCGE class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <string>
#include <vector>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <algorithm>

#include "containers/include/region_cge.h"
#include "containers/include/region.h"
#include "demographics/include/demographic.h"
#include "containers/include/gdp.h"
#include "util/base/include/summary.h"
#include "sectors/include/production_sector.h"
#include "sectors/include/sector.h"
#include "sectors/include/supply_sector.h"
#include "sectors/include/demand_sector.h"
#include "sectors/include/tran_sector.h"
#include "sectors/include/more_sector_info.h"
#include "resources/include/resource.h"
#include "sectors/include/ag_sector.h"
#include "demographics/include/population.h"
#include "emissions/include/ghg_policy.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "emissions/include/indirect_emiss_coef.h"
#include "containers/include/world.h"
#include "util/base/include/model_time.h" 
#include "marketplace/include/marketplace.h"
#include "util/base/include/configuration.h"
#include "util/base/include/util.h"
#include "util/logger/include/logger.h"
#include "util/curves/include/curve.h"
#include "util/curves/include/point_set_curve.h"
#include "util/curves/include/xy_data_point.h"
#include "util/curves/include/point_set.h"
#include "util/curves/include/explicit_point_set.h"
#include "sectors/include/final_demand_sector.h"
#include "demographics/include/demographic.h"
#include "sectors/include/factor_supply.h"
#include "containers/include/national_account.h"
// classes for reporting
#include "reporting/include/social_accounting_matrix.h"
#include "reporting/include/output_container.h"
#include "reporting/include/demand_components_table.h"
#include "reporting/include/sector_report.h"
#include "reporting/include/sgm_gen_table.h"
#include "reporting/include/input_output_table.h"
#include "reporting/include/sector_results.h"
#include "reporting/include/govt_results.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

// static initialize.
const string RegionCGE::XML_NAME = "regionCGE";

//! Default constructor
RegionCGE::RegionCGE() {
    // Resize all vectors to maximum period
    const int maxper = scenario->getModeltime()->getmaxper();
	nationalAccount.resize( maxper );

	// create empty tables for reporting
	createSGMGenTables();
}

//! Empty tables for reporting available for writing in each period
void RegionCGE::createSGMGenTables() {
	//***** output container for reporting *****
	// create empty tables for SGM general output
    const Modeltime* modeltime = scenario->getModeltime();
	OutputContainer* CO2Table = new SGMGenTable( "CO2", "CO2 Emissions Total (MTC)", modeltime );
	outputContainers.push_back( CO2Table );
    
    outputContainers.push_back( new SGMGenTable( "EmissBySource", "Emissions by Primary Fuel(MTC)", modeltime ) );

	OutputContainer* CO2bySecTable = new SGMGenTable( "CO2bySec", "CO2 Emissions by Sector (MTC)", modeltime );
	outputContainers.push_back( CO2bySecTable );

	OutputContainer* CO2byTechTable = new SGMGenTable( "CO2byTech", "CO2 Emissions by Technology (MTC)", modeltime );
	outputContainers.push_back( CO2byTechTable );

	OutputContainer* GNPrealTable = new SGMGenTable( "GNPREAL", "GNP REAL (1990 Million Dollar)", modeltime );
	outputContainers.push_back( GNPrealTable );

	OutputContainer* GNPnomTable = new SGMGenTable( "GNPNOM", "GNP NOMINAL (1990 Million Dollar)", modeltime );
	outputContainers.push_back( GNPnomTable );

	OutputContainer* PECTable = new SGMGenTable( "PEC", "Primary Energy Consumption (EJ)", modeltime );
	outputContainers.push_back( PECTable );

	OutputContainer* PEPTable = new SGMGenTable( "PEP", "Primary Energy Production (EJ)", modeltime );
	outputContainers.push_back( PEPTable );

	OutputContainer* SEPTable = new SGMGenTable( "SEP", "Secondary Energy Production (EJ)", modeltime );
	outputContainers.push_back( SEPTable );

	OutputContainer* ETRADETable = new SGMGenTable( "ETRADE", "Net Export of Energy (EJ)", modeltime );
	outputContainers.push_back( ETRADETable );

	OutputContainer* ELECTable = new SGMGenTable( "ELEC", "Electricity Generation by Technology (EJ)", modeltime );
	outputContainers.push_back( ELECTable );

	OutputContainer* ElecFuelTable = new SGMGenTable( "ElecFuel", "Fuel Consumption for Electricity Generation (EJ)", modeltime );
	outputContainers.push_back( ElecFuelTable );

	OutputContainer* NEPTable = new SGMGenTable( "NEP", "Non-Energy Output (1990 Million Dollar)", modeltime );
	outputContainers.push_back( NEPTable );

	OutputContainer* DEMTable = new SGMGenTable( "DEM", "Demographics (1000 Persons)", modeltime );
	outputContainers.push_back( DEMTable );

	OutputContainer* CAPTable = new SGMGenTable( "CAP", "Total Capital Stock and Carbon Permit Revenue (1990 Million Dollar?)", modeltime );
	outputContainers.push_back( CAPTable );

	OutputContainer* PRICETable = new SGMGenTable( "PRICE", "Prices Market (1990 Dollar)", modeltime );
	outputContainers.push_back( PRICETable );

	OutputContainer* EINVTable = new SGMGenTable( "EINV", "Energy Investments Annual (1990 Million Dollar)", modeltime );
	outputContainers.push_back( EINVTable );

	OutputContainer* NEINVTable = new SGMGenTable( "NEINV", "Non-Energy Investments Annual (1990 Million Dollar)", modeltime );
	outputContainers.push_back( NEINVTable );

	OutputContainer* PASSTRANTable = new SGMGenTable( "PASSTRAN", "Passenger Transport Vehicle Output (Million Passenger-Miles)", modeltime );
	outputContainers.push_back( PASSTRANTable );

	OutputContainer* PASSTRANFCTable = new SGMGenTable( "PASSTRANFC", "Passenger Transport Fuel Consumption by Fuel (EJ/year)", modeltime );
	outputContainers.push_back( PASSTRANFCTable );

	OutputContainer* PASSTRANFCMTable = new SGMGenTable( "PASSTRANFCM", "Passenger Transport Fuel Consumption by Mode (EJ/year)", modeltime );
	outputContainers.push_back( PASSTRANFCMTable );

	OutputContainer* PASSTRANFCTTable = new SGMGenTable( "PASSTRANFCT", "Passenger Transport Fuel Consumption by Vehicle Technology (EJ/year)", modeltime );
	outputContainers.push_back( PASSTRANFCTTable );

	OutputContainer* PASSTRANMPGTable = new SGMGenTable( "PASSTRANMPG", "Passenger Transport Vehicle Fuel Economy (MPG)", modeltime );
	outputContainers.push_back( PASSTRANMPGTable );

	OutputContainer* PASSTRANCOSTTable = new SGMGenTable( "PASSTRANCOST", "Passenger Transport Vehicle Service Cost ($/pass-mile)", modeltime );
	outputContainers.push_back( PASSTRANCOSTTable );
	//***** end output containers
}

//! Default destructor destroys sector, demsector, Resource, agSector, and population objects.
RegionCGE::~RegionCGE() {
    clear();
}

//! Clear member variables and initialize elemental members.
void RegionCGE::clear(){
    for ( FinalDemandSectorIterator demIter = finalDemandSector.begin(); demIter != finalDemandSector.end(); ++demIter ) {
        delete *demIter;
    }

    for ( FactorSupplyIterator facIter = factorSupply.begin(); facIter != factorSupply.end(); ++facIter ) {
        delete *facIter;
    }
	// delete memory for SGM gen output tables
	for( vector<OutputContainer*>::iterator iter = outputContainers.begin(); iter != outputContainers.end(); ++iter ){
		delete *iter;
	}
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& RegionCGE::getXMLName() const {
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
const std::string& RegionCGE::getXMLNameStatic() {
	return XML_NAME;
}

//! Parse xml file for data
bool RegionCGE::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
	if( nodeName == FinalDemandSector::getXMLNameStatic() ){
		parseContainerNode( curr, finalDemandSector, finalDemandSectorNameMap, new FinalDemandSector( Region::getName() ) );
	}
	else if( nodeName == FactorSupply::getXMLNameStatic() ){
		parseContainerNode( curr, factorSupply, factorSupplyNameMap, new FactorSupply() );
	}
	else if( nodeName == ProductionSector::getXMLNameStatic() ){
		parseContainerNode( curr, supplySector, supplySectorNameMap, new ProductionSector( name ) );
	}
	else {
		return false;
	}
    return true;
}

//! For derived classes to output XML data
void RegionCGE::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
	for ( unsigned int i = 0; i < finalDemandSector.size(); i ++ ) {
		finalDemandSector[i]->toInputXML( out, tabs );
	}
    for( unsigned int i = 0; i < factorSupply.size(); i++ ){
        factorSupply[ i ]->toInputXML( out, tabs );
    }
}

//! Output debug info for derived class
void RegionCGE::toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const {
    nationalAccount[ period ].toDebugXML( period, out, tabs );
	for ( unsigned int i = 0; i < finalDemandSector.size(); i ++ ) {
		finalDemandSector[i]->toDebugXML( period, out, tabs );
	}
    for( unsigned int i = 0; i < factorSupply.size(); i++ ){
        factorSupply[ i ]->toDebugXML( period, out, tabs );
    }
}

//! Complete the initialization.
void RegionCGE::completeInit() {
	Region::completeInit();
	for( unsigned int i = 0; i < finalDemandSector.size(); i++) {
        // Pass null for the dependency finder argument as CGE regions don't
        // have dependencies.
		finalDemandSector[i]->completeInit( mRegionInfo.get(), 0 );
	}
	for( unsigned int i = 0; i < factorSupply.size(); i++) {
		factorSupply[i]->completeInit( name );
	}
}

//! Call any initializations that are only done once per period
// \todo put somewhere (maybe not here) a check for prev period to see how well calibrations worked
void RegionCGE::initCalc( const int period ) 
{
	for (unsigned int i = 0; i < factorSupply.size(); i++) {
		factorSupply[i]->initCalc( name, period );
	}

	Region::initCalc( period );

	// SGM sequence of procedures
	for ( unsigned int i = 0; i < finalDemandSector.size(); i++ ) {
		finalDemandSector[i]->initCalc( nationalAccount[ period ], demographic.get(), period );
	}
}

/*! \brief Main regional calculation of economic supplies and demand
*
* \param period Model time period
*/
void RegionCGE::calc( const int period, const bool aDoCalibrations ) {
	nationalAccount[ period ].reset();
	// calls operate for both production and final demand sectors
    operate( period ); // This sector function operates existing capital, invests, and operates total.

}

//! Function which operates the capital for all production sectors.
void RegionCGE::operate( const int period ){
    for( vector<Sector*>::iterator currSec = supplySector.begin(); currSec != supplySector.end(); ++currSec ){
        (*currSec)->operate( nationalAccount[ period ], demographic.get(), period );
    }
	for (unsigned int i = 0; i < finalDemandSector.size(); i++) {
		finalDemandSector[i]->operate( nationalAccount[period], demographic.get(), period );
	}
}

/*! \brief Initialize the marketplaces in the base year to get initial demands for each region
 * 
 * \author Pralit Patel
 * \param period The period is usually the base period
 */
void RegionCGE::updateMarketplace( const int period ) {
	Region::updateMarketplace( period );
	for (unsigned int i = 0; i < finalDemandSector.size(); i++) {
		finalDemandSector[i]->updateMarketplace( period );
	}
}

/*! \brief For outputing SGM data to a flat csv File, wouldn't need to do anything for miniCAM
 * 
 * \author Pralit Patel
 * \param period 
 */
void RegionCGE::csvSGMOutputFile( ostream& aFile, const int period ) const {
	std::vector<OutputContainer*> outputContainers; // vector of output containers

	aFile << "Region:  " << name << endl << endl;
	nationalAccount[ period ].csvSGMOutputFile( aFile, period );

	for( vector<Sector*>::const_iterator currSec = supplySector.begin(); currSec != supplySector.end(); ++currSec ){
        (*currSec)->csvSGMOutputFile( aFile, period );
    }
	for (unsigned int i = 0; i < finalDemandSector.size(); i++) {
		finalDemandSector[i]->csvSGMOutputFile( aFile, period );
	}
	for (unsigned int i = 0; i < factorSupply.size(); i++) {
		factorSupply[i]->csvSGMOutputFile( aFile, period );
	}
	demographic.get()->csvSGMOutputFile( aFile, period );
	aFile << endl;


	// ****** HERE IS WHERE YOU ADD AS MANY OUTPUT CONTAINERS AS YOU WANT ****
	outputContainers.push_back( new SocialAccountingMatrix( name ) );
	outputContainers.push_back( new DemandComponentsTable );
    outputContainers.push_back( new SectorResults( name ) );
    outputContainers.push_back( new GovtResults( name ) );
    outputContainers.push_back( new InputOutputTable( name ) );
	
    // load values into all tables
	for (unsigned int i = 0; i < outputContainers.size(); i++) { 
		updateOutputContainer( outputContainers[ i ], period );
	}

	// print out all tables
	for( unsigned int i = 0; i < outputContainers.size(); i++) {
		outputContainers[ i ]->output( aFile, period );
	}
    
    // clean up memory.
    for( vector<OutputContainer*>::iterator iter = outputContainers.begin(); iter != outputContainers.end(); ++iter ){
        delete *iter;
    }
}

void RegionCGE::updateOutputContainer( OutputContainer* outputContainer, const int period ) const{
	outputContainer->updateRegionCGE( this );
	// for demographic
	demographic->updateOutputContainer( outputContainer, period );
	// for national account
	nationalAccount[ period ].updateOutputContainer( outputContainer, period );
	// loop for supply sectors
	for( CSectorIterator currSec = supplySector.begin(); currSec != supplySector.end(); ++currSec ){
		(*currSec)->updateOutputContainer( outputContainer, period );
    }
	// loop for final demand sectors
	for( CFinalDemandSectorIterator currSec = finalDemandSector.begin(); currSec != finalDemandSector.end(); ++currSec ){
		(*currSec)->updateOutputContainer( outputContainer, period );
    }
	// loop for factor supply sectors
	for( CFactorSupplyIterator currSec = factorSupply.begin(); currSec != factorSupply.end(); ++currSec ){
		(*currSec)->updateOutputContainer( outputContainer, period );
    }
}

//! update regional output tables for reporting
void RegionCGE::updateAllOutputContainers( const int period ) { 
	// update all tables for reporting
	// load values into all tables
	for ( unsigned int i = 0; i < outputContainers.size(); i++ ) { 
		updateOutputContainer( outputContainers[ i ], period );
	}
}

/*! \brief General SGM output is called at end of model run and includes all
*          periods.
* \param aFile Output file.
*/
void RegionCGE::csvSGMGenFile( ostream& aFile ) const {
	// print out all tables
	for( unsigned int i = 0; i < outputContainers.size(); i++) {
		outputContainers[ i ]->output( aFile, 0 );
	}
}
