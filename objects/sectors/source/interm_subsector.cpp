/*!
* \file interm_subsector.cpp
* \ingroup CIAM
* \brief Subsector class source file.
* \author Marshall Wise
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <vector>
#include <algorithm>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "sectors/include/subsector.h"
#include "technologies/include/technology.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/gdp.h"
#include "sectors/include/interm_subsector.h"
#include "marketplace/include/market_info.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string IntermittentSubsector::XML_NAME = "intermittent-subsector";

/*! \brief Default constructor.
*
* Constructor passes up to Subsector constructor
*
* \author Marshall Wise
*/

IntermittentSubsector::IntermittentSubsector( const string regionName, const string sectorName ) : Subsector ( regionName, sectorName ){

    backupCapacityPerEnergyOutput.resize( scenario->getModeltime()->getmaxper() );
    backupCapacityFraction.resize( scenario->getModeltime()->getmaxper() );
    resourceTechNumber = 0;
    backupTechNumber = 1;
    electricSectorName = "electricity";
}

/*! \brief Complete the initialization
*
* This routine is only called once per model run
*
* \author Marshall Wise
* \warning markets are not necesarilly set when completeInit is called
*  For the intermittent subsector, use this to make sure there is a trial market set for electricity
*  as there might not be one set if there are no simultaneities
*/
void IntermittentSubsector::completeInit( DependencyFinder* aDependencyFinder ) {

    // first call parent method
    Subsector::completeInit(aDependencyFinder);

    // now set the trial market for electricity
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->resetToPriceMarket(electricSectorName, regionName);
}




//! Parses any input variables specific to derived classes
bool IntermittentSubsector::XMLDerivedClassParse( const string nodeName, const DOMNode* curr ) {
    if( nodeName == "backupTechNumber" ){
        backupTechNumber = XMLHelper<int>::getValue( curr );
    }
    else if( nodeName == "resourceTechNumber" ){
        resourceTechNumber = XMLHelper<int>::getValue( curr );
    }
    else if( nodeName == "electricSectorName" ){
        electricSectorName = XMLHelper<string>::getValueString( curr );
    }
	else {
        return false;
	}
	return true;
}

/*! \brief Parses any attributes specific to derived classes
*
* Method parses any input data attributes (not child nodes, see XMLDerivedClassParse) that are specific to any classes derived from this class. Since Sector is the generic base class, there are no values here.
*
* \author Josh Lurz, Steve Smith
* \param node pointer to the current node in the XML input tree
*/
bool IntermittentSubsector::XMLDerivedClassParseAttr( const DOMNode* node ) {
    return false;
    // do nothing
}

/*! \brief XML output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML
* \author Josh Lurz
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void IntermittentSubsector::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    // Write out parent class information.
    Subsector::toInputXMLDerived( out, tabs );
    XMLWriteElement( backupTechNumber, "backupTechNumber", out, tabs);
    XMLWriteElement( resourceTechNumber, "resourceTechNumber", out, tabs);
    XMLWriteElement( electricSectorName, "electricSectorName", out, tabs);
}	


//! XML output for viewing.
void IntermittentSubsector::toOutputXMLDerived( ostream& out, Tabs* tabs ) const {
    // Write out parent class information.
    Subsector::toOutputXMLDerived( out, tabs );
    XMLWriteElement( backupTechNumber, "backupTechNumber", out, tabs);
    XMLWriteElement( resourceTechNumber, "resourceTechNumber", out, tabs);
    XMLWriteElement( electricSectorName, "electricSectorName", out, tabs);
}	

//! Write object to debugging xml output stream.
void IntermittentSubsector::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    // Write out parent class information.
    Subsector::toDebugXMLDerived( period, out, tabs );
    XMLWriteElement( backupTechNumber, "backupTechNumber", out, tabs);
    XMLWriteElement( resourceTechNumber, "resourceTechNumber", out, tabs);
    XMLWriteElement( electricSectorName, "electricSectorName", out, tabs);
}


/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& IntermittentSubsector::getXMLName() const {
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
const std::string& IntermittentSubsector::getXMLNameStatic() {
    return XML_NAME;
}

/*! \brief initilaization before each period
*
*
* \author Marshall Wise
* \param period Model period
*/
void IntermittentSubsector::initCalc(const MarketInfo* aSectorInfo, 
                          NationalAccount& aNationalAccount,
                          Demographic* aDemographics,
                          const MoreSectorInfo* aMoreSectorInfo,
                          const int aPeriod ) {

    // this needs to be done before control is passed to Subsector:initCalc() so that information is available to technology initCalc() routines
    // though these particular members are not needed at technology level yet.
    elecReserveMargin = aSectorInfo->getItemValue( "elecReserveMargin" , true );
    aveGridCapacityFactor = aSectorInfo->getItemValue( "aveGridCapacityFactor", true );
    backupCapacityFactor = aSectorInfo->getItemValue( "backupCapacityFactor", true );
    backupCost = aSectorInfo->getItemValue( "backupCost", true );
    // call parent method
    Subsector::initCalc( aSectorInfo, aNationalAccount, aDemographics, aMoreSectorInfo, aPeriod );
}

/*! \brief set tech shares based on backup energy needs
* for an intermittent resource
*
*  \author Marshall Wise
* \param period model period
*
*/
void IntermittentSubsector::calcTechShares( const GDP* gdp, const int period ) {


    const double EJ_PER_GWH = 0.0000036;  // conversion: 1 gigaWattHour of electricity = 3.6E-6 ExaJoules
    const int HOURS_PER_YEAR = 8760;   //number of hours in a year

    // Call method to compute demand for backup
    calcBackupFraction(period);

    /* Convert backup capacity per unit of resource energy to energy required (in EJ)
    per unit of resource energy (in EJ) using backup capacity factor */

    double backupEnergyFraction = backupCapacityPerEnergyOutput[period] 
                                * (HOURS_PER_YEAR * backupCapacityFactor * EJ_PER_GWH);

    // Assert that the read-in resourceTechNumber abd backupTechNumber are within the size of the tech vector

    assert(resourceTechNumber < techs.size());
    assert(backupTechNumber < techs.size());

    // Normalize shares so that they add to 1
    techs[resourceTechNumber][period]->setTechShare(1.0 / (1.0 + backupEnergyFraction) );
    techs[backupTechNumber][period]->setTechShare(backupEnergyFraction / (1.0 + backupEnergyFraction) );

    techs[resourceTechNumber][period]->calcCost(regionName,sectorName,period);
    techs[backupTechNumber][period]->calcCost(regionName,sectorName,period);
}



/*! \brief compute backup share needed
* for an intermittent resource
*
*  \author Marshall Wise
*
*/
void IntermittentSubsector::calcBackupFraction(const int per) {

    const double EJ_PER_GWH = 0.0000036;  // conversion: 1 gigaWattHour of electricity = 3.6E-6 ExaJoules
    const int HOURS_PER_YEAR = 8760;   //number of hours in a year
    
    // Get resource name from fuel name of technology
	std::string resourceName = techs[resourceTechNumber][per]->getFuelName();

    // get variance of resource from marketinfo
    Marketplace* marketplace = scenario->getMarketplace();
    double variance = marketplace->getMarketInfo(resourceName,regionName,per,"resourceVariance");

	// get resource capacity factor from market info
    double resourceCapacityFactor = marketplace->getMarketInfo(resourceName,regionName,per,"resourceCapacityFactor");

    //get trial amount of overall regional electricity supplied  (Josh, how avoid hardcode this?)
    // trial markets have a demand but not supply
    double elecSupply = marketplace->getDemand( electricSectorName, regionName, per );

	//get amount of this resource supplied
    double resourceSupply = marketplace->getSupply( resourceName, regionName, per );

    /*! Using average capacity factor for resource, translate resource supplied in electricity or energy terms
    in EJ to equivalent capacity terms in GW or gigawatts.  Note that model's enery units need to be converted to
	capacity units for use in the NREL WINDS operating reserve computation. */
    double resourceSupplyCapacity = 0.0;  // capacity in GW
    if ( resourceSupply > util::getSmallNumber() ) {
        resourceSupplyCapacity  = resourceSupply / (EJ_PER_GWH * HOURS_PER_YEAR * resourceCapacityFactor);
    }

	// Compute terms for Winds operating reserve due to intermittency formula
 
	// square the supply to use in the WINDS formula
    double resourceSupplySquared = pow(resourceSupplyCapacity,2);

	/*! Compute reserve capacity as the product of electricity reserve margin and the
	   total regional electric capacity (which is converted from electric supply in Energy units
	   to capacity units using its average capacity factor).  */
    double reserveTotal = elecReserveMargin * elecSupply /(EJ_PER_GWH * HOURS_PER_YEAR * aveGridCapacityFactor);//in GW
    double reserveTotalSquared  = pow(reserveTotal,2);


    //! Compute operating reserve capacity based on formula from NREL WINDS model
    /* First compute in terms of backup capacity per total intermittent capacity, then convert to backup
    capacity as fraction of wind resource output in energy terms, since that is what the model and market
    are based on.  */

    backupCapacityFraction[per] = 0;  // percent of reserve capacity per unit of intermittent capacity (e.g., GW/GW)
    backupCapacityPerEnergyOutput[per] = 0;  // reserve capacity per intermittent electricity resource output (GW/EJ)
    if ( elecSupply > util::getSmallNumber()) {
        backupCapacityFraction[per] = reserveTotal * (pow((1.0 + variance * resourceSupplySquared / reserveTotalSquared),0.5) - 1.0);
        if ( resourceSupplyCapacity > util::getSmallNumber() ) {
            backupCapacityFraction[per] /= resourceSupplyCapacity;  // derivative per unit of intermittent resource supply
            /*! compute backup required per resource energy output (since energy output is what the modeled market 
			  is based on).  Convert intermittent resource output back to energy using the resource
			  capacity factor. This is the main result of this method and is used in computing the per unit
			  cost of operating reserve or backup capacity added to the cost of this subsector. */
            backupCapacityPerEnergyOutput[per] = backupCapacityFraction[per] / 
				  ( EJ_PER_GWH * HOURS_PER_YEAR * resourceCapacityFactor);
        }
    }
}

/*! \brief Computes weighted cost of all technologies in Subsector plus backup Costs.
*
*
* \author Marshall Wise
* \param period Model period
*/
void IntermittentSubsector::calcPrice( const int period ) {

    // first call parent subsector method
    Subsector::calcPrice( period );
	
	/* Add per unit cost of backup capacity to subsector price
    backup capacity is in GW/EJ, so have to convert to kW/GJ (multiply numerator by 1E6 and 
	denominator by 1E9 to get * 1/1000) to make consistent with market price
    which is in $/GJ.  BackupCost is in $/kw/yr. */

    subsectorprice[period] += backupCapacityPerEnergyOutput[period] /1000 * backupCost;

}

/*! \brief Write resource and backup capacity results to database
*
* Writes outputs with titles and units appropriate to supply sectors.
*
* \author Marshall Wise
*/
void IntermittentSubsector::MCoutputSupplySector() const {

    // First call parent subsector class method
    Subsector::MCoutputSupplySector();

    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);

    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    const double EJ_PER_GWH = 0.0000036;  // conversion: 1 gigaWattHour of electricity = 3.6E-6 ExaJoules
    const int HOURS_PER_YEAR = 8760;   //number of hours in a year
    const double CVRT90 = 2.212; //  convert '75 price to '90 price c/kwh
    vector<double> capacityGW(maxper);
    vector<double> backupCapacityGW(maxper);
    vector<double> backupCostCentsPerkWh(maxper);
    vector<double> resourceCostCentsPerkWh(maxper);

    Marketplace* marketplace = scenario->getMarketplace();
 
    // Intermittent Resource Subsector and Backup Capacity in GW
    for (int m=0;m<maxper;m++) {
        // Get resource name from fuel name of technology
	    std::string resourceName = techs[resourceTechNumber][m]->getFuelName();
        // get resource capacity factor from market info
        double resourceCapacityFactor = marketplace->getMarketInfo(resourceName,regionName,m,"resourceCapacityFactor");
        if (resourceCapacityFactor > util::getSmallNumber()) {
            capacityGW[m] = getOutput(m) / (EJ_PER_GWH * HOURS_PER_YEAR * resourceCapacityFactor);
            backupCapacityGW[m] = (getOutput(m) / (EJ_PER_GWH * HOURS_PER_YEAR * resourceCapacityFactor)) * backupCapacityFraction[m];
            backupCostCentsPerkWh[m] = backupCapacityPerEnergyOutput[m] /1000 * backupCost * CVRT90 * 0.36;
            resourceCostCentsPerkWh[m] = subsectorprice[m] * CVRT90 * 0.36 - backupCostCentsPerkWh[m];
        }
    }

    dboutput4(regionName,"Capacity",sectorName,name,"GW",capacityGW);
    dboutput4(regionName,"Capacity",sectorName,name + "Backup","GW",backupCapacityGW);
    dboutput4(regionName,"Capacity",sectorName,name + "BackupCost","90c/kWh",backupCostCentsPerkWh);
    dboutput4(regionName,"Capacity",sectorName,name + "ResourceCost","90c/kWh",resourceCostCentsPerkWh);
}