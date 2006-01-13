/*! 
* \file food_demand_sector.cpp
* \ingroup CIAM
* \brief FoodDemandSector class source file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/xml_helper.h"

#include "sectors/include/food_demand_sector.h"
#include "sectors/include/food_demand_subsector.h"
#include "containers/include/iinfo.h"
#include "containers/include/scenario.h"
#include "containers/include/gdp.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief Default constructor.
*
* Constructor initializes member variables with default values, sets vector sizes, and sets value of debug flag.
*
* \author Sonny Kim, Steve Smith, Josh Lurz
*/
FoodDemandSector::FoodDemandSector( const string regionName ): DemandSector(regionName){
    // resize vectors
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();

    baseConsumptionPerCap.resize( maxper, -1 );

	caloriesperCapDay = -1;
}

/*! \brief Parses any attributes specific to derived classes
*
* Method parses any input data attributes (not child nodes, see XMLDerivedClassParse) that are specific to any classes derived from this class.
*
* \author James Blackwood
* \param nodeName The name of the curr node. 
* \param curr pointer to the current node in the XML input tree
*/
bool FoodDemandSector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    
	const Modeltime* modeltime = scenario->getModeltime();

	if ( nodeName == FoodDemandSubsector::getXMLNameStatic() ) {
		parseContainerNode( curr, subsec, subSectorNameMap, new FoodDemandSubsector( regionName, name ) );
	}
	else if( nodeName == "caloriesperCapDay" ) {
		caloriesperCapDay = XMLHelper<double>::getValue( curr );
	}
	else if( nodeName == "baseConsumptionPerCap" ) {
		XMLHelper<double>::insertValueIntoVector( curr, baseConsumptionPerCap, modeltime );
	}
	else if (!DemandSector::XMLDerivedClassParse( nodeName, curr )) {
		return false;
	}
	return true;
}

//! XML output for viewing.
void FoodDemandSector::toInputXMLDerived( std::ostream& out, Tabs* tabs ) const {
	const Modeltime* modeltime = scenario->getModeltime();
	XMLWriteElement ( caloriesperCapDay, "caloriesperCapDay", out, tabs );
	XMLWriteVector ( baseConsumptionPerCap, "baseConsumptionPerCap", out, tabs, modeltime );
	DemandSector::toInputXMLDerived ( out, tabs );
}

//! Write object to debugging xml output stream.
void FoodDemandSector::toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const {
	const Modeltime* modeltime = scenario->getModeltime();
	XMLWriteElement ( caloriesperCapDay, "caloriesperCapDay", out, tabs );
	XMLWriteVector ( baseConsumptionPerCap, "baseConsumptionPerCap", out, tabs, modeltime );
	DemandSector::toDebugXMLDerived ( period, out, tabs );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& FoodDemandSector::getXMLName() const {
	return getXMLNameStatic();
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
const std::string& FoodDemandSector::getXMLNameStatic() {
	const static string XML_NAME = "FoodDemandSector";
	return XML_NAME;
}

/*! \brief Returns sectoral energy consumption.
* \details Food demand sectors do not currently use any energy.
* \author Steve Smith
* \param aPeriod Model period.
* \return Total energy input.
*/
double FoodDemandSector::getEnergyInput( const int aPeriod ) const {
    return 0;
}

/*! \brief Returns the share weighted energy price.
* \details Food demand sectors do not have any energy usage, so their energy
*          price is zero.
* \param aPeriod Model period.
* \return Weighted energy price.
*/
double FoodDemandSector::getWeightedEnergyPrice( const int aPeriod ) const {
	return 0;
}

/*! \brief check for fixed demands and set values to counter
*
* If there is a baseConsumptionPerCap in this period, then the service demand is calculated, otherwise it is -1.
* The service demand is then added to a SectorInfo, which is passed down to subsector and then technology.
* At the technology level the service demand is added to the "calDemand" variable of the marketplace.
*
* \author James Blackwood
*/
void FoodDemandSector::tabulateFixedDemands( const int period, const GDP* gdp ) {

	if ( baseConsumptionPerCap[ period ] >= 0 ) {
		double serviceDemand = calcServiceDemand ( gdp, period );
		mSectorInfo->setDouble( "ServiceDemand", serviceDemand );
	}
	else {
		mSectorInfo->setDouble( "ServiceDemand", -1 );
	}

	for( vector<Subsector*>::const_iterator j = subsec.begin(); j != subsec.end(); j++ ){
		( *j )->tabulateFixedDemands( period, mSectorInfo.get() );
	}
}

/*! \brief Aggrgate sector food service demand function
*
* Function calculates the aggregate demand for food services and passes that down to the sub-sectors. 
* Demand is proportional to GDP (to a power) times population times calories per person per day.
*
* \author Sonny Kim, Steve Smith, James Blackwood
* \param gdp GDP
* \param period Model period
* \pre Sector price attribute must have been previously calculated and set (via calcPrice)
*/
void FoodDemandSector::aggdemand( const GDP* gdp, const int period ) {
	// demand for service
	if (period == 0) {
		service[ period ] = getOutput(0); // base output is initialized by data
		techChangeCumm[period] = 1; // base year technical change
	}
	else {
        // calculate cummulative technical change using AEEI, autonomous end-use energy intensity
    	const Modeltime* modeltime = scenario->getModeltime();
        techChangeCumm[period] = techChangeCumm[ period - 1 ]
                                 * pow(1+aeei[period],modeltime->gettimestep(period));

    	// demand sector output is total end-use sector demand for service
	    // adjust demand using cummulative technical change
        service[ period ] = calcServiceDemand( gdp, period ) / techChangeCumm[ period ];
     }

	// sets subsector outputs, technology outputs, and market demands
	setOutput( service[ period ], gdp, period );
}

double FoodDemandSector::calcServiceDemand( const GDP* gdp, const int period ) {
	const Modeltime* modeltime = scenario->getModeltime();
	const int normPeriod = modeltime->getyr_to_per(1990);
	double priceRatio = 1;
    if( period > normPeriod ){
		priceRatio = getPrice( period ) / getPrice( normPeriod );
	}

	// GDP is in millions and GDP/cap is in $1000 per capita, so this is pop in thousands.
	double population = gdp->getGDP( period ) / gdp->getApproxGDPperCap( period );

	double scaledGDPperCap = gdp->getScaledGDPperCap( period ) / gdp->getScaledGDPperCap( normPeriod );
	
    double serviceDemand;
	if ( baseConsumptionPerCap[ period ] >= 0 ) {
		// calculate base year scalers
        caloriesperCapDay = baseConsumptionPerCap[ period ] / pow( priceRatio, pElasticity[period] );
        
        caloriesperCapDay /= pow( scaledGDPperCap, iElasticity[period] ) * 365;

        serviceDemand = baseConsumptionPerCap[ period ] * population / 1000;
	} 
	else {
		serviceDemand = caloriesperCapDay * pow( priceRatio, pElasticity[period] ) * pow( scaledGDPperCap, iElasticity[period] ); // unitless
		// need to multiply above by population.
		serviceDemand *= population * 365 / 1000;  // (kCal * 1000's  = MCal/yr. Mcal/1000 = GCal )
	}

	return serviceDemand;
}

void FoodDemandSector::scaleCalibratedValues( const int period, const string& goodName, const double scaleValue ) {
    cout << "Error: This method is not implemented!" << endl;
}
