/*! 
* \file forest_demand_sector.cpp
* \ingroup Objects
* \brief ForestDemandSector class source file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include "util/base/include/xml_helper.h"
#include "sectors/include/forest_demand_sector.h"
#include "containers/include/iinfo.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/scenario.h"
#include "containers/include/gdp.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string ForestDemandSector::prefix = "Future"; 

/*! \brief Default constructor.
* \author James Blackwood
*/
ForestDemandSector::ForestDemandSector( const string& regionName ): DemandSector(regionName){
	rotationPeriod = 0;
}

/*! \brief Parses any attributes specific to derived classes
* \author James Blackwood
* \param nodeName The name of the curr node. 
* \param curr pointer to the current node in the XML input tree
*/
bool ForestDemandSector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    // Call the derived class parse which is one up in the chain.
    return DemandSector::XMLDerivedClassParse( nodeName, curr );
}

//! XML output for viewing.
void ForestDemandSector::toInputXMLDerived( std::ostream& out, Tabs* tabs ) const {
    // Call the xml writing routine that is one up in the chain.
    DemandSector::toInputXMLDerived( out, tabs );
}

//! Write object to debugging xml output stream.
void ForestDemandSector::toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const {
    // Call the xml writing routine that is one up in the chain.
    DemandSector::toDebugXMLDerived( period, out, tabs );    
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& ForestDemandSector::getXMLName() const {
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
const std::string& ForestDemandSector::getXMLNameStatic() {
	const static string XML_NAME = "ForestDemandSector";
	return XML_NAME;
}

/*! \brief Perform any initializations needed for each period.
*
* Any initializations or calcuations that only need to be done once per period
* (instead of every iteration) should be placed in this function.
*
* \author James Blackwood
* \param period Model period
*/
void ForestDemandSector::initCalc( NationalAccount& aNationalAccount, const Demographic* aDemographics, const int aPeriod )
{
    // Set a string defining the good consumed by this sector. 
    // If there is more than one good then calibration logic will not work
    // sjsTEMP -- need more robust method of obtaining this name
    demandedGoodName = "Forest";

    // TODO: Calibration checking needs to be implemented.

	DemandSector::initCalc( aNationalAccount, aDemographics, aPeriod );
}

/*! \brief Complete the initialization of a forest demand sector.
*/
void ForestDemandSector::completeInit( const IInfo* aRegionInfo,
                                       DependencyFinder* aDependencyFinder,
                                       ILandAllocator* aLandAllocator,
                                       const GlobalTechnologyDatabase* aGlobalTechDB )
{
    DemandSector::completeInit( aRegionInfo, aDependencyFinder, aLandAllocator, aGlobalTechDB );
	rotationPeriod = aRegionInfo->getInteger( "rotationPeriod", true );
}

/*! \brief Aggrgate sector forest service demand function
*
* Function calculates the aggregate demand for forest services and passes that down to the sub-sectors. 
* Demand is proportional to GDP (to a power) times population.
*
* \author Sonny Kim, Steve Smith, James Blackwood
* \param gdp GDP (relative or absolute?)
* \param period Model period
* \todo Steve: need to find way to get the name of the corresponding supply sector here
* \pre Sector price attribute must have been previously calculated and set (via calcPrice)
*/
void ForestDemandSector::aggdemand( const GDP* gdp, const int period ) {
    Marketplace* marketplace = scenario->getMarketplace();
    const Modeltime* modeltime = scenario->getModeltime();
    double forestDemand;
    double forestDemandFuture;
    const int future = rotationPeriod / modeltime->gettimestep( period );


    // initialize for base periods
    if ( period == 0 ) {
        forestDemandFuture = forestDemand = getOutput(0); // This is not used, here just to avoid invalid values
        techChangeCumm[ 0 ] = 1;
    }
    else if ( period == 1 ) {
        // This does not work if more than one period is calibrated. Must fix this at some point for demand sectors in general.
        // GDP is in millions and GDP/cap is in $1000 per capita, so this is pop in thousands.
        double population1990 = gdp->getGDPNotAdjusted( 1 ) / gdp->getGDPPerCapitaNotAdjusted( 1 );

        perCapitaBaseOutput = getOutput(0)/ population1990;

        // If calibration values are set, then reset base outputs appropriately
        // This only works for a base year of 1990. Need more general method with read-in calibration flag value
        if ( outputsAllFixed( period ) ) {

            double population1990 = gdp->getGDPNotAdjusted( 1 ) / gdp->getGDPPerCapitaNotAdjusted( 1 );         
            perCapitaBaseOutput = getCalAndFixedOutputs( period, "allInputs" , true ) / population1990;

            // If output is calibrated, then re-set base output value to appropriate value
            mBaseOutput = perCapitaBaseOutput * population1990;
            service[ 0 ] = mBaseOutput;
        }
    }

    if ( period > 0 ) {
        // Here, the priceRatio is calculated from the price this period over the the price in 1990
        const int normPeriod = modeltime->getyr_to_per( 1990 );
        double currPriceRatio = getPrice( period ) / getPrice( normPeriod );
        // TODO: Should this have error checking as below?

        // The priceRatio is then passed in to calculate demand.
        forestDemand = calcForestDemand ( gdp, period, normPeriod, currPriceRatio );

        // Here, the priceRatio needs the prices from the future market of this product, not this market.
        double basePrice = marketplace->getPrice( prefix + demandedGoodName, regionName, normPeriod );
        double futurePriceRatio;
        if ( basePrice < util::getSmallNumber() ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Invalid future forest price of " << basePrice << " in region " << regionName << "." << endl;
            futurePriceRatio = 1;
        }
        else {
            futurePriceRatio = marketplace->getPrice( prefix + demandedGoodName, regionName, period ) / basePrice;
        }

        // The priceRatio is then passed in to calculate demand.
        forestDemandFuture = calcForestDemand ( gdp, (period + future), normPeriod, futurePriceRatio );

        // calculate cummulative technical change using AEEI, autonomous end-use energy intensity
        techChangeCumm[period] = techChangeCumm[period-1]*pow(1+aeei[period],modeltime->gettimestep(period));
    }

    // Save the service demand without technical change applied for comparison with miniCAM.
    service[ period ] = forestDemand;

    // demand sector output is total end-use sector demand for service
    // adjust demand using cummulative technical change
    // For forests, these functions take care of current demand for forest products
    service[ period ] = forestDemand/techChangeCumm[period];

    // sets subsector outputs, technology outputs, and market demands
    setOutput( service[ period ], gdp, period );

    // Need to put the demand for future forests into the marketplace
    marketplace->addToDemand( prefix + demandedGoodName, regionName, forestDemandFuture, period ); // fuelname will be name of market, input is amount
}

double ForestDemandSector::calcForestDemand ( const GDP* gdp, const int period, const int normPeriod, double priceRatio ) {
    
    // If the current trial market price is zero the price ratio will be zero.
    // Set this to a small number to prevent invalid numbers.
    priceRatio = max( priceRatio, util::getSmallNumber() );

	// TODO -- pass in proper price ratio (for future forest, or today's forest)    
    int modelPeriod = period;
	const Modeltime* modeltime = scenario->getModeltime();
    // For future forests, for now, simply get parameters for last model period.
    if ( period >= modeltime->getmaxper() ) {
        modelPeriod = modeltime->getmaxper() - 1;
    }
    
	// GDP is in millions and GDP/cap is in $1000 per capita, so this is pop in thousands.
	double population = gdp->getGDPNotAdjusted( modelPeriod ) 
                        / gdp->getGDPPerCapitaNotAdjusted( modelPeriod );

	double scaledGDPperCap = gdp->getGDPPerCapitaNotAdjusted( modelPeriod )
                            / gdp->getGDPPerCapitaNotAdjusted( normPeriod );
   

	double forestDemand = perCapitaBaseOutput * pow(priceRatio, pElasticity[ modelPeriod ] )
                          *pow(scaledGDPperCap,iElasticity[ modelPeriod ])
                          * population;

    assert( util::isValidNumber( forestDemand ) );

	return forestDemand;
}
