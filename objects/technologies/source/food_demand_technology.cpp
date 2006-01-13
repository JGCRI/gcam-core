/*! 
* \file food_demand_technology.cpp
* \ingroup CIAM
* \brief FoodDemandTechnology class source file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

// User headers
#include "technologies/include/technology.h"
#include "technologies/include/food_demand_technology.h"
#include "emissions/include/ghg.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

// Technology class method definition

//! Default constructor.
FoodDemandTechnology::FoodDemandTechnology( ) : technology (){
}

//! Parses any input variables specific to derived classes
bool FoodDemandTechnology::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
	return false;
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& FoodDemandTechnology::getXMLName1D() const {
	return getXMLNameStatic1D();
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
const std::string& FoodDemandTechnology::getXMLNameStatic1D() {
	const static string XML_NAME = "FoodDemandTechnology";
	return XML_NAME;
}

/*! \brief check for fixed demands and set values to counter
*
* If the output of this technology is fixed then set that value to the appropriate marketplace counter
* If it is not, then reset counter
*
* \author James Blackwood
* \param period Model period
*/
void FoodDemandTechnology::tabulateFixedDemands( const string regionName, const int period, const IInfo* aSubsectorIInfo ) {
    Marketplace* marketplace = scenario->getMarketplace();

    // Checking for market existence here.  This is not called every period, so it does not consume too much processor time.
	if ( marketplace->getPrice( fuelname, regionName, period, true ) != Marketplace::NO_MARKET_PRICE ) {
		double fixedInput = aSubsectorIInfo->getDouble( "ServiceDemand", true );
		if ( fixedInput >= 0 ) {
            // set demand for fuel in marketInfo counter
			double exisitingDemand = max( marketplace->getMarketInfo( fuelname, regionName , period, true )->getDouble( "calDemand", true ) , 0.0 );
			marketplace->getMarketInfo( fuelname, regionName, period, true )->setDouble( "calDemand", exisitingDemand + fixedInput );        
        } else {
            // If not fixed, then set to -1 to indicate a demand that is not completely fixed
			marketplace->getMarketInfo( fuelname, regionName, period, true )->setDouble( "calDemand", -1 );
        }
    }
}
