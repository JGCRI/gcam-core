/*! 
* \file forest_supply_sector.cpp
* \ingroup Objects
* \brief ForestSupplySector class source file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/xml_helper.h"

#include "sectors/include/forest_supply_sector.h"
#include "sectors/include/forest_supply_subsector.h"
#include "util/base/include/model_time.h"
#include "containers/include/iinfo.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/scenario.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string ForestSupplySector::prefix = "Future"; 

/*! \brief Constructor.
* \author James Blackwood
*/
ForestSupplySector::ForestSupplySector( std::string& aRegionName )
: FoodSupplySector( aRegionName ) {
}

//! Destructor
ForestSupplySector::~ForestSupplySector( ) {
}

/*! \brief Parses any attributes specific to derived classes
* \author Josh Lurz, James Blackwood
* \param nodeName The name of the curr node. 
* \param curr pointer to the current node in the XML input tree
*/
bool ForestSupplySector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ){
    if ( nodeName == ForestSupplySubsector::getXMLNameStatic() ) {
		parseContainerNode( curr, subsec, subSectorNameMap, new ForestSupplySubsector( regionName, name ) );
	}
    else if( !FoodSupplySector::XMLDerivedClassParse( nodeName, curr ) ) {
        return false;
    }
	return true;
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& ForestSupplySector::getXMLName() const {
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
const string& ForestSupplySector::getXMLNameStatic() {
	const static string XML_NAME = "ForestSupplySector";
	return XML_NAME;
}

//! Create markets
void ForestSupplySector::setMarket() {
    Marketplace* marketplace = scenario->getMarketplace();
    const Modeltime* modeltime = scenario->getModeltime();
    const double CVRT90 = 2.212; // 1975 $ to 1990 $

    vector <double> tempCalPrice;
    tempCalPrice.resize( modeltime->getmaxper(), calPrice / CVRT90 ); // market prices are in $1975
    if ( marketplace->createMarket( regionName, mMarketName, name, IMarketType::NORMAL ) ) {
        marketplace->setPriceVector( name, regionName, tempCalPrice  );
        for( int per = 2; per < modeltime->getmaxper(); ++per ){
            marketplace->setMarketToSolve( name, regionName, per );
        }
        for( int per = 0; per < modeltime->getmaxper(); ++per ){
            marketplace->getMarketInfo( name, regionName, per, true )->setDouble( "calPrice", calPrice );
        }
    }
    if ( marketplace->createMarket( regionName, mMarketName, prefix + name, IMarketType::NORMAL ) ) {
        marketplace->setPriceVector( prefix + name, regionName, tempCalPrice );
        for( int per = 1; per < modeltime->getmaxper(); ++per ){
            marketplace->getMarketInfo( prefix + name, regionName, per, true )->setDouble( "calPrice", calPrice );
            marketplace->setMarketToSolve( prefix + name, regionName, per );
        }
    }
}
