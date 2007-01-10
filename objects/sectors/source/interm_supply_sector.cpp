/*!
* \file interm_supply_sector.cpp
* \ingroup Objects
* \brief Sector class source file.
* \author Marshall Wise
*/

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>

#include <xercesc/dom/DOMNode.hpp>

#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "sectors/include/interm_supply_sector.h"
#include "sectors/include/interm_subsector.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

IntermittentSupplySector::IntermittentSupplySector( const string& aRegionName ):
SupplySector( aRegionName ),
elecReserveMargin( 0 ),
aveGridCapacityFactor( 1 ),
backupCapacityFactor( 0 ),
backupCost( 0 )
{
    // Set that this sector requires a trial supply market.
    mHasTrialSupply = true;
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. This function may be virtual to be overriden by derived class
* pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& IntermittentSupplySector::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. The "==" operator that is used when parsing, required this second
* function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const std::string& IntermittentSupplySector::getXMLNameStatic() {
	const static string XML_NAME = "intermittent-supplysector";
    return XML_NAME;
}

/*! \brief Parses any child nodes specific to derived classes
*
* Method parses any input data from child nodes that are specific to the classes
* derived from this class. Since Sector is the generic base class, there are no
* values here.
*
* \author Marshall Wise
* \param nodeName name of current node
* \param curr pointer to the current node in the XML input tree
*/
bool IntermittentSupplySector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    // call the supply sector XML parse (one parent level up) to fill supply sector attributes
    if( SupplySector::XMLDerivedClassParse( nodeName, curr ) ){
    }
    else if( nodeName == IntermittentSubsector::getXMLNameStatic() ){
        parseContainerNode( curr, subsec, subSectorNameMap, new IntermittentSubsector( regionName, name ) );
    }
    else if( nodeName == "elecReserveMargin" ){
        elecReserveMargin = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "aveGridCapacityFactor" ){
        aveGridCapacityFactor = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "backupCapacityFactor" ){
        backupCapacityFactor = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "backupCost" ){
        backupCost = XMLHelper<double>::getValue( curr );
    }
    else {
        return false;
    }
    return true;
}

/*! \brief XML output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML
* \author Marshall Wise
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void IntermittentSupplySector::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    // Write out parent class information.
    SupplySector::toInputXMLDerived( out, tabs );

    XMLWriteElementCheckDefault( elecReserveMargin, "elecReserveMargin", out, tabs, 0.0);
    XMLWriteElementCheckDefault( aveGridCapacityFactor, "aveGridCapacityFactor", out, tabs, 1.0);
    XMLWriteElementCheckDefault( backupCapacityFactor, "backupCapacityFactor", out, tabs, 0.0);
    XMLWriteElementCheckDefault( backupCost, "backupCost", out, tabs, 0.0);
}	

//! Write object to debugging xml output stream.
void IntermittentSupplySector::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    // Write out parent class information.
    SupplySector::toDebugXMLDerived( period, out, tabs );

    XMLWriteElement( elecReserveMargin, "elecReserveMargin", out, tabs );
    XMLWriteElement( aveGridCapacityFactor, "aveGridCapacityFactor", out, tabs );
    XMLWriteElement( backupCapacityFactor, "backupCapacityFactor", out, tabs );
    XMLWriteElement( backupCost, "backupCost", out, tabs );
}

/*! \brief Complete the initialization of the intermittent suppply sector.
* \details The intermittent supply sector overrides the SupplySector
*          completeInit so that it can create a trial market for the
*          intermittent supply. This is required to determine the capacity share
*          of the sector.
* \author Josh Lurz
* \param aRegionInfo Region info container.
* \param aDependencyFinder Object which tracks sector dependencies.
* \param aLandAllocator Regional land allocator.
*/
void IntermittentSupplySector::completeInit( const IInfo* aRegionInfo,
                                             DependencyFinder* aDependencyFinder,
                                             ILandAllocator* aLandAllocator,
                                             const GlobalTechnologyDatabase* aGlobalTechDB )
{
	// Call the parent supply sector complete init.
	SupplySector::completeInit( aRegionInfo, aDependencyFinder, aLandAllocator, aGlobalTechDB );
}

/*! \brief Perform any initializations needed for each period.
* \details Any initializations or calculations that only need to be done once
*          per period(instead of every iteration) should be placed in this
*          function.
* \author Marshall Wise
* \param period Model period
*/
void IntermittentSupplySector::initCalc( NationalAccount* aNationalAccount,
                                         const Demographic* aDemographics,
                                         const int aPeriod )
{
    // add items to sectorinfo must be done before Sector:initCalc() so that
    // information is available to subsector and technology initCalc() routines
	mSectorInfo->setDouble( "elecReserveMargin", elecReserveMargin );
    mSectorInfo->setDouble( "aveGridCapacityFactor", aveGridCapacityFactor );
    mSectorInfo->setDouble( "backupCapacityFactor", backupCapacityFactor );
    mSectorInfo->setDouble( "backupCost", backupCost );

    SupplySector::initCalc( aNationalAccount, aDemographics, aPeriod );
}
