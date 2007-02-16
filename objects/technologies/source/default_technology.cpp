/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial
 * Institute. Battelle has certain unperfected rights in the software which
 * should not be copied or otherwise disseminated outside your organization
 * without the express written authorization from Battelle. All rights to the
 * software are reserved by Battelle. Battelle makes no warranty, express or
 * implied, and assumes no liability or responsibility for the use of this
 * software.
 */

/*! 
 * \file default_technology.cpp
 * \ingroup Objects
 * \brief DefaultTechnology class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>

#include "technologies/include/default_technology.h"
#include "containers/include/scenario.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//! Constructor
DefaultTechnology::DefaultTechnology( const string& aName,
                                      const int aYear )
                                      : Technology( aName, aYear )
{
}

//! Clone Function. Returns a deep copy of the current technology.
DefaultTechnology* DefaultTechnology::clone() const {
    return new DefaultTechnology( *this );
}

const string& DefaultTechnology::getXMLName1D() const {
    return getXMLNameStatic1D();
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
const string& DefaultTechnology::getXMLNameStatic1D() {
    const static string XML_NAME1D = "technology";
    return XML_NAME1D;
}

void DefaultTechnology::completeInit( const string& aRegionName,
                                      const string& aSectorName,
                                      DependencyFinder* aDepFinder,
                                      const IInfo* aSubsectorInfo,
                                      ILandAllocator* aLandAllocator,
                                      const GlobalTechnologyDatabase* aGlobalTechDB )
{
    Technology::completeInit( aRegionName, aSectorName, aDepFinder,
                              aSubsectorInfo, aLandAllocator, aGlobalTechDB );
}

void DefaultTechnology::initCalc( const string& aRegionName,
                                  const string& aSectorName,
                                  const IInfo* aSubsectorInfo,
                                  const Demographic* aDemographics,
                                  const int aPeriod )
{
    Technology::initCalc( aRegionName, aSectorName, aSubsectorInfo,
                          aDemographics, aPeriod );
}

void DefaultTechnology::postCalc( const string& aRegionName,
                                  const int aPeriod )
{
    Technology::postCalc( aRegionName, aPeriod );
}

void DefaultTechnology::production( const string& aRegionName,
                                    const string& aSectorName,
                                    double aVariableDemand,
                                    double aFixedOutputScaleFactor,
                                    const GDP* aGDP,
                                    const int aPeriod )
{
    Technology::production( aRegionName, aSectorName, aVariableDemand,
                            aFixedOutputScaleFactor, aGDP, aPeriod );
}

void DefaultTechnology::calcCost( const string& aRegionName,
                                  const string& aSectorName,
                                  const int aPeriod )
{
    Technology::calcCost( aRegionName, aSectorName, aPeriod );
}
double DefaultTechnology::getFuelCost( const string& aRegionName,
                                       const string& aSectorName,
                                       const int aPeriod ) const 
{
    return Technology::getFuelCost( aRegionName, aSectorName, aPeriod );
}

double DefaultTechnology::calcShare( const string& aRegionName,
                                     const string& aSectorName,
                                     const GDP* aGDP,
                                     const int aPeriod ) const
{
    return Technology::calcShare( aRegionName, aSectorName, aGDP, aPeriod );
}

double DefaultTechnology::getEfficiency( const int aPeriod ) const {
    return Technology::getEfficiency( aPeriod );
}

double DefaultTechnology::getNonEnergyCost( const int aPeriod ) const {
    return Technology::getNonEnergyCost( aPeriod );
}

bool DefaultTechnology::XMLDerivedClassParse( const string& aNodeName,
                                              const DOMNode* aCurr )
{
    // Empty implementation as the base class will parse all the variables
    // in XMLParse.
    return false;
}

void DefaultTechnology::toInputXMLDerived( ostream& aOut,
                                           Tabs* aTabs ) const
{
    // Empty implementation as the base class will print all the variables
    // in toInputXML.
}

void DefaultTechnology::toDebugXMLDerived( const int aPeriod,
                                           ostream& aOut,
                                           Tabs* aTabs ) const
{
    // Empty implementation as the base class will print all the variables
    // in toDebugXML.
}
