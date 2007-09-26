/*! 
* \file food_supply_subsector.cpp
* \ingroup Objects
* \brief FoodSupplySubsector class source file.
* \author James Blackwood
*/

#include "util/base/include/definitions.h"
#include <string>
#include <xercesc/dom/DOMNode.hpp>

#include "sectors/include/food_supply_subsector.h"
#include "technologies/include/food_production_technology.h"
#include "technologies/include/unmanaged_land_technology.h"
#include "util/base/include/xml_helper.h"

using namespace std;
using namespace xercesc;

/*! \brief Constructor.
*/
FoodSupplySubsector::FoodSupplySubsector( const string& regionName,
                                          const string& sectorName )
										  : Subsector( regionName, sectorName ){
}

FoodSupplySubsector::~FoodSupplySubsector() {
}

/*! \brief Returns true if the nodename is a valid child for this class.
*
* Virtual function which specifies the XML name of the possible technology children of this class.
* This function allows all technologies to be properly parsed using the base subsector code.
* \author Steve Smith
* \pre Needs corresponding createChild() function
* \return True if nodename is a valid child of this class.
*/
bool FoodSupplySubsector::isNameOfChild( const string& nodename ) const {
    return ( nodename == FoodProductionTechnology::getXMLNameStatic1D() ) ||
           ( nodename == UnmanagedLandTechnology::getXMLNameStatic1D() );
}

/*!
 * \brief Derived helper function to generate a child element or construct the
 *        appropriate technology.
 * \param aTechType The name of the XML node, which is the type of the
 *        technology.
 * \param aTechName The name of the new technology.
 * \param aYear The year of the new technology.
 * \pre isNameOfChild returned that the type could be created.
 * \author Steve Smith
 * \return A newly created technology of the specified type.
 */
ITechnology* FoodSupplySubsector::createChild( const string& aTechType,
                                               const string& aTechName,
                                               const int aTechYear ) const
{
    if ( aTechType == FoodProductionTechnology::getXMLNameStatic1D() ) {
      return new FoodProductionTechnology( aTechName, aTechYear );
    }
    else if ( aTechType == UnmanagedLandTechnology::getXMLNameStatic1D() ) {
      return new UnmanagedLandTechnology( aTechName, aTechYear );
    }
}

//! Parses any input variables specific to derived classes
bool FoodSupplySubsector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    return false;
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& FoodSupplySubsector::getXMLName() const {
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
const string& FoodSupplySubsector::getXMLNameStatic() {
    const static string XML_NAME = "FoodSupplySubsector";
    return XML_NAME;
}

double FoodSupplySubsector::calcShare( const int aPeriod,
                                       const GDP* aGDP ) const
{
    return 1;
}

void FoodSupplySubsector::adjustForCalibration( double aSubsectorVariableDemand,
                                                const GDP* aGDP,
                                                const int aPeriod )
{
    // Food and forestry supply sectors do not calibrate. Calibration occurs in
    // the land allocator. Call the Technology adjustForCalibration in case it
    // has something to do.
    for( unsigned int i = 0; i < techs.size(); ++i ){
        // Shares are always one and so subsector variable demand is the same as
        // technology variable demand.
        techs[ i ][ aPeriod ]->adjustForCalibration( aSubsectorVariableDemand,
                                                     regionName,
                                                     mSubsectorInfo.get(),
                                                     aPeriod );
    }
}

void FoodSupplySubsector::MCoutputAllSectors( const GDP* aGDP,
                                              const IndirectEmissionsCalculator* aIndirectEmissionsCalc,
                                              const vector<double> aSectorOutput ) const
{            
    Subsector::MCoutputAllSectors( aGDP,
                                   aIndirectEmissionsCalc,
                                   aSectorOutput );

    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);

    // do for all technologies in the Subsector
    for( unsigned int i = 0; i < techs.size(); ++i ){
        // technology fuel cost
        for ( int m= 0;m < maxper;m++) {
            temp[m] = techs[i][m]->getFuelCost( regionName, sectorName, m );
        }
        dboutput4( regionName,"Price",sectorName+" "+name+" Variable Cost",techs[i][ 0 ]->getName(),"75$/Ser",temp);
    }
}
