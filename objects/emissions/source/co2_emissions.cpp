/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
 */

/*! 
 * \file co2_emissions.h
 * \ingroup Objects
 * \brief CO2Emissions class header file.
 * \author Jim Naslund
 */

#include "util/base/include/definitions.h"
//#include "emissions/include/aghg.h"

#include "emissions/include/co2_emissions.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/iinfo.h"
#include "technologies/include/ioutput.h"
#include "technologies/include/icapture_component.h"

using namespace std;
using namespace xercesc;


extern Scenario* scenario;

//! Default Constructor with default emissions unit.
CO2Emissions::CO2Emissions(){
    mEmissionsUnit = "MTC";
}

//! Default Destructor.
CO2Emissions::~CO2Emissions(){
}

//! Clone operator.
CO2Emissions* CO2Emissions::clone() const {
    return new CO2Emissions( *this );
}

void CO2Emissions::copyGHGParameters( const AGHG* aPrevGHG ){
    // Nothing needs to be copied.
}

/*!
 * \brief Get the XML node name for output to XML.
 * \details This public function accesses the private constant string, XML_NAME.
 *          This way the tag is always consistent for both read-in and output and can be easily changed.
 *          This function may be virtual to be overridden by derived class pointers.
 * \author Jim Naslund
 * \return The constant XML_NAME.
 */
const string& CO2Emissions::getXMLName() const {
    return getXMLNameStatic();
}

const string& CO2Emissions::getXMLNameStatic(){
    static const string XML_NAME = "CO2";
    return XML_NAME;
}

const string& CO2Emissions::getName() const {
    return getXMLNameStatic();
}

void CO2Emissions::initCalc( const string& aRegionName,
                             const string& aFuelName,
                             const IInfo* aLocalInfo,
                             const int aPeriod )
{
    // Setup the cached fuel CO2 coefficient.
    // Fuel market may not exist.
    const Marketplace* marketplace = scenario->getMarketplace();
    const IInfo* fuelInfo = marketplace->getMarketInfo( aFuelName,
                                                        aRegionName,
                                                        aPeriod,
                                                        false );

    mFuelCoefficient = fuelInfo ? fuelInfo->getDouble( "CO2Coef", false ) : 0;
}

double CO2Emissions::getGHGValue( const string& aRegionName,
                                  const string& aFuelName,
                                  const vector<IOutput*>& aOutputs,
                                  const double aEfficiency,
                                  const ICaptureComponent* aSequestrationDevice,
                                  const int aPeriod ) const
{
    // Constants
    const double CVRT90 = 2.212; // 1975 $ to 1990 $
    // Conversion from teragrams of carbon per EJ to metric tons of carbon per GJ
    const double CVRT_TG_MT = 1e-3; 

    // Get carbon storage cost from the sequestrion device if there is one.
    double storageCost = aSequestrationDevice ? aSequestrationDevice->getStorageCost( aRegionName,
                                                                                      aPeriod )
                                              : 0;

    // Get the remove fraction from the sequestration device. The remove
    // fraction is zero if there is no sequestration device.
    double removeFraction = aSequestrationDevice ? aSequestrationDevice->getRemoveFraction() : 0;

    // Get the greenhouse gas tax from the marketplace.
    const Marketplace* marketplace = scenario->getMarketplace();
    double GHGTax = marketplace->getPrice( getName(), aRegionName, aPeriod, false );
    if( GHGTax == Marketplace::NO_MARKET_PRICE ){
        GHGTax = 0;
    }

    // Calculate the generalized emissions cost per unit.
    double coefProduct = calcOutputCoef( aOutputs, aPeriod );
    
    assert( mFuelCoefficient.isInited() );

    // units for generalized cost is in 1975$/GJ
    double generalizedCost = ( ( 1 - removeFraction ) * GHGTax + removeFraction * storageCost )
            * ( mFuelCoefficient / aEfficiency - coefProduct) / CVRT90 * CVRT_TG_MT;

    return generalizedCost;
}

void CO2Emissions::calcEmission( const string& aRegionName,
                                 const string& aFuelname,
                                 const double aInput,
                                 const vector<IOutput*>& aOutputs,
                                 const GDP* aGDP,
                                 ICaptureComponent* aSequestrationDevice,
                                 const int aPeriod )
{
    // Primary output is always stored at position zero and used to drive
    // emissions.
    assert( aOutputs[ 0 ] );
    double primaryOutput = aOutputs[ 0 ]->getPhysicalOutput( aPeriod );
    
    assert( mFuelCoefficient.isInited() );
    double coefProduct = calcOutputCoef( aOutputs, aPeriod );

    // Note that negative emissions can occur here since biomass has a coef of 0.
    double emissionFraction = 1 - ( aSequestrationDevice ? aSequestrationDevice->getRemoveFraction() : 0 );
    double inputEmissions = aInput * mFuelCoefficient;
    mEmissions[ aPeriod ] = emissionFraction * ( inputEmissions - primaryOutput * coefProduct );
    mEmissionsByFuel[ aPeriod ] = emissionFraction * inputEmissions;

    // Calculate sequestered emissions if there is a sequestration device.
    if( aSequestrationDevice ){
        aSequestrationDevice->calcSequesteredAmount( aRegionName, getName(), aInput, primaryOutput,
                                                     mFuelCoefficient, coefProduct, aPeriod );
    }

    addEmissionsToMarket( aRegionName, aPeriod );
}

bool CO2Emissions::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ){
    return false;
}

// Do nothing because the name is always CO2.
void CO2Emissions::parseName( const string& aNameAttr ){
}

void CO2Emissions::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    // write the xml for the class members.
    XMLWriteElementCheckDefault( mEmissionsUnit, "emissions-unit", out, tabs, string("MTC") );
}

void CO2Emissions::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    // write the xml for the class members.
    XMLWriteElement( mEmissionsUnit, "emissions-unit", out, tabs );
}
