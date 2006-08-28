/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Labratory operated by Battelle Memorial Institute.
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

#include "emissions/include/co2_emissions.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/iinfo.h"
#include "technologies/include/ioutput.h"
#include "containers/include/gdp.h"

using namespace std;
using namespace xercesc;


extern Scenario* scenario;

//! Default Destructor.
CO2Emissions::~CO2Emissions(){
}

//! Clone operator.
CO2Emissions* CO2Emissions::clone() const {
    return new CO2Emissions( *this );
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

double CO2Emissions::getGHGValue( const string& regionName, const string& fuelName,
                                  const vector<IOutput*>& aOutputs,
                                  const double efficiency, const int period ) const {
    const Marketplace* marketplace = scenario->getMarketplace();
    
    // Constants
    const double CVRT90 = 2.212; // 1975 $ to 1990 $
    const double CVRT_tg_MT = 1e-3; // to get teragrams of carbon per EJ to metric tons of carbon per GJ
    
    // get carbon storage cost from the market
    double marketStorageCost = Marketplace::NO_MARKET_PRICE;
    // Get the price from the market if there is a name.
    if( storageName != "" ){
        marketStorageCost = marketplace->getPrice( storageName, regionName, period, false );
    }
    // If the market storage cost is unset use the read in storage cost.
    if( marketStorageCost == Marketplace::NO_MARKET_PRICE ){
        marketStorageCost = storageCost;
    }
    
    double GHGTax = marketplace->getPrice( getName(), regionName, period, false );
    if( GHGTax == Marketplace::NO_MARKET_PRICE ){
        GHGTax = 0;
        // If there is no tax market, turn off sequestration technology by increasing storage cost
        // This is still not correct.
        marketStorageCost = util::getLargeNumber();
    }

    // units for generalized cost is in 75$/gj
    double generalizedCost = 0;

    // Fuel market may not exist.
    const IInfo* fuelInfo = marketplace->getMarketInfo( fuelName, regionName, period, false );
    const double coefFuel = fuelInfo ? fuelInfo->getDouble( "CO2Coef", false ) : 0;

    double coefProduct = calcOutputCoef( aOutputs, period );
    // if remove fraction is greater than zero and storage cost is required
    if (rmfrac > 0) {
        // add geologic sequestration cost
        if (isGeologicSequestration) {
            // gwp applied only on the amount emitted
            // account for conversion losses through efficiency
            generalizedCost = ((1.0 - rmfrac)*GHGTax*gwp + rmfrac*marketStorageCost)
                * (coefFuel/efficiency - coefProduct) / CVRT90 * CVRT_tg_MT;
        }
        // no sequestration or storage cost added for non-energy use of fossil fuels
        else {
            generalizedCost = ((1.0 - rmfrac)*GHGTax*gwp)
                * (coefFuel/efficiency - coefProduct) / CVRT90 * CVRT_tg_MT;
        }
    }
    // no storage required
    else {
        generalizedCost = GHGTax * gwp * (coefFuel/efficiency - coefProduct) / CVRT90 * CVRT_tg_MT;
    }

    // The generalized cost returned by the GHG may be negative if
    // emissions crediting is occuring.
    return generalizedCost;
}

void CO2Emissions::calcEmission( const string& regionName,
                                 const string& fuelname,
                                 const double input,
                                 const vector<IOutput*>& aOutputs,
                                 const GDP* aGDP,
                                 const int aPeriod ){
    // Primary output is always stored at position zero and used to drive
    // emissions.
    assert( aOutputs[ 0 ] );
    double primaryOutput = aOutputs[ 0 ]->getPhysicalOutput( aPeriod );

    const Marketplace* marketplace = scenario->getMarketplace();

    // Fuel market may not exist.
    const IInfo* fuelInfo = marketplace->getMarketInfo( fuelname, regionName, aPeriod, false );
    const double coefFuel = fuelInfo ? fuelInfo->getDouble( "CO2Coef", false ): 0;
    double coefProduct = calcOutputCoef( aOutputs, aPeriod );

    // sequestered emissions
    if (rmfrac > 0) {
        // geologic sequestration
        if(isGeologicSequestration) {
            sequestAmountGeologic = rmfrac * ( (input * coefFuel ) - ( primaryOutput * coefProduct ) );
        }
        // non-energy use of fuel, ie petrochemicals
        else {
            sequestAmountNonEngy = rmfrac * ( (input * coefFuel ) - ( primaryOutput * coefProduct ) );
        }
    }
    // Note that negative emissions can occur here since biomass has a coef of 0. 
    mEmissions[ aPeriod ] = ( 1.0 - rmfrac ) * ( ( input * coefFuel ) - ( primaryOutput * coefProduct ) );
    mEmissionsByFuel[ aPeriod ] = ( 1.0 - rmfrac ) * input * coefFuel;

    addEmissionsToMarket( regionName, aPeriod );
}
bool CO2Emissions::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ){
    return false;
}

// Do nothing because the name is always CO2.
void CO2Emissions::parseName( const string& aNameAttr ){
}

void CO2Emissions::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
}

void CO2Emissions::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
}


