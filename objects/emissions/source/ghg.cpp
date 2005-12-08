/*! 
* \file ghg.cpp
* \ingroup Objects
* \brief Ghg class source file.
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <map>
#include <vector>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "emissions/include/ghg.h"
#include "emissions/include/indirect_emiss_coef.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/gdp.h"
#include "emissions/include/ghg_mac.h"
#include "functions/include/input.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/iinfo.h"
#include "util/logger/include/ilogger.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string Ghg::XML_NAME = "GHG";

Ghg::Ghg( const string& nameIn, const string& unitIn, const double rmfracIn, const double gwpIn, const double emissCoefIn ){
    name = nameIn;
    unit = unitIn;
    rmfrac = rmfracIn;
    gwp = gwpIn;
    emissCoef = emissCoefIn;
    isGeologicSequestration = true;
    storageCost = util::getLargeNumber(); // default to a large cost to turn off CCS
    sequestAmountGeologic = 0;
    sequestAmountNonEngy = 0;
    emissInd = 0;
    inputEmissions = -1;
    valueWasInputAtSomePoint = false;
    emAdjust = 0;
    gdpCap = 0;
    maxCntrl = -1000;
    techDiff = 0;
    gdpcap0 = 0;
    finalEmissCoef = -1;
    tau = 0;
    // this is inefficient as it is greater than the lifetime
    // but much simpler than converting period to liftime period 
    // TODO: Fix this so it has one spot per active period.
    mEmissions.resize( scenario->getModeltime()->getmaxper() );
    mEmissionsByFuel.resize( scenario->getModeltime()->getmaxper() );
    adjMaxCntrl = 1;
    multMaxCntrl = 1;
}

//! Destructor
Ghg::~Ghg(){
}

//! Copy constructor.
Ghg::Ghg( const Ghg& other ){
    copy( other );
}

//! Assignment operator.
Ghg& Ghg::operator=( const Ghg& other ){
    if( this != &other ){
        // If there was a destructor it would need to be called here.
        copy( other );
    }
    return *this;
}

//! Copy helper function.
void Ghg::copy( const Ghg& other ){
    name = other.name;
    unit = other.unit;
    rmfrac = other.rmfrac;
    gwp = other.gwp;
    emissCoef = other.emissCoef;
    isGeologicSequestration = other.isGeologicSequestration;
    storageCost = other.storageCost;
    sequestAmountGeologic = other.sequestAmountGeologic;
    sequestAmountNonEngy = other.sequestAmountNonEngy;
    emissInd = other.emissInd;
    inputEmissions = other.inputEmissions;
    valueWasInputAtSomePoint = other.valueWasInputAtSomePoint;
    emAdjust = other.emAdjust;
    maxCntrl = other.maxCntrl;
    techDiff = other.techDiff;
    gdpcap0 = other.gdpcap0;
    adjMaxCntrl = other.adjMaxCntrl;
    multMaxCntrl = other.multMaxCntrl;
    finalEmissCoef = other.finalEmissCoef;
    tau = other.tau;
    mEmissions.resize( scenario->getModeltime()->getmaxper() );
    mEmissionsByFuel.resize( scenario->getModeltime()->getmaxper() );
    // Perform a deep copy on the GhgMac.
    if( ghgMac.get() ){
        ghgMac.reset( other.ghgMac->clone() );
    }
}

//! Clone function which returns a deep copy of the Ghg.
Ghg* Ghg::clone() const {
    return new Ghg( *this );
}

/*! \brief initialize Ghg object with xml data
*
*/
void Ghg::XMLParse(const DOMNode* node) {   
    /*! \pre Assume we are passed a valid node. */
    assert( node );

    // get the name attribute.
    name = XMLHelper<string>::getAttrString( node, "name" );
    DOMNodeList* nodeList = node->getChildNodes();

    for( unsigned int i = 0; i < nodeList->getLength(); ++i ) {
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );      

        if( nodeName == "#text" ){
            continue;
        }
        else if( nodeName == "unit"){
            unit = XMLHelper<string>::getValueString( curr );
        }
        else if( nodeName == "inputEmissions" ){
            inputEmissions = XMLHelper<double>::getValue( curr );
            emissCoef = 0;
        }
        else if( nodeName == "emAdjust" ){
            emAdjust = XMLHelper<double>::getValue( curr );
        }
        else if( ( nodeName == "maxCntrl" ) ){
            maxCntrl = XMLHelper<double>::getValue( curr );
         }
        else if( ( nodeName == "gdpcap0" ) ){
            gdpcap0 = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "tau" ){
            tau = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "finalEmissCoef" ){
            finalEmissCoef = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "techDiff" ){
            techDiff = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "emisscoef" ){
            emissCoef = XMLHelper<double>::getValue( curr );
            inputEmissions = -1; // Reset inputEmissions to default value
        }
        // Adjust max Control level, leaving current emissions constant
        else if( nodeName == "adjMaxCntrl" ){
            adjMaxCntrl = XMLHelper<double>::getValue( curr );
        }
        // Multiply maximum control level, changing current emissions
        else if( nodeName == "multMaxCntrl" ){
            multMaxCntrl = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "removefrac" ){
            rmfrac = XMLHelper<double>::getValue( curr );
        }
    
        // is geologic sequestration, true or false
        else if( nodeName == "isGeologicSequestration" ){
            isGeologicSequestration = XMLHelper<bool>::getValue( curr );
        }
        // fixed storage cost read in from data
        else if( nodeName == "storageCost" ){
            storageName = XMLHelper<string>::getAttrString( curr, "name" );
            storageCost = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "GWP" ){
            gwp = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == GhgMAC::getXMLNameStatic() ){
            // Delete the MAC if requested.
            if( XMLHelper<int>::getAttr( curr, "delete" ) ){
                // Check if the curve exists.
                if( ghgMac.get() ){
                    ILogger& mainLog = ILogger::getLogger( "main_log" );
                    mainLog.setLevel( ILogger::DEBUG);
                    mainLog << "Deleting GHG MAC from GHG " << name << endl;
                    ghgMac.reset( 0 );
                }
            }
            // Create and parse the MAC.
            else {
                if( !ghgMac.get() ){
                    ghgMac.reset( new GhgMAC() );
                }
                ghgMac->XMLParse( curr );
            }
        }
        else if( XMLDerivedClassParse( nodeName, curr ) ){
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing GHG." << endl;
        }
    }
}
/*! \brief Parses any child nodes specific to derived classes
*
* Method parses any input data from child nodes that are specific to the classes derived from this class. Since Sector is the generic base class, there are no values here.
*
* \author Josh Lurz, Steve Smith
* \param nodeName name of current node
* \param curr pointer to the current node in the XML input tree
* \return Whether any node was parsed.
*/
bool Ghg::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    return false;
}

//! Writes datamembers to datastream in XML format.
void Ghg::toInputXML( ostream& out, Tabs* tabs ) const {

    XMLWriteOpeningTag( getXMLName(), out, tabs, name );

    // write xml for data members
    XMLWriteElementCheckDefault( inputEmissions, "inputEmissions", out, tabs, -1.0 );
    XMLWriteElementCheckDefault( emissCoef, "emisscoef", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( rmfrac, "removefrac", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( isGeologicSequestration, "isGeologicSequestration", out, tabs, true );
    XMLWriteElementCheckDefault( storageCost, "storageCost", out, tabs, util::getLargeNumber() );
    XMLWriteElementCheckDefault( gwp, "GWP", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( emAdjust, "emAdjust", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( maxCntrl, "maxCntrl", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( gdpcap0, "gdpcap0", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( tau, "tau", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( finalEmissCoef, "finalEmissCoef", out, tabs, -1.0 );
    XMLWriteElementCheckDefault( adjMaxCntrl, "adjMaxCntrl", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( multMaxCntrl, "multMaxCntrl", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( techDiff, "techDiff", out, tabs, 0.0 );

    // Write out the GHGMAC
    if( ghgMac.get() ){
        ghgMac->toInputXML( out, tabs );
    }
    toInputXMLDerived( out, tabs );
    // done writing xml for data members.

    XMLWriteClosingTag( getXMLName(), out, tabs );
}


//! Write out any inherited class specific datamembers. Since GHG is not an ABC, it must define this as a noop.
void Ghg::toInputXMLDerived( std::ostream& out, Tabs* tabs ) const {
}

//! Writes datamembers to debugging datastream in XML format.
void Ghg::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    XMLWriteOpeningTag( getXMLName(), out, tabs, name );

    // write xml for data members
    XMLWriteElement( unit, "unit", out, tabs );
    XMLWriteElement( rmfrac, "removefrac", out, tabs );
    XMLWriteElement( gwp, "GWP", out, tabs );
    XMLWriteElement( mEmissions[ period ], "emission", out, tabs );
    XMLWriteElement( isGeologicSequestration, "isGeologicSequestration", out, tabs );
    XMLWriteElement( storageCost, "storageCost", out, tabs );
    XMLWriteElement( sequestAmountGeologic, "sequestAmountGeologic", out, tabs );
    XMLWriteElement( sequestAmountNonEngy, "sequestAmountNonEngy", out, tabs );
    XMLWriteElement( emissCoef, "emisscoef", out, tabs );
    XMLWriteElement( inputEmissions, "inputEmissions", out, tabs );
    XMLWriteElement( mEmissionsByFuel[ period ], "emissFuel", out, tabs );
    XMLWriteElement( emissInd, "emissInd", out, tabs );
    XMLWriteElement( emAdjust, "emAdjust", out, tabs );
    XMLWriteElement( maxCntrl, "maxCntrl", out, tabs );
    XMLWriteElement( gdpcap0, "gdpcap0", out, tabs );
    XMLWriteElement( tau, "tau", out, tabs );
    XMLWriteElement( finalEmissCoef, "finalEmissCoef", out, tabs );
    XMLWriteElement( adjMaxCntrl, "adjMaxCntrl", out, tabs );
    XMLWriteElement( techDiff, "techDiff", out, tabs );

     // Write out the GHGMAC
    if( ghgMac.get() ){
        ghgMac->toDebugXML( period, out, tabs );
    }
    toDebugXMLDerived( period, out, tabs );
    // done writing xml for data members.

    XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Write out any inherited class specific datamembers. Since GHG is not an ABC, it must define this as a noop.
void Ghg::toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const {
}

/*! \brief Get the XML node name for output to XML.
*
* This protected function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& Ghg::getXMLName() const {
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
const std::string& Ghg::getXMLNameStatic() {
    return XML_NAME;
}

/*! \brief Copies parameters such as Tau, GDP0, and MAC curve that should only be specified once
* \detailed Certain parameters for GHG emissions should only be specified once so that they are
* consistent for all years (and also to simplify input). Given that GHG objects are embedded in 
* technology objects this means that these parameters need to be copied from object to object.
* This method copies any needed parameters from the previous year's GHG object.
* Also included in this function is code for the variable adjMaxCntrl.  The code for this varible
* needs to be run only once, with the values at the end of the period, so it is useful to have it here
* where those values are defined.  adjMaxCntrl has a default of 1, so if it is not input, maxCntrl will simply
* be multiplied by 1, and the function for adjusting gdpcap0 will simplify to gdpcap0 = gdpcap0, thus keeping it
* at the same value.  If adjMaxCntrl != 1, it will adjust gdpcap0 up or down so that the base year emissions
* remain unchanged.  adjMaxCntrl should be input once, in the base year. 
*
* \author Steve Smith and Nick Fernandez
* \param prevGHG pointer to previous period's GHG object
*/
void Ghg::copyGHGParameters( const Ghg* prevGHG ) {

    assert( prevGHG ); // Make sure valid pointer was passed
   
   // Copy values that always need to be the same for all periods.
    // Note that finalEmissCoef is not copied, since maxCntrl has already been set appropriately
    maxCntrl = prevGHG->maxCntrl;
    gdpcap0 = prevGHG->gdpcap0;
    tau = prevGHG->tau;

    unit = prevGHG->unit;
    
    adjMaxCntrl = prevGHG->adjMaxCntrl;

    // Adjust for maximum control level once GDP per capita is determined
    // This could better be put into a post-calculation processing function if we implimented that in general
    adjustMaxCntrl( prevGHG->gdpCap );
    

    // Copy values that could change, so only copy if these are still zero (and, thus, were never read-in)
   if ( !techDiff ) { 
        techDiff = prevGHG->techDiff; // only copy if techDiff has not changed
    }
 
   if ( !gwp ) { 
        gwp = prevGHG->gwp; // only copy if GWP has not changed
    }
    
    // Default value for emissCoef is zero, so only copy if a new value was read in
    if ( !emissCoef ) {
        emissCoef = prevGHG->emissCoef;
    }

    // If an emissions value was input last period, and none was input this period, then copy emissions coefficient
    // This overwrites anything that was read in
    if (  !valueWasInputAtSomePoint && prevGHG->valueWasInputAtSomePoint ) {
        emissCoef = prevGHG->emissCoef;
    }

    // If Mac curve was input then copy it, as long as one was not read in for this period
    if ( !ghgMac.get() && prevGHG->ghgMac.get() ) {
        ghgMac.reset( prevGHG->ghgMac->clone() );
    }
}

//! Perform initializations that only need to be done once per period
void Ghg::initCalc( ) {

    maxCntrl *= multMaxCntrl;
    // Make sure control percentage never goes above 100% so there are no negative emissions!
    maxCntrl = min( maxCntrl, 100.0 );
    
    // Set flag that an emission input value was set
    if( inputEmissions >= 0 ) {
        valueWasInputAtSomePoint = true;
    }

     // Perform any MAC initializations
    if( ghgMac.get() ){
        ghgMac->initCalc( name );
    }
}

/*! \brief Second Method: Convert GHG tax and any storage costs into energy units using GHG coefficients
*   and return the value or cost of the tax and storage for the GHG.
*   Apply taxes only if emissions occur.  Emissions occur if there is a difference in the emissions
*   coefficients.
*  \author Sonny Kim
*  \param regionName Name of the region for GHG
*  \param fuelName Name of the fuel
*  \param prodName The name of the output product.
*  \param efficiency The efficiency of the technology this ghg emitted by.
*  \param period The period in which this calculation is occurring. 
*  \return Generalized cost or value of the GHG
*/
double Ghg::getGHGValue( const string& regionName, const string& fuelName, const string& prodName, const double efficiency, const int period ) const {

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
    
    double GHGTax = marketplace->getPrice( name, regionName, period, false );
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
    
    // Product market may not exist if it is a demand sector.
    const IInfo* productInfo = marketplace->getMarketInfo( prodName, regionName, period, false );
    const double coefProduct = productInfo ? productInfo->getDouble( "CO2Coef", false ) : 0;

    if (name == "CO2") {
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
    }
    // for all other gases used read-in emissions coefficient
    else {
        // apply carbon equivalent to emiss coefficient
        // if remove fraction is greater than zero and storage is required
        if (rmfrac > 0) {
            // add geologic sequestration cost
            if (isGeologicSequestration) {
                generalizedCost = ((1.0 - rmfrac)*GHGTax*gwp + rmfrac*storageCost) * emissCoef / CVRT90;
            }
            // no storage cost added
            else {
                generalizedCost = ((1.0 - rmfrac)*GHGTax*gwp) * emissCoef / CVRT90;
            }
        }
        // no storage required
        else {
            generalizedCost = GHGTax * gwp * emissCoef / CVRT90;
        }
    }
    // The generalized cost returned by the GHG may be negative if
    // emissions crediting is occuring.
    return generalizedCost;
}

/*! \brief finds an appropriate value for maxCntrl and adjusts gdpcap0 as necessary
* The control function is needed in the calcEmission function and takes 4 parameters, maxCntrl, tau, gdpcap0, and gdpCap.
* tau and gdpcap0 are necessary inputs to the control function, maxCntrl can either be inputed directly, or
* can be computed in this function using the input "finalEmissCoef."
* if either tau or gdpcap0 are not input, or are 0, or if the emissions coefficient is 0, the function will set
* fControl to 0, and hence, there will be no emissions adjustment due to controls. In the case that both maxCntrl and
* finalEmissCoef are input (which does not make sense)  only finalEmissCoef will be used.
* The function additionally calls calcTechChange which reduces gdpcap0 over time to account for technological change/diffusion.
* \author Nick Fernandez & Steve Smith
* \param gdpCap - The gdp per capita for the current period 
* \param emissDrive The amount of fuel that emissions are proportional to
* \param period the current period where calculations are occurring
* \return adjustedGdpCap0
* \todo let initCalc know the period so that calcTechChange calculation can be moved there and will only have to be done once per period
*/
double Ghg::adjustControlParameters( const double gdpCap, const double emissDrive, const double macReduction, const int period ){
    double adjustedGdpCap0 = gdpcap0; //! gdpCap0 used by the control function- adjusted for techDiffusion

    if (techDiff !=0){
        adjustedGdpCap0 = gdpcap0 / calcTechChange(period);
    }
    if ( finalEmissCoef > 0 ){
        if ( inputEmissions >= 0 ){
            const double multiplier = emissDrive * ( 1 - emAdjust ) * ( 1 - macReduction );

            const double B = (1/controlFunction(100,tau,adjustedGdpCap0,gdpCap));
            if ( multiplier != 0){
                //cannot divide by zero- when no driver is present
                maxCntrl = 100 * (1 - (finalEmissCoef * ((B - 1)) / (((B * inputEmissions) / multiplier ) - finalEmissCoef)));
            }
            else{
                maxCntrl = 0;
            }
            // method for calculating an maxCntrl when using emissions coefficients that require maxCntrl in their
            // calculation.  The formula is derived by setting up equations where maxCntrl can be eliminated through 
            // substitution, the emissions coefficient can be solved for, and that expression can be substituted back in 
            // into the expression for maxCntrl. See formula for emission in calcEmission()
        }
        else{
            if (emissCoef != 0){
                // cannot divide by 0 

                maxCntrl = 100 * (1 - ( finalEmissCoef / (emissCoef)));

            }
            else {
                maxCntrl = 0;
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::WARNING );
                mainLog << " emissCoef = 0, control function set to 0"<< endl;

            }
        }
        // Control values should never be larger than 100%.
        maxCntrl = min( maxCntrl, 100.0 );
    }
    return adjustedGdpCap0;
}

/*! \brief Returns the value by which to adjust gdpcap0, based on technological diffusion
* The Variable TechCh represents the percent reduction in gdpcap0 per year, due to technological change and diffusion.
* The overall reduction in gdpcap0 is 1 + the techCh percentage raised to the power of the number of years after 
* the base year.  When applied to the control function, this will allow emissions controls to approach maxCntrl sooner.
*\ Author Nick Fernandez
* \param period the current period where calculations occur
* \returns amount to reduce the parameter gdpCap0 by
*/
double Ghg::calcTechChange( const int period ){
    const Modeltime* modeltime = scenario->getModeltime();
    int year = modeltime->getper_to_yr( period ); 
    year -=  modeltime->getper_to_yr(1); // subtracts off base year to find number of years after base year
    return pow(1 + (techDiff / 100), year );
}

/*! Second Method: Convert GHG tax and any storage costs into energy units using
*   GHG coefficients and return the value or cost of the tax and storage for the
*   GHG. Apply taxes only if emissions occur. Emissions occur if there is a
*   difference in the emissions coefficients.
*  \param aInput Input for which to calculate the carbon tax.
*  \param aRegionName The name of the current region.
*  \param aGoodName The name of the output product.
*  \param aPeriod The period in which this calculation is occurring. 
*  \return Generalized cost or value of the GHG
*  \todo Sequestration and collapsing two methods.
*/
double Ghg::getGHGValue( const Input* aInput, const string& aRegionName,
                         const string& aGoodName, const int aPeriod ) const
{
    // Determine if there is a tax.
    const Marketplace* marketplace = scenario->getMarketplace();
    double ghgTax = marketplace->getPrice( name, aRegionName, aPeriod, false );
    if( ghgTax == Marketplace::NO_MARKET_PRICE ){
        ghgTax = 0;
    }
    // Get the emissions coef for the input.
    double currInputGasCoef = aInput->getGHGCoefficient( name, aRegionName );
    
    // Get the conversion factor.
    double convFactor = aInput->getConversionFactor( aRegionName );
    
    // Return the rate.
    return ghgTax * gwp * currInputGasCoef * convFactor;
}

/*! \brief Calculate the input emissions for a good.
* \details Calculates the sum of all emissions contained in the inputs to the production of a good. This is calculated
* by looping over all the inputs and for each input, determining its carbon by multiplying its coefficient and its
* physical demand. This amount of carbon is then added to the total, which is returned by the function. This carbon
* may not all be emitted, as a portion may remain in the output good.
* \param aInputs Vector of inputs to determine the amount of carbon in.
* \param aRegionName Name of the region in which the emission is occurring.
* \param aPeriod Period in which the emission is occurring. 
*/
double Ghg::calcInputEmissions( const vector<Input*>& aInputs, const string& aRegionName, const int aPeriod ) const {
    double totalEmissions = 0;

    const Marketplace* marketplace = scenario->getMarketplace();
    // Loop over the inputs calculating the amount of carbon in each.
    for( vector<Input*>::const_iterator input = aInputs.begin(); input != aInputs.end(); ++input ){
        // Add on the physical amount of the input multplied by the amount of
        // emissions per unit of physical output.
        totalEmissions += (*input)->getDemandPhysical( aRegionName ) 
                             * (*input)->getGHGCoefficient( name, aRegionName );
        
    }
    return totalEmissions;
}

/*! \brief Calculates emissions of GHG's
* \detailed Emissions of these gases are equal to the emissions driver multiplied by the emissions coefficient (how much of the
* chemical forming the GHG is emitted per unit driver) multiplied by the control function (the extent to which regions
* are expected to put controls on end-of-pipe emissions- based on their pppGdp) multiplied by the result of the 
* Marginal Abatement curve, and finally by an external read-in emissions Adjustment factor(if any).  The function also
* sets the emissions coefficient if emissions are read in.  
* \author Nick Fernandez, Steve Smith
* \param regionName Name of the region for GHG
* \param fuelname The name of the fuel
* \param input The amount of fuel sent out
* \param prodname The name of the output product
* \param output The amount of fuel consumed
* \param period The period in which this calculation is occurring.
* \todo PRIORITY - separate out CO2 from non-CO2 GHGs since CO2 is much simpler.
* \todo Emissions calc will not work properly with vintaging (base-year emissions will not work, and some thought needs to be given to how emissions controls should work)
*/
void Ghg::calcEmission( const string& regionName, const string& fuelname, const double input, 
                        const string& prodname, const double output, const GDP* gdp, const int aPeriod )
{
    // for CO2 use default emissions coefficient by fuel
    // remove fraction only applicable for CO2
    if (name == "CO2") {
        const Marketplace* marketplace = scenario->getMarketplace();

        // Fuel market may not exist.
        const IInfo* fuelInfo = marketplace->getMarketInfo( fuelname, regionName, aPeriod, false );
        const double coefFuel = fuelInfo ? fuelInfo->getDouble( "CO2Coef", false ): 0;

        // Product market may not exist if the product is a demand sector.
        const IInfo* productInfo = marketplace->getMarketInfo( prodname, regionName, aPeriod, false );
        const double coefProduct = productInfo ? productInfo->getDouble( "CO2Coef", false ) : 0;

        // 100% efficiency and same coefficient, no emissions
        if (input==output && coefFuel == coefProduct ) {
            mEmissions[ aPeriod ] = 0;
            sequestAmountGeologic = 0;
            sequestAmountNonEngy = 0;
            mEmissionsByFuel[ aPeriod ] = (1.0-rmfrac)*input* coefFuel;
            // Note: The primary fuel emissions will not be correct if sequestered emissions occur down the line.
        }
        else {
            // sequestered emissions
            if (rmfrac > 0) {
                // geologic sequestration
                if(isGeologicSequestration) {
                    sequestAmountGeologic = rmfrac * ( (input * coefFuel ) - ( output * coefProduct ) );
                }
                // non-energy use of fuel, ie petrochemicals
                else {
                    sequestAmountNonEngy = rmfrac * ( (input * coefFuel ) - ( output * coefProduct ) );
                }
            }
            // Note that negative emissions can occur here since biomass has a coef of 0. 
            mEmissions[ aPeriod ] = ( 1.0 - rmfrac ) * ( ( input* coefFuel ) - ( output* coefProduct ) );
            mEmissionsByFuel[ aPeriod ] = ( 1.0 - rmfrac ) * input* coefFuel;
        }
    }
    // for all other gases used read-in emissions coefficient or base-year emissions
    else {
        double macReduction = 0;
        gdpCap = gdp->getPPPGDPperCap( aPeriod );

        const double emissDriver = emissionsDriver(input, output);
        if ( ghgMac.get() ){
            macReduction = ghgMac->findReduction(regionName, aPeriod);
        }

        double fControl = 0;    
        double adjustedGdpCap0 = adjustControlParameters( gdpCap, emissDriver, macReduction, aPeriod );
        if ( ( finalEmissCoef > 0 ) || ( maxCntrl > -999 ) ){
            fControl = controlFunction( maxCntrl, tau, adjustedGdpCap0, gdpCap );
        }

        if ( inputEmissions >= 0 ) {
            mEmissions[ aPeriod ] = inputEmissions;
            mEmissionsByFuel[ aPeriod ] = inputEmissions;
            if ( (emissDriver != 0) && (emAdjust != 1) && (fControl != 1) && (macReduction != 1)) {
                emissCoef = inputEmissions / (emissDriver * (1 - emAdjust) * (1 - fControl)* ( 1 - macReduction ) );
            } else {
                emissCoef = 0;
            }
        } else {
            mEmissions[ aPeriod ] = emissDriver * emissCoef * ( 1 - emAdjust )* ( 1 - fControl ) * ( 1 - macReduction ) ;
            mEmissionsByFuel[ aPeriod ] =  mEmissions[ aPeriod ];
        }
    }

    // set emissions as demand side of gas market
    Marketplace* marketplace = scenario->getMarketplace();
    // Optimize special case of no-emission ghg.
    if( mEmissions[ aPeriod ] != 0 ){
        marketplace->addToDemand( name, regionName, mEmissions[ aPeriod ], aPeriod, false );
    }
    if( sequestAmountGeologic != 0 ){
        // set sequestered amount as demand side of carbon storage market
        marketplace->addToDemand( "carbon storage", regionName, sequestAmountGeologic, aPeriod, false );
    }
}

/*! \brief Calculate Ghg emissions.
* \details Performs an activity based calculation of the emissions produced by the technology. The calculation
* is performed by summing the total carbon contained in the inputs to the good and the subtracting the carbon
* contained in the physical output. The carbon contained in the output is not removed for primary fuel sectors,
* as their inputs do not account for the carbon extracted in the fuel from the ground. This function also stores
* the emissions of the primary fuel sectors separately so they can be reported later for emissions by fuel.
* The emission is then converted to a global-warming-potential based emission and added to the constraint market.
* \todo Sequestration
* \author Josh Lurz
* \param aInputs Vector of inputs to the technology.
* \param aRegionName Name of the region where the emission will occur.
* \param aGoodName Name of the sector creating the emission.
* \param aOutput Physical quantity of output.
* \param aPeriod Period in which the emissions is occurring.
* \note aOutput is in physical units, not currency units.
*/
void Ghg::calcEmission( const vector<Input*> aInputs, const string& aRegionName, const string& aGoodName,
                        const double aOutput, const int aPeriod )
{
    // Calculate the aggregate emissions of all inputs.
    double tempEmission = calcInputEmissions( aInputs, aRegionName, aPeriod );
    
    // Determine the output coefficient. The output coefficent will not exist for consumers.
    const static string COEF_STRING = "coefficient";
    Marketplace* marketplace = scenario->getMarketplace();
    const IInfo* outputMarketInfo = marketplace->getMarketInfo( aGoodName, aRegionName, 0, false );
    const double outputCoef = outputMarketInfo ? outputMarketInfo->getDouble( name + COEF_STRING, false ) : 0;
    
    // calculate the output emissions.
    const double outputEmissions = aOutput * outputCoef;

    // If the good is a primary fuel, don't subtract output emissions as this is
    // extraction of the resource, not sequestration, and store the output
    // emissions as emissions by primary fuel.
    if( Input::isInputPrimaryEnergyGood( aGoodName, aRegionName ) ){
        mEmissionsByFuel[ aPeriod ] = outputEmissions;
    }
    else {
        // Remove emissions contained in the output from the total technology emissions.
        tempEmission -= outputEmissions;
    }

    // Calculate emissions for the constraint market based on the global warming potential of the gas.
    // CO2 is 1.
    double emissGwp = gwp * tempEmission;

    // Store the total emissions.
    mEmissions[ aPeriod ] = tempEmission;

    // TODO: Need to do sequestered emissions here.
    // Add to the constraint market. 
    marketplace->addToDemand( name, aRegionName, emissGwp, aPeriod, false );
}

//! calculates emissions associated with the use of secondary energy
/*! get indirect emissions coefficient from map object */
// I've got a better way to do this.
void Ghg::calcIndirectEmission( const double input, const string& fuelname, const vector<Emcoef_ind>& emcoef_ind ) {
    emissInd = 0; // to initialize
    for (int i=0;i< static_cast<int>( emcoef_ind.size() );i++) {
        if (emcoef_ind[i].getName() == fuelname) { // sector name
            emissInd = emcoef_ind[i].getemcoef(name) * input;
        }
    }
}

//! Return name of Ghg.
const string& Ghg::getName() const {
    return name;
}

//! Return unit for Ghg.
const string& Ghg::getUnit() const {
    return unit;
}

//! Return Ghg emissions.
double Ghg::getEmission( const int aPeriod ) const {
    assert( aPeriod < static_cast<int>( mEmissions.size() ) );
    return mEmissions[ aPeriod ];
}

//! Return geologic sequestered ghg emissions.
double Ghg::getSequestAmountGeologic() const {
    return sequestAmountGeologic;
}

//! Return non-energy sequestered ghg emissions.
double Ghg::getSequestAmountNonEngy() const {
    return sequestAmountNonEngy;
}

//! Return ghg emissions inplicit in fuel.
double Ghg::getEmissFuel( const int aPeriod ) const {
    return mEmissionsByFuel[ aPeriod ];
}

//! Return indirect ghg emissions.
double Ghg::getEmissInd() const {
    return emissInd;
}

//! Return ghg emissions coefficient.
double Ghg::getEmissCoef() const {
    return emissCoef;
}

/*! \brief Returns the control level for this gas in the current period
* \detailed The control function is a logistic exponential function.  It approaches 0 for values of gdpCap much less
* than gdpcap0, and approaches maxCntrl for values of gdpCap much greater than gdpcap0. CLOGIT is a constant equal
* to 2 times the natural log of 9, such that fControl is equal to 1/2 maxCntrl at gdpcap0.  
* the function returns the value of 0 in the case that either gdpcap0 or tau are not input, or are equal to 0.
* \author Nick Fernandez
* \param maxCntrlIn maximum emissions reduction fraction due to controls
* \param tauIn the range over which the control percentage goes from 10% maxCntrl to 90% maxCntrl
* \param gdpcap0In the midpoint gdpCap value of the control curve
* \param gdpCapIn the gdp per capita in PPP terms for the current period
*/
double Ghg::controlFunction( const double maxCntrlIn, const double tauIn, const double gdpcap0In, const double gdpCapIn ){
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    if( tauIn != 0 && gdpcap0In != 0 ){
        const double CLOGIT = 4.394; // See above for documentation.
        return (maxCntrlIn/100) / (1 + exp( -CLOGIT * (gdpCapIn - gdpcap0In) / tauIn ));
    }
    else {
        if ( tauIn == 0 ){
            mainLog.setLevel( ILogger::WARNING );
            mainLog << " control function requires an input of tau." << endl;
        }
        if ( gdpcap0In == 0 ){
            mainLog.setLevel( ILogger::WARNING );
            mainLog << " control function requires an input of gdpcap0." << endl;
        }
        return 0;
    }
}

//! returns the emissions Driver value. emissions are proportional to input minus output.
double Ghg::emissionsDriver( const double inputIn, const double outputIn ) const {
    return inputIn - outputIn;
}

/*! \brief Get the carbon tax paid for the ghg.
* \details Calculate and return the total amount of carbon tax paid, or credit received
* for a single greenhouse gas. The tax or credit is calculated as emissions multiplied by
* the tax from the marketplace for the gas and the global warming potential.
* \warning This function calculates this value dynamically which requires a call to the marketplace,
* so it is slow. It should be avoided except for reporting purposes.
* \param aRegionName The name of the region containing the ghg.
* \param aPeriod The name of the period for which to calculate carbon tax paid.
* \author Josh Lurz
* \return The total carbon tax paid.
*/
double Ghg::getCarbonTaxPaid( const string& aRegionName, const int aPeriod ) const {
    const Marketplace* marketplace = scenario->getMarketplace();
    double GHGTax = marketplace->getPrice( name, aRegionName, aPeriod, false );
    if( GHGTax == Marketplace::NO_MARKET_PRICE ){
        GHGTax = 0;
    }
    // The carbon tax paid is the amount of the emission multiplied by the tax and the global
    // warming emission. This may be a negative in the case of a credit.
    return GHGTax * mEmissions[ aPeriod ] * gwp;
}

/*! \brief adjusts maxCntrl (and then gdpcap0 to recalibrate emissions) based on the read in multiplier adjMaxCntrl
*\ detailed adjMaxCntrl is a read in variable that represents a multiplier to maxCntrl.
* This is used to adjust the maximum emissions control level while leaving current emissions constant.
* the function multiplies maxCntrl by this value, and chacks to make sure maxCntrl has not been
* given a multiplier that makes it greater that 100.  If this happens, it sets maxCntrl to 100
* and resets adjMaxCntrl to the value necessary to make maxCntrl 100.
* It then solves the control function for gdpcap0, keeping the base year emissions the same,
* so that changing maxCntrl does not mess with the base year calibrations.
* Note also that adjustMaxCntrl is run only once, in the base year when and if adjMaxCntrl != 1
* \author Nick Fernandez
* \param GDPcap the previous periods GDP per capita in PPP terms for this region
*/
void Ghg::adjustMaxCntrl( const double GDPcap ){
    if ( tau != 0 && gdpcap0 != 0 && adjMaxCntrl != 1 ) {
        // Note that maxCntrl is in percentage units
        maxCntrl *= adjMaxCntrl;
        if ( maxCntrl > 100 ) {
            adjMaxCntrl *= ( 100 / maxCntrl );
            maxCntrl = 100;
        }

        const double CLOGIT = 4.394;
        double factor1 =  1 + exp( -CLOGIT * ( GDPcap - gdpcap0 ) / tau );
        
        if ( adjMaxCntrl != 1 ){
            gdpcap0 = ( tau / CLOGIT ) * log( adjMaxCntrl * factor1 - 1 ) + GDPcap;
        }
        // After finished adjustments, adjMaxCntrl should be set to one so is not used anymore
        adjMaxCntrl = 1;
    }
}

/*! \brief Update a visitor with information from a GHG for a given period.
* \param aVisitor The visitor to update.
* \param aPeriod The period for which to update.
*/
void Ghg::accept( IVisitor* aVisitor, const int aPeriod ) const {
	aVisitor->startVisitGHG( this, aPeriod );
	aVisitor->endVisitGHG( this, aPeriod );
}
