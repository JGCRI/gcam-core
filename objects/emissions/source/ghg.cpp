/*! 
* \file ghg.cpp
* \ingroup CIAM
* \brief Ghg class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
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
#include "containers/include/world.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/gdp.h"
#include "emissions/include/ghg_mac.h"

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
    emission = 0;
    isGeologicSequestration = true;
    storageCost = util::getLargeNumber(); // default to a large cost to turn off CCS
    sequestAmountGeologic = 0;
    sequestAmountNonEngy = 0;
    emissGwp = 0;
    emissFuel = 0;
    emissInd = 0;
    emissCoefPrev = 0;
    inputEmissions = 0;
    emissionsWereInput = false;
    valueWasInput = false;
    fMaxWasInput = false;
    finalEmissCoefWasInput = false;
    emAdjust = 0;
    fMax = 1;
    fControl = 0;
    techCh = 0;
    mac = 0;
    gdp0 = 0;
    finalEmissCoef = 0;
    tau = 0;
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
    emission = other.emission;
    isGeologicSequestration = other.isGeologicSequestration;
    storageCost = other.storageCost;
    sequestAmountGeologic = other.sequestAmountGeologic;
    sequestAmountNonEngy = other.sequestAmountNonEngy;
    emissGwp = other.emissGwp;
    emissFuel = other.emissFuel;
    emissInd = other.emissInd;
    emissCoefPrev = other.emissCoefPrev;
    inputEmissions = other.inputEmissions;
    emissionsWereInput = other.emissionsWereInput;
    valueWasInput = other.valueWasInput;
    fMaxWasInput = other.fMaxWasInput;
    finalEmissCoefWasInput = other.finalEmissCoefWasInput;
    emAdjust = other.emAdjust;
    fMax = other.fMax;
    fControl = other.fControl;
    techCh = other.techCh;
    mac = other.mac;
    gdp0 = other.gdp0;
    finalEmissCoef = other.finalEmissCoef;
    tau = other.tau;
    // Perform a deep copy on the GhgMac.
    ghgMac.reset( other.ghgMac->clone() );
}

//! Clone function which returns a deep copy of the Ghg.
Ghg* Ghg::clone() const {
    return new Ghg( *this );
}

//! initialize Ghg object with xml data
void Ghg::XMLParse(const DOMNode* node) {	
    /*! \pre Assume we are passed a valid node. */
    assert( node );

    // get the name attribute.
    // name of the GHG
    name = XMLHelper<string>::getAttrString( node, "name" );

    DOMNodeList* nodeList = node->getChildNodes();
    for( int i = 0; i < static_cast<int>( nodeList->getLength() ); i++ ) {
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
            emissionsWereInput = true;
            valueWasInput = true;
        }
        else if( nodeName == "emAdjust" ){
            emAdjust = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "fMax" ){
            fMax = XMLHelper<double>::getValue( curr );
            fMaxWasInput = true;
        }
        else if( nodeName == "gdp0" ){
            gdp0 = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "tau" ){
            tau = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "finalEmissCoef" ){
            finalEmissCoef = XMLHelper<double>::getValue( curr );
            finalEmissCoefWasInput = true;
        }
        else if( nodeName == "techCh"){
            techCh = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "emisscoef" ){
            emissCoef = XMLHelper<double>::getValue( curr );
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
            if( !ghgMac.get() ){
                ghgMac.reset( new GhgMAC() );
            }
            ghgMac->XMLParse( curr );
        }
        else if( XMLDerivedClassParse( nodeName, curr ) ){
        }
        else {
            cout << "Unrecognized text string: " << nodeName << " found while parsing GHG." << endl;
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
    XMLWriteElement( unit, "unit", out, tabs );
    if( emissionsWereInput ) {
        XMLWriteElement( inputEmissions, "inputEmissions", out, tabs );
    } else {
        XMLWriteElementCheckDefault( emissCoef, "emisscoef", out, tabs, 0.0 );
    }
    XMLWriteElementCheckDefault( rmfrac, "removefrac", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( isGeologicSequestration, "isGeologicSequestration", out, tabs, true );
    XMLWriteElementCheckDefault( storageCost, "storageCost", out, tabs, util::getLargeNumber() );
    XMLWriteElementCheckDefault( gwp, "GWP", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( emAdjust, "emAdjust", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( fMax, "fMax", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( gdp0, "gdp0", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( tau, "tau", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( finalEmissCoef, "finalEmissCoef", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( techCh, "techCh", out, tabs, 0.0 );
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
    XMLWriteElement( emission, "emission", out, tabs );
    XMLWriteElement( isGeologicSequestration, "isGeologicSequestration", out, tabs );
    XMLWriteElement( storageCost, "storageCost", out, tabs );
    XMLWriteElement( sequestAmountGeologic, "sequestAmountGeologic", out, tabs );
    XMLWriteElement( sequestAmountNonEngy, "sequestAmountNonEngy", out, tabs );
    XMLWriteElement( emissGwp, "emissGwp", out, tabs );
    XMLWriteElement( emissCoef, "emisscoef", out, tabs );
    XMLWriteElement( emissFuel, "emissFuel", out, tabs );
    XMLWriteElement( emissInd, "emissInd", out, tabs );
    XMLWriteElement( emAdjust, "emAdjust", out, tabs );
    XMLWriteElement( fMax, "fMax", out, tabs );
    XMLWriteElement( gdp0, "gdp0", out, tabs );
    XMLWriteElement( tau, "tau", out, tabs );
    XMLWriteElement( finalEmissCoef, "finalEmissCoef", out, tabs );
    XMLWriteElement( techCh, "techCh", out, tabs );
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
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
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
*
* \author Steve Smith
* \param prevGHG pointer to previous period's GHG object
*/
void Ghg::copyGHGParameters( const Ghg* prevGHG ) {

   assert( prevGHG ); // Make sure valid pointer was passed
   
   // Copy values that always need to be the same for all periods.
	// Note that finalEmissCoef is not copied, since fmax has already been set appropriately
	fMax = prevGHG->fMax;
	gdp0 = prevGHG->gdp0;
	tau = prevGHG->tau;
   unit = prevGHG->unit;

	// Copy values that could change, so only copy if these are still zero (and, thus, were never read-in)
   if ( !techCh ) { 
		techCh = prevGHG->techCh; // only copy if GWP has not changed
	}
   if ( !gwp ) { 
		gwp = prevGHG->gwp; // only copy if GWP has not changed
	}
   if ( !emissCoef ) {
		emissCoef = prevGHG->emissCoef; // only copy if emissCoef has not changed
	}

	// If an emissions value was input last period, and none was input this period, then copy emissions coefficient
	if (  !valueWasInput && prevGHG->valueWasInput ) {
		emissCoef = prevGHG->emissCoef;
		setEmissionsInputStatus();	// Set valueWasInput to true so that the next GHG object will get this coefficient passed on to it
	}

	// If Mac curve was input then copy it, as long as one was not read in for this period
	if ( !ghgMac.get() && prevGHG->ghgMac.get() ) {
		ghgMac.reset( prevGHG->ghgMac->clone() );
	}
}


/*! Second Method: Convert GHG tax and any storage costs into energy units using GHG coefficients
*   and return the value or cost of the tax and storage for the GHG.
*   Apply taxes only if emissions occur.  Emissions occur if there is a difference in the emissions
*   coefficients.
*  \author Sonny Kim
*  \param regionName Name of the region for GHG
*  \param fuelName Name of the fuel
*  \param prodName The name of the output product.
*  \param efficiency The efficience of the technology this ghg emitted by.
*  \param period The period in which this calculation is occurring. 
*  \return Generalized cost or value of the GHG
*/
double Ghg::getGHGValue( const string& regionName, const string& fuelName, const string& prodName, const double efficiency, const int period ) const {

    const World* world = scenario->getWorld();
    const Marketplace* marketplace = scenario->getMarketplace();
    
    // Check if there is a tax. If not, return.
    // Note: This function does not work correctly if the CarbonTax is assumed to be zero
    // and this early return is removed. May indicate a problem.
    if ( !marketplace->doesMarketExist( name, regionName, period ) ){
        return 0;
    }
    
    // Constants
    const double CVRT90 = 2.212; // 1975 $ to 1990 $
    const double SMALL_NUM = util::getSmallNumber();
    const double CVRT_tg_MT = 1e-3; // to get teragrams of carbon per EJ to metric tons of carbon per GJ
    
    double GHGTax = marketplace->getPrice(name,regionName,period);
    // get carbon storage cost from the market
    double marketStorageCost = 0;
    if ( marketplace->doesMarketExist( storageName, regionName, period ) ) {
        // market exists, use market storage cost
        marketStorageCost = marketplace->getPrice( storageName, regionName, period );
    }
    else {
        // market does not exist, use default or read in storage cost
        marketStorageCost = storageCost;
    }

    // if tax is 0 or small, turn off sequestration technology by increasing storage cost
    if (GHGTax < SMALL_NUM) {
        marketStorageCost = util::getLargeNumber();
    }

    // units for generalized cost is in 75$/gj
    double generalizedCost = 0; 
    const double coefFuel = world->getPrimaryFuelCO2Coef( regionName, fuelName );
    const double coefProduct = world->getPrimaryFuelCO2Coef( regionName, prodName );

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
        //******* override generalizedCost if coefFuel is 0 *******
        // need to fix this
        if (coefFuel < SMALL_NUM) {
            generalizedCost = 0;
        }
    }
    // for all other gases used read-in emissions coefficient
    else {
        // apply carbon equivalent to emiss coefficienr
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
    // for debugging
    if (generalizedCost < 0) {
        cout<<"generalized cost " << generalizedCost << endl;
        cout<<"GHGTax "<<GHGTax<<"  coefFuel  "<<coefFuel<<"  coefProduct"<<coefProduct<<endl;
        exit(-1);
    }
    return generalizedCost;
}

// finds an appropriate value for fMax, adjusts gdp0 and sets fControl
/* The control function is needed in the calcEmission function and takes 4 parameters, fMax, tau, gdp0, and gdpCap.
* tau and gdp0 are necessary inputs to the control function, fMax can either be inputed directly, or
* can be computed in this function using the input "finalEmissCoef."
* if either tau or gdp0 are not input, or are 0, or if the emissions coefficient is 0, the function will set
* fControl to 0, and hence, there will be no emissions adjustment due to controls. In the case that both fMax and
* finalEmissCoef are input (which does not make sense)  only finalEmissCoef will be used.
* The function additionally calls calcTechChange which reduces gdp0 over time to account for technological change/diffusion.
* \author Nick Fernandez & Steve Smith
* param gdpCap - The gdp per capita for the current period 
* param emissDrive The amount fo fuel that emissions are prortional to
* param period the current period where calculations are occuring
*/
void Ghg::findControlFunction( const double gdpCap, const double emissDrive, const int period ){
    double gdp0Adj = gdp0;
    if (techCh !=0){
        gdp0Adj = calcTechChange(period);
    }
    if ( finalEmissCoefWasInput ){
        if ( emissionsWereInput ){
            const double multiplier = emissDrive * (1 - emAdjust) * (1 - mac);
            const double B = (1/controlFunction(1,tau,gdp0Adj,gdpCap));
            fMax = 100 * (1 - (finalEmissCoef * ((B - 1)) / (((B * inputEmissions) / multiplier ) - finalEmissCoef)));
            // method for calculating an fMax when using emissions coefficients that require fMax in their
            // calculation.  The formula is derived by setting up equations where fMax can be eliminated through 
            // substitution, the emissions coeffiecient can be solved for, and that expression can be substituted back in 
            // into the expression for fMax. See formula for emission in calcEmission()
        }
        else{
            if (emissCoef != 0){
                // cannot divide by 0 
                fMax = 100 * (1 - ( finalEmissCoef / (emissCoef)));
            }
            else {
                fMax = 0;
                cout << "Warning: emissCoef = 0, control function set to 0"<< endl;
            }
        }
        fControl = controlFunction(fMax,tau,gdp0Adj,gdpCap);
    }
    else {
        if ( fMaxWasInput ){
            fControl = controlFunction( fMax, tau, gdp0Adj, gdpCap );
        }
    }
}

// Adjusts the value of gdp0, based on technological change, returns that adjusted value.
/* The Variable TechCh represents the percent reduction in gdp0 per year, due to technological change and diffusion.
* The overall reduciton in gdp0 is 1 + the techCh percentage raised to the power of the number of years after 
* the base year.  When applied to the control funciton, this will allow emissions controls to approach fMax sooner.
*\ Author Nick Fernandez
* param period the current period where calculations occur
*/
double Ghg::calcTechChange( const int period ){
    const Modeltime* modeltime = scenario->getModeltime();
    double gdpAdj; // adjusted gdp0 based on technological change;
    int year = modeltime->getper_to_yr( period ); 
    year -=  modeltime->getper_to_yr(1); // subtracts off base year to find number of years after base year
    gdpAdj = gdp0 / pow(1 + (techCh / 100), year );
    return gdpAdj;
}

/*! \brief Calculates emissions of GHG's that use input-output as the emissions Driver
* \detailed Emissions of these gases are equal to the emissions driver multiplied by the emissions coefficient (how much of the
* chemical forming the GHG is present in the fuel) multiplied by the control function (the extent to which regions
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
* \todo seperate out CO2.
*/
void Ghg::calcEmission( const string& regionName, const string& fuelname, const double input, const string& prodname, const double output, const GDP* gdp, const int period ) {

    const World* world = scenario->getWorld();

    // for CO2 use default emissions coefficient by fuel
    // remove fraction only applicable for CO2
    if (name == "CO2") {
        const double coefFuel = world->getPrimaryFuelCO2Coef( regionName, fuelname );
        const double coefProduct = world->getPrimaryFuelCO2Coef( regionName, prodname );

        // 100% efficiency and same coefficient, no emissions
        if (input==output && coefFuel == coefProduct ) {
            emission = 0;
            emissGwp = 0;
            sequestAmountGeologic = 0;
            sequestAmountNonEngy = 0;
            emissFuel = (1.0-rmfrac)*input* coefFuel;
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
            emission = ( 1.0 - rmfrac ) * ( ( input* coefFuel ) - ( output* coefProduct ) );
            emissGwp = ( 1.0 - rmfrac ) * gwp * ( ( input * coefFuel ) - ( output * coefProduct ) );
            emissFuel = ( 1.0 - rmfrac ) * input* coefFuel;
        }
    }
    // for all other gases used read-in emissions coefficient
    else {
        const double gdpCap = gdp->getPPPGDPperCap( period );
        const double emissDriver = emissionsDriver(input, output);
        if ( ghgMac.get() ){
            mac = ghgMac->findReduction(regionName, period);
        }
        findControlFunction(gdpCap, emissDriver, period);
        if ( emissionsWereInput ) {
            emission = inputEmissions;
            emissFuel = inputEmissions;
            if ( emissDriver != 0 ) {
                emissCoef = inputEmissions / (emissDriver * (1 - emAdjust) * (1 - fControl)* ( 1 - mac ) );
            } else {
                emissCoef = 0;
            }
        } else {
            emission = emissDriver * emissCoef * ( 1 - emAdjust )* ( 1 - fControl ) * ( 1 - mac ) ;
            emissFuel =  emission;
        }
        emissGwp = gwp * emission;
    }
}

//! calculates emissions associated with the use of secondary energy
/*! get indirect emissions coefficient from map object */
void Ghg::calcIndirectEmission( const double input, const string& fuelname, const vector<Emcoef_ind>& emcoef_ind ) {
    emissInd = 0; // to initialize
    for (int i=0;i< static_cast<int>( emcoef_ind.size() );i++) {
        if (emcoef_ind[i].getName() == fuelname) { // sector name
            emissInd = emcoef_ind[i].getemcoef(name) * input;
        }
    }
}

//! Return name of Ghg.
string Ghg::getName() const {
    return name;
}

//! Return unit for Ghg.
string Ghg::getUnit() const {
    return unit;
}

//! Return Ghg emissions.
double Ghg::getEmission() const {
    return emission;
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
double Ghg::getEmissFuel() const {
    return emissFuel;
}

//! Return indirect ghg emissions.
double Ghg::getEmissInd() const {
    return emissInd;
}

//! Return ghg emissions coefficient.
double Ghg::getEmissCoef() const {
    return emissCoef;
}

//! Return ghg emissions coefficient.
void Ghg::setEmissCoef( const double emissCoefIn ) {
    emissCoef = emissCoefIn;
}

//! Return flag that indicates if emissions were input for this technology 
bool Ghg::getEmissionsInputStatus() const {
    return valueWasInput;
}
//! Set the flag that indicates that emissions were input for this technology 
void Ghg::setEmissionsInputStatus() {
    valueWasInput = true;
}

/*! \brief performs the calculations that solve the control function in the current period
* \detailed The control function is an inverse exponential funciton.  It approaches 0 for values of gdpCap much less
* than gdp0, and approaches fMax for values of gdpCap much greater than gdp0. CLOGIT is a constant equal
* to 2 times the natural log of 9, such that fControl is equal to 1/2 fMax at gdp0.  
* the function returns the value of 0 in the case that either gdp0 or tau are not input, or are equal to 0.
* \author Nick Fernandez
* \pre mac member variable was set from the MAC curve. 
* \param fMaxIn maximum emissions reduction fraction due to controls
* \param tauIn the range over which the control percentage goes from 10% fMax to 90% fMax
* \param gdp0In the midpoint gdpCap value of the control curve
* \param gdpCapIn the gdp per capita in PPP erms for the current period
*/
double Ghg::controlFunction( const double fMaxIn, const double tauIn, const double gdp0In, const double gdpCapIn ){
    if( tauIn != 0 && gdp0In != 0 ){
        const double CLOGIT = 4.394; // See above for documentation.
        return (fMaxIn/100) / (1 + exp( -CLOGIT * (gdpCapIn - gdp0In) / tauIn ));
    }
    else {
        if ( tauIn == 0 ){
            cout << "Warning: control function requires an input of tau." << endl;
        }
        if ( gdp0In == 0 ){
            cout << "Warning: control function requires an input of gdp0." << endl;
        }
        return 0;
    }
}

//! returns the emissions Driver value. This needs to be more commented.
double Ghg::emissionsDriver( const double inputIn, const double outputIn ) const {
    return inputIn - outputIn;
}


