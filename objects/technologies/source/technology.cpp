/*! 
* \file technology.cpp
* \ingroup Objects
* \brief technology class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/
// Standard Library headers
#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

// User headers
#include "technologies/include/technology.h"
#include "emissions/include/ghg.h"
#include "emissions/include/ghg_output.h"
#include "emissions/include/so2_emissions.h"
#include "emissions/include/ghg_input.h"
#include "containers/include/scenario.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "emissions/include/indirect_emiss_coef.h"
#include "containers/include/gdp.h"
#include "util/base/include/configuration.h"
#include "util/logger/include/ilogger.h"
#include "marketplace/include/market_info.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string technology::XML_NAME1D = "technology";
const string technology::XML_NAME2D = "period";
const double LOGIT_EXP_DEFAULT = -6;

// Technology class method definition

//! Default constructor.
technology::technology() {
    initElementalMembers();
}

//! Destructor
technology::~technology() {
    clear();
}

//! Copy constructor
technology::technology( const technology& techIn ) {
    copy( techIn );
}

//! Assignment operator.
technology& technology::operator = ( const technology& techIn ) {
    if( this != &techIn ) { // check for self assignment 
        clear();
        copy( techIn );
    }
    return *this;
}

//! Helper copy function to avoid replicating code.
void technology::copy( const technology& techIn ) {
    year = techIn.year;
    shrwts = techIn.shrwts;
    eff = techIn.eff; 
    effBase = techIn.effBase; 
    effPenalty = techIn.effPenalty; 
    intensity = techIn.intensity; 
    necost = techIn.necost;
    neCostBase = techIn.neCostBase;
    neCostPenalty = techIn.neCostPenalty;
    fuelcost = techIn.fuelcost;
    techcost = techIn.techcost;
    tax = techIn.tax;
    fMultiplier = techIn.fMultiplier;
    pMultiplier = techIn.pMultiplier;
    lexp = techIn.lexp;
    share = techIn.share;
    input = techIn.input;
    output = techIn.output;
    techchange = techIn.techchange;
    fixedOutput = techIn.fixedOutput;
    fixedOutputVal = techIn.fixedOutputVal;
    name = techIn.name;
    unit = techIn.unit;
    fuelname = techIn.fuelname;
    doCalibration = techIn.doCalibration;
    calInputValue = techIn.calInputValue;
    doCalOutput = techIn.doCalOutput;
    calOutputValue = techIn.calOutputValue;
    emissmap = techIn.emissmap; 
    emfuelmap = techIn.emfuelmap; 
    emindmap = techIn.emindmap; 
    totalGHGCost = techIn.totalGHGCost;
    fuelPrefElasticity = techIn.fuelPrefElasticity;
    resource = techIn.resource;
    A = techIn.A;
    B = techIn.B;
    ghgNameMap = techIn.ghgNameMap; 
    
    for (vector<Ghg*>::const_iterator iter = techIn.ghg.begin(); iter != techIn.ghg.end(); iter++) {
        ghg.push_back( (*iter)->clone() );
    }
}

//! Clone Function. Returns a deep copy of the current technology.
technology* technology::clone() const {
    return new technology( *this );
}

//! Clear member variables.
void technology::clear(){
    // Delete the GHGs to avoid a memory leak.
    for( vector<Ghg*>::iterator iter = ghg.begin(); iter != ghg.end(); iter++ ) {
        delete *iter;
    }
}

//! Initialize elemental data members.
void technology::initElementalMembers(){
    year = 0;
    shrwts = 1;
    eff = 1; 
    effBase = 1; 
    effPenalty = 0; 
    intensity = 1; 
    fuelcost = 0;
    necost = 0;
    neCostBase = 0;
    neCostPenalty = 0;
    techcost = 0;
    tax = 0;
    fMultiplier = 1;
    pMultiplier = 1;
    lexp = LOGIT_EXP_DEFAULT; 
    share = 0;
    input = 0;
    output = 0;
    techchange = 0;
    A = 0;
    B = 0;
    resource = 0;
    fixedOutput = 0; // initialize to no fixed supply
    fixedOutputVal = 0;
    doCalibration = false;
    doCalOutput = false;
    calInputValue = 0;
    calOutputValue = 0;
    totalGHGCost = 0;
    fuelPrefElasticity = 0;
}

/*! \brief initialize technology with xml data
*
* \author Josh Lurz
* \param node current node
* \todo Add Warning when addin files add new containers (regions, sectors, technologies, etc.)
*/
void technology::XMLParse( const DOMNode* node ) {	
    /*! \pre Assume we are passed a valid node. */
    assert( node );
    
    DOMNodeList* nodeList = node->getChildNodes();
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ) {
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );		
        
        if( nodeName == "#text" ) {
            continue;
        }
        else if( nodeName == "name" ) {
            name = XMLHelper<string>::getValueString( curr );
        } 
        else if( nodeName == "year" ){
            year = XMLHelper<int>::getValue( curr );
        }
        else if( nodeName == "fuelname" ){
            fuelname = XMLHelper<string>::getValueString( curr );
        }
        else if( nodeName == "sharewt" ){
            shrwts = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "fuelprefElasticity" ){
            fuelPrefElasticity = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "calInputValue" ){
            calInputValue = XMLHelper<double>::getValue( curr );
            doCalibration = true;
        }
        else if( nodeName == "calOutputValue" ){
            calOutputValue = XMLHelper<double>::getValue( curr );
            doCalibration = true;
            doCalOutput = true;
        }
        else if( nodeName == "efficiency" ){
            effBase = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "efficiencyPenalty" ){
            effPenalty = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "nonenergycost" ){
            neCostBase = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "neCostPenalty" ){
            neCostPenalty = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "tax" ){
            tax = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "pMultiplier" ){
            pMultiplier = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "fMultiplier" ){
            fMultiplier = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "logitexp" ){
            lexp = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "fixedOutput" ){
            fixedOutput = XMLHelper<double>::getValue( curr );
            // need to initialize this here (needs to be available for setting simul markets before init() is called)
            fixedOutputVal = fixedOutput; 
        }
        else if( nodeName == "techchange" ){
            techchange = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "resource" ){
            resource = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "A" ){
            A = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "B" ){
            B = XMLHelper<double>::getValue( curr );
        }
		else if( nodeName == Ghg::getXMLNameStatic() ){
            parseContainerNode( curr, ghg, ghgNameMap, new Ghg() );
        }
        else if( nodeName == GhgOutput::getXMLNameStatic() ){
            parseContainerNode( curr, ghg, ghgNameMap, new GhgOutput() );
        }
        else if( nodeName == GhgInput::getXMLNameStatic() ){
            parseContainerNode( curr, ghg, ghgNameMap, new GhgInput() );
        }
        else if( nodeName == SO2Emissions::getXMLNameStatic() ){
            parseContainerNode( curr, ghg, ghgNameMap, new SO2Emissions() );
        }
        else if( nodeName == "note" ){
            note = XMLHelper<string>::getValue( curr );
        }
        // parse derived classes
        else if( XMLDerivedClassParse( nodeName, curr ) ){
        } 
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing " << getXMLName1D() << "." << endl;
        }

    }
}

//! Parses any input variables specific to derived classes
bool technology::XMLDerivedClassParse( const string nodeName, const DOMNode* curr ){
    // do nothing. Defining method here even though it does nothing so that we do not
    // create an abstract class.
    return false;
}

/*! \brief Complete the initialization
*
* This routine is only called once per model run
*
* \author Josh Lurz
* \warning markets are not necesarilly set when completeInit is called
*/
void technology::completeInit() {
    const string CO2_NAME = "CO2";
    if( !util::hasValue( ghgNameMap, CO2_NAME ) ) {
        // arguments: gas, unit, remove fraction, GWP, and emissions coefficient
        // for CO2 this emissions coefficient is not used
        Ghg* CO2 = new Ghg( CO2_NAME, "MTC", 0, 1, 0 ); // at least CO2 must be present
        ghg.push_back( CO2 );
        ghgNameMap[ CO2_NAME ] = static_cast<int>( ghg.size() ) - 1;
    }
	// calculate effective efficiency
	eff = effBase * (1 - effPenalty); // reduces efficiency by penalty
	necost = neCostBase * (1 + neCostPenalty); // increases cost by penalty

}

//! write object to xml output stream
void technology::toInputXML( ostream& out, Tabs* tabs ) const {
    
	XMLWriteOpeningTag( getXMLName2D(), out, tabs, "", year );
    // write the xml for the class members.
    
    XMLWriteElement( name, "name", out, tabs );
    XMLWriteElement( year, "year", out, tabs );
    
    XMLWriteElementCheckDefault( shrwts, "sharewt", out, tabs, 1.0 );
    
    if (doCalibration) {
        XMLWriteElement( calInputValue, "calInputValue", out, tabs );
    }
    
    XMLWriteElement( fuelname, "fuelname", out, tabs );
    XMLWriteElementCheckDefault( effBase, "efficiency", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( effPenalty, "efficiencyPenalty", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( neCostBase, "nonenergycost", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( neCostPenalty, "neCostPenalty", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( neCostPenalty, "fuelprefElasticity", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( tax, "tax", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( fMultiplier, "fMultiplier", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( pMultiplier, "pMultiplier", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( lexp, "logitexp", out, tabs, LOGIT_EXP_DEFAULT );
    XMLWriteElementCheckDefault( fixedOutput, "fixedOutput", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( techchange, "techchange", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( resource, "resource", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( A, "A", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( B, "B", out, tabs, 0.0 );
	XMLWriteElementCheckDefault( note, "note", out, tabs );
    
    for( vector<Ghg*>::const_iterator ghgIter = ghg.begin(); ghgIter != ghg.end(); ghgIter++ ){
        ( *ghgIter )->toInputXML( out, tabs );
    }
    
    // finished writing xml for the class members.
    toInputXMLDerived( out, tabs );
    XMLWriteClosingTag( getXMLName2D(), out, tabs );
}

//! write object to xml debugging output stream
void technology::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
    
	XMLWriteOpeningTag( getXMLName1D(), out, tabs, name, year );
    // write the xml for the class members.
    
    XMLWriteElement( unit, "unit", out, tabs );
    XMLWriteElement( fuelname, "fuelname", out, tabs );
    XMLWriteElement( shrwts, "sharewt", out, tabs );
    if (doCalibration) {
        XMLWriteElement( calInputValue, "calInputValue", out, tabs );
    }
    XMLWriteElement( eff, "efficiencyEffective", out, tabs );
    XMLWriteElement( effBase, "efficiencyBase", out, tabs );
    XMLWriteElement( effPenalty, "efficiencyPenalty", out, tabs );
    XMLWriteElement( fuelcost, "fuelcost", out, tabs );
    XMLWriteElement( necost, "nonEnergyCostEffective", out, tabs );
    XMLWriteElement( neCostBase, "neCostBase", out, tabs );
    XMLWriteElement( neCostPenalty, "neCostPenalty", out, tabs );
    XMLWriteElement( tax, "tax", out, tabs );
    XMLWriteElement( fMultiplier, "fMultiplier", out, tabs );
    XMLWriteElement( pMultiplier, "pMultiplier", out, tabs );
    XMLWriteElement( lexp, "logitexp", out, tabs );
    XMLWriteElement( share, "share", out, tabs );
    XMLWriteElement( output, "output", out, tabs );
    XMLWriteElement( input, "input", out, tabs );
    XMLWriteElement( techchange, "techchange", out, tabs );
    XMLWriteElement( resource, "resource", out, tabs );
    XMLWriteElement( A, "A", out, tabs );
    XMLWriteElement( B, "B", out, tabs );

    // write our ghg object, vector is of number of gases
    for( vector<Ghg*>::const_iterator i = ghg.begin(); i != ghg.end(); i++ ){
        ( *i )->toDebugXML( period, out, tabs );
    }
    
    // finished writing xml for the class members.
    toDebugXMLDerived( period, out, tabs );
    XMLWriteClosingTag( getXMLName1D(), out, tabs );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& technology::getXMLName1D() const {
	return XML_NAME1D;
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
const std::string& technology::getXMLNameStatic1D() {
	return XML_NAME1D;
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& technology::getXMLName2D() const {
	return XML_NAME2D;
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
const std::string& technology::getXMLNameStatic2D() {
	return XML_NAME2D;
}

//! Perform initializations that only need to be done once per period
void technology::initCalc( const MarketInfo* aSubsectorInfo ) {    
    if ( doCalOutput ) {
        calInputValue = calOutputValue/eff;
        doCalibration = true;
    }

    if ( calInputValue < 0 ) {
        if( Configuration::getInstance()->getBool( "debugChecking" ) ){
            cerr << "Calibration value < 0 for tech " << name << ". Calibration removed" << endl;
        }
        doCalibration = false;
    }

    for( unsigned int i = 0; i < ghg.size(); i++ ){
        ghg[i]->initCalc( );
    }
   
}

/*! \brief This function calculates the sum of the Carbon Values for all GHG's in this technology.
* \details The function first checks if a carbon tax exists for the technology, and 
* if it does loops through all GHGs to calculate a sum carbon value. The GHG function which
* it calls, getGHGValue() calculates the carbon equivalent of all GHG's contained in this technology.
* The totalGHGCost attribute of the technology is then set to this new value.
* \author Sonny Kim, Josh Lurz
* \param regionName The region containing this technology.
* \param sectorName The sector containing this technology.
* \param per The period to calculate this value for.
* \note At one time this code may have worked for multiple GHG markets. This is not currently the case.
*/
void technology::calcTotalGHGCost( const string& regionName, const string& sectorName, const int period ) {
    totalGHGCost = 0; // initialize
    // totalGHGCost and carbontax must be in same unit as fuel price
    for( unsigned int i = 0; i < ghg.size(); i++ ){
        totalGHGCost += ghg[i]->getGHGValue( regionName, fuelname, sectorName, eff, period );
    }
}

/*! \brief Calculate technology fuel cost and total cost.
*
* This caculates the cost (per unit output) of this specific technology. 
* The cost includes fuel cost, carbon value, and non-fuel costs.
* Conversion efficiency, and optional fuelcost and total price multipliers are used if specified.
*
* Special check for "none" fuel is implimented here.
*
* \warning Check for "none" fueltype is implimented here. Need to find a better way do do this and renewable fuels.
* \author Sonny Kim, Steve Smith
* \param period Model regionName
* \param period Model per
*/
void technology::calcCost( const string& regionName, const string& sectorName, const int per ) {
    Marketplace* marketplace = scenario->getMarketplace();

	 // code specical case where there is no fuel input. sjs
	 // used now to drive non-CO2 GHGs
    double fuelprice;
    if ( fuelname == "none" || fuelname == "renewable" ) {
        fuelprice = 0;
    }
    else {
        fuelprice = marketplace->getPrice( fuelname, regionName, per );
    } 
	 
	// fMultiplier and pMultiplier are initialized to 1 for those not read in
	fuelcost = ( fuelprice * fMultiplier ) / eff;
	techcost = ( fuelcost + necost ) * pMultiplier;
    calcTotalGHGCost( regionName, sectorName, per );
	techcost += totalGHGCost;
    
    // techcost can drift below zero in disequalibrium.
    techcost = max( techcost, util::getSmallNumber() );
}

/*! \brief calculate technology unnormalized shares
*
* fuelPrefElasticity added 11/05/2004 sjs
* 
* \author Sonny Kim, Steve Smith
* \param regionName region name
* \param per model period
* \todo Check to see if power function for trival values really wastes time
*/
void technology::calcShare( const string& regionName, const GDP* gdp, const int period ) {
    share = shrwts * pow(techcost,lexp);
    // This is rarely used, so probably worth it to not to waste cycles on the power function. sjs
    if ( fuelPrefElasticity != 0 ) {
      double scaledGdpPerCapita = gdp->getBestScaledGDPperCap( period );
      share *= pow( scaledGdpPerCapita, fuelPrefElasticity );
    }
}

/*! \brief normalizes technology shares
*
* \author Sonny Kim
* \param sum sum of sector shares
* \warning sum must be correct sum of shares
* \pre calcShares must be called first
*/
void technology::normShare(double sum) 
{
    if (sum==0) {
        share = 0;
    }
    else {
        share /= sum;
    }
}

/*! \brief This function sets the value of fixed supply
* This needs to be called only once per period. Sets the amount of fixed supply to either the read-in value or the "MiniCAM-style" formula used for hydro. 
* 
* A == Minicam HYDRO(1,L)
* B == Minicam HYDRO(2,L) ??
* resource == Minicam HYDRO(3,L)
*
* \author Sonny Kim, Steve Smith
* \param per model period
*/
void technology::calcfixedOutput(int per)
{
    const Modeltime* modeltime = scenario->getModeltime();
    const string FIXED_TECH = "hydro";
    
    // check for non zero value for coefficient so that if this is not input, this will work with fixedOutput input instead. sjs.
    // This is support for legacy hydro technology input format.
    // MiniCAM style hydro specification
    if( name == FIXED_TECH && A != 0 ) {
        const int T = per * modeltime->gettimestep( per );
        // resource and logit function 
        const double fact = exp( A + B * T );
        output = fixedOutputVal = resource * fact / ( 1 + fact );
        fixedOutput = fixedOutputVal;
    }
    
    // Data-driven specification
    if ( fixedOutput > 0 ) {
        fixedOutputVal = fixedOutput;
    }
}


/*! \brief This function resets the value of fixed supply to the maximum value
* See calcfixedOutput
*
* \author Steve Smith
* \param per model period
*/
void technology::resetfixedOutput( int per ) {
    fixedOutputVal = fixedOutput;
}

/*! \brief Return fixed technology output
* 
* returns the current value of fixed output. This may differ from the variable fixedOutput due to scale down if demand is less than the value of fixedOutput.
*
* \author Steve Smith
* \param per model period
* \return value of fixed output for this technology
*/
double technology::getFixedOutput() const {
    return fixedOutputVal;
}

/*! \brief Return fixed technology input
* 
* returns the current value of fixed input.
* This may differ from the variable fixedOutput/eff due to scale down if demand is less than 
* the value of fixed Output.
*
* \author Steve Smith
* \param per model period
* \return value of fixed input for this technology
*/
double technology::getFixedInput() const {
    if ( eff != 0 ) {
		return fixedOutputVal / eff;
	} else {
		return 0;
	}
}

/*! \brief Scale fixed technology supply
* 
* Scale down fixed supply. Used if total fixed production is greater than actual demand.
*
* \author Steve Smith
* \param scaleRatio multipliciative value to scale fixed supply
*/
void technology::scalefixedOutput(const double scaleRatio)
{
    // dmd is total subsector demand
    if( fixedOutputVal != 0 ) {
        output *= scaleRatio;
        fixedOutputVal *= scaleRatio;
    }
}

/*! \brief Adjusts shares to be consistant with any fixed production
* 
* Adjust shares to account for fixed supply. Sets appropriate share for fixed supply and scales shares of other technologies appropriately.
*
* \author Steve Smith
* \param subsecdmd subsector demand
* \param subsecfixedOutput total amount of fixed supply in this subsector
* \param varShareTot Sum of shares that are not fixed
* \param per model period
* \warning This version may not work if more than one (or not all) technologies within each sector 
has a fixed supply
*/
void technology::adjShares(double subsecdmd, double subsecfixedOutput, double varShareTot, int per)
{
    double remainingDemand = 0;
    
    if(subsecfixedOutput > 0) {
        remainingDemand = subsecdmd - subsecfixedOutput;
        if (remainingDemand < 0) {
            remainingDemand = 0;
        }
        
        if ( fixedOutputVal > 0 ) {	// This tech has a fixed supply
            if (subsecdmd > 0) {
                share = fixedOutputVal/subsecdmd;
                // Set value of fixed supply
                if (fixedOutputVal > subsecdmd) {
                    fixedOutputVal = subsecfixedOutput; // downgrade output if > fixedOutput
                }  
            }
            else {
                share = 0;
            }
        }
        else {	// This tech does not have fixed supply
            if (subsecdmd > 0) {
                share = share * (remainingDemand/subsecdmd)/varShareTot;
            }
            else {
                // Maybe not correct, but if other params are zero then something else is wrong
                share = 0; 
            }
        }
    }    
}

//! Calculates fuel input and technology output.
/*! Adds demands for fuels and ghg emissions to markets in the marketplace
* \param regionName name of the region
* \param prodName name of the product for this sector
* \param gdp pointer to gdp object
* \param dmd total demand for this subsector
* \param per Model period
*/
void technology::production(const string& regionName,const string& prodName,
                            double dmd, const GDP* gdp, const int per) {
    // dmd is total subsector demand
    const static string HYDRO = "hydro";
    if( name == HYDRO ) {
        output = fixedOutputVal = dmd;
    }
    else {
        output = share * dmd; // use share to get output for each technology
    }
    	   
    if ( output < 0 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Output value less than zero for technology " << name << endl;
    }
    
    // Calculate input demand.
    input = output / eff;

    Marketplace* marketplace = scenario->getMarketplace();
    // set demand for fuel in marketplace
    if( ( fuelname != "renewable" ) && ( fuelname != "none" ) ){ 
        marketplace->addToDemand( fuelname, regionName, input, per );
    }
    // Set the supply of the good to the marketplace.
    // Market doesn't exist for demand goods.
    marketplace->addToSupply( prodName, regionName, output, per, false );

    // calculate emissions for each gas after setting input and output amounts
    for ( unsigned int i = 0; i < ghg.size(); ++i ) {
        ghg[ i ]->calcEmission( regionName, fuelname, input, prodName, output, gdp, per );
    }
}

/*! \brief Adjusts technology share weights to be consistent with calibration value.
* This is done only if there is more than one technology
* Calibration is, therefore, performed as part of the iteration process. 
* Since this can change derivatives, best to turn calibration off when using N-R solver.
*
* This routine adjusts technology shareweights so that relative shares are correct for each subsector.
* Note that all calibration values are scaled (up or down) according to total sectorDemand 
* -- getting the overall scale correct is the job of the TFE calibration
*
* \author Steve Smith
* \param subSectorDemand total demand for this subsector
*/
void technology::adjustForCalibration( double subSectorDemand, const string& regionName, const MarketInfo*, const int period ) {

   // total calibrated outputs for this sub-sector
   double calOutput = getCalibrationOutput( );

    // make sure share weights aren't zero or else can't calibrate
    if ( shrwts  == 0 && ( calOutput > 0 ) ) {
        shrwts  = 1;
    }
   
   // Next block adjusts calibration values if total cal + fixed demands for this subsector 
//   if ( !( totalCalOutputs > subSectorDemand ) ) {
 //    calOutput = calOutput * ( subSectorDemand  / totalCalOutputs );
 //  }
   
   // Adjust share weights
   double technologyDemand = share * subSectorDemand;
   if ( technologyDemand > 0 ) {
      double shareScaleValue = calOutput / technologyDemand;
      shrwts  = shrwts * shareScaleValue;
   }
    
   // Check to make sure share weights are not less than zero (and reset if they are)
   if ( shrwts < 0 ) {
     cerr << "Share Weight is < 0 in technology " << name << endl;
     cerr << "    shrwts: " << shrwts << " (reset to 1)" << endl;
     shrwts = 1;
   }

   Configuration* conf = Configuration::getInstance();
   bool debugChecking = conf->getBool( "debugChecking" );
   
  // Report if share weight gets extremely large
   if ( debugChecking && (shrwts > 1e4 ) ) {
         cout << "Large share weight in calibration for technology: " << name << endl;
   }
}

//! calculate GHG emissions from technology use
void technology::calcEmission( const string& aGoodName, const int aPeriod ) {
    // alternative ghg emissions calculation
    emissmap.clear(); // clear emissions map
    emfuelmap.clear(); // clear emissions map
    for (int i=0; i< static_cast<int>( ghg.size() ); i++) {
        // emissions by gas name only
        emissmap[ghg[i]->getName()] = ghg[i]->getEmission( aPeriod );
        // emissions by gas and fuel names combined
        // used to calculate emissions by fuel
        emissmap[ghg[i]->getName() + fuelname] = ghg[i]->getEmission( aPeriod );
        // add sequestered amount to emissions map
        // used to calculate emissions by fuel
        emissmap[ghg[i]->getName() + "sequestGeologic"] = ghg[i]->getSequestAmountGeologic();
        emissmap[ghg[i]->getName() + "sequestNonEngy"] = ghg[i]->getSequestAmountNonEngy();
        
        // emfuelmap[ghg[i]->getName()] = ghg[i]->getEmissFuel();
        // This really should include the GHG name as well.
        emfuelmap[fuelname] = ghg[i]->getEmissFuel( aPeriod );
    }
}

//! calculate indirect GHG emissions from technology use
void technology::indemission( const vector<Emcoef_ind>& emcoef_ind )
{
    emindmap.clear(); // clear emissions map
    for (int i=0; i< static_cast<int>( ghg.size() ); i++) {
        ghg[i]->calcIndirectEmission(input,fuelname, emcoef_ind );
        emindmap[ghg[i]->getName()] = ghg[i]->getEmissInd();
    }
}

/*! \brief Returns technology name
*
* \author Sonny Kim
* \return sector name as a string
*/
string technology::getName() const {
    return name;
}

/*! \brief Returns name of the fuel consumed by this technology
*
* \author Sonny Kim
* \return fuel name as a string
*/
string technology::getFuelName() const {
    return fuelname;
}

/*! \brief Returns the ratio of output to input for this technology
*
* \author Sonny Kim
* \return efficiency (out/In) of this technology
*/
double technology::getEff() const {
    return eff;
}

/*! \brief Return fuel intensity (input over output) for this technology
*
* \author Sonny Kim
* \return fuel intensity (input/output) of this technology
* \todo Need to impliment method of adding appropriate units (btu/kwh; gallons/mile, etc.)
*/
double technology::getIntensity(const int per) const {
    return intensity;
}

/*! \brief returns share for this technology
*
* \author Sonny Kim
* \pre calcShare
* \return share value
*/
double technology::getShare() const {
    return share;
}

/*! \brief returns share weight for this technology
*
* \author Steve Smith
* \return share weight
*/
double technology::getShareWeight() const {
    return shrwts;
}

/*! \brief scale share weight for this technology
*
* \author Steve Smith
* \param scaleValue multiplicative scaling factor for share weight
*/
void technology::scaleShareWeight( double scaleValue ) {
    shrwts *= scaleValue;
}

/*! \brief scale share weight for this technology
*
* \author Steve Smith
* \param shareWeightValue new value for share weight
*/
void technology::setShareWeight( double shareWeightValue ) {
    shrwts = shareWeightValue;
}

/*! \brief Returns calibration status for this technoloy
*
* This is true if a calibration value has been read in for this technology.
* 
* \author Steve Smith
* \return Boolean that is true if technoloy is calibrated
*/
bool technology::getCalibrationStatus( ) const {
    return doCalibration;
}

//! returns true if all output is either fixed or calibrated
bool technology::ouputFixed( ) const {
    bool outputFixed = false;

   if ( doCalibration || ( fixedOutput != 0 ) || ( shrwts == 0 ) ) {
      outputFixed = true;  // this sector has fixed output
   } 
   
   return outputFixed;
}

/*! \brief Returns true if this technology is available for production and not fixed
*
* A true value means that this technology is available to respond to a demand and vary its output
* 
* \author Steve Smith
* \return Boolean that is true if technology is available
*/
bool technology::techAvailable( ) const {
   if ( !doCalibration && ( fixedOutput != 0  ||  shrwts == 0 ) ) {
      return false;  // this sector is not available to produce variable output
   } 
   return true;
}

//! return fuel input for technology
double technology::getInput() const {
    return input;
}

//! return output of technology
double technology::getOutput() const {
    return output;
}

//! return technology fuel cost only
double technology::getFuelcost( ) const {
    return fuelcost;
}

//! return technology calibration value
double technology::getCalibrationInput( ) const {
    return calInputValue;
}

//! scale technology calibration value
void technology::scaleCalibrationInput( const double scaleFactor ) {
    if ( scaleFactor != 0 ) {
        calInputValue = calInputValue * scaleFactor;
        calOutputValue = calOutputValue * scaleFactor;
    }
}

//! return technology calibration value
double technology::getCalibrationOutput( ) const {
    return calInputValue * eff;
}

//! return the cost of technology
double technology::getTechcost() const {
    return techcost;
}

//! return the non-energy cost of the technology
double technology::getNecost() const {
    return necost;
}

//! return any carbon tax and storage cost applied to technology
double technology::getTotalGHGCost() const {
    // (75$/GJ)
    return totalGHGCost;
}

//! return carbon taxes paid by technology
double technology::getCarbonTaxPaid( const string& aRegionName, int aPeriod ) const {
    double sum = 0;
    for( vector<Ghg*>::const_iterator currGhg = ghg.begin(); currGhg != ghg.end(); ++currGhg ){
        sum += (*currGhg)->getCarbonTaxPaid( aRegionName, aPeriod );
    }
    return sum;
}

/*! \brief Return a vector listing the names of all the GHGs within the Technology.
* \details This function returns all GHG names the Technology contains. It does 
* this by searching the underlying ghgNameMap.
* \author Josh Lurz
* \return A vector of GHG names contained in the Technology.
*/
const vector<string> technology::getGHGNames() const {
    return util::getKeys( ghgNameMap );
}

/*! \brief Return a GHG emissions coefficient for a given GHG name.
* This function searches the mapping of GHG names to values and
* returns the emissions coefficient from the GHG with the given name,
* or -1 if there is not a GHG with the given name.
* \todo Eliminate this function and getGHGNames when technology is converted to a multi-timeperiod structure.
* \param ghgName The name of a GHG to return the emissions coefficient for.
* \warning Assumes there is only one GHG object with any given name
* \return The emissions coefficient of the GHG with ghgName, -1 if the GHG does not exist.
*/
double technology::getGHGEmissionCoef( const std::string& ghgName ) const {
    const int ghgIndex = util::searchForValue( ghgNameMap, ghgName );
    double emissCoef;

    // Need to perform error checking b/c the searchForValue function will return 0 if the name
    // is not found or if the correct element is at position 1. This handles the first case. 
    if( ( ghgIndex == 0 ) && ( ( ghg.size() == 0 ) || ( ghg[ 0 ]->getName() != ghgName ) ) ){
        // A ghg with the passed in name does not exist.
        emissCoef = -1;
    }
    else {
        emissCoef = ghg[ ghgIndex ]->getEmissCoef();
    }
    return emissCoef;
}

/*! \brief Copies parameters across periods for a specific GHG 
* \param prevGHG Pointer to the previous GHG object that needs to be passed to the corresponding object this period.
* \warning Assumes there is only one GHG object with any given name
*/
void technology::copyGHGParameters( const Ghg* prevGHG ) {
	const int ghgIndex = util::searchForValue( ghgNameMap, prevGHG->getName() );

	if ( prevGHG ) {
		ghg[ ghgIndex ]->copyGHGParameters( prevGHG );
	}
	 
}

/*! \brief Returns the pointer to a specific GHG 
* \param ghgName Name of GHG 
* \warning Assumes there is only one GHG object with any given name
*/
Ghg* technology::getGHGPointer( const std::string& ghgName ) {
    const int ghgIndex = util::searchForValue( ghgNameMap, ghgName );

	return ghg[ ghgIndex ];
	 
}

/*! \brief sets the emissions coefficient for a GHG specified byname.
* This function searches the mapping of GHG names to values and
* returns the emissions coefficient from the GHG with the given name,
* or -1 if there is not a GHG with the given name.
* \todo Eliminate this function and getGHGNames when technology is converted to a multi-timeperiod structure.
* \param ghgName The name of a GHG to return the emissions coefficient for.
* \warning Assumes there is only one GHG object with any given name
* \return The emissions coefficient of the GHG with ghgName, -1 if the GHG does not exist.
*/
void technology::setGHGEmissionCoef( const std::string& ghgName, const double emissionsCoef ) {
    const int ghgIndex = util::searchForValue( ghgNameMap, ghgName );
    // Need to perform error checking b/c the searchForValue function will return 0 if the name
    // is not found or if the correct element is at position 1. This handles the first case. 
    if( ( ghgIndex == 0 ) && ( ( ghg.size() == 0 ) || ( ghg[ 0 ]->getName() != ghgName ) ) ){
        // A ghg with the passed in name does not exist.
    }
    else {
        ghg[ ghgIndex ]->setEmissCoef( emissionsCoef );
    }
}

/*! \brief Return the flag that tells if the GHG had an emissions value read in
* This function searches the mapping of GHG names to values and
* returns the appropriate flag,
* or -1 if there is not a GHG with the given name.
* \todo Eliminate this function and getGHGNames when technology is converted to a multi-timeperiod structure.
* \param ghgName The name of a GHG to return the emissions coefficient for.
* \warning Assumes there is only one GHG object with any given name
* \warning No error checking is possible for this function. Error checking is done in function getGHGEmissionCoef.
* \return A boolean that indicates if the GHG with the given name had an emissions read-in.
*/
bool technology::getEmissionsInputStatus( const std::string& ghgName ) const {
    const int ghgIndex = util::searchForValue( ghgNameMap, ghgName );
    return ghg[ ghgIndex ]->getEmissionsInputStatus();
}

/*! \brief Set the flag that indicates that a GHG had an emissions value read in
* This function searches the mapping of GHG names to values and
* returns the appropriate flag,
* or -1 if there is not a GHG with the given name.
* \todo Eliminate this function and getGHGNames when technology is converted to a multi-timeperiod structure.
* \param ghgName The name of a GHG to return the emissions coefficient for.
* \warning Assumes there is only one GHG object with any given name
* \warning No error checking is possible for this function. Error checking is done in function getGHGEmissionCoef.
* \return A boolean that indicates if the GHG with the given name had an emissions read-in.
*/
void technology::setEmissionsInputStatus( const std::string& ghgName ){
    const int ghgIndex = util::searchForValue( ghgNameMap, ghgName );
    return ghg[ ghgIndex ]->setEmissionsInputStatus();
}

//! return map of all ghg emissions
const map<string,double>& technology::getemissmap() const {
    return emissmap;
}

//! return map of all ghg emissions
const map<string,double>& technology::getemfuelmap() const {
    return emfuelmap;
}

//! return map of all ghg emissions
const map<string,double>& technology::getemindmap() const {
    return emindmap;
}

//! return value for ghg
double technology::get_emissmap_second( const string& str) const {
    return util::searchForValue( emissmap, str );
}

//! returns technology logit exponential
double technology::getlexp()  const {
    return lexp;
}

//! Set the technology year.
void technology::setYear( const int yearIn ) {
    year = yearIn;
}

/*! \brief returns the number of ghg objects.
*
* Calcuation is done using length of GHG string to be consistant with use of ghg names to access GHG information.
*
*
* \author Steve Smith
*/
int technology::getNumbGHGs()  const {
    std::vector<std::string> ghgNames = getGHGNames();
    if ( ghgNames[0] != "" ) {
        return static_cast<int>( ghgNames.size() ); 
    } else {
        return 0;
    }
}

/*! \brief check for fixed demands and set values to counter
*
* If the output of this technology is fixed then set that value to the appropriate marketplace counter
* If it is not, then reset counter
*
* \author Steve Smith
* \param period Model period
*/
void technology::tabulateFixedDemands( const string regionName, const int period ) {
    Marketplace* marketplace = scenario->getMarketplace();

    // Checking for market existence here avoids emitting a warning (which happens for fuel "renewable")
    if ( marketplace->getPrice( fuelname, regionName, period, false ) != Marketplace::NO_MARKET_PRICE ) {
        if ( doCalibration || ( fixedOutput != 0 ) || ( shrwts == 0 ) ) {
            double fixedInput = 0;
            // this sector has fixed output
            if ( doCalibration ) {
                fixedInput = getCalibrationInput();
            } else if ( fixedOutput != 0 ) {
                fixedInput = getFixedInput();
            }
            // set demand for fuel in marketInfo counter
            double exisitingDemand = max( marketplace->getMarketInfo( fuelname, regionName , period, "calDemand" ), 0.0 );
            marketplace->setMarketInfo( fuelname, regionName, period, "calDemand", exisitingDemand + fixedInput );        
        } else {
            // If not fixed, then set to -1 to indicate a demand that is not completely fixed
            marketplace->setMarketInfo( fuelname, regionName, period, "calDemand", -1 );
        }
    }
}
