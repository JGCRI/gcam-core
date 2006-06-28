/*! 
* \file technology.cpp
* \ingroup Objects
* \brief technology class source file.
* \author Sonny Kim
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
#include "emissions/include/ghg_output_aggr.h"
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
#include "containers/include/dependency_finder.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/iinfo.h"
#include "technologies/include/ioutput.h"
#include "technologies/include/secondary_output.h"
#include "technologies/include/primary_output.h"
#include "technologies/include/ical_data.h"

// TODO: Factory for cal data objects.
#include "technologies/include/cal_data_input.h"
#include "technologies/include/cal_data_output.h"
#include "technologies/include/cal_data_output_percap.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const double LOGIT_EXP_DEFAULT = -6;

typedef vector<IOutput*>::const_iterator COutputIterator;
typedef vector<Ghg*>::const_iterator CGHGIterator;
// Technology class method definition

/*! 
 * \brief Constructor.
 * \param aName Technology name.
 * \param aYear Technology year.
 */
technology::technology( const string& aName, const int aYear )
: name( aName ), year ( aYear ) {
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
    mBaseEfficiency = techIn.mBaseEfficiency; 
    effPenalty = techIn.effPenalty;
    mBaseNonEnergyCost = techIn.mBaseNonEnergyCost;
    neCostPenalty = techIn.neCostPenalty;
    fuelcost = techIn.fuelcost;
    techcost = techIn.techcost;
    fMultiplier = techIn.fMultiplier;
    pMultiplier = techIn.pMultiplier;
    lexp = techIn.lexp;
    share = techIn.share;
    input = techIn.input;
    fixedOutput = techIn.fixedOutput;
    fixedOutputVal = techIn.fixedOutputVal;
    name = techIn.name;
    fuelname = techIn.fuelname;

    emissmap = techIn.emissmap; 
    emfuelmap = techIn.emfuelmap; 
    emindmap = techIn.emindmap; 
    fuelPrefElasticity = techIn.fuelPrefElasticity;
    ghgNameMap = techIn.ghgNameMap; 
    
    // Clone the existing cal data object if there is one.
    // TODO: This may correct given the usage of clone in technology to copy forward.
    mCalValue.reset( techIn.mCalValue.get() ? techIn.mCalValue->clone() : 0 );
    for (CGHGIterator iter = techIn.ghg.begin(); iter != techIn.ghg.end(); ++iter) {
        ghg.push_back( (*iter)->clone() );
    }
    for ( COutputIterator iter = techIn.mOutputs.begin(); iter != techIn.mOutputs.end(); ++iter) {
        mOutputs.push_back( (*iter)->clone() );
    }
}

//! Clone Function. Returns a deep copy of the current technology.
technology* technology::clone() const {
    return new technology( *this );
}

//! Clear member variables.
void technology::clear(){
    // Delete the GHGs to avoid a memory leak.
    for( vector<Ghg*>::iterator iter = ghg.begin(); iter != ghg.end(); ++iter ) {
        delete *iter;
    }
    for( vector<IOutput*>::iterator iter = mOutputs.begin(); iter != mOutputs.end(); ++iter ) {
        delete *iter;
    }
}

//! Initialize elemental data members.
void technology::initElementalMembers(){
    shrwts = 1;
    mBaseEfficiency = 1; 
    effPenalty = 0; 
    fuelcost = 0;
    mBaseNonEnergyCost = 0;
    neCostPenalty = 0;
    techcost = 0;
    fMultiplier = 1;
    pMultiplier = 1;
    lexp = LOGIT_EXP_DEFAULT; 
    share = 0;
    input = 0;
    fixedOutput = getFixedOutputDefault(); // initialize to no fixed supply
    fixedOutputVal = getFixedOutputDefault();
    fuelPrefElasticity = 0;
}

/*! \brief Default value for fixedOutput;
* \author Steve Smith
*/
double technology::getFixedOutputDefault() {
    return -1.0;
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
            // Note: Parsing the name inside technology is now deprecated.
            //       This will eventually be an error.
        } 
        else if( nodeName == "year" ){
            // Note: Parsing the year inside technology is now deprecated.
            //       This will eventually be an error.
        }
        else if( nodeName == "fuelname" ){
            fuelname = XMLHelper<string>::getValue( curr );
        }
        else if( nodeName == "sharewt" ){
            shrwts = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "fuelprefElasticity" ){
            fuelPrefElasticity = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "efficiency" ){
            mBaseEfficiency = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "efficiencyPenalty" ){
            effPenalty = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "nonenergycost" ){
            mBaseNonEnergyCost = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "neCostPenalty" ){
            neCostPenalty = XMLHelper<double>::getValue( curr );
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
        }
		else if( nodeName == CalDataInput::getXMLNameStatic() ){
            parseSingleNode( curr, mCalValue, new CalDataInput );
        }
        else if( nodeName == CalDataOutput::getXMLNameStatic() ){
            parseSingleNode( curr, mCalValue, new CalDataOutput );
        }
		else if( nodeName == CalDataOutputPercap::getXMLNameStatic() ){
            parseSingleNode( curr, mCalValue, new CalDataOutputPercap );
		}
		else if( nodeName == Ghg::getXMLNameStatic() ){
            parseContainerNode( curr, ghg, ghgNameMap, new Ghg );
        }
        else if( nodeName == GhgOutput::getXMLNameStatic() ){
            parseContainerNode( curr, ghg, ghgNameMap, new GhgOutput );
        }
        else if( nodeName == GhgInput::getXMLNameStatic() ){
            parseContainerNode( curr, ghg, ghgNameMap, new GhgInput );
        }
        else if( nodeName == GhgOutputAggr::getXMLNameStatic() ){
            parseContainerNode( curr, ghg, ghgNameMap, new GhgOutputAggr );
        }
        else if( nodeName == SO2Emissions::getXMLNameStatic() ){
            parseContainerNode( curr, ghg, ghgNameMap, new SO2Emissions );
        }
        else if( nodeName == SecondaryOutput::getXMLNameStatic() ){
            parseContainerNode( curr, mOutputs, new SecondaryOutput );
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
bool technology::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ){
    // do nothing. Defining method here even though it does nothing so that we do not
    // create an abstract class.
    return false;
}

/*!
* \brief Complete the initialization of the technology.
* \note This routine is only called once per model run
* \param aSectorName Sector name, also the name of the product.
* \param aDepDefinder Regional dependency finder.
* \param aSubsectorInfo Subsector information object.
* \param aLandAllocator Regional land allocator.
* \author Josh Lurz
* \warning Markets are not necesarilly set when completeInit is called
*/
void technology::completeInit( const string& aSectorName,
                               DependencyFinder* aDepFinder,
                               const IInfo* aSubsectorInfo,
                               ILandAllocator* aLandAllocator )
{
    // Check for an unset or invalid year.
    if( year == 0 ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Technology " << name << " in sector " << aSectorName
            << " has an invalid year attribute." << endl;
    }
    
    // Check for non-sensical efficiency.
    if( mBaseEfficiency <= 0 ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Resetting invalid effiency for Technology " << name << endl;
        mBaseEfficiency = 1;
    }

    const string CO2_NAME = "CO2";
    if( !util::hasValue( ghgNameMap, CO2_NAME ) ) {
        // arguments: gas, unit, remove fraction, GWP, and emissions coefficient
        // for CO2 this emissions coefficient is not used
        Ghg* CO2 = new Ghg( CO2_NAME, "MTC", 1 ); // at least CO2 must be present
        ghg.push_back( CO2 );
        ghgNameMap[ CO2_NAME ] = static_cast<int>( ghg.size() ) - 1;
    }

    // Create the primary output for this technology. All technologies will have
    // a primary output. Always insert the primary output at position 0.
    mOutputs.insert( mOutputs.begin(), new PrimaryOutput( aSectorName ) );

    for( unsigned int i = 0; i < mOutputs.size(); ++i ){
        mOutputs[ i ]->completeInit( aSectorName, aDepFinder, !hasNoInputOrOutput() );
    }

    // Add the input dependency to the dependency finder if there is one. There
    // will not be one if this is a demand technology.

    if( aDepFinder ){
        // Don't add dependency if technology does not ever function. If this is
        // the case, the technology can never have an effect on the markets.
        // This is necessary for export sectors to operate correctly, but will
        // be true in general.
        if ( !hasNoInputOrOutput() ) { // note the NOT operator
            aDepFinder->addDependency( aSectorName, fuelname );
        }
    }

    // initialize fixedOutputVal
    if ( fixedOutput >= 0 ) {
        fixedOutputVal = fixedOutput; 
    }
}

//! write object to xml output stream
void technology::toInputXML( ostream& out, Tabs* tabs ) const {
    
    XMLWriteOpeningTag( getXMLName2D(), out, tabs, "", year );
    // write the xml for the class members.
    
    XMLWriteElementCheckDefault( shrwts, "sharewt", out, tabs, 1.0 );
    
    if ( mCalValue.get() ){
		mCalValue->toInputXML( out, tabs );
    }
    
    XMLWriteElement( fuelname, "fuelname", out, tabs );
    XMLWriteElementCheckDefault( mBaseEfficiency, "efficiency", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( effPenalty, "efficiencyPenalty", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( mBaseNonEnergyCost, "nonenergycost", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( neCostPenalty, "neCostPenalty", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( fuelPrefElasticity, "fuelprefElasticity", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( fMultiplier, "fMultiplier", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( pMultiplier, "pMultiplier", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( lexp, "logitexp", out, tabs, LOGIT_EXP_DEFAULT );
    XMLWriteElementCheckDefault( fixedOutput, "fixedOutput", out, tabs, getFixedOutputDefault() );
    XMLWriteElementCheckDefault( note, "note", out, tabs );
    
    for( COutputIterator iter = mOutputs.begin(); iter != mOutputs.end(); ++iter ){
        ( *iter )->toInputXML( out, tabs );
    }
    for( CGHGIterator iter = ghg.begin(); iter != ghg.end(); ++iter ){
        ( *iter )->toInputXML( out, tabs );
    }
    
    // finished writing xml for the class members.
    toInputXMLDerived( out, tabs );
    XMLWriteClosingTag( getXMLName2D(), out, tabs );
}

//! write object to xml debugging output stream
void technology::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
    
    XMLWriteOpeningTag( getXMLName1D(), out, tabs, name, year );
    // write the xml for the class members.
    
    XMLWriteElement( fuelname, "fuelname", out, tabs );
    XMLWriteElement( shrwts, "sharewt", out, tabs );
	if ( mCalValue.get() ) {
		mCalValue->toDebugXML( out, tabs );
    }
    XMLWriteElement( getEfficiency( period ), "efficiencyEffective", out, tabs );
    XMLWriteElement( mBaseEfficiency, "efficiencyBase", out, tabs );
    XMLWriteElement( effPenalty, "efficiencyPenalty", out, tabs );
    XMLWriteElement( fuelcost, "fuelcost", out, tabs );
    XMLWriteElement( getNonEnergyCost( period ), "nonEnergyCostEffective", out, tabs );
    XMLWriteElement( mBaseNonEnergyCost, "neCostBase", out, tabs );
    XMLWriteElement( neCostPenalty, "neCostPenalty", out, tabs );
    XMLWriteElement( fMultiplier, "fMultiplier", out, tabs );
    XMLWriteElement( pMultiplier, "pMultiplier", out, tabs );
    XMLWriteElement( lexp, "logitexp", out, tabs );
    XMLWriteElement( share, "share", out, tabs );
    XMLWriteElement( input, "input", out, tabs );
    XMLWriteElement( fixedOutput, "fixedOutput", out, tabs );

    for( COutputIterator iter = mOutputs.begin(); iter != mOutputs.end(); ++iter ){
        ( *iter )->toDebugXML( period, out, tabs );
    }

    // write our ghg object, vector is of number of gases
    for( CGHGIterator i = ghg.begin(); i != ghg.end(); i++ ){
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
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& technology::getXMLName1D() const {
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
const string& technology::getXMLNameStatic1D() {
	const static string XML_NAME1D = "technology";
	return XML_NAME1D;
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& technology::getXMLName2D() const {
	return getXMLNameStatic2D();
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
const string& technology::getXMLNameStatic2D() {
	const static string XML_NAME2D = "period";
	return XML_NAME2D;
}

/*! \brief Perform initializations that only need to be done once per period.
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aSubsectorInfo Parent information container.
* \param aDemographics Regional demographics container.
* \param aPeriod Model period.
*/
void technology::initCalc( const string& aRegionName,
                           const string& aSectorName,
                           const IInfo* aSubsectorInfo,
                           const Demographic* aDemographics,
                           const int aPeriod )
{
    // initialize calDataobjects for the period.
    if ( mCalValue.get() ){
        mCalValue->initCalc( aDemographics, aPeriod );
    }

	if ( mCalValue.get() && ( mCalValue->getCalInput( getEfficiency( aPeriod ) ) < 0 ) ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::DEBUG );
        mainLog << "Negative calibration value for technology " << name
                << ". Calibration removed." << endl;
		mCalValue.reset( 0 );
    }

    for( unsigned int i = 0; i < ghg.size(); i++ ){
        ghg[i]->initCalc( aSubsectorInfo );
    }

    for( unsigned int i = 0; i < mOutputs.size(); ++i ){
        mOutputs[ i ]->initCalc( aRegionName, aPeriod );
    }
}

/*!
 * \brief Calculates all technology benefits and costs not accounted for by the
 *        primary output.
 * \details Technologies may contain greenhouse gases and secondary output,
 *          which incur both costs and benefits to the technology. Costs can be
 *          incurred if the emissions are taxed, or if the secondary output has
 *          a cost. Benefits may accrue if a the emissions are negative or if
 *          the secondary output is has a positive value.
 * \author Sonny Kim, Josh Lurz
 * \param aRegionName The region containing this technology.
 * \param aPeriod The period to calculate this value for.
 * \return Total secondary value.
 */
double technology::calcSecondaryValue( const string& aRegionName, const int aPeriod ) const {
    double totalValue = 0;
    // Add all costs from the GHGs.
    for( unsigned int i = 0; i < ghg.size(); ++i ){
        totalValue -= ghg[i]->getGHGValue( aRegionName, fuelname, mOutputs, getEfficiency( aPeriod ), aPeriod );
    }

    // Add all values from the outputs. The primary output is included in this
    // loop but will have a value of zero.
    for( unsigned int i = 0; i < mOutputs.size(); ++i ){
        totalValue += mOutputs[ i ]->getValue( aRegionName, aPeriod );
    }
    return totalValue;
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
        
        /*! \invariant The market price of the fuel must be valid. */
        if ( fuelprice == Marketplace::NO_MARKET_PRICE ) {
           ILogger& mainLog = ILogger::getLogger( "main_log" );
           mainLog.setLevel( ILogger::ERROR );
           mainLog << "Requested fuel >" << fuelname << "< with no price in technology " << name 
                   << " in sector " << sectorName << " in region " << regionName << "." << endl;
           // set fuelprice to a valid, although arbitrary, number
           fuelprice = util::getLargeNumber();
        }
    } 
     
    // fMultiplier and pMultiplier are initialized to 1 for those not read in
    fuelcost = ( fuelprice * fMultiplier ) / getEfficiency( per );
    techcost = ( fuelcost + getNonEnergyCost( per ) ) * pMultiplier;
    techcost -= calcSecondaryValue( regionName, per );

    // techcost can drift below zero in disequalibrium.
    techcost = max( techcost, util::getSmallNumber() );
}

/*!
* \brief Calculate unnormalized technology unnormalized shares.
* \author Sonny Kim, Steve Smith
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aGDP Regional GDP container.
* \param aPeriod Model period.
* \todo Check to see if power function for trival values really wastes time
*/
void technology::calcShare( const string& aRegionName,
                            const string& aSectorName,
                            const GDP* aGDP,
                            const int aPeriod )
{
    share = shrwts * pow( techcost, lexp );
    // This is rarely used, so probably worth it to not to waste cycles on the power function. sjs
    if ( fuelPrefElasticity != 0 ) {
      double scaledGdpPerCapita = aGDP->getBestScaledGDPperCap( aPeriod );
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

/*! \brief This function resets the value of fixed supply to the maximum value
* See calcfixedOutput
*
* \author Steve Smith
* \param per model period
*/
void technology::resetFixedOutput( int per ) {
    if ( fixedOutput >= 0 ) {
        fixedOutputVal = fixedOutput;
    }
}

/*! \brief Return true if technology is fixed for no output or input
* 
* returns true if this technology is set to never produce output or input
* At present, this can only be guaranteed by assigning a fixedOutput value of zero.
*
* \author Steve Smith
* \return Returns wether this technology will always have no output or input
*/
bool technology::hasNoInputOrOutput() const {
    // Technology has zero fixed output if fixed output was read-in as zero.
    return( util::isEqual( fixedOutput, 0.0 ) );
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
    // Return 0 if the fixed output value is not initialized.
    return ( fixedOutputVal == getFixedOutputDefault() ) ? 0 : fixedOutputVal;
}

/*! \brief Return fixed technology input
* 
* Returns the current value of fixed input. This may differ from the variable
* fixedOutput/eff due to scale down if demand is less than the value of fixed
* Output.
*
* \author Steve Smith
* \param aPeriod Model period.
* \return Fixed input for this technology
*/
double technology::getFixedInput( const int aPeriod ) const {
    // Return zero as the fixed input if the fixed output is the default value
    // or if this is not the initial investment year of the technology.
    if ( fixedOutputVal == getFixedOutputDefault()
        || year != scenario->getModeltime()->getper_to_yr( aPeriod ) ) 
    {
        return 0;
    }
    return fixedOutputVal / getEfficiency( aPeriod );
}

/*
 * \brief Return the amount of input required to produce a specified amount of output.
 * \param aRequiredOutput The required amount of output.
 * \param aPeriod Model period.
 * \return The amount of input required for the output.
 */
double technology::getInputRequiredForOutput( double aRequiredOutput,
                                              const int aPeriod ) const
{
    // Efficiency should be positive because invalid efficiencies were
    // already corrected. 
    assert( getEfficiency( aPeriod ) > 0 );
    return aRequiredOutput / getEfficiency( aPeriod );
}

/*! \brief Scale fixed technology supply
* 
* Scale down fixed supply. Used if total fixed production is greater than actual demand.
*
* \author Steve Smith
* \param scaleRatio multipliciative value to scale fixed supply
*/
void technology::scaleFixedOutput( const double scaleRatio)
{
    // dmd is total subsector demand
    if( fixedOutputVal >= 0 ) {
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
        
        if ( fixedOutputVal >= 0 ) {    // This tech has a fixed supply
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
        else {  // This tech does not have fixed supply
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

/*! \brief Calculates the amount of output from the technology.
* \details Calculates the amount of output of the technology based on the share
*          of the subsector demand. This is then used to determine the amount of
*          input used, and emissions created.
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aDemand Subsector demand for output.
* \param aGDP Regional GDP container.
* \param aPeriod Model period.
*/
void technology::production( const string& aRegionName,
                             const string& aSectorName,
                             const double aDemand,
                             const GDP* aGDP,
                             const int aPeriod )
{
    assert( util::isValidNumber( aDemand ) && aDemand >= 0 );

    // dmd is total subsector demand. Use share to get output for each
    // technology
    double primaryOutput = share * aDemand;
           
    if ( primaryOutput < 0 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Primary output value less than zero for technology " << name << endl;
    }
    
    // Calculate input demand.
    input = primaryOutput / getEfficiency( aPeriod );

    Marketplace* marketplace = scenario->getMarketplace();
    // set demand for fuel in marketplace
    if( ( fuelname != "renewable" ) && ( fuelname != "none" ) ){ 
        marketplace->addToDemand( fuelname, aRegionName, input, aPeriod );
    }

    calcEmissionsAndOutputs( aRegionName, input, primaryOutput, aGDP, aPeriod );
}

/*!
 * \brief Calculate the emissions, primary and secondary outputs for the
 *        Technology.
 * \details Determines the output levels and emissions for the Technology once
 *          the primary output and input quantities are known. Emissions and
 *          outputs are added to the marketplace by the Output and GHG objects.
 * \param aRegionName Region name.
 * \param aInput Input quantity.
 * \param aPrimaryOutput Primary output quantity.
 * \param aGDP Regional GDP container.
 * \param aPeriod Period.
 */
void technology::calcEmissionsAndOutputs( const string& aRegionName,
                                          const double aInput,
                                          const double aPrimaryOutput,
                                          const GDP* aGDP,
                                          const int aPeriod )
{
    for( unsigned int i = 0; i < mOutputs.size(); ++i ){
        mOutputs[ i ]->setPhysicalOutput( aPrimaryOutput, aRegionName, aPeriod );
    }

    // calculate emissions for each gas after setting input and output amounts
    for ( unsigned int i = 0; i < ghg.size(); ++i ) {
        ghg[ i ]->calcEmission( aRegionName, fuelname, aInput, mOutputs, aGDP, aPeriod );
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
void technology::adjustForCalibration( double subSectorDemand,
                                       const string& regionName,
                                       const IInfo*,
                                       const int period )
{
   // total calibrated outputs for this sub-sector
   double calOutput = getCalibrationOutput( period );

    // make sure share weights aren't zero or else can't calibrate
    if ( ( shrwts == 0 ) && ( calOutput > 0 ) ) {
        shrwts  = 1;
    }
   
    // Adjust share weights
    double technologyDemand = share * subSectorDemand;
    if ( technologyDemand > 0 ) {
        double shareScaleValue =  calOutput / technologyDemand;
        shrwts  = shrwts * shareScaleValue;
    }
    
    // Check to make sure share weights are not less than zero (and reset if they are)
    if ( shrwts < 0 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Share weight is less than zero in technology " << name << endl;
        mainLog << "Share weight was " << shrwts << "(reset to 1)" << endl;
        shrwts = 1;
    }

    // Report if share weight gets extremely large
    const static bool debugChecking = Configuration::getInstance()->getBool( "debugChecking" );
    if ( debugChecking && shrwts > 1e6 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Large share weight in calibration for technology: " << name << endl;
    }
}

//! calculate GHG emissions from technology use
void technology::calcEmission( const string& aGoodName, const int aPeriod ) {
    // alternative ghg emissions calculation
    emissmap.clear(); // clear emissions map
    emfuelmap.clear(); // clear emissions map
    for ( unsigned int i = 0; i < ghg.size(); ++i) {
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
const string& technology::getName() const {
    return name;
}

/*! \brief Returns name of the fuel consumed by this technology
*
* \author Sonny Kim
* \return fuel name as a string
*/
const string& technology::getFuelName() const {
    return fuelname;
}

/*! \brief Returns the ratio of output to input for this technology
*
* \author Sonny Kim
* \return efficiency (out/In) of this technology
*/
double technology::getEfficiency( const int aPeriod ) const {
    // calculate effective efficiency
    return mBaseEfficiency * ( 1 - effPenalty );
}

/*! \brief Return fuel intensity (input over output) for this technology
*
* \author Sonny Kim
* \return fuel intensity (input/output) of this technology
* \todo Need to impliment method of adding appropriate units (btu/kwh; gallons/mile, etc.)
*/
double technology::getIntensity( const int aPeriod ) const {
    // Efficiency should be positive because invalid efficiencies were
    // already corrected.
    assert( getEfficiency( aPeriod ) > 0 );
    return 1 / getEfficiency( aPeriod );
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
    return mCalValue.get() != 0;
}

//! returns true if all output is either fixed or calibrated
bool technology::outputFixed( ) const {
	return ( technology::getCalibrationStatus() || ( fixedOutput >= 0 ) || ( shrwts == 0 ) );
}

/*! \brief Returns true if this technology is available for production and not fixed
*
* A true value means that this technology is available to respond to a demand and vary its output
* 
* \author Steve Smith
* \return Boolean that is true if technology is available
*/
bool technology::techAvailable( ) const {
   if ( !technology::getCalibrationStatus() && ( fixedOutput >= 0  ||  shrwts == 0 ) ) {
      return false;  // this sector is not available to produce variable output
   } 
   return true;
}

//! return fuel input for technology
double technology::getInput() const {
    return input;
}

//! return output of technology
double technology::getOutput( const int aPeriod ) const {
    // Primary output is at position zero.
    return mOutputs[ 0 ]->getPhysicalOutput( aPeriod );
}

//! return technology fuel cost only
double technology::getFuelcost() const {
    return fuelcost;
}

/*!
 * \brief Return technology input calibration value.
 * \param aPeriod Model period.
 * \return Input calibration value.
 */
double technology::getCalibrationInput( const int aPeriod ) const {
    // Calibration output is for the initial year of the technology.
    if( mCalValue.get() && year == scenario->getModeltime()->getper_to_yr( aPeriod ) ){
        return mCalValue->getCalInput( getEfficiency( aPeriod ) );
    }
    return 0;
}

//! scale technology calibration or fixed values
void technology::scaleCalibrationInput( const double scaleFactor ) {
    if ( mCalValue.get() ){
        mCalValue->scaleValue( scaleFactor );
    }
}

/*!
 * \brief Return technology output calibration value.
 * \param aPeriod Model period.
 * \return Output calibration value.
 */
double technology::getCalibrationOutput( const int aPeriod ) const {
    // Calibration output is for the initial year of the technology.
    if( mCalValue.get() && year == scenario->getModeltime()->getper_to_yr( aPeriod ) ){
        return mCalValue->getCalOutput( getEfficiency( aPeriod ) );
    }
    return 0;
}

//! return the cost of technology
double technology::getTechcost() const {
    return techcost;
}

/*!
 * \brief Return the non-energy cost of the Technology
 * \param aPeriod Model period.
 * \return Non-energy cost for the Technology.
 */
double technology::getNonEnergyCost( const int aPeriod ) const {
   return mBaseNonEnergyCost * ( 1 + neCostPenalty );
}

//! return any carbon tax and storage cost applied to technology
double technology::getTotalGHGCost( const string& aRegionName, const int aPeriod ) const {
    double totalValue = 0;
    // Add all value from the GHGs.
    for( unsigned int i = 0; i < ghg.size(); ++i ){
        totalValue += ghg[i]->getGHGValue( aRegionName, fuelname, mOutputs, getEfficiency( aPeriod ), aPeriod );
    }
    return totalValue;
}

//! return carbon taxes paid by technology
double technology::getCarbonTaxPaid( const string& aRegionName, int aPeriod ) const {
    double sum = 0;
    for( CGHGIterator currGhg = ghg.begin(); currGhg != ghg.end(); ++currGhg ){
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
Ghg* technology::getGHGPointer( const string& ghgName ) {
    const int ghgIndex = util::searchForValue( ghgNameMap, ghgName );

    return ghg[ ghgIndex ];
     
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
void technology::setYear( const int aYear ) {
    // This is called through parsing, so report an error to the user.
    if( aYear <= 0 ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Invalid year passed to set year for technology " << name << "." << endl;
    }
    else {
        year = aYear;
    }
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
    return static_cast<int>( ghgNames.size() ); 
}

/*! \brief check for fixed demands and set values to counter
*
* If the output of this technology is fixed then set that value to the appropriate marketplace counter
* If it is not, then reset counter
*
* \author Steve Smith
* \param period Model period
*/
void technology::tabulateFixedDemands( const string regionName, const int period, const IInfo* aSubsectorIInfo ) {
	const double MKT_NOT_ALL_FIXED = -1; 
    Marketplace* marketplace = scenario->getMarketplace();

    IInfo* marketInfo = marketplace->getMarketInfo( fuelname, regionName, period, false );
    // Fuel may not have a market, as is the case with renewable.
    if( marketInfo ){
        if ( outputFixed() ) {
            double fixedOrCalInput = 0;
            double fixedInput = 0;
            // this sector has fixed output
			if ( technology::getCalibrationStatus() ) {
                fixedOrCalInput = getCalibrationInput( period );
            } else if ( fixedOutput >= 0 ) {
                fixedOrCalInput = getFixedInput( period );
                fixedInput = fixedOrCalInput;
            }
            // set demand for fuel in marketInfo counter
            double existingDemand = max( marketInfo->getDouble( "calDemand", false ), 0.0 );
            marketInfo->setDouble( "calDemand", existingDemand + fixedOrCalInput );        
            
            // Track fixedDemand separately since this will not be scaled. Not
            // all markets have calFixedDemand.
            existingDemand = max( marketInfo->getDouble( "calFixedDemand", false ), 0.0 );
            marketInfo->setDouble( "calFixedDemand", existingDemand + fixedInput );        
        }
        else {
            // If not fixed, then set to -1 to indicate a demand that is not
            // completely fixed.
            marketInfo->setDouble( "calDemand", MKT_NOT_ALL_FIXED );
        }
    }
}

/*! \brief sets a tech share to an input amount
*
*
* \author Marshall Wise
*/

void technology::setTechShare(const double shareIn) {
    share = shareIn;
}

/*! \brief Update an output container with information from a technology for a
*          given period.
* \param aOutputContainer The output container to update.
* \param aPeriod The period for which to update.
*/
void technology::accept( IVisitor* aVisitor, const int aPeriod ) const {
	aVisitor->startVisitTechnology( this, aPeriod );

    for( unsigned int i = 0; i < mOutputs.size(); ++i ){
        mOutputs[ i ]->accept( aVisitor, aPeriod );
    }

	for( unsigned int i = 0; i < ghg.size(); ++i ){
		ghg[ i ]->accept( aVisitor, aPeriod );
	}
	aVisitor->endVisitTechnology( this, aPeriod );
}
