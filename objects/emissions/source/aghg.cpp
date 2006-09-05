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
#include "emissions/include/aghg.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/gdp.h"
#include "functions/include/input.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/iinfo.h"
#include "util/logger/include/ilogger.h"
#include "technologies/include/ioutput.h"
#include "emissions/include/aemissions_driver.h"
#include "emissions/include/emissions_driver_factory.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.

typedef vector<Input*>::const_iterator CInputIterator;

//! Default constructor.
AGHG::AGHG():
gwp( 1 ),
rmfrac( 0 ),
isGeologicSequestration( true ),
storageCost( util::getLargeNumber() ), // default to a large cost to turn off CCS
sequestAmountGeologic( 0 ),
sequestAmountNonEngy( 0 ),
// this is inefficient as it is greater than the lifetime
// but much simpler than converting period to liftime period 
// TODO: Fix this so it has one spot per active period.
mEmissions( scenario->getModeltime()->getmaxper() ),
mEmissionsByFuel( scenario->getModeltime()->getmaxper() )
{
}

//! Destructor
AGHG::~AGHG(){
}

//! Copy constructor.
AGHG::AGHG( const AGHG& other ){
    copy( other );
}

//! Assignment operator.
AGHG& AGHG::operator=( const AGHG& other ){
    if( this != &other ){
        // If there was a destructor it would need to be called here.
        copy( other );
    }
    return *this;
}

//! Copy helper function.
void AGHG::copy( const AGHG& other ){
    rmfrac = other.rmfrac;
    gwp = other.gwp;
    isGeologicSequestration = other.isGeologicSequestration;
    storageCost = other.storageCost;
    storageName = other.storageName;
    sequestAmountGeologic = other.sequestAmountGeologic;
    sequestAmountNonEngy = other.sequestAmountNonEngy;

    mEmissions.resize( scenario->getModeltime()->getmaxper() );
    std::copy( other.mEmissions.begin(), other.mEmissions.end(), mEmissions.begin() );
    mEmissionsByFuel.resize( scenario->getModeltime()->getmaxper() );
    std::copy( other.mEmissionsByFuel.begin(), other.mEmissionsByFuel.end(), mEmissionsByFuel.begin() );

    // Deep copy the auto_ptr
    if( other.mEmissionsDriver.get() ){
        mEmissionsDriver.reset( other.mEmissionsDriver->clone() );
    }
}

//! \brief initialize Ghg object with xml data
void AGHG::XMLParse(const DOMNode* node) {   
    /*! \pre Assume we are passed a valid node. */
    assert( node );

    DOMNodeList* nodeList = node->getChildNodes();

    // Parse the name attribute.
    parseName( XMLHelper<string>::getAttr( node, "name" ) );

    for( unsigned int i = 0; i < nodeList->getLength(); ++i ) {
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );      

        if( nodeName == "#text" ){
            continue;
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
            storageName = XMLHelper<string>::getAttr( curr, "name" );
            storageCost = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "GWP" ){
            gwp = XMLHelper<double>::getValue( curr );
        }
        else if( EmissionsDriverFactory::isEmissionsDriverNode( nodeName ) ){
            setEmissionsDriver( EmissionsDriverFactory::create( nodeName ) );
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

//! Writes datamembers to datastream in XML format.
void AGHG::toInputXML( ostream& out, Tabs* tabs ) const {

    XMLWriteOpeningTag( getXMLName(), out, tabs, getName() );

    // write xml for data members
    XMLWriteElementCheckDefault( rmfrac, "removefrac", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( isGeologicSequestration, "isGeologicSequestration", out, tabs, true );
    XMLWriteElementCheckDefault( storageCost, "storageCost", out, tabs, util::getLargeNumber() );
    XMLWriteElementCheckDefault( gwp, "GWP", out, tabs, 1.0 );

    toInputXMLDerived( out, tabs );
    // done writing xml for data members.

    XMLWriteClosingTag( getXMLName(), out, tabs );

}
//! Writes datamembers to debugging datastream in XML format.
void AGHG::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    XMLWriteOpeningTag( getXMLName(), out, tabs, getName() );

    // write xml for data members
    XMLWriteElement( rmfrac, "removefrac", out, tabs );
    XMLWriteElement( gwp, "GWP", out, tabs );
    XMLWriteElement( mEmissions[ period ], "emission", out, tabs );
    XMLWriteElement( isGeologicSequestration, "isGeologicSequestration", out, tabs );
    XMLWriteElement( storageCost, "storageCost", out, tabs );
    XMLWriteElement( sequestAmountGeologic, "sequestAmountGeologic", out, tabs );
    XMLWriteElement( sequestAmountNonEngy, "sequestAmountNonEngy", out, tabs );
    XMLWriteElement( mEmissionsByFuel[ period ], "emissFuel", out, tabs );

    toDebugXMLDerived( period, out, tabs );
    // done writing xml for data members.

    XMLWriteClosingTag( getXMLName(), out, tabs );
}

/*!
 * \brief Get the XML node name in static form for comparison when parsing XML.
 * \details This public function accesses the private constant string, XML_NAME.
 *          This way the tag is always consistent for both read-in and output and can be easily changed.
 *          The "==" operator that is used when parsing, required this second function to return static.
 * \note A function cannot be static and virtual.
 * \note This was left as GHG, not AGHG to avoid breaking the xml database.
 * \author Jim Naslund
 * \return The constant XML_NAME as a static.
 */
const string& AGHG::getXMLNameStatic(){
    static const string XML_NAME = "GHG";
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
void AGHG::copyGHGParameters( const AGHG* prevGHG ) {

    assert( prevGHG ); // Make sure valid pointer was passed
 
    if ( !gwp ) { 
        gwp = prevGHG->gwp; // only copy if GWP has not changed
    }
}

//! Perform initializations that only need to be done once per period
void AGHG::initCalc( const IInfo* aSubsectorInfo ) {
}

/*!
 * \brief Calculate the aggregate output emissions coefficient for the gas.
 * \details The output coefficient is the sum of all output coefficients of all
 *          the outputs.
 * \param aOutputs Vector of Technology outputs.
 * \param aPeriod Period.
 * \return Aggregate output coefficient.
 */
double AGHG::calcOutputCoef( const vector<IOutput*>& aOutputs, const int aPeriod ) const {
    // The output coefficient is the sum of the output coefficients of all outputs.
    double outputCoef = 0;
    for( unsigned int i = 0; i < aOutputs.size(); ++i ){
        outputCoef += aOutputs[ i ]->getEmissionsPerOutput( getName(), aPeriod );
    }
    return outputCoef;
}

/*!
 * \brief Sets the emissions as the demand side of the gas market.
 * \param aRegionName the region to set
 * \param aPeriod the period
 */
void AGHG::addEmissionsToMarket( const string& aRegionName, const int aPeriod ){
    // set emissions as demand side of gas market
    Marketplace* marketplace = scenario->getMarketplace();
    // Optimize special case of no-emission ghg.
    if( mEmissions[ aPeriod ] != 0 ){
        marketplace->addToDemand( getName(), aRegionName, mEmissions[ aPeriod ], aPeriod, false );
    }
    if( sequestAmountGeologic != 0 ){
        // set sequestered amount as demand side of carbon storage market
        marketplace->addToDemand( "carbon storage", aRegionName, sequestAmountGeologic, aPeriod, false );
    }
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
double AGHG::getGHGValue( const Input* aInput, const string& aRegionName,
                          const string& aGoodName, const int aPeriod ) const
{
    // Determine if there is a tax.
    const Marketplace* marketplace = scenario->getMarketplace();
    double ghgTax = marketplace->getPrice( getName(), aRegionName, aPeriod, false );
    if( ghgTax == Marketplace::NO_MARKET_PRICE ){
        ghgTax = 0;
    }
    // Get the emissions coef for the input.
    double currInputGasCoef = aInput->getGHGCoefficient( getName(), aRegionName );
    
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
double AGHG::calcInputEmissions( const vector<Input*>& aInputs, const string& aRegionName, const int aPeriod ) const {
    double totalEmissions = 0;

    const Marketplace* marketplace = scenario->getMarketplace();
    // Loop over the inputs calculating the amount of carbon in each.
    for( CInputIterator input = aInputs.begin(); input != aInputs.end(); ++input ){
        // Add on the physical amount of the input multplied by the amount of
        // emissions per unit of physical output.
        totalEmissions += (*input)->getDemandPhysical( aRegionName ) 
                             * (*input)->getGHGCoefficient( getName(), aRegionName );      
    }
    return totalEmissions;
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
void AGHG::calcEmission( const vector<Input*> aInputs, const string& aRegionName, const string& aGoodName,
                        const double aOutput, const int aPeriod )
{
    // Calculate the aggregate emissions of all inputs.
    double tempEmission = calcInputEmissions( aInputs, aRegionName, aPeriod );
    
    // Determine the output coefficient. The output coefficent will not exist for consumers.
    const static string COEF_STRING = "coefficient";
    Marketplace* marketplace = scenario->getMarketplace();
    const IInfo* outputMarketInfo = marketplace->getMarketInfo( aGoodName, aRegionName, 0, false );
    const double outputCoef = outputMarketInfo ? outputMarketInfo->getDouble( getName() + COEF_STRING, false ) : 0;
    
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
    marketplace->addToDemand( getName(), aRegionName, emissGwp, aPeriod, false );
}

//! Return Ghg emissions.
double AGHG::getEmission( const int aPeriod ) const {
    assert( aPeriod < static_cast<int>( mEmissions.size() ) );
    return mEmissions[ aPeriod ];
}

//! Return geologic sequestered ghg emissions.
double AGHG::getSequestAmountGeologic() const {
    return sequestAmountGeologic;
}

//! Return non-energy sequestered ghg emissions.
double AGHG::getSequestAmountNonEngy() const {
    return sequestAmountNonEngy;
}

//! Return ghg emissions inplicit in fuel.
double AGHG::getEmissFuel( const int aPeriod ) const {
    return mEmissionsByFuel[ aPeriod ];
}

//! returns the emissions Driver value. emissions are proportional to input minus output.
double AGHG::emissionsDriver( const double inputIn, const double outputIn ) const {
    return mEmissionsDriver->calcEmissionsDriver( inputIn, outputIn );
}

/*!
 * \brief Sets the emissions driver.
 * \note Ownership of the auto_ptr is transfered.
 * \param aEmissionsDriver New emissions driver.
 */
void AGHG::setEmissionsDriver( auto_ptr<AEmissionsDriver>& aEmissionsDriver ){
    mEmissionsDriver = aEmissionsDriver;
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
double AGHG::getCarbonTaxPaid( const string& aRegionName, const int aPeriod ) const {
    const Marketplace* marketplace = scenario->getMarketplace();
    double GHGTax = marketplace->getPrice( getName(), aRegionName, aPeriod, false );
    if( GHGTax == Marketplace::NO_MARKET_PRICE ){
        GHGTax = 0;
    }
    // The carbon tax paid is the amount of the emission multiplied by the tax and the global
    // warming emission. This may be a negative in the case of a credit.
    return GHGTax * mEmissions[ aPeriod ] * gwp;
}


/*! \brief Update a visitor with information from a GHG for a given period.
* \param aVisitor The visitor to update.
* \param aPeriod The period for which to update.
*/
void AGHG::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitGHG( this, aPeriod );
    aVisitor->endVisitGHG( this, aPeriod );
}
