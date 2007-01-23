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
        else if( EmissionsDriverFactory::isEmissionsDriverNode( nodeName ) ){
            auto_ptr<AEmissionsDriver> newDriver = EmissionsDriverFactory::create( nodeName );
            setEmissionsDriver( newDriver );
        }
        else if( nodeName == "emissionsUnit" ){
            mEmissionsUnit = XMLHelper<string>::getValue( curr );
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
    toInputXMLDerived( out, tabs );
    // done writing xml for data members.

    XMLWriteClosingTag( getXMLName(), out, tabs );

}
//! Writes datamembers to debugging datastream in XML format.
void AGHG::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    XMLWriteOpeningTag( getXMLName(), out, tabs, getName() );

    // write xml for data members
    XMLWriteElement( mEmissions[ period ], "emission", out, tabs );
    XMLWriteElement( mEmissionsByFuel[ period ], "emissFuel", out, tabs );

    toDebugXMLDerived( period, out, tabs );
    // done writing xml for data members.

    XMLWriteClosingTag( getXMLName(), out, tabs );
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
    // Emissions can be positive or negative.
    if( !util::isEqual( mEmissions[ aPeriod ], 0.0 ) ){
        // set emissions as demand side of gas market
        scenario->getMarketplace()->addToDemand( getName(), aRegionName,
                                                mEmissions[ aPeriod ],
                                                aPeriod, false );
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
    return ghgTax * currInputGasCoef * convFactor;
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
        totalEmissions += (*input)->getDemandPhysical( aRegionName, aPeriod ) 
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
void AGHG::calcEmission( const vector<Input*> aInputs,
                         const string& aRegionName,
                         const string& aGoodName,
                         const double aOutput,
                         const int aPeriod )
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

    // Store the total emissions.
    mEmissions[ aPeriod ] = tempEmission;

    // TODO: Need to do sequestered emissions here.
    // Add to the constraint market. 
    marketplace->addToDemand( getName(), aRegionName, tempEmission, aPeriod, false );
}

//! Return Ghg emissions.
double AGHG::getEmission( const int aPeriod ) const {
    assert( aPeriod < static_cast<int>( mEmissions.size() ) );
    return mEmissions[ aPeriod ];
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

/*! \brief Update a visitor with information from a GHG for a given period.
* \param aVisitor The visitor to update.
* \param aPeriod The period for which to update.
*/
void AGHG::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitGHG( this, aPeriod );
    aVisitor->endVisitGHG( this, aPeriod );
}
