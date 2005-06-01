/*
This software, which is provided in confidence, was prepared by employees
of Pacific Northwest National Labratory operated by Battelle Memorial
Institute. Battelle has certain unperfected rights in the software
which should not be copied or otherwise disseminated outside your
organization without the express written authorization from Battelle. All rights to
the software are reserved by Battelle.  Battelle makes no warranty,
express or implied, and assumes no liability or responsibility for the 
use of this software.
*/

/*! 
* \file Input.cpp
* \ingroup Objects
* \brief The Input class source file.
*
*  Detailed description.
*
* \author Pralit Patel
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include <cmath>

#include "functions/include/input.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/xml_helper.h"
#include "reporting/include/output_container.h"
#include "util/base/include/configuration.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//! Default Constructor
Input::Input() {
    // Initially set the special types as false until we parse the input.
    mIsNumeraire = false;
    mIsCapital = false;
}

//! Destructor
Input::~Input() {}

//! Parse the Input's XML.
void Input::XMLParse( const xercesc::DOMNode* node ) {
	/*! \pre make sure we were passed a valid node. */
	assert( node );

	// get the name attribute.
	mName = XMLHelper<string>::getAttrString( node, "name" );

	// get all child nodes.
	const DOMNodeList* nodeList = node->getChildNodes();

	// loop through the child nodes.
	for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
		const DOMNode* curr = nodeList->item( i );
        if( curr->getNodeType() == DOMNode::TEXT_NODE ){
            continue;
        }
		const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

		if ( nodeName == "coefficient" ) {
			mCoefficient.init( XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "demandCurrency" ) {
			mDemandCurrency.init( XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "priceAdjustFactor" ) {
			mPriceAdjustFactor.init( XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "technicalChange" ) {
			mTechnicalChange.init( XMLHelper<double>::getValue( curr ) );
		}
        else if( !XMLDerivedClassParse( nodeName, curr ) ){
            cout << "Unrecognized text string: " << nodeName << " found while parsing " << getXMLName() << "." << endl;
		}
	}
}

void Input::completeInit(){
    // If the input has a special type, store it for speed.
    mIsCapital = ( mName == "Capital" );

    // Get the name of the numeraire from the configuration class. This is done
    // statically, so the configuration isn't checked once per input.
    const static string numeraireInputName = Configuration::getInstance()->getString( "numeraire-input", "ETE" );
    mIsNumeraire = ( mName == numeraireInputName );
}

void Input::copyParam( const Input* aInput ){
    // Name must be already defined.
	mCoefficient.set( aInput->mCoefficient );
	mPricePaid.set( aInput->mPricePaid );
    
    // Parameters which should not get copied forward if they didn't exist.
    mDemandCurrency.init( aInput->mDemandCurrency );
    mPriceAdjustFactor.init( aInput->mPriceAdjustFactor );
    mTechnicalChange.init( aInput->mTechnicalChange );
}

//! Output to XML data
void Input::toInputXML( ostream& out, Tabs* tabs ) const {
	// write the beginning tag.
	XMLWriteOpeningTag ( getXMLName(), out, tabs, mName );

	XMLWriteElement( mCoefficient, "coefficient", out, tabs );
	XMLWriteElement( mDemandCurrency, "demandCurrency", out, tabs );
	XMLWriteElement( mConversionFactor, "conversionFactor", out, tabs );
	XMLWriteElement( mPriceAdjustFactor, "priceAdjustFactor", out, tabs );

	toInputXMLDerived( out, tabs );

	// write the closing tag.
	XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Output debug info to XML
void Input::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
	// Same as toXML...
	// write the beginning tag.
	XMLWriteOpeningTag ( getXMLName(), out, tabs, mName );

	XMLWriteElement( mCoefficient, "coefficient", out, tabs );
	XMLWriteElement( mDemandCurrency, "demandCurrency", out, tabs );
	XMLWriteElement( mPricePaid, "pricePaid", out, tabs );
	XMLWriteElement( mConversionFactor, "conversionFactor", out, tabs );
	XMLWriteElement( mPriceAdjustFactor, "priceAdjustFactor", out, tabs );

	toDebugXMLDerived( period, out, tabs );

	// write the closing tag.
	XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Get the name of the Input
const string& Input::getName() const {
	return mName;
}

/*! \brief Get the currency to physical conversion factor for the input.
* \details This function returns the conversion factor used to convert the input
*          from a currency to a physical demand. To determine the coefficient,
*          the function checks the internally cached value, and if that has not
*          been set it queries the market info for the good.
* \param aRegionName The region name containing the input, neccessary to check
*        the market info.
* \return The currency to physical conversion factor.
* \author Josh Lurz
*/
double Input::getConversionFactor( const string& aRegionName ) const {
    assert( !aRegionName.empty() );
    // If the conversion factor has not been initialized, store the value from
    // the marketplace internally. This is much faster than searching the
    // marketplace every time.
    if( !mConversionFactor.isInited() ){
        // Get the conversion factor from the marketplace.
        const Marketplace* marketplace = scenario->getMarketplace();
        const double convFactor = marketplace->getMarketInfo( mName, aRegionName, 0, "ConversionFactor", false );
        if( convFactor == 0 && marketplace->getMarketInfo( mName, aRegionName, 0, "IsEnergyGood", false ) ){
            cout << "Warning conversion factor of zero for energy input " << mName << "." << endl;
        }
        mConversionFactor.init( convFactor );
    }
    return mConversionFactor;
}

/*! \brief Get the emissions gas coefficient for an input.
* \details This function returns the emissions gas coefficent for an input. To
*          determine the coefficient, the function checks the internally cached
*          value, and if that has not been set it queries the market info for
*          the good.
* \param aGHGName Name of the GHG to return the coefficient for.
* \param aRegionName The region name containing the input, neccessary to check
*        the market info.
* \return The emissions coefficient.
* \todo This doesn't work for multiple gases.
* \todo This might cause a problem if it was called too early before the market
*       info was initialized.
* \author Josh Lurz
*/
double Input::getGHGCoefficient( const string& aGHGName, const string& aRegionName ) const {
    assert( !aRegionName.empty() );
    // Currently this assumes the only gas is CO2.
    assert( aGHGName == "CO2" );
    const static string COEF_STRING = "coefficient";
    // If the coefficient has not been initialized, store the value from
    // the marketplace internally. This is much faster than searching the
    // marketplace every time.
    if( !mGHGCoefficient.isInited() ){
        // Get the coefficient from the marketplace.
        const Marketplace* marketplace = scenario->getMarketplace();
        const double coef = marketplace->getMarketInfo( mName, aRegionName, 0,
                                                        aGHGName + COEF_STRING, false );
        mGHGCoefficient.init( coef );
    }
    return mGHGCoefficient;
}

/*! \brief Get the physical demand for the input.
* \details Return the quantity of demand for this input in physical units. This
*          is calculated by multiplying the currency demand by the conversion
*          factor for the input.
* \param aRegionName The region name containing the good, needed to determine
*        the conversion factor.
* \return The demand for this input in physical units.
* \author Josh Lurz
*/
double Input::getDemandPhysical( const string& aRegionName ) const {
    return mDemandCurrency * getConversionFactor( aRegionName );
}

//! Get the Currency Demand
double Input::getDemandCurrency() const {
	return mDemandCurrency;
}

/*! \brief Get the technical change which should be applied to the input.
* \details Determines the correct technical change to apply to the input and
*          returns that value. The technical change may be one of three values.
*          The function first checks if there was a non-zero technical change
*          read in by the input, this is the highest priority technical change.
*          If there is no internal technical change, the function determines if
*          the input is a material or energy good, and returns the appropriate
*          technical change based on that and material and energy technical
*          change values passed into the function. If it is not a material or
*          energy input(a factor supply), and it does not have a read-in
*          technical change, its technical change is zero.
* \param aEnergyTechChange The amount of technical change to apply to energy
*        inputs.
* \param aMaterialTechChange The amount of technical change to apply to material
*        inputs.
* \param aRegionName The name of the region containing this input, needed to
*        determine its type.
* \return The appropriate technical change value to apply.
* \author Josh Lurz
*/
double Input::getTechChange( double aEnergyTechChange, double aMaterialTechChange,
                             const string& aRegionName ) const
{
	// First check if technical change was directly read in.
    if( mTechnicalChange.isInited() && mTechnicalChange > 0 ) {
		return mTechnicalChange;
	}
	
    // Otherwise check if we should use an energy or material tech change.
    switch( getType( aRegionName ) ){
        case( ENERGY ):
            return aEnergyTechChange;
        case( MATERIAL ):
            return aMaterialTechChange;
        default: // Land, Labor and Capital
            return 0;
    }
}

//! Set Currency Demand.
void Input::setDemandCurrency( double aDemandCurrency, const string& aRegionName,
                               const string& aSectorName, int aPeriod )  {
	mDemandCurrency.set( aDemandCurrency );
    Marketplace* marketplace = scenario->getMarketplace();
    /* Removing this check becuase it is activated by trade and the government. 
    if( aDemandCurrency < 0 ){
        cout << "Adding negative demand currency for input " << mName 
             << " from " << aSectorName << " in " << aRegionName << "." <<endl;
    }
    */
	marketplace->addToDemand( mName, aRegionName, mDemandCurrency, aPeriod );
}

//! Function to get the input-output coefficient
double Input::getCoefficient() const {
	return mCoefficient;
}

//! Virtual function to scale the coefficient
void Input::scaleCoefficient( double aScaleValue ) {
    assert( aScaleValue != 0 ); // cant scale coefs to zero.
	mCoefficient.set( mCoefficient * aScaleValue );
}

/*! Set Coefficient
 *
 * \author Pralit Patel
 * \param coef new ceofficient value
 */
void Input::setCoefficient( double aCoef ) {
    assert( aCoef != 0 ); // Can't set coefficients to zero.
	mCoefficient.set( aCoef );
}

/*! \brief Return the market price, or unadjusted price, for the input.
* \param aRegionName Region containing the input.
* \param aPeriod Period to find the price in.
* \return The market or unadjusted price.
* \author Josh Lurz
*/
double Input::getPrice( const string& aRegionName, const int aPeriod ) const {
    return scenario->getMarketplace()->getPrice( mName, aRegionName, aPeriod );
}

/*! \brief Returns the price paid for each input.
*
* \author Sonny Kim
*/
double Input::getPricePaid() const{
	return mPricePaid;
}
/*! \brief Set the price paid for each input.
*
* \param aPricePaid new price paid value
* \author Sonny Kim
*/
void Input::setPricePaid( double aPricePaid ) {
	mPricePaid.set( aPricePaid );
}
/*! \brief Returns the price received for an input.
* \details Queries the marketplace and get the price received from the market info.
* \param aRegionName Name of the region containing the input.
* \param aPeriod Period
* \return The price received for the input.
* \author Josh Lurz
*/
double Input::getPriceReceived( const string& aRegionName, const int aPeriod ) const {
    return scenario->getMarketplace()->getMarketInfo( mName, aRegionName, aPeriod, "priceReceived" );
}

/*! \brief Returns the price adjustment.
* \note For all inputs other than capital this is a multiplier, for capital it is an adder.
* \author Sonny Kim
*/
double Input::getPriceAdjustment() const {
    return mPriceAdjustFactor;
}

/*! \brief Return whether the input is a factor supply: Land, Labor, or Capital.
* \author Josh Lurz
* \return Whether the input is a factor supply.
*/
bool Input::isFactorSupply() const {
    return ( mIsCapital || mName == "Land" || mName == "Labor" );
}



/*! \brief Return the type of intput.
* \author Josh Lurz
* \return The type of the input.
* \todo Determine if caching should be done, would have to happen after
*       isEnergyGood is set.
*/
Input::Type Input::getType( const string& aRegionName ) const {
    // Determine the type dynamically.
    if( mName == "Land" ){
        return LAND;
    }
    if( mName == "Labor" ){
        return LABOR;
    }
    if( mName == "Capital" ){
        return CAPITAL;
    }
    const static string ENERGY_GOOD = "IsEnergyGood";
    if( scenario->getMarketplace()->getMarketInfo( mName, aRegionName, 0, ENERGY_GOOD ) > 0 ){
        return ENERGY;
    }
    return MATERIAL;
}

/*! \brief For outputing SGM data to a flat csv File
 * 
 * \author Pralit Patel
 * \param period The period which we are outputing for
 */
void Input::csvSGMOutputFile( ostream& aFile, const int period ) const {
	aFile << mName << ',';
	aFile.precision(0);
	aFile << mDemandCurrency << ',';
	aFile.precision(3);
	aFile << mPricePaid << endl;
}

void Input::updateOutputContainer( OutputContainer* outputContainer, const int period ) const {
	outputContainer->updateInput( this );
}
