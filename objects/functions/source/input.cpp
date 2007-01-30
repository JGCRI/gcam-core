/*
This software, which is provided in confidence, was prepared by employees
of Pacific Northwest National Laboratory operated by Battelle Memorial
Institute. Battelle has certain unperfected rights in the software
which should not be copied or otherwise disseminated outside your
organization without the express written authorization from Battelle. All rights to
the software are reserved by Battelle.  Battelle makes no warranty,
express or implied, and assumes no liability or responsibility for the 
use of this software.
*/

/*! 
* \file input.cpp
* \ingroup Objects
* \brief The Input class source file.
* \author Pralit Patel
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include <cmath>

#include "functions/include/input.h"
#include "functions/include/function_utils.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/ivisitor.h"
#include "util/base/include/configuration.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/iinfo.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//! Default Constructor
Input::Input() {
    // Initially set the special types as false until we parse the input.
    mIsNumeraire = false;
    mIsCapital = false;
    mIsFixedTrade = false;
}

//! Destructor
Input::~Input() {}

//! Parse the Input's XML.
void Input::XMLParse( const xercesc::DOMNode* node ) {
    /*! \pre make sure we were passed a valid node. */
    assert( node );

    // get the name attribute.
    mName = XMLHelper<string>::getAttr( node, "name" );

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
            mInitialDemandCurrency = XMLHelper<double>::getValue( curr );
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

void Input::completeInit( const unsigned int aFirstOperationalPeriod )
{
    // Copy in the read-in value which is assumed to be for the start year.
    if( mInitialDemandCurrency.isInited() ){
        mDemandCurrency[ aFirstOperationalPeriod ] = mInitialDemandCurrency;
    }

    // If the input has a special type, store it for speed.
    mIsCapital = ( mName == "Capital" );

    // Get the name of the numeraire from the configuration class. This is done
    // statically, so the configuration isn't checked once per input.
    const static string numeraireInputName = Configuration::getInstance()->getString( "numeraire-input", "ETE" );
    mIsNumeraire = ( mName == numeraireInputName );
}

void Input::initCalc( const string& aRegionName,
                      const bool aIsTrade,
                      const int aPeriod )
{
    // Determine if this is a fixed trade good.
    // Note: Currently fixed trade is not determined on a per period basis.
    if( aIsTrade && !isFactorSupply() ){
        const Marketplace* marketplace = scenario->getMarketplace();
        const IInfo* marketInfo =
            scenario->getMarketplace()->getMarketInfo( mName, aRegionName,
            aPeriod,
            true );

        // An input is fixed trade if it is in a trade consumer, not fixed price,
        // and not a factor supply.
        mIsFixedTrade = marketInfo ? !marketInfo->getBoolean( "IsFixedPrice",
            false ) : true;
    }

    // Demand currencies for capital and fixed trade should be copied forward
    // as they are read-in.
    if( aPeriod > 0 && !mDemandCurrency[ aPeriod ].isInited() &&
        ( isCapital() || mIsFixedTrade ) ){
        // TODO: This is wrong for the capital input of government, which
        //       is not capital but the budget deficit.
        mDemandCurrency[ aPeriod ] = mDemandCurrency[ aPeriod - 1 ];
    }
}

void Input::copyParam( const Input* aInput,
                       const int aPeriod )
{
    // Name must be already defined.
    mCoefficient.set( aInput->mCoefficient );
    mPricePaid = aInput->mPricePaid;

    if( !mInitialDemandCurrency.isInited() ){
        mInitialDemandCurrency = aInput->mInitialDemandCurrency;
    }
    
    mPriceAdjustFactor.init( aInput->mPriceAdjustFactor );
    mTechnicalChange.init( aInput->mTechnicalChange );
}

//! Output to XML data
void Input::toInputXML( ostream& out, Tabs* tabs ) const {
    // write the beginning tag.
    XMLWriteOpeningTag ( getXMLName(), out, tabs, mName );

    XMLWriteElement( mCoefficient, "coefficient", out, tabs );
    XMLWriteElement( mInitialDemandCurrency, "demandCurrency", out, tabs );
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
    XMLWriteElement( mDemandCurrency[ period ], "demandCurrency", out, tabs );
    XMLWriteElement( mPricePaid[ period ], "pricePaid", out, tabs );
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
* \param aRegionName The region name containing the input, necessary to check
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
        // If this is a factor supply the conversion factor must be zero.
        if( isFactorSupply() ){
            mConversionFactor.init( 0 );
        }
        else {
            // Get the conversion factor from the marketplace.
            const IInfo* marketInfo = scenario->getMarketplace()->getMarketInfo( mName, aRegionName, 0, true );
            const double convFactor = marketInfo->getDouble( "ConversionFactor", false );
            if( convFactor == 0 && isInputEnergyGood( mName, aRegionName ) ){
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::WARNING );
                mainLog << "Conversion factor of zero for energy input " << mName << "." << endl;
            }
            mConversionFactor.init( convFactor );
        }
    }
    return mConversionFactor;
}

/*! \brief Get the emissions gas coefficient for an input.
* \details This function returns the emissions gas coefficient for an input. To
*          determine the coefficient, the function checks the internally cached
*          value, and if that has not been set it queries the market info for
*          the good.
* \param aGHGName Name of the GHG to return the coefficient for.
* \param aRegionName The region name containing the input, necessary to check
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
        const IInfo* marketInfo = scenario->getMarketplace()->getMarketInfo( mName, aRegionName, 0, false );
        double coef = 0;
        if( marketInfo ){
            coef = marketInfo->getDouble( aGHGName + COEF_STRING, false );
        }
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
* \param aPeriod Model period.
* \return The demand for this input in physical units.
* \author Josh Lurz
*/
double Input::getDemandPhysical( const string& aRegionName,
                                 const int aPeriod ) const
{
    assert( isCapital() || mDemandCurrency[ aPeriod ].isInited() );
    return mDemandCurrency[ aPeriod ] * getConversionFactor( aRegionName );
}

//! Get the Currency Demand
double Input::getDemandCurrency( const int aPeriod ) const {
    assert( isCapital() || mIsFixedTrade || mDemandCurrency[ aPeriod ].isInited() );
    return mDemandCurrency[ aPeriod ];
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
void Input::setDemandCurrency( double aDemandCurrency,
                               const string& aRegionName,
                               const string& aSectorName,
                               int aPeriod ) 
{
    // For fixed trade, the value set to the demand currency is equal
    // to the read in fixed demand currency.
    assert( !mIsFixedTrade ||
        util::isEqual( aDemandCurrency, mDemandCurrency[ aPeriod ].get() ) );

    mDemandCurrency[ aPeriod ] = aDemandCurrency;
    Marketplace* marketplace = scenario->getMarketplace();
    /* Removing this check because it is activated by trade and the government. 
    if( aDemandCurrency < 0 ){
        cout << "Adding negative demand currency for input " << mName 
             << " from " << aSectorName << " in " << aRegionName << "." <<endl;
    }
    */
    marketplace->addToDemand( mName, aRegionName, aDemandCurrency, aPeriod );
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

/*!
 * \brief Returns the price paid for each input.
 * \param aPeriod Model period.
* \author Sonny Kim
*/
double Input::getPricePaid( const int aPeriod ) const {
    assert( mPricePaid[ aPeriod ].isInited() );
    return mPricePaid[ aPeriod ];
}
/*! \brief Set the price paid for each input.
* \param aPricePaid new price paid value
 * \param aPeriod Model period.
* \author Sonny Kim
*/
void Input::setPricePaid( double aPricePaid,
                          const int aPeriod )
{
    mPricePaid[ aPeriod ] = aPricePaid;
}
/*! \brief Returns the price received for an input.
* \details Queries the marketplace and get the price received from the market info.
* \param aRegionName Name of the region containing the input.
* \param aPeriod Period
* \return The price received for the input.
* \author Josh Lurz
*/
double Input::getPriceReceived( const string& aRegionName, const int aPeriod ) const {
    return FunctionUtils::getPriceReceived( aRegionName, mName, aPeriod );
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
    if( isInputEnergyGood( mName, aRegionName ) ){
        return ENERGY;
    }
    return MATERIAL;
}

/*! \brief Static function which returns whether a given input name is an energy
*          good.
* \param aInputName The input name.
* \param aRegionName The region name.
* \return Whether the input name is of an energy good.
*/
bool Input::isInputEnergyGood( const string& aInputName, const string& aRegionName ){
    assert( !aInputName.empty() && !aRegionName.empty() );
    assert( aInputName != "USA" );

    const IInfo* inputMarketInfo = scenario->getMarketplace()->getMarketInfo( aInputName,
                                                                              aRegionName,
                                                                              0, true );

    // Assume that goods without markets are not energy goods.
    return inputMarketInfo ? inputMarketInfo->getBoolean( "IsEnergyGood", true ) : false;
}

/*! \brief Static function which returns whether a given input name is a primary
*          energy good.
* \param aInputName The input name.
* \param aRegionName The region name.
* \return Whether the input name is of a primary energy good.
*/
bool Input::isInputPrimaryEnergyGood( const string& aInputName, const string& aRegionName ){
    assert( !aInputName.empty() && !aRegionName.empty() );
    assert( aInputName != "USA" );
    // This is called by GHG to determine if the technology is a primary good producer. Since consumers
    // also call this, and they do not have markets, this cannot ensure the market exists.
    const IInfo* inputMarketInfo = scenario->getMarketplace()->getMarketInfo( aInputName,
                                                                              aRegionName,
                                                                              0, false );
    // If it doesn't have a market it can't be a primary energy good.
    return inputMarketInfo ? inputMarketInfo->getBoolean( "IsPrimaryEnergyGood", true ) : false;
}  

/*! \brief Static function which returns whether a given input name is a secondary
*          energy good.
* \param aInputName The input name.
* \param aRegionName The region name.
* \return Whether the input name is of a secondary energy good.
*/
bool Input::isInputSecondaryEnergyGood( const string& aInputName, const string& aRegionName ){
    assert( !aInputName.empty() && !aRegionName.empty() );
    assert( aInputName != "USA" );

    const IInfo* inputMarketInfo = scenario->getMarketplace()->getMarketInfo( aInputName,
                                                                              aRegionName,
                                                                              0, true );
    // If it doesn't have a market it can't be a secondary energy good.
    return inputMarketInfo ? inputMarketInfo->getBoolean( "IsSecondaryEnergyGood", true ) : false;
}

/*! \brief Static function which returns the conversion factor for the good
*          which is stored in the marketplace.
* \param aInputName The input name.
* \param aRegionName The region name.
* \return The conversion factor for the input.
*/
double Input::getMarketConversionFactor( const string& aInputName, const string& aRegionName ){
    assert( !aInputName.empty() && !aRegionName.empty() );
    assert( aInputName != "USA" );

    const IInfo* inputMarketInfo = scenario->getMarketplace()->getMarketInfo( aInputName,
                                                                              aRegionName,
                                                                              0, true );
    return inputMarketInfo ? inputMarketInfo->getDouble( "ConversionFactor", true ) : 0;
}

/*! \brief For outputing SGM data to a flat csv File
 * 
 * \author Pralit Patel
 * \param period The period which we are outputing for
 */
void Input::csvSGMOutputFile( ostream& aFile, const int period ) const {
    aFile << mName << ',';
    aFile.precision(0);
    aFile << mDemandCurrency[ period ] << ',';
    aFile.precision(3);
    aFile << mPricePaid[ period ] << endl;
}

void Input::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitInput( this, aPeriod );
    aVisitor->endVisitInput( this, aPeriod );
}
