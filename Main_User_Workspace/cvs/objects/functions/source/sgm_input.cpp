/*
 * LEGAL NOTICE
 * This computer software was prepared by Battelle Memorial Institute,
 * hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
 * with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
 * CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
 * sentence must appear on any copies of this computer software.
 * 
 * EXPORT CONTROL
 * User agrees that the Software will not be shipped, transferred or
 * exported into any country or used in any manner prohibited by the
 * United States Export Administration Act or any other applicable
 * export laws, restrictions or regulations (collectively the "Export Laws").
 * Export of the Software may require some form of license or other
 * authority from the U.S. Government, and failure to obtain such
 * export control license may result in criminal liability under
 * U.S. laws. In addition, if the Software is identified as export controlled
 * items under the Export Laws, User represents and warrants that User
 * is not a citizen, or otherwise located within, an embargoed nation
 * (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
 *     and that User is not otherwise prohibited
 * under the Export Laws from receiving the Software.
 * 
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*! 
* \file sgm_input.cpp
* \ingroup Objects
* \brief The SGMInput class source file.
* \author Pralit Patel
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include <cmath>

#include "functions/include/sgm_input.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/configuration.h"
#include "functions/include/function_utils.h"
#include "containers/include/iinfo.h"
#include "util/base/include/ivisitor.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

// static initialize.
const string SGMInput::XML_REPORTING_NAME = "input-SGM";

//! Default Constructor
SGMInput::SGMInput(): mTypeFlags( 0 ){
}

//! Destructor
SGMInput::~SGMInput() {}

/*! \brief Get the XML name for reporting to XML file.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for reporting outputs and can be easily
* changed.
* \author Sonny Kim
* \return The constant XML_NAME.
*/
const string& SGMInput::getXMLReportingName() const{
    return XML_REPORTING_NAME;
}

//! Parse the SGMInput's XML.
void SGMInput::XMLParse( const xercesc::DOMNode* node ) {
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
            mCurrencyDemand.init( XMLHelper<double>::getValue( curr ) );
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

//! Complete the initialization of the SGMInput.
void SGMInput::completeInit( const string& aRegionName,
                             const string& aSectorName,
                             const string& aSubsectorName,
                             const string& aTechName,
                             DependencyFinder* aDependencyFinder , const IInfo* aTechInfo)
{
}

void SGMInput::initCalc( const string& aRegionName,
                         const string& aSectorName,
                         const bool aIsNewInvestmentPeriod,
                         const bool aIsTrade,
                         const int aPeriod )
{
    // TODO: Need to merge the changes that got the XML database to work.
    //       aIsTrade parameter needs to be used.
    /*! \pre There must be a valid region name. */
    assert( !aRegionName.empty() );
    initializeCachedCoefficients( aRegionName );
    initializeTypeFlags( aRegionName );
}

void SGMInput::copyParam( const IInput* aInput,
                          const int aPeriod )
{
    // Name must be already defined.
    mCoefficient.set( aInput->getCoefficient(-1 ) );
    mPricePaid.set( aInput->getPricePaid(  "", -1 ) );
    
    // Parameters which should not get copied forward if they didn't exist.
    mCurrencyDemand.init( aInput->getCurrencyDemand( -1 ) );
    mPriceAdjustFactor.init( aInput->getPriceAdjustment() );
    mTechnicalChange.init( aInput->getTechChange( -1 ) );
}

bool SGMInput::isSameType( const string& aType ) const {
    return aType == getXMLName();
}

//! Output to XML data
void SGMInput::toInputXML( ostream& out, Tabs* tabs ) const {
    // write the beginning tag.
    XMLWriteOpeningTag ( getXMLName(), out, tabs, mName );

    XMLWriteElement( mCoefficient, "coefficient", out, tabs );
    XMLWriteElement( mCurrencyDemand, "demandCurrency", out, tabs );
    XMLWriteElement( mConversionFactor, "conversionFactor", out, tabs );
    XMLWriteElement( mPriceAdjustFactor, "priceAdjustFactor", out, tabs );

    toInputXMLDerived( out, tabs );

    // write the closing tag.
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Output debug info to XML
void SGMInput::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
    // write the beginning tag.
    XMLWriteOpeningTag ( getXMLName(), out, tabs, mName );

    XMLWriteElement( mCoefficient, "coefficient", out, tabs );
    XMLWriteElement( mCurrencyDemand, "demandCurrency", out, tabs );
    XMLWriteElement( mPricePaid, "pricePaid", out, tabs );
    XMLWriteElement( mConversionFactor, "conversionFactor", out, tabs );
    XMLWriteElement( mPriceAdjustFactor, "priceAdjustFactor", out, tabs );

    toDebugXMLDerived( period, out, tabs );

    // write the closing tag.
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Get the name of the SGMInput
const string& SGMInput::getName() const {
    return mName;
}

/*! \brief Get the currency to physical conversion factor for the SGMInput.
* \details This function returns the conversion factor used to convert the SGMInput
*          from a currency to a physical demand.
* \param aPeriod Model period.
* \return The currency to physical conversion factor.
* \author Josh Lurz
*/
double SGMInput::getConversionFactor( const int aPeriod ) const {
    /*! \pre The conversion factor is initialized. */
    assert( mConversionFactor.isInited() );
    return mConversionFactor;
}

/*! \brief Get the emissions gas coefficient for an SGMInput.
* \details This function returns the emissions gas coefficent for an SGMInput.
* \param aGHGName Name of the GHG to return the coefficient for.
* \param aPeriod Model period.
* \return The emissions coefficient.
* \todo This doesn't work for multiple gases.
* \author Josh Lurz
*/
double SGMInput::getCO2EmissionsCoefficient( const string& aGHGName, const int aPeriod ) const {
    // Currently this assumes the only gas is CO2.
    assert( aGHGName == "CO2" );
    
    /*! \pre The CO2 coefficient is initialized. */
    assert( mCO2Coefficient.isInited() );
    return mCO2Coefficient;
}

/*! \brief Get the physical demand for the SGMInput.
* \details Return the quantity of demand for this SGMInput in physical units. This
*          is calculated by multiplying the currency demand by the conversion
*          factor for the SGMInput.
* \param aPeriod Model period.
* \return The demand for this SGMInput in physical units.
* \author Josh Lurz
*/
double SGMInput::getPhysicalDemand( const int aPeriod ) const {
    return mCurrencyDemand * getConversionFactor( aPeriod );
}

/*! \brief Get the carbon content for the SGMInput.
* \param aPeriod Model period.
* \return Carbon content of input.
* \author Sonny Kim
*/
double SGMInput::getCarbonContent( const int aPeriod ) const {
    return 0;
}

/*! \brief Get the Currency Demand.
* \param aPeriod Model period.
* \return Currency demand.
*/
double SGMInput::getCurrencyDemand( const int aPeriod ) const {
    return mCurrencyDemand;
}

/*!
 * \brief Get the input specific technical change.
 * \param aPeriod Model period.
 * \return The input specific technical change.
 * \author Josh Lurz
 */
double SGMInput::getTechChange( const int aPeriod ) const {
    return mTechnicalChange;
}

//! Set Currency Demand.
void SGMInput::setCurrencyDemand( double aCurrencyDemand, const string& aRegionName, const int aPeriod )
{
    mCurrencyDemand.set( aCurrencyDemand );
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->addToDemand( mName, aRegionName, mCurrencyDemand, aPeriod );
}

//! Set physical Demand.
void SGMInput::setPhysicalDemand( double aPhysicalDemand, const string& aRegionName,
                               const int aPeriod )
{
    // SGM inputs cannot directly set physical demands.
    assert( false );
}

/*! \brief Get the SGMInput-output coefficient.
* \param aPeriod Model period.
* \return The IO coefficient.
*/
double SGMInput::getCoefficient( const int aPeriod ) const {
    return mCoefficient;
}

/*! \brief Set the IO coefficient.
 * \author Pralit Patel
 * \param aCoefficient new coefficient value
 * \param aPeriod Model period.
 */
void SGMInput::setCoefficient( const double aCoefficient, const int aPeriod ) {
    assert( aCoefficient != 0 ); // Can't set coefficients to zero.
    mCoefficient.set( aCoefficient );
}

/*! \brief Return the market price, or unadjusted price, for the SGMInput.
* \param aRegionName Region containing the SGMInput.
* \param aPeriod Period to find the price in.
* \return The market or unadjusted price.
* \author Josh Lurz
*/
double SGMInput::getPrice( const string& aRegionName, const int aPeriod ) const {
    return scenario->getMarketplace()->getPrice( mName, aRegionName, aPeriod );
}

void SGMInput::setPrice( const string& aRegionName,
                         const double aPrice,
                         const int aPeriod )
{
    // SGM markets are solved so the input price cannot be set.
}

/*! \brief Returns the price paid for each SGMInput.
* \param aRegionName Name of the containing region.-
* \param aPeriod Model period.
* \author Sonny Kim
*/
double SGMInput::getPricePaid( const string& aRegionName, const int aPeriod ) const{
    return mPricePaid;
}
/*! \brief Set the price paid for each SGMInput.
*
* \param aPricePaid new price paid value
* \param aPeriod Model period.
* \author Sonny Kim
*/
void SGMInput::setPricePaid( double aPricePaid, const int aPeriod ) {
    mPricePaid.set( aPricePaid );
}
/*! \brief Returns the price received for an SGMInput.
* \details Queries the marketplace and get the price received from the market info.
* \param aRegionName Name of the region containing the SGMInput.
* \param aPeriod Period
* \return The price received for the SGMInput.
* \author Josh Lurz
*/
double SGMInput::getPriceReceived( const string& aRegionName, const int aPeriod ) const {
    return FunctionUtils::getPriceReceived( aRegionName, mName, aPeriod );
}

/*! \brief Returns the price adjustment.
* \note For all inputs other than capital this is a multiplier, for capital it is an adder.
* \author Sonny Kim
*/
double SGMInput::getPriceAdjustment() const {
    return mPriceAdjustFactor;
}

bool SGMInput::hasTypeFlag( const int aTypeFlag ) const {

    /*! \pre The type flags must be initialized. */
    assert( mTypeFlags != INITIALIZED );
    assert( mTypeFlags != 0 );

    return ( ( aTypeFlag & ~mTypeFlags ) == 0 );
}

void SGMInput::tabulateFixedQuantity( const string& aRegionName,
                                      const double aFixedOutput,
                                      const bool aIsInvestmentPeriod,
                                      const int aPeriod )
{
}

void SGMInput::scaleCalibrationQuantity( const double aScaleFactor ){
}

double SGMInput::getCalibrationQuantity( const int aPeriod ) const
{
    return -1;
}

/*! \brief Initialize the cached emissions coefficient and physical to energy conversion factor.
* \param aRegionName Region name.
*/
void SGMInput::initializeCachedCoefficients( const string& aRegionName ){
    // If the conversion factor has not been initialized, store the value from
    // the marketplace internally.
    if( !mConversionFactor.isInited() ){
        // Get the conversion factor from the marketplace.
        const IInfo* marketInfo = scenario->getMarketplace()->getMarketInfo( mName, aRegionName, 0, false );
        const double convFactor = marketInfo ? marketInfo->getDouble( "ConversionFactor", false ) : 0;
        if( convFactor == 0 && isEnergyGood( aRegionName ) ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Conversion factor of zero for energy SGMInput " << mName << "." << endl;
        }
        mConversionFactor.init( convFactor );
    }
    
    // If the coefficient has not been initialized, store the value from the
    // marketplace internally.
    if( !mCO2Coefficient.isInited() ){
        mCO2Coefficient.init( FunctionUtils::getCO2Coef( aRegionName, mName, 0 ) );
    }
}

/*! \brief Initialize the type flags.
* \details TODO
* \param aRegionName Name of the region.
*/
void SGMInput::initializeTypeFlags( const string& aRegionName ) {
    // Initialize the type.
    if( mName == "Land" ){
        mTypeFlags |= IInput::LAND;
        mTypeFlags |= IInput::FACTOR;
    }
    else if( mName == "Labor" ){
        mTypeFlags |=  IInput::LABOR;
        mTypeFlags |=  IInput::FACTOR;
    }
    else if( mName == "Capital" ){
        mTypeFlags |= IInput::CAPITAL;
        mTypeFlags |= IInput::FACTOR;
    }
    else {
        if( isEnergyGood( aRegionName ) ){
            mTypeFlags |= IInput::ENERGY;
        }
        else {
            mTypeFlags |= IInput::MATERIAL;
        }
    }
    // Inititialize the numeraire flag.
    const static string numeraireInputName = Configuration::getInstance()->getString( "numeraire-SGMInput", "ETE" );
    if( mName == numeraireInputName ){
        mTypeFlags |= IInput::NUMERAIRE;
    }
    
    // Initialize primary and secondary energy flags.
    if( isPrimaryEnergyGood( aRegionName ) ){
        mTypeFlags |= IInput::PRIMARY;
    }
    else if( isSecondaryEnergyGood( aRegionName ) ){
        mTypeFlags |= IInput::SECONDARY;
    }
    // Set that the type flags are initialized.
    mTypeFlags |= IInput::INITIALIZED;
}

/*! \brief Return whether a good is an energy good.
* \param aGoodName Good name.
* \return Whether the good is an energy price good.
*/
bool SGMInput::isEnergyGood( const string& aRegionName ) const
{
    const IInfo* marketInfo = scenario->getMarketplace()->getMarketInfo( mName, aRegionName,
                                                                         0, false );
    return marketInfo && marketInfo->getBoolean( "IsEnergyGood", false );
}

/*! \brief Return whether a good is a primary energy good.
* \param aRegionName Region name.
* \param aGoodName Good name.
* \return Whether the good is a primary energy price good.
*/
bool SGMInput::isPrimaryEnergyGood( const string& aRegionName ) const
{
    const IInfo* marketInfo = scenario->getMarketplace()->getMarketInfo( mName, aRegionName,
                                                                         0, false );
    return marketInfo && marketInfo->getBoolean( "IsPrimaryEnergyGood", false );
}

/*! \brief Return whether a good is a secondary energy good.
* \param aRegionName Region name.
* \return Whether the good is a secondary energy price good.
*/
bool SGMInput::isSecondaryEnergyGood( const string& aRegionName ) const
{
    const IInfo* marketInfo = scenario->getMarketplace()->getMarketInfo( mName, aRegionName,
                                                                         0, false );
    return marketInfo && marketInfo->getBoolean( "IsSecondaryEnergyGood", false );
}

/*! \brief For outputing SGM data to a flat csv File
 * 
 * \author Pralit Patel
 * \param period The period which we are outputing for
 */
void SGMInput::csvSGMOutputFile( ostream& aFile, const int period ) const {
    aFile << mName << ',';
    aFile.precision(0);
    aFile << mCurrencyDemand << ',';
    aFile.precision(3);
    aFile << mPricePaid << endl;
}

void SGMInput::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitSGMInput( this, aPeriod );
    aVisitor->endVisitSGMInput( this, aPeriod );
}
