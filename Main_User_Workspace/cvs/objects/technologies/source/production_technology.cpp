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
* \file production_technology.cpp
* \ingroup Objects
* \brief The ProductionTechnology class source file.
* \author Pralit Patel
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <xercesc/dom/DOMNode.hpp>

#include "technologies/include/production_technology.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/util.h"
#include "containers/include/national_account.h"
#include "functions/include/iinput.h"
#include "functions/include/ifunction.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "functions/include/function_manager.h"
#include "sectors/include/more_sector_info.h"
#include "util/base/include/ivisitor.h"
#include "investment/include/iexpected_profit_calculator.h"
#include "emissions/include/aghg.h"
#include "technologies/include/technology_type.h"
#include "functions/include/function_utils.h"
#include "technologies/include/ishutdown_decider.h"
#include "technologies/include/profit_shutdown_decider.h"
#include "technologies/include/ioutput.h"
#include "containers/include/iinfo.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

typedef vector<AGHG*>::const_iterator CGHGIterator;
typedef vector<AGHG*>::iterator GHGIterator;

//! Default Constructor
ProductionTechnology::ProductionTechnology() {
    const int maxper = scenario->getModeltime()->getmaxper();
    mCostsReporting.resize( maxper );
    mProfits.resize( maxper );

    // Resize the cached value to the number of types in the CacheValue enum.
    mCachedValues.resize( END );

    mExpectedProfitRateReporting = 0;
    capital = 0;
    indBusTax = 0;
    sigma1 = 0;
    sigma2 = 0;
    lifeTime = 0;
    delayedInvestTime = 0;
    maxLifeTime = 0;
    retrofitLifeTime = 0;
    periodIniInvest = 0;
    periodInvestUnallowed = 0;
    mAnnualInvestment = 0;
    mBasePhysicalOutput = 0;
    mConversionFactor = 1;
    mFixedInvestment = -1;
    alphaZeroScaler = 0;
    currSigma = 0;
    mParentTechType = 0;
    mValidCachePeriod = -1;
}

//! Destructor
ProductionTechnology::~ProductionTechnology(){
}

void ProductionTechnology::copyParam( const BaseTechnology* baseTech,
                                      const int aPeriod ) {
    BaseTechnology::copyParam( baseTech, aPeriod );
    baseTech->copyParamsInto( *this, aPeriod );
}

void ProductionTechnology::copyParamsInto( ProductionTechnology& prodTechIn,
                                           const int aPeriod ) const {
     prodTechIn.alphaZeroScaler = alphaZeroScaler;
     prodTechIn.indBusTax = indBusTax;
     prodTechIn.sigma1 = sigma1;
     prodTechIn.sigma2 = sigma2;
     prodTechIn.mConversionFactor = mConversionFactor;
     prodTechIn.lifeTime = lifeTime;
     prodTechIn.delayedInvestTime = delayedInvestTime;
     prodTechIn.maxLifeTime = maxLifeTime;
     prodTechIn.retrofitLifeTime = retrofitLifeTime;
     prodTechIn.periodIniInvest = periodIniInvest;
     prodTechIn.periodInvestUnallowed = periodInvestUnallowed;
     prodTechIn.currSigma = currSigma; // not sure
}

ProductionTechnology* ProductionTechnology::clone() const {
    return new ProductionTechnology( *this );
}

const string& ProductionTechnology::getXMLName() const {
    return getXMLNameStatic();
}

const string& ProductionTechnology::getXMLNameStatic() {
    const static string XML_NAME = "productionTechnology";
    return XML_NAME;
}

//! Parse xml file for data
bool ProductionTechnology::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    if ( nodeName == "lifeTime" ) {
        lifeTime = XMLHelper<int>::getValue( curr );
    }
    //\todo this is capital stock change name
    else if (nodeName == "capital" ) {
        capital = XMLHelper<double>::getValue( curr );
    }
    else if (nodeName == "indirectBusinessTax" ) {
        indBusTax = XMLHelper<double>::getValue( curr );
    }
    else if (nodeName == "Sigma1" ) {
        sigma1 = XMLHelper<double>::getValue( curr );
    }
    else if (nodeName == "Sigma2" ) {
        sigma2 = XMLHelper<double>::getValue( curr );
    }
    else if (nodeName == "basePhysicalOutput" ) {
        mBasePhysicalOutput = XMLHelper<double>::getValue( curr );
    }
    else if (nodeName == "delayedInvestTime" ) {
        delayedInvestTime = XMLHelper<int>::getValue( curr );
    }
    else if (nodeName == "maxLifeTime" ) {
        maxLifeTime = XMLHelper<int>::getValue( curr );
    }
    else if (nodeName == "retrofitLifeTime" ) {
        retrofitLifeTime = XMLHelper<int>::getValue( curr );
    }
    else if (nodeName == "periodIniInvest" ) {
        periodIniInvest = XMLHelper<int>::getValue( curr );
    }
    else if (nodeName == "periodInvestUnallowed" ) {
        periodInvestUnallowed = XMLHelper<int>::getValue( curr );
    }
    else if (nodeName == "investmentAnnual" ) {
        mAnnualInvestment = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "fixed-investment" ){
        mFixedInvestment = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "technicalChangeHicks" ){
        mTechChange.mHicksTechChange = XMLHelper<double>::getValue( curr );
    }
    else if (nodeName == "technicalChangeEnergy" ) {
        mTechChange.mEnergyTechChange = XMLHelper<double>::getValue( curr );
    }
    else if (nodeName == "technicalChangeMaterial" ) {
        mTechChange.mMaterialTechChange = XMLHelper<double>::getValue( curr );
    }
    else {
        return false;
    }
    return true;
}

//! For derived classes to output XML data
void ProductionTechnology::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    // Note: We should be using checkDefault version of this function.
    XMLWriteElement(lifeTime, "lifeTime", out, tabs );
    XMLWriteElement(capital, "capital", out, tabs );
    XMLWriteElement(indBusTax, "indBusTax", out, tabs );
    XMLWriteElement(sigma1, "Sigma1", out, tabs );
    XMLWriteElement(sigma2, "Sigma2", out, tabs );
    XMLWriteElement(mBasePhysicalOutput, "basePhysicalOutput", out, tabs );
    XMLWriteElement(delayedInvestTime, "delayedInvestTime", out, tabs );
    XMLWriteElement(maxLifeTime, "maxLifeTime", out, tabs );
    XMLWriteElement(retrofitLifeTime, "retrofitLifeTime", out, tabs );
    XMLWriteElement(periodIniInvest, "periodIniInvest", out, tabs );
    XMLWriteElement(periodInvestUnallowed, "periodInvestUnallowed", out, tabs );
    XMLWriteElement(mAnnualInvestment, "investmentAnnual", out, tabs );
    XMLWriteElementCheckDefault( mFixedInvestment, "fixed-investment", out, tabs, -1.0 );
    XMLWriteElement( mTechChange.mEnergyTechChange, "technicalChangeEnergy", out, tabs );
    XMLWriteElement( mTechChange.mMaterialTechChange, "technicalChangeMaterial", out, tabs );
    XMLWriteElement( mTechChange.mHicksTechChange, "technicalChangeHicks", out, tabs );
}

//! Output debug info for derived class
void ProductionTechnology::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    XMLWriteElement(lifeTime, "lifeTime", out, tabs );
    XMLWriteElement(capital, "capital", out, tabs );
    XMLWriteElement(indBusTax, "indBusTax", out, tabs );
    XMLWriteElement(alphaZeroScaler, "alphaZero", out, tabs );
    XMLWriteElement(sigma1, "Sigma1", out, tabs );
    XMLWriteElement(sigma2, "Sigma2", out, tabs );
    XMLWriteElement(mBasePhysicalOutput, "basePhysicalOutput", out, tabs );
    XMLWriteElement(delayedInvestTime, "delayedInvestTime", out, tabs );
    XMLWriteElement(maxLifeTime, "maxLifeTime", out, tabs );
    XMLWriteElement(retrofitLifeTime, "retrofitLifeTime", out, tabs );
    XMLWriteElement(periodIniInvest, "periodIniInvest", out, tabs );
    XMLWriteElement(periodInvestUnallowed, "periodInvestUnallowed", out, tabs );
    XMLWriteElement(mAnnualInvestment, "investmentAnnual", out, tabs );
    XMLWriteElement( mFixedInvestment, "fixed-investment", out, tabs );
    XMLWriteElement( mTechChange.mEnergyTechChange, "technicalChangeEnergy", out, tabs );
    XMLWriteElement( mTechChange.mMaterialTechChange, "technicalChangeMaterial", out, tabs );
    XMLWriteElement( mTechChange.mHicksTechChange, "technicalChangeHicks", out, tabs );
}

void ProductionTechnology::completeInit( const string& aRegionName,
                                         const string& aSectorName,
                                         const string& aSubsectorName )
{
    prodDmdFnType = "CES";
    currSigma = sigma1;
    BaseTechnology::completeInit( aRegionName, aSectorName, aSubsectorName );
}

void ProductionTechnology::updateMarketplace( const string& sectorName, const string& regionName,
                                              const int period )
{
    BaseTechnology::updateMarketplace( sectorName, regionName, period );
}

void ProductionTechnology::initCalc( const MoreSectorInfo* aMoreSectorInfo, const string& aRegionName,
                                    const string& aSectorName, NationalAccount& nationalAccount,
                                    const Demographic* aDemographics, const double aCapitalStock, const int aPeriod )
{
    const int BASE_PERIOD = 0; // for base period only
    // calculate currency to energy conversion factor base on base year physical output
    // Warning: using getDemandSum in the base year gives correct total but is incorrect in future years 
    // due to capital row that is not updated
    // TODO: make sure that this reordering is safe, we need to calculate the mConversionFactor before
    //  SGMOutput::initCalc is called
    if ( aPeriod == BASE_PERIOD ) {
        if(  mBasePhysicalOutput != 0 ) {
            // 1000 is for conversion from PJ to EJ, use Value class for this
            mConversionFactor = mBasePhysicalOutput / 1000 / 
                ( FunctionUtils::getCurrencyDemandSum( input, aPeriod ) + indBusTax );
        } else {
            IInfo* marketInfo = scenario->getMarketplace()->getMarketInfo( aSectorName, aRegionName, 0, true );
            mConversionFactor = marketInfo->getDouble( "ConversionFactor", false );
        }
    }
    // set the conversion factor in the market info so that the output can get it
    IInfo* marketInfo = scenario->getMarketplace()->getMarketInfo( aSectorName, aRegionName, 0, true );
    double convBefore = marketInfo->getDouble( "ConversionFactor", false );
    marketInfo->setDouble( "ConversionFactor", mConversionFactor );
            
    BaseTechnology::initCalc( aMoreSectorInfo, aRegionName, aSectorName,
                              nationalAccount, aDemographics, aCapitalStock,
                              aPeriod );
    marketInfo->setDouble( "ConversionFactor", convBefore );

    // Setup the cached values for the period.
    mValidCachePeriod = aPeriod;
    mCachedValues[ AVAILABLE ] = calcIsAvailable( aPeriod );
    mCachedValues[ RETIRED ] = calcIsRetired( aPeriod );
    mCachedValues[ NEW_INVESTMENT ] = calcIsNewInvestment( aPeriod );

    // Only do full initialization for active vintages.
    if( isAvailable( aPeriod ) && !isRetired( aPeriod ) ){
        // Define constants.
        const double LEONTIEF_THRESHOLD = 0.05;
        // Does not require Coefficients
        // Its the first year of the technology.
        // Make sure the current sigma is set to sigma1.
        if( year == scenario->getModeltime()->getper_to_yr( aPeriod ) ){
            currSigma = sigma1; // this would normally happen in complete init, but it is not always called.
        }
        // Apply technical change to all technologies and all vintages after base period
        if( aPeriod > BASE_PERIOD && year == scenario->getModeltime()->getper_to_yr(aPeriod) ) {
            // Apply technical change to input coefficients and alpha zero scaler.
            alphaZeroScaler = prodDmdFn->applyTechnicalChange( input, mTechChange, aRegionName,
                aSectorName, aPeriod,
                alphaZeroScaler, currSigma );
        }

        if( aPeriod == BASE_PERIOD ) {
            for(unsigned int i= 0; i<input.size(); i++) {
                if (input[i]->getName() == "Land") {
                    nationalAccount.addToAccount(NationalAccount::LAND_RENTS, input[i]->getCurrencyDemand( aPeriod ));
                }
                else if (input[i]->getName() == "Labor") {
                    nationalAccount.addToAccount(NationalAccount::LABOR_WAGES, input[i]->getCurrencyDemand( aPeriod ));
                }
            }

            prodDmdFnType = "CES";
            currSigma = sigma1;

            if( aCapitalStock <= 0 ){
                cout << "Warning: Trying to calculate an alpha zero in the base period with a zero level of capital." << endl;
            }
            alphaZeroScaler = prodDmdFn->calcCoefficient( input, 0, aRegionName, aSectorName, aPeriod, currSigma,
                indBusTax, aCapitalStock ); 
            assert( util::isValidNumber( alphaZeroScaler ) );
        }
        // Convert all vintages, not just last.
        else if( ( year == scenario->getModeltime()->getper_to_yr(aPeriod - 1) ) || 
            ( year < scenario->getModeltime()->getper_to_yr(0) && (aPeriod == 1) ) ) 
        { // transform base and pre-base period vintages in period 1.
            double priceReceivedLastPer = FunctionUtils::getPriceReceived( aRegionName, aSectorName, aPeriod - 1 );
            currSigma = sigma2;
            double sigmaNew = sigma1;
            double sigmaOld = sigma2;

            // Coefficients are already transformed to use elasticity of substitution directly
            // Check if we should convert to leontief
            if( currSigma < LEONTIEF_THRESHOLD ){
                prodDmdFnType = "Leontief";
                prodDmdFn = FunctionManager::getFunction( "Leontief" );
                // Transform CES coefficients to Leontief.
                alphaZeroScaler = prodDmdFn->changeElasticity( input, aRegionName, priceReceivedLastPer,
                                                               mProfits[ aPeriod - 1 ], capital, aPeriod,
                                                               alphaZeroScaler, sigmaNew, sigmaOld );
            }
            else if( sigma1 != sigma2 ){
                // Transform coefficients to use short-term instead of long-term elasticity of substitution.
                alphaZeroScaler = prodDmdFn->changeElasticity( input, aRegionName, priceReceivedLastPer,
                                                               mProfits[ aPeriod - 1 ], capital, aPeriod,
                                                               alphaZeroScaler, sigmaNew, sigmaOld );
            }    
        }
    }
}

/*! \brief Operate the technology.
* \author Josh Lurz
* \param aNationalAccount Regional national accounts container.
* \param aDemographic Regional demographic information.
* \param aMoreSectorInfo Sector information container.
* \param aRegionName Region name.
* \param aSectorName Sector name.
* \param aIsNewVintageMode Whether to operate only old technologies, or new and old technologies.
* \param aPeriod Period to operate in.
* \note sets the output value for the current period. Also sets input values.
*/

void ProductionTechnology::operate( NationalAccount& aNationalAccount, const Demographic* aDemographic, 
                                   const MoreSectorInfo* aMoreSectorInfo, const string& aRegionName, 
                                   const string& aSectorName, const bool aIsNewVintageMode, const int aPeriod )
{
    // Always operate old techs and operate old techs if they are still .
    if( isAvailable( aPeriod ) && !isRetired( aPeriod ) ){
        assert( !input.empty() );
        // calculate prices paid for technology inputs
        BaseTechnology::calcPricePaid( aMoreSectorInfo, aRegionName, aSectorName, aPeriod );
        expenditures[ aPeriod ].reset();
        if( ( aIsNewVintageMode && isNewInvestment( aPeriod ))
            || (!aIsNewVintageMode && !isNewInvestment( aPeriod )) )
        {

            // \todo capital here is capital stock, change name
            double shutdownCoef = calcShutdownCoef( aRegionName, aSectorName, aPeriod );

            // Add wages and land rents to national account
            for( unsigned int i = 0; i < input.size(); i++ ) {
                if( input[i]->hasTypeFlag( IInput::LABOR ) ) {
                    double wages = input[i]->getCurrencyDemand( aPeriod ) 
                        * input[ i ]->getPrice( aRegionName, aPeriod );
                    expenditures[ aPeriod ].addToType( Expenditure::WAGES, wages );
                    aNationalAccount.addToAccount(NationalAccount::LABOR_WAGES, wages );
                }
                else if( input[i]->hasTypeFlag( IInput::LAND ) ) {
                    double landRents = input[i]->getCurrencyDemand( aPeriod )
                                     * input[ i ]->getPrice( aRegionName, aPeriod );
                    expenditures[ aPeriod ].addToType(Expenditure::LAND_RENTS, landRents );
                    aNationalAccount.addToAccount(NationalAccount::LAND_RENTS, landRents );
                }
                else if( !input[ i ]->hasTypeFlag( IInput::CAPITAL ) ){
                    expenditures[ aPeriod ].addToType( Expenditure::INTERMEDIATE_INPUTS,
                                           input[i]->getCurrencyDemand( aPeriod ) );
                }
            }
            
             // TODO: Move this to setInvestment
            // Add annual investment in new production technology to National Accounts
            // and to marketplace demand for capital or annual investments, coordinate with Josh
            // Not for old vintages where investment does not occur
            if( aIsNewVintageMode && isNewInvestment( aPeriod ) ){
                aNationalAccount.addToAccount(NationalAccount::ANNUAL_INVESTMENT, mAnnualInvestment );
            }
            
            // Calculate output of Production Technology
            double primaryOutput = prodDmdFn->calcOutput( input, aRegionName, aSectorName, shutdownCoef,
                                                          aPeriod, capital, alphaZeroScaler, currSigma );

            assert( primaryOutput >= 0 );
            mOutputs[ 0 ]->setCurrencyOutput( aRegionName, primaryOutput, aPeriod );
            expenditures[ aPeriod ].addToType( Expenditure::SALES, primaryOutput );

            // calculate taxes and subsidies
            calcTaxes( aNationalAccount, aMoreSectorInfo, aRegionName, aSectorName, aPeriod );
            calcEmissions( aSectorName, aRegionName, aPeriod );
        }
    } // if operational
}

void ProductionTechnology::calcEmissions( const string& aGoodName, const string& aRegionName, const int aPeriod )
{
    // Loop over GHGs and calculate emissions.
    for( GHGIterator ghg = mGhgs.begin(); ghg != mGhgs.end(); ++ghg ){
        (*ghg)->calcEmission( aRegionName, input, mOutputs, 0, 0, aPeriod );
    }
}

/*! \brief Set a level of new investment for a given period.
* \param aRegionName Region name.
* \param aAnnualInvestment The level of annual investment at the end of the time period.
* \param aTotalInvestment The level of new investment.
* \param aPeriod The period in which to add investment.
* \return The level of new investment actually set.
* \author Josh Lurz
*/
double ProductionTechnology::setInvestment( const string& aRegionName, const double aAnnualInvestment,
                                            const double aTotalInvestment, const int aPeriod )
{
    /*! \pre Annual investment is greater than zero, total investment is greater than zero,
    and the period is greater than zero. */
    assert( aAnnualInvestment >= 0 );
    assert( aTotalInvestment >= 0 );
    assert( aPeriod > 0 );
    assert( util::isValidNumber( aAnnualInvestment ) );
    assert( util::isValidNumber( aTotalInvestment ) );

    // Check to make sure the technology year is the same as the period in which 
    // we are trying to set investment.
    assert( year == scenario->getModeltime()->getper_to_yr( aPeriod ) );

    // Set the annual investment.
    mAnnualInvestment = aAnnualInvestment;

    // Set capital level to the total investment.
    capital = aTotalInvestment;
    
    // This is done in operate currently.
    // set investment demand as capital demand in the marketplace.
    // the capital input is OVA, do not override that.
    //scenario->getMarketplace()->addToDemand( "Capital", aRegionName, mAnnualInvestment, aPeriod );

    return mAnnualInvestment;
}

void ProductionTechnology::calcTaxes( NationalAccount& aNationalAccount, const MoreSectorInfo* aMoreSectorInfo,
                                     const string& aRegionName, const string aSectorName, const int aPeriod )
{
    double investTaxCreditRate = aMoreSectorInfo->getValue(MoreSectorInfo::INVEST_TAX_CREDIT_RATE);
    double investmentTaxCredit = investTaxCreditRate * mAnnualInvestment;
    double indBusTaxRate = aMoreSectorInfo->getValue(MoreSectorInfo::IND_BUS_TAX_RATE);
    Marketplace* marketplace = scenario->getMarketplace();
    indBusTax = indBusTaxRate * mOutputs[ 0 ]->getCurrencyOutput( aPeriod ) 
    * FunctionUtils::getPriceReceived( aRegionName, aSectorName, aPeriod );
    
                // calculate profits
    if( capital <= 0 ){
        cout << "Error: Negative or zero capital in production technology: " << name << " in vintage " << year << endl;
    }

    double shutdownCoef = calcShutdownCoef( aRegionName, aSectorName, aPeriod );

    double profits = prodDmdFn->calcProfits( input, aRegionName, aSectorName, shutdownCoef, aPeriod, capital,
        alphaZeroScaler, currSigma );
    assert( util::isValidNumber( profits ) );
    assert( profits >= 0 );

    // corporate income tax rate
    // add other value added to rentals, does not include land and labor
    expenditures[ aPeriod ].addToType( Expenditure::RENTALS, profits );
    double corpIncomeTaxRate = aMoreSectorInfo->getValue(MoreSectorInfo::CORP_INCOME_TAX_RATE);
    double corpIncomeTax = corpIncomeTaxRate * profits - investmentTaxCredit;

    double reRate = aMoreSectorInfo->getValue(MoreSectorInfo::MAX_CORP_RET_EARNINGS_RATE)
        *(1 - exp(aMoreSectorInfo->getValue(MoreSectorInfo::RET_EARNINGS_PARAM)
        *marketplace->getPrice("Capital",aRegionName,aPeriod)));

    double dividends = (profits * (1 - corpIncomeTaxRate) + investmentTaxCredit) * (1 - reRate);
    double retainedEarnings = (profits * (1 - corpIncomeTaxRate) + investmentTaxCredit) * reRate;
    
    // All retained earnings to marketplace capital supply
    assert( retainedEarnings >= 0 );
    assert( util::isValidNumber( retainedEarnings ) );

    marketplace->addToSupply( "Capital", aRegionName, retainedEarnings, aPeriod );
    // Add all taxes and accounts to expenditure
    expenditures[ aPeriod ].setType(Expenditure::INDIRECT_TAXES, indBusTax);
    expenditures[ aPeriod ].setType(Expenditure::DIRECT_TAXES, corpIncomeTax);
    expenditures[ aPeriod ].setType(Expenditure::DIVIDENDS, dividends);
    expenditures[ aPeriod ].setType(Expenditure::RETAINED_EARNINGS, retainedEarnings);
    // Add all taxes to national account
    aNationalAccount.addToAccount(NationalAccount::RETAINED_EARNINGS, retainedEarnings);
    aNationalAccount.addToAccount(NationalAccount::DIVIDENDS, dividends);
    aNationalAccount.addToAccount(NationalAccount::INDIRECT_BUSINESS_TAX, indBusTax);
    aNationalAccount.addToAccount(NationalAccount::CORPORATE_INCOME_TAXES, corpIncomeTax);
    // Add corporate profits to national account
    aNationalAccount.addToAccount(NationalAccount::CORPORATE_PROFITS, profits);

}

/*! \brief Returns the capital stock of the technology.
* \return The capital stock.
* \author Sonny Kim
*/
double ProductionTechnology::getCapital() const {
    assert( util::isValidNumber( capital ) );
    assert( capital >= 0 );
    return capital;
}

/*! \brief Get the annual investment for this technology.
* \param aPeriod Period to get the annual investment for. If it is -1 return any
*        annual investment.
* \return The annual investment for this technology. Not applicable to all
*         technologies.
* \author Josh Lurz
*/
double ProductionTechnology::getAnnualInvestment( const int aPeriod ) const {
    // Check if the technology had any investment in the period. The -1 flag
    // specifies that the technology should not check that the period matches
    // the period when this vintage is new.
    if( aPeriod == -1 || isNewInvestment( aPeriod ) ){
        return mAnnualInvestment;
    }
    return 0;
}

/*! \brief Returns the expected profit rate of the the technology.
* \param aNationalAccount The regional accounting object.
* \param aRegionName The name of the region containing this subsector.
* \param aSectorName The name of the sector containing this subsector.
* \param aExpProfitRateCalc The calculator of expected profit rates.
* \param aInvestmentLogitExp The investment logit exponential.
* \param aIsShareCalc Whether this expected profit rate is being used to
*        calculate shares. Not great.
* \param aPeriod The period for which to calculate expected profit.
* \return The expected profit rate.
* \author Josh Lurz
*/
double ProductionTechnology::getExpectedProfitRate( const NationalAccount& aNationalAccount,
                                                    const string& aRegionName,
                                                    const string& aSectorName,
                                                    const IExpectedProfitRateCalculator* aExpProfitRateCalc,
                                                    const double aInvestmentLogitExp,
                                                    const bool aIsShareCalc,
                                                    const int aPeriod ) const
{
    // If its not operational or it is a fixed investment the expected profit
    // rate is 0.
    if( !isNewInvestment( aPeriod ) || mFixedInvestment != -1 ){
        return 0;
    }
    
    // Calculate the number of years in the lifetime. This would not be correct
    // with variable time steps. This would cause problems with variable
    // time steps.
    const int timeStep = scenario->getModeltime()->gettimestep( aPeriod );
    const int lifetimeYears = lifeTime * timeStep;
    
    // Create the structure of info for the production function.
    ProductionFunctionInfo prodFunc = { input, prodDmdFn, currSigma, alphaZeroScaler, capital };

    // Use the expected profit visitor to determine the expected profit rate.
    double finalExpectedProfit = aExpProfitRateCalc->calcTechnologyExpectedProfitRate( prodFunc,
                                                                                       aNationalAccount,
                                                                                       aRegionName,
                                                                                       aSectorName,
                                                                                       delayedInvestTime,
                                                                                       lifetimeYears,
                                                                                       timeStep,
                                                                                       aPeriod );
    assert( finalExpectedProfit >= 0 );
    return finalExpectedProfit;
}

/*! \brief Get the amount of capital required to produce one unit of output.
* \param aDistributor Investment distributor.
* \param aExpProfitRateCalc Expected profit rate calculator.
* \param aNationalAccount Regional national accounts container.
* \param aRegionName The name of the region.
* \param aSectorName The name of the sector.
* \param aPeriod The period.
* \return The capital output ratio.
* \author Josh Lurz 
*/
double ProductionTechnology::getCapitalOutputRatio( const IDistributor* aDistributor,
                                                    const IExpectedProfitRateCalculator* aExpProfitRateCalc,
                                                    const NationalAccount& aNationalAccount,
                                                    const string& aRegionName,
                                                    const string& aSectorName, 
                                                    const int aPeriod ) const
{
    // Only new investment has a capital to output ratio.
    if( isNewInvestment( aPeriod ) ){
        // Calculate the number of years in the lifetime. This would not be
        // correct with variable time steps.
        const double lifetimeYears = lifeTime * scenario->getModeltime()->gettimestep( aPeriod );
        return prodDmdFn->getCapitalOutputRatio( input, aRegionName, aSectorName,
                                                 lifetimeYears, aPeriod, alphaZeroScaler,
                                                 currSigma );
    }
    return 0;
}

/*! \brief Get the quantity of fixed investment for the production technology.
* \author Josh Lurz
* \param aPeriod The period for which to get fixed investment.
* \return Fixed investment amount for the vintage.
*/
double ProductionTechnology::getFixedInvestment( const int aPeriod ) const {
    // The -1 flag specified the value wasn't read in, return zero in that case.
    // Use a value class here.
    if( isNewInvestment( aPeriod ) && mFixedInvestment != -1 ){
        return mFixedInvestment;
    }
    return 0;
}

/*! \brief Distribute investment to the vintage.
* \param aDistributor The distributor of new investment.
* \param aNationalAccount The national accounts.
* \param aExpProfitRateCalc The calculator of expected profit rates.
* \param aRegionName The name of the containing region.
* \param aSectorName The name of the containing sector.
* \param aNewInvestment The amount of new investment.
* \param aPeriod The period in which to add investment.
* \return The total amount of investment distributed.
* \author Josh Lurz
*/
double ProductionTechnology::distributeInvestment( const IDistributor* aDistributor,
                                                   NationalAccount& aNationalAccount,
                                                   const IExpectedProfitRateCalculator* aExpProfitRateCalc,
                                                   const string& aRegionName,
                                                   const string& aSectorName,
                                                   const double aNewInvestment,
                                                   const int aPeriod )
{
    assert( mParentTechType );
    // Use the parent technology type to distribute investment. This will
    // eventually call back into this object. Check if investment is fixed.
    double actInvestment = aNewInvestment;
    if( mFixedInvestment != -1 ){
        actInvestment = mFixedInvestment;
        if( !util::isEqual( aNewInvestment, 0.0 ) ){
            cout << "Error: Vintage " << name 
                 << " received investment to distribute but it has fixed investment. " << endl;
        }
    }
    if( actInvestment > 0 ){
        assert( scenario->getModeltime()->getyr_to_per( year ) == aPeriod );
        return mParentTechType->setTotalInvestment( aRegionName,
                                                    year - scenario->getModeltime()->gettimestep( aPeriod ),
                                                    year, actInvestment, aPeriod );
    }
    // Return that no investment was done.
    return 0;
}

/*! \brief Function to finalize objects after a period is solved.
* \details This function is used to calculate and store variables which are only
*          needed after the current period is complete.
* \param aRegionName Region name.
* \param aSectorName Sector name.
* \param aPeriod The period to finalize.
* \todo Finish this function, could move transform here.
* \author Josh Lurz
*/
void ProductionTechnology::postCalc( const string& aRegionName, const string& aSectorName, 
                                     const int aPeriod )
{
    if( !isRetired( aPeriod ) && year <= scenario->getModeltime()->getper_to_yr( aPeriod ) ){
        // Save cost, profit rate, and output. Profit rate is needed to convert
        // elasticities, not just for reporting. Calculate the number of years
        // in the lifetime. This would not be correct with variable time steps.
        double shutdownCoef = calcShutdownCoef( aRegionName, aSectorName, aPeriod );
        mProfits[ aPeriod ] = prodDmdFn->calcProfits( input, aRegionName, aSectorName, shutdownCoef,
                                                      aPeriod, capital,
                                                      alphaZeroScaler, currSigma );
        assert( mProfits[ aPeriod ] >= 0 );
        mCostsReporting[ aPeriod ] = prodDmdFn->calcCosts( input, aRegionName, alphaZeroScaler, aPeriod );

        // If this is new investment store the expected price received and
        // expected profit rate.
        if( isNewInvestment( aPeriod ) ){
            // This is the unadjusted expected profit rate, doesn't use helper
            // object. Calculate the number of years in the lifetime. This would
            // not be correct with variable time steps.
            const double lifetimeYears = lifeTime * scenario->getModeltime()->gettimestep( aPeriod );
            mExpectedProfitRateReporting = prodDmdFn->calcExpProfitRate( input, aRegionName, aSectorName,
                lifetimeYears, aPeriod,
                alphaZeroScaler, currSigma );
        }
    }
}

void ProductionTechnology::csvSGMOutputFile( ostream& aFile, const int period ) const {
    // print for all operating vintages
    if( isAvailable( period ) && !isRetired( period ) ){
        // BaseTechnology::csvSGMOutputFile( aFile, period );
    }
}

void ProductionTechnology::accept( IVisitor* aVisitor, const int aPeriod ) const
{
    aVisitor->startVisitProductionTechnology( this, aPeriod );
    BaseTechnology::accept( aVisitor, aPeriod );
    aVisitor->endVisitProductionTechnology( this, aPeriod );
}

//! Set the parent technology type helper object. This may change.
void ProductionTechnology::setTypeHelper( TechnologyType* aTechType ){
    assert( aTechType );
    mParentTechType = aTechType;
}

/*! \brief Calculate the coefficient used to shutdown older unprofitable
*          vintages.
* \details This coefficient is always one for new vintages. MORE HERE.
* \param aRegionName Name of the region containing the vintage.
* \param aSectorName Name of the sector containing the vintage.
* \param aPeriod Period
* \return Coefficient which scales down unprofitable vintages.
* \author Josh Lurz
*/
double ProductionTechnology::calcShutdownCoef( const string& aRegionName,
                                               const string& aSectorName,
                                               const int aPeriod ) const
{
    // Never shutdown new investment. This would be inconsistent, and avoiding
    // the calculation should be faster. This should not occur in a solved
    // iteration.
    if( isNewInvestment( aPeriod ) ){
        return 1;
    }
    // Could optimize by storing the shutdown coef.
    // Create the structure of info for the production function.
    ProductionFunctionInfo prodFunc = { input, prodDmdFn, currSigma,
                                        alphaZeroScaler, capital };

    // Create a new shutdown decider.
    ProfitShutdownDecider shutdownDecider;
    double shutdownCoef = shutdownDecider.calcShutdownCoef( &prodFunc,
                                                            IShutdownDecider::getUncalculatedProfitRateConstant(), 
                                                            aRegionName,
                                                            aSectorName,
                                                            year,
                                                            aPeriod );
    return shutdownCoef;
}

/*! \brief Calculate dynamically whether a technology is new investment for the
*          current period.
* \param aPeriod The current period.
* \return Whether the technology is new investment in the period.
* \warning This function is slower than the cached version and should only be
*          used to setup the cache or by the optimized version of the function
*          when the cached value is not available for a period.
* \sa isNewInvestment
* \author Josh Lurz
*/
bool ProductionTechnology::calcIsNewInvestment( const int aPeriod ) const {
    // Return whether the technology's first year is the same as the period.
    return( year == scenario->getModeltime()->getper_to_yr( aPeriod ) );
}

/*! \brief Calculate dynamically whether a technology has been retired yet.
* \param aPeriod The current period.
* \return Whether the technology has been retired.
* \warning This function is slower than the cached version and should only be
*          used to setup the cache or by the optimized version of the function
*          when the cached value is not available for a period.
* \sa isRetired
* \author Josh Lurz
*/
bool ProductionTechnology::calcIsRetired( const int aPeriod ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    // Return whether the period is past the lifetime of the technology as
    // calculated from when it came on-line.
    return( modeltime->getper_to_yr( aPeriod ) >= ( year + delayedInvestTime 
                                                  + lifeTime * modeltime->gettimestep( aPeriod ) ) );
}

/*! \brief Calculate dynamically whether a technology is available to go on-line.
* \param aPeriod The current period.
* \return Whether the technology has gone on-line.
* \warning This function is slower than the cached version and should only be
*          used to setup the cache or by the optimized version of the function
*          when the cached value is not available for a period.
* \sa isAvailable
* \author Josh Lurz
*/
bool ProductionTechnology::calcIsAvailable( const int aPeriod ) const {
    // Return whether we are past the year when the technology came on-line.
    return ( scenario->getModeltime()->getper_to_yr( aPeriod ) >= year + delayedInvestTime );
}
