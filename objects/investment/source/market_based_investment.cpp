/* 
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial
 * Institute. Battelle has certain unperfected rights in the software which
 * should not be copied or otherwise disseminated outside your organization
 * without the express written authorization from Battelle. All rights to the
 * software are reserved by Battelle. Battelle makes no warranty, express or
 * implied, and assumes no liability or responsibility for the use of this
 * software.
 */


/*! 
 * \file market_based_investment.cpp
 * \ingroup Objects
 * \brief MarketBasedInvestor class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <string>
#include <vector>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "util/base/include/xml_helper.h"
#include "investment/include/market_based_investment.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/scenario.h"
#include "marketplace/include/imarket_type.h"
#include "investment/include/investment_utils.h"
#include "investment/include/levelized_cost_calculator.h"
#include "investment/include/rate_logit_distributor.h"
#include "util/logger/include/ilogger.h"
#include "functions/include/function_utils.h"

using namespace std;
extern Scenario* scenario;

//! Constructor
MarketBasedInvestor::MarketBasedInvestor():
mInvestmentLogitExp( 1 ),
mInvestments( scenario->getModeltime()->getmaxper() ),
mFixedInvestments( scenario->getModeltime()->getmaxper(), -1.0 ){
}

/*! \brief Complete the initialization of the market based investor.
* \details This function performs a series of tasks needed before it can be
*          used. It stores the region and sector name, and then sets up the
*          neccessary market. This is done by constructing a unique market name,
*          creating the market, and setting the market to solve for all periods
*          where investment is not fixed at the sector level.
* \param aRegionName Name of the region containing this investor.
* \param aSectorName Name of the sector the investor is investing in.
* \author Josh Lurz
*/
void MarketBasedInvestor::completeInit( const string& aRegionName, const string& aSectorName ){
    // Store the region and sector name
    mRegionName = aRegionName;
    mSectorName = aSectorName;
    
    // Set the name of the "good" for this market. This must be unique.
    mMarketName = mSectorName + getXMLNameStatic();

    // Create the trial market. 
    // Merge-Note: This will have to loop from period 1.
    Marketplace* marketplace = scenario->getMarketplace();
    if ( marketplace->createMarket( mRegionName, mRegionName, mMarketName, IMarketType::NORMAL ) ) {
        // Set the market to solve if the investment in the period is not fixed.
        for( int per = 1; per < scenario->getModeltime()->getmaxper(); ++per ){
            if( mFixedInvestments[ per ] == -1 ){
                marketplace->setMarketToSolve( mMarketName, mRegionName, per );
            }
        }
    }
    else { // This should not occur unless there is invalid data read in.
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Multiple sectors cannot use the same trial market for investment." << endl;
    }

    // Warn if fixed investment for the period 0 was read in, as it will be ignored.
    if( mFixedInvestments[ 0 ] != -1 ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING ); 
        mainLog << "Ignoring fixed investment in the base period for sector "
                << mSectorName << " in region " << mRegionName << endl;
    }
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
* \details This public function accesses the private constant string, XML_NAME.
*          This way the tag is always consistent for both read-in and output and
*          can be easily changed. The "==" operator that is used when parsing,
*          required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz
* \return The constant XML_NAME as a static.
*/
const std::string& MarketBasedInvestor::getXMLNameStatic() {
    const static string XML_NAME = "market-based-investor";
    return XML_NAME;
}

/*! \brief Parses any data from XML.
*
* \author Josh Lurz
* \param aCurr pointer to the current node in the XML input tree
*/
void MarketBasedInvestor::XMLParse( const xercesc::DOMNode* aCurr ) {
    /*! \pre make sure we were passed a valid node. */
    assert( aCurr );

    // get all child nodes.
    const xercesc::DOMNodeList* nodeList = aCurr->getChildNodes();

    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); ++i ){
        const xercesc::DOMNode* curr = nodeList->item( i );
        // Skip any text nodes.
        if( curr->getNodeType() == xercesc::DOMNode::TEXT_NODE ){
            continue;
        }
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
        if( nodeName == "FixedInvestment" ){
            XMLHelper<double>::insertValueIntoVector( curr, mFixedInvestments, scenario->getModeltime() );
        }
        else if( nodeName == "InvestmentLogitExp" ){
            mInvestmentLogitExp = XMLHelper<double>::getValue( curr );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING ); 
            mainLog << "Unrecognized node " << nodeName << " found while parsing " << getXMLNameStatic() 
                    << "." << endl;
        }
    }
}

/*! \brief Write the object to an XML output stream for debugging.
* \param aPeriod Period to write debugging information for.
* \param aOut The output stream to write to.
* \param aTabs The object which tracks the number of tabs to write.
*/
void MarketBasedInvestor::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    
    XMLWriteElement( mInvestments[ aPeriod ], "investment", aOut, aTabs,
        scenario->getModeltime()->getper_to_yr( aPeriod ) );
    
    XMLWriteElement( mFixedInvestments[ aPeriod ], "FixedInvestment", aOut, aTabs,
        scenario->getModeltime()->getper_to_yr( aPeriod ) );
    
    XMLWriteElement( mInvestmentLogitExp, "InvestmentLogitExp", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

/*! \brief Write the object to an XML output stream.
* \param aOut The output stream to write to.
* \param aTabs The object which tracks the number of tabs to write.
*/
void MarketBasedInvestor::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteVector( mFixedInvestments, "FixedInvestment", aOut, aTabs, scenario->getModeltime(), -1.0 );

    XMLWriteElementCheckDefault( mInvestmentLogitExp, "InvestmentLogitExp", aOut, aTabs, 1.0 );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

/*! \brief Distribute the total new investment as solved in the marketplace.
* \todo Reword this comment.
* \details In period 0, investment is summed not calculated. This function
*          determines the level of variable trial investment from the solved
*          parameter in the market, or the price unless the sector investment is
*          fixed. If the sector is fixed, the market should not have been set to
*          solve, and the trial value is ignored. This trial investment does not
*          include subsector and techology fixed investment, it is only the
*          variable investment. It then sets one side of the equation, supply,
*          to the price received for the good. It sets the other side of the
*          equation, demand, to the average levelized cost for the sector. This
*          is calculated using a LevelizedCostCalculator object. The equation
*          will solve when price received is equal to the levelized cost, or
*          profit equals 0. The trial investment plus the sum of all fixed
*          investment at the subsector level and below is then distributed to
*          the children of the investor, stored internally, and returned.
* \param aInvestables The vector of children which will receive investment.
* \param aNationalAccount The national accounts container.
* \param aDemographic A pointer to the Demographics object.
* \param aPeriod The period in which investment is calculated and distributed.
* \return The total investment which occurred.
* \author Josh Lurz
*/
double MarketBasedInvestor::calcAndDistributeInvestment( vector<IInvestable*>& aInvestables,
                                                         NationalAccount& aNationalAccount, 
                                                         const Demographic* aDemographic,
                                                         const int aPeriod )
{
    /*! \pre Check that the period is not nonsensical */
    assert( aPeriod >= 0 );
    // In period 0 calculate the total investment and store it, do not distribute any investment.
    if( aPeriod == 0 ){
        // Don't need to determine investment for the base period.
        mInvestments[ aPeriod ] = InvestmentUtils::sumInvestment( aInvestables, aPeriod );
        assert( mInvestments[ aPeriod ] >= 0 );
        return mInvestments[ aPeriod ];
    }
    
    // May need to initialize the market in initCalc type function.
    double totalInvestment = 0;
    
    // Create a standard profit rate calculator. This will be used to calculate the average
    // levelized cost and to distribute investment. 
    LevelizedCostCalculator expProfitRateCalc;
    
    // Check if parent level investment is fixed.
    if( mFixedInvestments[ aPeriod ] != -1 ){
        // Use sector level fixed investment instead of the trial value. If all
        // children are fixed and that sum does not equal this value, there will
        // be a warning later when the investment is distributed.
        totalInvestment = mFixedInvestments[ aPeriod ];
        // Check if total child investment is greater than this amount, as that
        // will always override this amount.
        double childSumFixed = InvestmentUtils::sumFixedInvestment( aInvestables, aPeriod );
        if( childSumFixed > mFixedInvestments[ aPeriod ] ){
            totalInvestment = childSumFixed;
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Overriding parent level investment with child level investment sum. " << endl;
        }
    }
    // Otherwise use the solved value.
    else {
        Marketplace* marketplace = scenario->getMarketplace();
        // Get the price received for the good from the marketInfo for the
        // sector market. We need to check to make sure this is up to date.
        const double priceReceived = FunctionUtils::getPriceReceived( mRegionName, mSectorName, aPeriod );
        /*! \invariant Price received should always be positive and non-zero.*/
        assert( priceReceived > 0 );
        /*! \pre The market supply is zero as this is the only place it is added
        *        to. 
        */
        assert( marketplace->getSupply( mMarketName, mRegionName, aPeriod ) 
                < util::getVerySmallNumber() );
        // Set the left hand side of the equation to the price received for the
        // good.
        marketplace->addToSupply( mMarketName, mRegionName, priceReceived, aPeriod, true );

        const double sectorExpProfit = expProfitRateCalc.calcSectorExpectedProfitRate( aInvestables,
                                                                                       aNationalAccount,
                                                                                       mRegionName,
                                                                                       mSectorName,
                                                                                       mInvestmentLogitExp,
                                                                                       false,
                                                                                       aPeriod );

        // Set the sector expected profit as the right hand side. It will have
        // been cleared at the end of the last iteration, so there should not be
        // a residual.
        /*! \pre The market demand is zero as this is the only place it is added
        *        to. 
        */
        assert( marketplace->getDemand( mMarketName, mRegionName, aPeriod ) 
                < util::getVerySmallNumber() );
        marketplace->addToDemand( mMarketName, mRegionName, sectorExpProfit, aPeriod, true );

        // Determine the amount of fixed investment in the sector.
        const double fixedInvestment = InvestmentUtils::sumFixedInvestment( aInvestables, aPeriod );
        /*! \invariant Fixed investment is positive. */
        assert( fixedInvestment >= 0 );
        
        double trialInvestment = marketplace->getPrice( mMarketName, mRegionName, aPeriod, true );
        // Warn if trial investment reaches zero.
        if( trialInvestment < util::getSmallNumber() ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Zero trial investment for with average profit rate of " 
                    << sectorExpProfit << " in sector " << mSectorName 
                    << " in " << mRegionName << "." << endl;
        }
        /*! \invariant The trial level of investment should always be zero or
        *              greater. 
        */
        assert( trialInvestment >= 0 );
        // Probably should warn on zero, not entirely sure if it is possible(all
        // fixed subsectors?)

        // Calculate the total investment for the sector as all fixed
        // investment, which cannot be avoided, plus the trial quantity from the
        // market.
        totalInvestment = fixedInvestment + trialInvestment;
    }
    
    // Create a logit based investment distributor.
    RateLogitDistributor invDistributor( mInvestmentLogitExp );
    
    // Use the investment distributor to distribute the trial investment plus
    // the fixed investment.
    mInvestments[ aPeriod ] = invDistributor.distribute( &expProfitRateCalc,
                                                         aInvestables,
                                                         aNationalAccount,
                                                         mRegionName,
                                                         mSectorName,
                                                         totalInvestment,
                                                         aPeriod );

    // Check that total investment and distributed investment are equal.
    if( !util::isEqual( totalInvestment, mInvestments[ aPeriod ] ) ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << totalInvestment - mInvestments[ aPeriod ]
                << " difference between desired investment and distributed investment at the sector level in "
                << mSectorName << " in " << mRegionName << endl;
    }
    /*! \post A positive amount of investment occurred. */
    assert( mInvestments[ aPeriod ] >= 0 );

    // Return the total amount of investment actually distributed
    return mInvestments[ aPeriod ];
}
