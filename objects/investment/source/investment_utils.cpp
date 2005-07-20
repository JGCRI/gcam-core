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
* \file investment_utils.cpp
* \ingroup Objects
* \brief InvestmentUtils class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <numeric>

#include "investment/include/investment_utils.h"
#include "investment/include/iinvestable.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/scenario.h"
#include "util/base/include/util.h"

using namespace std;

extern Scenario* scenario; // for marketplace.

/*! \brief Calculate the total investment level from two annual flows.
* \detals The sum of linearly interpolated annual flows for the 5-year period
*         starting with the current model period t and going back over the
*         previous 4 years can be shown equal to 2 times the annual flow at time
*         t-5 plus 3 times the flow at time t.
* \todo Make sure this is still right if the timestep is not 5 years.
* \param aPrevInvestment The previous period's annual investment.
* \param aCurrInvestment The current period's annual investment.
* \param aIntervalYear The number of years inbetween the previous and current
*        period.
* \return The summed interpolated investment after the previous period up to and
*         including the current period.
*/
double InvestmentUtils::interpolateAndSumFlows( const double aPrevInvestment, const double aCurrInvestment,
                                                const int aIntervalYears )
{
    return ( 0.4 * aIntervalYears * aPrevInvestment ) + ( 0.6 * aIntervalYears * aCurrInvestment );
}

/*! \brief Calculate the base level of capital.
* \details This function determines the initial level of capital for a sector to
*          begin scaling from. If the sector had capital in the previous period,
*          this is used as the base level of capital. Otherwise, the function
*          computes a base level by multiplying the read-in aggregate investment
*          fraction, defined as the scale of this sector relative to the entire
*          economy, by the total amount of capital in the previous period as
*          returned by the marketplace.
* \param The region in which the current sector resides.
* \param aPrevInvestment Total sector level capital in the previous period.
* \param aAggInvFrac The fraction of the total regional capital to use as the
*        base capital if there was no capital in the previous period.
* \param aPeriod Period in which base capital is being calculated. Should not be
*        zero.
* \return The base level of capital for the sector to being scaling from.
*/
double InvestmentUtils::calcBaseCapital( const string& aRegionName,
                                         const double aPrevInvestment,
                                         const double aAggInvFrac,
                                         const int aPeriod )
{
    /*! \pre Base capital should not be calculated in the base period. */
    assert( aPeriod > 0 );

    // Initialize the base capital to the previous period's capital.
    double baseCapital;
    if( aPrevInvestment > util::getSmallNumber() ){
        baseCapital = aPrevInvestment;
    }
    else {
        // If the base capital is 0, use a set fraction of the total regional
        // capital pool. In the previous aPeriod markets cleared, so supply ==
        // demand. In the base period supply and demand should be equal if the
        // IO table balanced.
        const Marketplace* marketplace = scenario->getMarketplace();
        const static string CAPITAL_MARKET = "Capital";
        baseCapital = aAggInvFrac 
                      * marketplace->getSupply( CAPITAL_MARKET, aRegionName, aPeriod - 1 );
    }
    /*! \post Base capital must be positive and non-zero. */
    assert( baseCapital > 0 );
    return baseCapital;
}

/*! \brief Sum the existing capital stock of underlying technologies of a set of
*          subsectors.
* \author Josh Lurz
* \param aSubsectors A set of subsectors for which to sum investment.
* \param aPeriod The period to sum investment.
* \return The total amount of technology level investment.
*/
double InvestmentUtils::sumInvestment( const vector<IInvestable*>& aInvestables, const int aPeriod ) {
    double totalInvestment = 0;
    for( CInvestableIterator currInv = aInvestables.begin(); currInv != aInvestables.end(); ++currInv ){
        totalInvestment += (*currInv)->getAnnualInvestment( aPeriod );
    }
    /*! \post Annual investment must be positive. */
    assert( totalInvestment >= 0 );
    return totalInvestment;
}

/*! \brief Normalize a set of investment shares.
* \param aShares A set of shares to normalize.
* \note If a vector with no non-zero shares is passed into the function it will
*       be returned in the same state.
* \return The normalized sum of the shares.
*/
double InvestmentUtils::normalizeShares( vector<double>& aShares ){
    // Calculate the total of the shares so they can be normalized.
    const double sum = accumulate( aShares.begin(), aShares.end(), 0.0 );

    // Check for an unnormalizable vector.
    if( sum > 0 ){
        // Adjust all the shares.
        for( unsigned int i = 0; i < aShares.size(); ++i ){
            aShares[ i ] /= sum;
        }
        // Check that the normalization was performed correctly.
        assert( util::isEqual( accumulate( aShares.begin(), aShares.end(), 0.0 ), 1.0 ) );
        // If the shares could be normalized assume they were correct.
        return 1;
    }
    // Return that the shares could not be normalized.
    return 0;
}

/*! \brief Return the total amount of fixed investment for the vector.
* \details Calculates and returns the total amount of fixed investment in a
*          given period for a set of investable objects.
* \param aInvestables Child investable vector to return fixed investment for.
* \param aPeriod Period in which to sum fixed investment.
* \return Sum of fixed investment
*/
double InvestmentUtils::sumFixedInvestment( const vector<IInvestable*>& aInvestables,
                                             const int aPeriod )
{
    // Calculate the sum of fixed investments.
    double sumFixed = 0;
    for( CInvestableIterator currInv = aInvestables.begin(); currInv != aInvestables.end(); ++currInv ){
        sumFixed += (*currInv)->getFixedInvestment( aPeriod );
    }
    return sumFixed;
}
