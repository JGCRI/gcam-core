#ifndef _TRIALVALUE_MARKET_H_
#define _TRIALVALUE_MARKET_H_
#if defined(_MSC_VER_)
#pragma once
#endif

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
* \file trial_value_market.h
* \ingroup CIAM
* \brief The TrialValueMarket class header file.
* \author Steve Smith
*/

#include "marketplace/include/market.h"

/*!
* \ingroup CIAM
* \brief A class which defines the trail value type market.
* This market type sets up the solution mechanism to solve for a trial value of
* some quantity. The quantity here need not be associated with any particular
* supply or demand. The object using this market must set this market to solved,
* and then call addToDemand to set the trial value, and call getPrice to get the
* trial value.
*
* \author Steve Smith
*/

class TrialValueMarket: public Market {
public:
    TrialValueMarket( const std::string& goodNameIn, const std::string& regionNameIn, const int periodIn );
    virtual IMarketType::Type getType() const;

    virtual void initPrice();
    virtual void setPriceFromLast( const double lastPrice );

    virtual void addToDemand( const double demandIn );
    virtual bool meetsSpecialSolutionCriteria() const;
    virtual bool shouldSolve() const;
    virtual bool shouldSolveNR() const;
protected:
    virtual void toDebugXMLDerived( std::ostream& out, Tabs* tabs ) const;
};

#endif // _TRIALVALUE_MARKET_H_
