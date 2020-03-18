#ifndef _LINKED_MARKET_H_
#define _LINKED_MARKET_H_
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
 * \file linked_market.h
 * \ingroup Objects
 * \brief The LinkedMarket class header file.
 * \author Pralit Patel
 */

#include "marketplace/include/market.h"

/*! 
 * \brief A market which links another market such that supplies/demands
 *        will get added to both markets and the price is always that of the
 *        market which is linked.
 * \details This type of market can be useful for instance when trying to 
 *          connect various GHGs into a single policy market.  This market
 *          will allow setting price and quantity multipliers for unit
 *          conversions.  In the GHG case they would be interpreted as GWPs.
 *          The multipliers are set through this Market's info object and will
 *          take hold after the call to initPrice.
 */
class LinkedMarket: public Market {
    friend class MarketDependencyFinder;
public:
    LinkedMarket( Market* aLinkedMarket, const MarketContainer* aContainer );
    virtual IMarketType::Type getType() const;

    virtual void resetLinkedMarket( Market* aLinkedMarket );
    
    virtual void initPrice();
    virtual void setPrice( const double aPrice );
    virtual void set_price_to_last_if_default( const double aLastPrice );
    virtual void set_price_to_last( const double aLastPrice );
    virtual double getPrice() const;
    
    virtual void nullDemand();
    virtual double getDemand() const;
    virtual void addToDemand( const double aDemand );
    
    virtual void nullSupply();
    virtual double getSupply() const;
    virtual void addToSupply( const double aSupply );
    
    virtual void setSolveMarket( const bool aShouldSolve );
    virtual bool meetsSpecialSolutionCriteria() const;
    virtual bool shouldSolve() const;
    virtual bool shouldSolveNR() const;
protected:
    //! The pointer to the market linked to.
    Market* mLinkedMarket;
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        Market,
    
        //! A price scalar which could be used to convert units from this good to those
        //! of the linked market.
        DEFINE_VARIABLE( SIMPLE, "price-adjust", mPriceMult, double ),
        
        //! A quantity scalar which could be used to convert units from this good to those
        //! of the linked market.
        DEFINE_VARIABLE( SIMPLE, "demand-adjust", mQuantityMult, double )
    )
    
    virtual void toDebugXMLDerived( std::ostream& out, Tabs* tabs ) const;
};

#endif // _LINKED_MARKET_H_
