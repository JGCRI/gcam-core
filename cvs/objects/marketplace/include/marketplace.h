#ifndef _MARKETPLACE_H_
#define _MARKETPLACE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy ( DOE ). NEITHER THE GOVERNMENT NOR THE
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
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


/*! 
* \file marketplace.h
* \ingroup Objects
* \brief The Marketplace class header file.
* \author Sonny Kim
*/

#include <vector>
#include <iosfwd>
#include <string>
#include <memory>
#include <boost/core/noncopyable.hpp>

#include "marketplace/include/imarket_type.h"
#include "util/base/include/ivisitable.h"
#include "util/base/include/fltcmp.hpp"
#include "util/base/include/data_definition_util.h"

#if GCAM_PARALLEL_ENABLED
#include "tbb/parallel_for.h"
#include "tbb/blocked_range.h"
#endif 

class Tabs;
class Market;
class MarketContainer;
class MarketLocator;
class IVisitor;
class IInfo;
class CachedMarket;
class MarketDependencyFinder; 

/*! 
 * \ingroup Objects
 * \brief The Marketplace is a global repository of information about the
 *        markets, or equations, in the model.
 * \details The Marketplace contains Markets, which may be created, modified,
 *          and accessed from throughout the model. A Market is created using
 *          the createMarket method. See IMarketType for the various types of
 *          Markets that can be created.
 *
 *          Each Market is created with a market region. This is independent of
 *          a Region in the model, and it may encompass multiple model Regions.
 *          It may be a global region, containing all Markets, may be a
 *          multi-region market, such as North America, or may contain a single
 *          model Region. A market region represents a area in which a good may
 *          move without a cost. Each model Region within the market region must
 *          call createMarket to be included in the market region. Each Market
 *          tracks model Regions it contains.
 *
 *          Markets are by default not solved. This means that price is not
 *          determined by the solution mechanism to equilibrate supply and
 *          demand. For unsolved Markets, prices must be set by the model, and
 *          supplies must be ensured to equal demands. The model will not
 *          successfully solve if unsolved markets are in disequilibrium. A
 *          market can be set to solve using setMarketToSolve. If a single
 *          region in a multi-region market sets the market to solve, all
 *          regions will see a solved price. An initial price may be set for
 *          solved markets. This will be used as the starting point for the
 *          solution search. Prices should not be set for solved markets after
 *          this point.
 *
 *          The main operations performed on markets once they are initialized
 *          are to get/set the price, get/set the supply and get/set the demand.
 *          Supplies and demands will be initialized to zero at the outset of
 *          each iterations. For solved markets, prices will be set by the
 *          solution mechanism to their trial values. For unsolved markets, they
 *          will remain at whatever value they were set to.
 *
 *          Markets also contain an IInfo object. This is a mapping of names to
 *          values. It should be used for additional information about the good
 *          in the market. It should not be used as a general method to transfer
 *          information around the model. Note that since a Market may contain
 *          multiple regions, the market info should not be used to store region
 *          specific values.
 *
 *          Market access functions have a flag, aMustExist, which specifies
 *          whether it is an error for the market to be missing. It should be
 *          set to true unless it is known to be possible for the market not to
 *          exist. The market will print a warning if the flag is true and the
 *          market does not exist. It will always return the default value when
 *          the market does not exist. The default value is 0 for supply,
 *          demand, and any info, and NO_MARKET_PRICE for prices.
 *
 * \author Sonny Kim
 * \todo ( re )storeInfo, init_to_last and initPrices should be removed.
 * \todo setPriceVector should be removed, it can be easily implemented using
 *       setPrice.
 * \todo An interface should be put in front of this class for model consumers.
 *       It should have limited functionality, offering only what is required.
 *       A separate interface should be created for solution mechanism specific
 *       items.
 */

class Marketplace: public IVisitable, private boost::noncopyable
{
    friend class CachedMarket;
    friend class SolverLibrary;
    friend class MarketDependencyFinder;
    friend class LogEDFun;
public:
    Marketplace();
    ~Marketplace();

    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    bool createMarket( const std::string& regionName, const std::string& marketName,
                       const std::string& goodName, const IMarketType::Type aMarketType );
    bool createLinkedMarket( const std::string& regionName, const std::string& marketName,
                             const std::string& goodName, const std::string& linkedMarket );
    void initPrices();
    void nullSuppliesAndDemands( const int period );
    void assignMarketSerialNumbers( int aPeriod );
    void setPrice( const std::string& goodName, const std::string& regionName, const double value,
                   const int period, bool aMustExist = true );
    void setPriceVector( const std::string& goodName, const std::string& regionName,
                         const std::vector<double>& prices );
    double addToSupply( const std::string& goodName, const std::string& regionName, const double value,
                      const double lastDerivValue, const int period, bool aMustExist = true );
    double addToDemand( const std::string& goodName, const std::string& regionName, const double value,
                      const double lastDerivValue, const int period, bool aMustExist = true );
    double getPrice( const std::string& goodName, const std::string& regionName, const int period,
                     bool aMustExist = true ) const;
    double getSupply( const std::string& goodName, const std::string& regionName,
        const int period ) const;
    double getDemand( const std::string& goodName, const std::string& regionName,
        const int period ) const;

    void init_to_last( const int period );
    void dbOutput() const; 
    void csvOutputFile( std::string marketsToPrint = "" ) const; 
    int resetToPriceMarket( const int aMarketNumber );
    void setMarketToSolve( const std::string& goodName, const std::string& regionName,
        const int period );
    void unsetMarketToSolve( const std::string& goodName, const std::string& regionName,
        const int period );

    const IInfo* getMarketInfo( const std::string& aGoodName, const std::string& aRegionName,
                                const int aPeriod, const bool aMustExist ) const;

    IInfo* getMarketInfo( const std::string& aGoodName, const std::string& aRegionName,
                         const int aPeriod, const bool aMustExist );
    
    std::auto_ptr<CachedMarket> locateMarket( const std::string& aGoodName, const std::string& aRegionName,
                                               const int aPeriod ) const;

    void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
    void accept( IVisitor* aVisitor, const int aPeriod ) const;

    std::vector<Market*> getMarketsToSolve( const int period ) const;
    static const std::string& getXMLNameStatic();
    
    //! The price to return if no market exists.
    const static double NO_MARKET_PRICE;
    
    // TODO: replace these these store/restore with something more flexible and
    // that does not get confused with store/restore state that occurrs during
    // partial derivative calculations.  Perhaps a way of extracting all prices
    // which can then held and restored as needed by which ever driver needs this
    // functionality.
    void storeinfo( const int period );
    void restoreinfo( const int period );
    void store_prices_for_cost_calculation();
    void restore_prices_for_cost_calculation();
    
    MarketDependencyFinder* getDependencyFinder() const;

    // The methods from here down are diagnostics
    std::vector<double> fullstate( int period ) const; //!< Return all supplies and demands in all markets in a single vector
    bool checkstate(int period, const std::vector<double>&, std::ostream *log=0, unsigned tol=0) const;
    void prnmktbl(int period, std::ostream &out) const;
    void logForecastEvaluation( int aPeriod ) const;
protected:
    
    DEFINE_DATA(
        // Marketplace is the only member of this container hierarchy.
        DEFINE_SUBCLASS_FAMILY( Marketplace ),

        //! List of all markets.  Note that MarketContainer wraps the list of markets by
        //! period.
        CREATE_CONTAINER_VARIABLE( mMarkets, std::vector<MarketContainer*>, NamedFilter, "market" )
    )

    //! An object which determines the correct market number.
    std::auto_ptr<MarketLocator> mMarketLocator;
    
    //! A gobal object which tracks dependencies between all of the model activities
    //! and their linkages to their respective markets.  It can be used to create a
    //! sorted global ordering or get an inorder list of model activities that are
    //! affected by changing the price of a single market.
    std::auto_ptr<MarketDependencyFinder> mDependencyFinder;
    
    //! Flag indicating whether the next call to world->calc() will be part of a partial derivative calculation 
    bool mIsDerivativeCalc;

#if GCAM_PARALLEL_ENABLED
    //! helper class for tbb parallel_for over null supplies and demands
    struct NullSDHelper {
        const std::vector<MarketContainer*>& mMarkets;
        const int mPeriod;
        NullSDHelper( const std::vector<MarketContainer*>& aMarkets, const int aPeriod )
            : mMarkets( aMarkets ), mPeriod( aPeriod ) {}
        void operator()( const tbb::blocked_range<int>& aRange ) const;
    };

    //! helper class for tbb parallel_for over restore info
    struct RestoreHelper {
        const std::vector<MarketContainer*>& mMarkets;
        const int mPeriod;
        RestoreHelper( const std::vector<MarketContainer*>& aMarkets, const int aPeriod )
            : mMarkets( aMarkets ), mPeriod( aPeriod ) {}
        void operator()( const tbb::blocked_range<int>& aRange ) const;
    };
    
#endif
};

#endif
