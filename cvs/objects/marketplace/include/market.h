#ifndef _MARKET_H_
#define _MARKET_H_
#if defined(_MSC_VER_)
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
* \file market.h
* \ingroup Objects
* \brief The Market class header file.
* \author Sonny Kim
*/

#include <vector>
#include <memory>
#include <boost/core/noncopyable.hpp>

#include "marketplace/include/imarket_type.h"
#include "util/base/include/iyeared.h"
#include "util/base/include/ivisitable.h"
#include "util/base/include/value.h"
#include "util/base/include/data_definition_util.h"

#if GCAM_PARALLEL_ENABLED
#include "tbb/spin_rw_mutex.h"
#endif

class IInfo;
class Tabs;
class IVisitor;
class MarketContainer;
namespace objects {
    class Atom;
}

// Need to forward declare the subclasses as well.
class NormalMarket;
class MarketTax;
class MarketRES;
class MarketSubsidy;
class CalibrationMarket;
class InverseCalibrationMarket;
class DemandMarket;
class TrialValueMarket;
class PriceMarket;
class LinkedMarket;

/*!
 * \ingroup Objects
 * \brief A single market, or equation, in the model.
 * \details A Market conceptually represents the trade for a single good in an
 *          area in which there are no transportation costs. See Marketplace for
 *          an explanation of a market region vs. model region. The market has a
 *          price, supply and demand for the good. It also contains an object
 *          with additional information about the good, the market info. Market
 *          objects may also be used in cases where a true market is not
 *          required, but a solved equation. See the TrialValueMarket for an
 *          explanation of this feature.
 *
 *          The Market's functions are divided into two main areas:
 
 *          - There are setters and accessors which are called whenever a
 *            Marketplace function to set or get supply, demand, or price is
 *            called. These methods may be overridden by derived Market classes
 *            to add different behaviors to the markets. Because of this, calls
 *            to a getter may not return the Market variable of the same name.
 *            For example, getSupply does not necessarily return Market::supply.
 *            It will return whatever conceptually is the supply.
 *
 *          - There are functions to directly set and get the underlying supply,
 *            demand, and price variables. They are named
 *            (set|get)Raw(Supply|Demand|Price). These functions modify the left
 *            hand side, right hand side, and trial values of the equation the
 *            Market represents. These are only used by the solution mechanism,
 *            and cannot be overridden.
 *
 * \author Sonny Kim
 */

class Market: public IYeared, public IVisitable, private boost::noncopyable
{
    friend class XMLDBOutputter;
    friend class PriceMarket;
public:
    Market( const MarketContainer* aContainer );
    virtual ~Market();

    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    static const std::string& getXMLNameStatic();
    const std::vector<const objects::Atom*>& getContainedRegions() const;

    virtual void initPrice();
    virtual void setPrice( const double priceIn );
    void setRawPrice( const double priceIn );
    virtual void set_price_to_last_if_default( const double lastPrice );
    virtual void set_price_to_last( const double lastPrice );
    virtual double getPrice() const;
    double getRawPrice() const;

    void setForecastPrice( double aForecastPrice );
    double getForecastPrice() const;
    void setForecastDemand( double aForecastDemand );
    double getForecastDemand() const;

    virtual void nullDemand();
    virtual void addToDemand( const double demandIn );
    virtual double getSolverDemand() const;
    double getRawDemand() const;
    virtual double getDemand() const;

    virtual void nullSupply();
    virtual double getSolverSupply() const;
    double getRawSupply() const;
    virtual double getSupply() const;
    virtual void addToSupply( const double supplyIn );
    
    const std::string& getName() const;
    const std::string& getRegionName() const;
    const std::string& getGoodName() const;
    const IInfo* getMarketInfo() const;
    IInfo* getMarketInfo();
    void store_original_price();
    void restore_original_price();

    virtual void setSolveMarket( const bool doSolve );
    virtual bool meetsSpecialSolutionCriteria() const = 0;
    virtual bool shouldSolve() const;
    virtual bool shouldSolveNR() const;
    bool isSolvable() const;
    
    virtual int getSerialNumber() const;
    
    int getYear() const;
    void setYear( const int aYear );

    /*!
     * \brief Change a LinkedMarket to link to an alternative market
     *        than the one it was originally created with.
     * \details Note this method is only relevant for LinkedMarket
     * \param aLinkedMarket The new Market to link to.
     */
    virtual void resetLinkedMarket( Market* aLinkedMarket ) { }

    /*!
    * \brief Return the type of the market as defined by the IMarketTypeEnum
    *        which is unique for each derived market class.
    * \return The type of the market.
    */
    virtual IMarketType::Type getType() const = 0;
    static const std::string& convert_type_to_string( const IMarketType::Type aType );

    void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    void copy( const Market& aMarket );
    
    DEFINE_DATA(
        /* Declare all subclasses of Market to allow automatic traversal of the
         * hierarchy under introspection.
         */
        DEFINE_SUBCLASS_FAMILY( Market, NormalMarket, MarketTax, MarketRES, MarketSubsidy,
                                CalibrationMarket, InverseCalibrationMarket, DemandMarket,
                                TrialValueMarket, PriceMarket, LinkedMarket ),
        
        //! The market price.
        DEFINE_VARIABLE( SIMPLE | STATE, "price", mPrice, Value ),
        
        //! The original market price, used in re/store_original_price.
        DEFINE_VARIABLE( SIMPLE, "orginal_price", mOriginal_price, double ),

        //! Forecast price (used for setting solver initial guess)
        DEFINE_VARIABLE( SIMPLE, "forecast-price", mForecastPrice, double ),

        //! Forecast demand (used for rescaling in solver)
        DEFINE_VARIABLE( SIMPLE, "forecast-demand", mForecastDemand, double ),
        
        //! The market demand.
        DEFINE_VARIABLE( SIMPLE | STATE, "demand", mDemand, Value ),
        
        //! The market supply.
        DEFINE_VARIABLE( SIMPLE | STATE, "supply", mSupply, Value ),
                
        //! The year associated with this market.
        DEFINE_VARIABLE( SIMPLE, "year", mYear, int ),
        
        //! Whether to solve the market given other constraints are satisfied.
        DEFINE_VARIABLE( SIMPLE, "solved_Market_Flag", mSolveMarket, bool )
    )
    
#if GCAM_PARALLEL_ENABLED
    typedef tbb::speculative_spin_rw_mutex Mutex;
    //! A fast lock to protect conccurent adds to demand.
    mutable Mutex mDemandMutex;
    
    //! A fast lock to protect concurrent adds to supply.
    mutable Mutex mSupplyMutex;
#endif
    
    //! Object containing information related to the market.
    std::auto_ptr<IInfo> mMarketInfo;
    
    //! Weak pointer to the container that hold this market.  The container will
    //! keep shared market data such as name and contained regions so we hold a
    //! reference to it here to access such information.
    const MarketContainer* mContainer;

    /*! \brief Add additional information to the debug xml stream for derived
    *          classes.
    * \details This method is inherited from by derived class if they which to
    *          add any additional information to the printout of the class.
    * \param out Output stream to print to.
    * \param tabs A tabs object responsible for printing the correct number of
    *        tabs. 
    */
    virtual void toDebugXMLDerived( std::ostream& out, Tabs* tabs ) const = 0;
    
    IInfo* releaseMarketInfo();
};

#endif // _MARKET_H_
