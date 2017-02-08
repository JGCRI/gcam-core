#ifndef _MARKET_CONTAINER_H_
#define _MARKET_CONTAINER_H_
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
 * Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
 * Distributed as open-source under the terms of the Educational Community
 * License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
 *
 * For further details, see: http://www.globalchange.umd.edu/models/gcam/
 *
 */



/*!
 * \file market_container.h
 * \ingroup Objects
 * \brief The MarketContainer class header file.
 * \author Pralit Patel
 */

#include <vector>
#include <string>
#include <boost/core/noncopyable.hpp>

#include "marketplace/include/imarket_type.h"
#include "util/base/include/inamed.h"
#include "util/base/include/time_vector.h"
#include "util/base/include/data_definition_util.h"

class Market;
namespace objects {
    class Atom;
}

/*!
 * \brief A class which simply manages the list of markets by model period.
 * \details The shared information of markets will be held by this container to avoid
 *          duplication while period specific information will still be held by the
 *          Market.
 * \author Pralit Patel
 */
class MarketContainer : public INamed, private boost::noncopyable {
public:
    MarketContainer( const IMarketType::Type aMarketType, const std::string& aGoodName, const std::string& aRegionName );
    MarketContainer( MarketContainer* aMarketToLink, const std::string& aGoodName, const std::string& aRegionName );
    virtual ~MarketContainer();
    
    void resetToPriceMarket( MarketContainer* aDemandMarkets );
    
    Market* getMarket( const int aPeriod ) const;

    void addRegion( const std::string& aRegion );
    const std::vector<const objects::Atom*>& getContainedRegions() const;
    
    const std::string& getName() const;
    const std::string& getRegionName() const;
    const std::string& getGoodName() const;
    
    int size() const;
    
    /*!
     * \brief Assign a serial number to this market.
     * \details Serial numbers are used to place markets in a canonical
     *          order (generally to make it easier to interpret logging
     *          output).  They are assigned by the Marketplace at the
     *          start of a period and should remain fixed through the
     *          entire period (but there is no requirement for consistency
     *          between periods).  No other class besides the Marketplace
     *          should call this function.
     */ 
    void assignSerialNumber( int aSerialNumber ) {mSerialNumber = aSerialNumber;}
    /*!
     * \brief Get this market's serial number.
     */
    virtual int getSerialNumber( void ) const {return mSerialNumber;}

    typedef double (Market::*getpsd_t)() const; // Can point to Market::getPrice, Market::getRawPrice, Market::getRawDemand, etc.
    double forecastDemand( const int aPeriod );
    double forecastPrice( const int aPeriod );
    double extrapolate( const int aPeriod, getpsd_t aDataFn );
protected:
    
    DEFINE_DATA(
        // MarketContainer is the only member of this container hierarchy.
        DEFINE_SUBCLASS_FAMILY( MarketContainer ),

        //! The period vector of the contained markets which has all of the actual
        //! market data such as prices, supplies, and demands.
        DEFINE_VARIABLE( CONTAINER, "market-period", mMarkets, objects::PeriodVector<Market*> ),
                
        //! The name of the market.
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),
        
        //! The good the market represents
        DEFINE_VARIABLE( SIMPLE, "good", mGood, std::string ),
        
        //! The region of the market.
        DEFINE_VARIABLE( SIMPLE, "region", mRegion, std::string ),

        //! serial number for putting markets into canonical order
        DEFINE_VARIABLE( SIMPLE, "serial-number", mSerialNumber, int ),
        
        //! Vector of atoms of all regions contained within this market.
        DEFINE_VARIABLE( ARRAY, "contained-regions", mContainedRegions, std::vector<const objects::Atom*> )
    )
    
    Market* createMarket( const IMarketType::Type aMarketType );
};

#endif // _MARKET_CONTAINER_H_
