#ifndef _MARKET_LOCATOR_H_
#define _MARKET_LOCATOR_H_
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
* \file market_locator.h
* \ingroup Objects
* \brief The MarketLocator class header file.
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <unordered_map>

/*!
* \ingroup Objects
* \brief This class is responsible for rapidly looking up the location of a
*        market within the marketplace given region and good names.
* \details This class is designed to efficiently lookup the location of a market
*          within the Marketplace given the region of the object requesting the
*          lookup, and the name of the good they want to lookup information
*          about. This object is setup initially by the Marketplace through
*          calls to Marketplace::addMarket. During this function, the
*          Marketplace gives the MarketLocator the name of a market area, a
*          region, a good name, and a lookup number to use if the MarketLocator
*          does not already know the location of the market. The MarketLocator
*          stores this information in a hash map designed for speed.  They key is
*          a std::pair of (Region, Good) names.  The value is the market
*          number.  Note we keep and extra list of (Market, Good) names to index
*          as well to ensure correct market number assignment, however during
*          model operation just the (Region, Good) map is used.
* \author Josh Lurz
*/
class MarketLocator
{
public:
    MarketLocator();
    ~MarketLocator();
    int addMarket( const gcamstr& aMarket, const gcamstr& aRegion, const gcamstr& aGoodName,
        const int aUniqueNumber );
    int getMarketNumber( const gcamstr& aRegion, const gcamstr& aGoodName ) const;

    //! An identifier returned by the various functions if the market does not
    //! exist.
    static const int MARKET_NOT_FOUND = -1;
private:
    
    // provide a hash implementation for a pair of gcamstr objects so that we can use
    // them as a key in a hash map (std::unordered_map)
    struct GCAMStrPairHasher {
        // provide the implementation here so it can ideally be inlined
        size_t operator()(const std::pair<gcamstr, gcamstr>& aPair) const {
            // just hash the memory address of the interned string which is
            // guaranteed to be unique for gcamstr objects by definition
            std::hash<const std::string*> hasher;
            
            // a naive method for combining the hashes of the individual gcamstr
            // objects which generally is not appropriate because hash(a,b) == hash(b,a)
            // however in this case we would never expect to encounter such a situation
            // and this will be performance critical therefore we go with it
            return hasher(&aPair.first.get()) ^ hasher(&aPair.second.get());
        }
    };
    
    //! The type of the list of regions/markets + good => market number
    typedef std::unordered_map<std::pair<gcamstr, gcamstr>, int, GCAMStrPairHasher> RegionMarketList;

    //! A map of (Market, Good) names => market number, which is needed for correct unique
    //! market number assignment during addMarket
    RegionMarketList mMarketList;

    //! A map of (Region, Good) names => market number, which is used during model operation
    //! to quickly look up a market index
    RegionMarketList mRegionList;
};

#endif // _MARKET_LOCATOR_H_
