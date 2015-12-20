#ifndef _SECTOR_ACTIVITY_H_
#define _SECTOR_ACTIVITY_H_
#if defined(_MSC_VER)
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
 * \file sector_activity.h
 * \ingroup Objects
 * \brief The SectorActivity class header file.
 * \author Pralit Patel
 */

#include <boost/shared_ptr.hpp>

#include "containers/include/iactivity.h"

class Sector;
class GDP;

/*! 
 * \ingroup Objects
 * \brief An adaptor class to generically call a sector to set intermediate prices,
 *        and supplies and demands.
 * \details This class isn't really an activity instead it is shared between the
 *          sector price and demand activities.  This object is shared in order to
 *          lazily recalculate stale shares within the sector.
 * \author Pralit Patel
 */
class SectorActivity
{
public:
    SectorActivity( Sector* aSector, const GDP* aGDP, const std::string& aRegionName );
    ~SectorActivity();
    
    void setPrices( const int aPeriod );
    
    void setDemands( const int aPeriod );
    
    void setStale();
    
    std::string getDescription() const;
    
    IActivity* getSectorPriceActivity() const;
    
    IActivity* getSectorDemandActivity() const;
private:
    //! The wrapped sector.
    Sector* mSector;
    
    //! The regional GDP object which may be required for the sector to calculate.
    const GDP* mGDP;
    
    //! The name of the region this sector is contained in.
    const std::string& mRegionName;
    
    //! If setting supplies and demands require setting prices first to avoid
    //! stale shares.
    bool mIsStale;
    
    //! A pointer to the activity used to call setPrices.  Note this memory is
    //! managed by the market dependency finder.
    IActivity* mPriceActivity;

    //! A pointer to the activity used to call setDemands.  Note this memory is
    //! managed by the market dependency finder.
    IActivity* mDemandActivity;
};

/*! 
 * \ingroup Objects
 * \brief An adaptor class to generically call a sector to set intermediate prices.
 * \author Pralit Patel
 */
class SectorPriceActivity : public IActivity
{
    // Friend to allow creation of this class
    friend class SectorActivity;
public:
    ~SectorPriceActivity();
    
    // IActivity methods
    virtual void calc( const int aPeriod );
    
    virtual void setStale();
    
    virtual std::string getDescription() const;
private:
    SectorPriceActivity( boost::shared_ptr<SectorActivity> aSectorActivity );
    
    //! The shared sector activity used to set prices.
    boost::shared_ptr<SectorActivity> mSectorActivity;
};

/*! 
 * \ingroup Objects
 * \brief An adaptor class to generically call a sector to set intermediate demands.
 * \author Pralit Patel
 */
class SectorDemandActivity : public IActivity
{
    // Friend to allow creation of this class
    friend class SectorActivity;
public:
    ~SectorDemandActivity();
    
    // IActivity methods
    virtual void calc( const int aPeriod );
    
    virtual void setStale();
    
    virtual std::string getDescription() const;
private:
    SectorDemandActivity( boost::shared_ptr<SectorActivity> aSectorActivity );
    
    //! The shared sector activity used to set demands.
    boost::shared_ptr<SectorActivity> mSectorActivity;
};

#endif // _SECTOR_ACTIVITY_H_
