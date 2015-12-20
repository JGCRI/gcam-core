#ifndef _FINAL_DEMAND_ACTIVITY_H_
#define _FINAL_DEMAND_ACTIVITY_H_
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
 * \file final_demand_activity.h
 * \ingroup Objects
 * \brief The FinalDemandActivity class header file.
 * \author Pralit Patel
 */

#include "containers/include/iactivity.h"

class AFinalDemand;
class GDP;
class Demographic;

/*! 
 * \ingroup Objects
 * \brief An adaptor class to generically call a final demand to set demands.
 * \author Pralit Patel
 */
class FinalDemandActivity : public IActivity
{
public:
    FinalDemandActivity( AFinalDemand* aFinalDemand, const GDP* aGDP, 
                         const Demographic* aDemographic, const std::string& aRegionName );
    virtual ~FinalDemandActivity();
    
    // IActivity methods
    virtual void calc( const int aPeriod );
    
    virtual void setStale();
    
    virtual std::string getDescription() const;
private:
    //! The wrapped final demand.
    AFinalDemand* mFinalDemand;
    
    //! The regional GDP object which is required for the final demand to calculate.
    const GDP* mGDP;
    
    //! The regional demographics which is required for the final demand to calculate.
    const Demographic* mDemographic;
    
    //! The name of the region this final demand is contained in.
    const std::string& mRegionName;
};

#endif // _FINAL_DEMAND_ACTIVITY_H_
