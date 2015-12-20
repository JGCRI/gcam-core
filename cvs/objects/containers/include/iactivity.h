#ifndef _IACTIVITY_H_
#define _IACTIVITY_H_
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
 * \file iactivity.h
 * \ingroup Objects
 * \brief The IActivity interface header file.
 * \author Pralit Patel
 */

/*! 
 * \ingroup Objects
 * \brief An interface to activities which comprise the model that can be used to
 *        calculate prices, supplies, or demands.
 * \author Pralit Patel
 */
class IActivity
{
public:
    //! Destructor
    virtual inline ~IActivity();
    
    /*!
     * \brief Calculate the activity.
     * \details Sets supplies, demands, or prices into the market place; whichever
     *          is appropriate for this activity.
     * \param aPeriod The model period in which to calculate.
     */
    virtual void calc( const int aPeriod ) = 0;
    
    /*!
     * \brief Sets the stale flag with in the object indicating that a price may
     *        need to be recalculated before calculating this activity.
     * \details This is a hack to allow lazy re-evaluation of sectors to ensure
     *          they reset their shares after a partial derivative calc.
     */
    virtual void setStale() = 0;
    
    /*!
     * \brief Get a description of this activity which could be used in error
     *        and debug messages.
     * \return A description of this activity.
     */
    virtual std::string getDescription() const = 0;
};

// Inline definitions.
IActivity::~IActivity() {
}

#endif // _IACTIVITY_H_
