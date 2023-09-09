#ifndef _CALIBRATE_SHARE_WEIGHT_VISITOR_H_
#define _CALIBRATE_SHARE_WEIGHT_VISITOR_H_
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
 * \file calibrate_share_weight_visitor.h
 * \ingroup Objects
 * \brief CalibrateShareWeightVisitor class header file.
 * \author Pralit Patel
 */

#include "util/base/include/default_visitor.h"
#include <string>

class ITechnology;
class IDiscreteChoice;

/*! 
 * \ingroup Objects
 * \brief A visitor which determines if a logit nest has calibrated values
 *        and will adjust share weights algebraically to reproduce those values
 *        at the current set of prices.
 * \details For both subectors and technologies we first sum all of the calibration
 *          values and find the largest child in terms of largest calibration value
 *          so that we may make the share weights relative to that subsector/technology.
 *          We then back out the appropriate share weight to reproduce the calibration shares.
 *          This visitor relies on the discrete choice function to invert to logit
 *          formulation to back out the share weight so as to be flexible to which
 *          ever formulation is used.
 * \sa IDiscreteChoice
 * \author Pralit Patel
 * \warning This class never actually checks whether calibration is active.
 * \warning This methodology will not work if in a given nest there is a mix
 *          of subsectors/technologies with and without calibrated values.
 */
class CalibrateShareWeightVisitor : public DefaultVisitor {
public:

    CalibrateShareWeightVisitor( const std::string& aRegionName );

    // Documentation for visitor methods is inherited.
    virtual void startVisitSector( const Sector* aSector,
                                   const int aPeriod );

    virtual void endVisitSector( const Sector* aSector,
                                 const int aPeriod );

    virtual void startVisitSubsector( const Subsector* aSubsector,
                                     const int aPeriod );
    
    virtual void endVisitNestingSubsector( const NestingSubsector* aSubsector, const int aPeriod );

private:
    //! Name of the Region the for which we are calibrating
    std::string mCurrentRegionName;

    //! Name of the sector currently being tabulated.
    std::string mCurrentSectorName;

    template<typename ContainerType>
    void calibrateShareWeights( const ContainerType* aContainer, const int aPeriod ) const;
    
    // Adaptor methods to be able to use the generic calibrateShareWeights method with
    // subsector/nesting-subsector/technology even though they may not share a common
    // interface
    std::vector<Subsector*> getChildren( const Sector* aSector, const int aPeriod ) const;
    std::vector<Subsector*> getChildren( const NestingSubsector* aSubsector, const int aPeriod ) const;
    std::vector<ITechnology*> getChildren( const Subsector* aSubsector, const int aPeriod ) const;
    
    bool isAvailable( const Subsector* aSubsector, const int aPeriod ) const;
    bool isAvailable( const ITechnology* aTechnology, const int aPeriod ) const;
    
    double getCalValue( const Subsector* aSubsector, const int aPeriod ) const;
    double getCalValue( const ITechnology* aTechnology, const int aPeriod ) const;
    
    double getCost( const Subsector* aSubsector, const int aPeriod ) const;
    double getCost( const ITechnology* aTechnology, const int aPeriod ) const;
    
    double getFuelPrefElasticity( const Subsector* aSubsector, const int aPeriod ) const;
    double getFuelPrefElasticity( const ITechnology* aTechnology, const int aPeriod ) const;
    
    IDiscreteChoice* getDiscreteChoice( const Sector* aSector ) const;
    IDiscreteChoice* getDiscreteChoice( const Subsector* aSubsector ) const;
    
    void setShareWeight( Subsector* aSubsector, const double aShareWeight, const int aPeriod ) const;
    void setShareWeight( ITechnology* aTechnology, const double aShareWeight, const int aPeriod ) const;
};

#endif // _CALIBRATE_SHARE_WEIGHT_VISITOR_H_
