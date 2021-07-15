#ifndef _GOMPERTZ_FUNCTION_H_
#define _GOMPERTZ_FUNCTION_H_
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
 * \file gompertz_function.h
 * \ingroup Objects
 * \brief GompertzDemandFunction class header file.
 * \author Jon Sampedro
 */

#include <xercesc/dom/DOMNode.hpp>
#include <boost/core/noncopyable.hpp>

#include "util/base/include/value.h"
#include "util/base/include/iparsable.h"
#include "util/base/include/data_definition_util.h"

/*!
 * \ingroup Objects
 * \brief A function which asymptotes to a specified level as
 *        the driver approaches infinity.
 * \details The gompertz demand function represents consumption behavior where demand 
 *			for a good or service increases with its affordability, but ultimately reaching 
 *			a saturation point.  This function is used for estimating future floorspace demand.
 *
 *          Per capita floorspace=( unadjust_satiation − land_density_param ∗ log⁡( population/habitable_land))

 									∗ exp(−base_floorspace_param ∗ log⁡(base per capita floorspace) 
									
									∗ exp(−income_param ∗log⁡(per capita GDP) ) ) − bias_adjust_param
 
 *
 *          <b>XML specification for GompertzDemandFunction</b>
 *          - XML name: \c GompertzDemandFunction::getXMLNameStatic()
 *          - Contained by: Many classes.
 *          - Parsing inherited from class: None
 *          - Attributes: none
 *          - Elements:
 *              - \c unadjust_satiation GompertzDemandFunction::mParsedUnadjustSatiation
 *                   The exogenously specified asymptote of this function.
  *              - \c habitable_land GompertzDemandFunction::mParsedHabitableLand
 *                   The exogenously specified habitable land per GCAM region/GCAM-USA state
 *              - \c land_density_param GompertzDemandFunction::mParsedLandDensityParam
 *                   A parameter estimated from regression of historical observations
  *              - \c base_floorspace_param GompertzDemandFunction::mParsedBaseFloorspaceParam
 *                   A parameter estimated from regression of historical observations
  *              - \c income_param GompertzDemandFunction::mParsedIncomeParam
 *                   A parameter estimated from regression of historical observations
  *              - \c bias_adjust_param GompertzDemandFunction::mParsedBiasAdjustParam
 *                   A parameter estimated from regression of historical observations

 *
 * \author Pralit Patel
 * \author Jiyong Eom
 */
class GompertzDemandFunction : public INamed, public IParsable, private boost::noncopyable {
	friend class XMLDBOutputter;
public:
	GompertzDemandFunction();
    
	GompertzDemandFunction* clone();

    double calcDemand( const double aDemandDriver ) const;
    

    static const std::string& getXMLNameStatic();

    // INamed methods
    virtual const std::string& getName() const;
    
    // IParsable methods
    virtual bool XMLParse( const xercesc::DOMNode* aNode );

protected:
    
    DEFINE_DATA(
        // GompertzDemandFunction is the only member of this container hierarchy.
        DEFINE_SUBCLASS_FAMILY(GompertzDemandFunction),

        //! The unadjusted satiation level which may have been parsed directly by the user.
        DEFINE_VARIABLE( SIMPLE, "parsed-unadjust_satiation", mParsedUnadjustSatiation, Value ),

		//! The unadjusted satiation level to use during calcDemand. This value may be adjusted
        //! from the parsed value during some calibration periods.
		DEFINE_VARIABLE(SIMPLE | STATE, "unadjust_satiation", mUnadjustSatiation, Value),

		//! The habitable land which may have been parsed directly by the user.
		DEFINE_VARIABLE(SIMPLE, "parsed-habitable_land", mParsedHabitableLand, Value),

		//! The habitable land to use during calcDemand. This value may be adjusted
        //! from the parsed value during some calibration periods.
		DEFINE_VARIABLE(SIMPLE | STATE, "habitable_land", mHabitableLand, Value),

		//! The land density parameter which may have been parsed directly by the user.
		DEFINE_VARIABLE(SIMPLE, "parsed-land_density_param", mParsedLandDensityParam, Value),

		//! The land density parameter to use during calcDemand. This value may be adjusted
		//! from the parsed value during some calibration periods.
		DEFINE_VARIABLE(SIMPLE | STATE, "land_density_param", mLandDensityParam, Value),

		//! The base floorspace parameter which may have been parsed directly by the user.
		DEFINE_VARIABLE(SIMPLE, "parsed-base_floorspace_param", mParsedBaseFloorspaceParam, Value),

		//! The base floorspace parameter to use during calcDemand. This value may be adjusted
		//! from the parsed value during some calibration periods.
		DEFINE_VARIABLE(SIMPLE | STATE, "base_floorspace_param", mBaseFloorspaceParam, Value),

		//! The income parameter which may have been parsed directly by the user.
		DEFINE_VARIABLE(SIMPLE, "parsed-income_param", mParsedIncomeParam, Value),

		//! The income parameter to use during calcDemand. This value may be adjusted
		//! from the parsed value during some calibration periods.
		DEFINE_VARIABLE(SIMPLE | STATE, "income_param", mIncomeParam, Value),

		//! The bias correction which may have been parsed directly by the user.
		DEFINE_VARIABLE(SIMPLE, "parsed-bias_adjust_param", mParsedBiasAdjustParam, Value),

		//! The bias correction to use during calcDemand. 
		DEFINE_VARIABLE(SIMPLE | STATE, "bias_adjust_param", mBiasAdjustParam, Value)



    )
    
    void copy( const GompertzDemandFunction& aOther );
};

#endif // _GOMPERTZ_DEMAND_FUNCTION_H_
