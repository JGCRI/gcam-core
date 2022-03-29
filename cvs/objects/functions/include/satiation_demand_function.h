#ifndef _SATIATION_DEMAND_FUNCTION_H_
#define _SATIATION_DEMAND_FUNCTION_H_
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
 * \file satiation_demand_function.h
 * \ingroup Objects
 * \brief SatiationDemandFunction class header file.
 * \author Pralit Patel
 * \author Jiyong Eom
 */

#include <boost/core/noncopyable.hpp>

#include "util/base/include/value.h"
#include "util/base/include/data_definition_util.h"

/*!
 * \ingroup Objects
 * \brief A function which asymptotes to a specified level (satiation level) as
 *        the driver approaches infinity.
 * \details The satiation demand function represents consumption behavior where demand 
 *			for a good or service increases with its affordability, but ultimately reaching 
 *			a satiation point. The expression represents a reduced-form demand choice that 
 *			maximizes non-conventional utility under budget constraint, where marginal utility
 *			does not continue to be positive but becomes negative at some level which 
 *			we call the satiation point. More information about the underlying economics, 
 *			see Eom et al. (PNNL 21073, 2012). Examples include demands for space heating, 
 *			space cooling, and water heating. 
 *			This object is designed to be self contained and flexible such that it
 *          may be used for multiple purposes.  Thus the parameters and arguments
 *          are generically named, for instance the demand driver could be income or
 *          affordability.  The function has a shape parameter satiation adder to
 *          provide a subsistence level, as well as the satiation impedance which is defined
 *          as the value of the demand driver that is half way to the satiation level.
 *          Satiation impedance is calibrated internally through the method
 *          calibrateSatiationImpedance. The satiation level itself may be specified
 *          exogenously or as a percentage increase to the demand level given during
 *          calibration.  The functional form this class represents is as follows:
 *
 *          service density = ( satiation level - satiation adder ) 
 *                 * ( 1 - e^( -log(2) / satiation impedance * demand driver ) ) + satiation adder
 *
 *          <b>XML specification for SatiationDemandFunction</b>
 *          - XML name: \c SatiationDemandFunction::getXMLNameStatic()
 *          - Contained by: Many classes.
 *          - Parsing inherited from class: None
 *          - Attributes: none
 *          - Elements:
 *              - \c satiation-level SatiationDemandFunction::mParsedSatiationLevel
 *                   The exogenously specified asymptote of this function.  Parsing
 *                   this value precludes the use of satiation-base-year-increase.
 *              - \c satiation-base-year-increase SatiationDemandFunction::mBaseYearSatiationMultiplier
 *                   A multiplier of the demand used during calibration to use to
 *                   set mSatiationLevel.  Parsing this value precludes the use of 
 *                   satiation-level.
 *              - \c satiation-adder SatiationDemandFunction::mParsedSatiationAdder
 *                   Shape parameter which represents the subsistence level or the
 *                   minimum demand when the driver is zero.  Optional parameter.
 *
 * \author Pralit Patel
 * \author Jiyong Eom
 */
class SatiationDemandFunction : public INamed, private boost::noncopyable {
	friend class XMLDBOutputter;
public:
    SatiationDemandFunction();
    
    SatiationDemandFunction* clone();

    double calcDemand( const double aDemandDriver ) const;
    
    void calibrateSatiationImpedance( const double aDemand, const double aDemandDriver, const int aPeriod );

    static const std::string& getXMLNameStatic();

    // INamed methods
    virtual const std::string& getName() const;
    
protected:
    
    DEFINE_DATA(
        // SatiationDemandFunction is the only member of this container hierarchy.
        DEFINE_SUBCLASS_FAMILY( SatiationDemandFunction ),

        //! Calibrate the satiation level based on a multiplier increase of base
        //! year demand.  Note this value must be strictly greater than 1.
        DEFINE_VARIABLE( SIMPLE, "satiation-base-year-increase", mBaseYearSatiationMultiplier, Value ),

        //! The satiation level which may have been parsed directly by the user.
        DEFINE_VARIABLE( SIMPLE, "satiation-level", mParsedSatiationLevel, Value ),

        //! The satiation level to use during calcDemand.  This could have been read
        //! in directly by the user or set as a percentage increase from the base year
        //! demand.
        DEFINE_VARIABLE( SIMPLE | STATE | NOT_PARSABLE, "real-satiation-level", mSatiationLevel, Value ),

        //! Satiation impedance or midpoint demand driver.  Note that this value is
        //! calibrated via calibrateSatiationImpedance.
        DEFINE_VARIABLE( SIMPLE | STATE | NOT_PARSABLE, "satiation-impedance", mSatiationImpedance, Value ),

        //! Satiation adder, determines subsistence level.  This is the parsed value
        //! and will not change.
        DEFINE_VARIABLE( SIMPLE, "satiation-adder", mParsedSatiationAdder, Value ),

        //! Satiation adder, determines subsistence level.  This value may be adjusted
        //! from the parsed value during some calibration periods.
        DEFINE_VARIABLE( SIMPLE | STATE | NOT_PARSABLE, "real-satiation-adder", mSatiationAdder, Value )
    )
    
    void copy( const SatiationDemandFunction& aOther );
};

#endif // _SATIATION_DEMAND_FUNCTION_H_
