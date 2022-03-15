#ifndef _INTERPOLATION_RULE_H_
#define _INTERPOLATION_RULE_H_
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
 * \file interpolation_rule.h
 * \ingroup Objects
 * \brief Header file for the InterpolationRule class.
 * \author Pralit Patel
 */
#include <string>
#include <boost/core/noncopyable.hpp>

#include "util/base/include/aparsable.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"
#include "util/base/include/iinterpolation_function.h"
#include "util/base/include/data_definition_util.h"

/*!
 * \ingroup Objects
 * \brief An interpolation rule which determines how to interpolate values in
 *        a PeriodVector.
 * \details A rule consists of from and to years to define end points of a
 *          range of values which may be interpolated.  Note that the end points
 *          are not interpolated and they also do not need to be a model year.
 *          A user may also optionally parse the values to use for the end
 *          points of the interpolation.  If values were not parsed it is assumed
 *          that they meant to use the value that is currently set in the
 *          PeriodVector to interpolate for that end point year.  If an end point
 *          year is not a model year then the user must specify the value at that
 *          end point.
 *          The other key parts to an interpolation rule are a function to
 *          perform the interpolation and a policy on how we will deal with
 *          overwriting values that have already been set.
 *          <b>XML specification for InterpolationRule</b>
 *          - XML name: \c interpolation-rule
 *          - Contained by:
 *          - Parsing inherited from class: None.
 *          - Attributes:
 *              - \c apply-to InterpolationRule::mApplyTo
 *                      The XML name for which this rule applys to.  This is used
 *                      by the containing class only.
 *              - \c from-year InterpolationRule::mFromYear
 *                      The left end point for the range of years this rule applies.
 *              - \c to-year InterpolationRule::mToYear
 *                      The right end point for the range of years this rule applies.
 *                      Note that a to-year that is equal to
 *                      InterpolationRule::getLastModelYearConstant() will automatically
 *                      be converted to the last model year.
 *          - Elements:
 *              - \c from
 *                      Value InterpolationRule::mFromValue
 *                          (optional) The value to interpolate from if parsed, otherwise
 *                          the value will be obtained through the given PeriodVector to
 *                          interpolate.
 *              - \c to
 *                      Value InterpolationRule::mToValue
 *                          (optional) The value to interpolate to if parsed, otherwise
 *                          the value will be obtained through the given PeriodVector to
 *                          interpolate.
 *              - \c (any IInterpolationFunction) IInterpolationFunction* InterpolationRule::mInterpolationFunction
 *                      Can be any interpolation function contained in InterpolationFunctionFactory.
 *                      This will be the function used to interpolate values within the
 *                      range of years defined by this rule.
 *              - \c overwrite-policy
 *                      Attributes: \c warn bool InterpolationFunction::mWarnWhenOverwritting
 *                          A flag to indicate if a warning message should be produced when
 *                          overwriting a value.
 *                      OverwritePolicy InterpolationRule::mOverwritePolicy
 *                          The policy to use when a value might be overwritten. The choices are:
 *                              - \c NEVER Values will never be overwritten.
 *                              - \c INTERPOLATED Values that were set by another interpolation
 *                                  rule can be overwritten.  To put it another way values which
 *                                  were parsed can not be overwritten.
 *                              - \c ALWAYS Values will always be overwritten.
 *
 * \author Pralit Patel
 * \author Sonny Kim
 */
class InterpolationRule : public AParsable, private boost::noncopyable {
public:

    /*!
     * \brief Define different overwrite policies that an InterpolationRule will
     *        follow when a value already exists where an inpertolated one might
     *        be placed.
     */
    enum OverwritePolicy {
        //! Never overwrite.
        NEVER,

        //! Overwrite interpolated values only.
        INTERPOLATED,

        //! Always overwrite.
        ALWAYS
    };

    InterpolationRule();
    ~InterpolationRule();
    
    InterpolationRule* clone() const;
    
    static const std::string& getXMLNameStatic();
    
    void applyInterpolations( objects::PeriodVector<Value>& aValuesToInterpolate,
        const objects::PeriodVector<Value>& aParsedValues ) const;
        
    // AParsable methods
    virtual bool XMLParse( rapidxml::xml_node<char>* & aNode );

    virtual void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const;

protected:
    
    DEFINE_DATA(
        // InterpolationRule is the only member of this container hierarchy.
        DEFINE_SUBCLASS_FAMILY( InterpolationRule ),

        //! The from-year that this rule applies.
        DEFINE_VARIABLE( SIMPLE, "from-year", mFromYear, int ),

        //! The optional from-value to use during interpolations
        DEFINE_VARIABLE( SIMPLE, "from-value", mFromValue, Value ),

        //! The to-year that this rule applies.
        DEFINE_VARIABLE( SIMPLE, "to-year", mToYear, int ),

        //! The optional to-value to use during interpolations
        DEFINE_VARIABLE( SIMPLE, "to-value", mToValue, Value ),

        //! The interpolation function that will be used to perform the
        //! interpolations
        DEFINE_VARIABLE( CONTAINER | NOT_PARSABLE, IInterpolationFunction::getXMLNameStatic(), mInterpolationFunction, IInterpolationFunction* ),

        //! The policy this rule will use in the event that an existing
        //! value may get overwritten.
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "overwrite-policy", mOverwritePolicy, OverwritePolicy ),

        //! A flag to indicate a warning should be produced when an
        //! existing value gets overwritten.
        DEFINE_VARIABLE( SIMPLE, "overwrite-warn", mWarnWhenOverwritting, bool )
    )

    //! Flag to check if the interpolation function is fixed in which
    //! case we enable the hack to set the value in the from-year as well
    bool mIsFixedFunction;
    
    //! Internal variable to save apply-to attribute so it can be written out to the debug file
    std::string mApplyTo;
    
    void copy( const InterpolationRule& aOther );

    std::string overwritePolicyEnumToStr( const OverwritePolicy aPolicy ) const;
    
    static const int getLastModelYearConstant();
};

#endif // _INTERPOLATION_RULE_H_
