#ifndef _ABSOLUTE_COST_LOGIT_HPP_
#define _ABSOLUTE_COST_LOGIT_HPP_
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
 * \file absolute_cost_logit.hpp
 * \ingroup Objects
 * \brief AbsoluteCostLogit class declaration file
 * \author Robert Link
 */

#include "functions/include/idiscrete_choice.hpp"
#include "util/base/include/time_vector.h"

/*!
 * \ingroup Objects
 * \brief The absolute cost logit based discrete choice function.
 * \details A more technical description for the absolute cost logit function is
 *          the Generalized Extreme Value (GEV) variant of the logit discrete choice
 *          function.
 *
 *          <b>XML specification for AbsoluteCostLogit</b>
 *          - XML name: \c AbsoluteCostLogit::getXMLNameStatic()
 *          - Contained by: Sector and Subsector
 *          - Parsing inherited from class: None
 *          - Attributes: None
 *          - Elements:
 *              - \c "logit-exponent" AbsoluteCostLogit::mLogitExponent
 *                   The logit exponent used to describe the width of the distribution.
 *              - \c "base-cost" AbsoluteCostLogit::mBaseCost
 *                   The base cost that sets the scale for cost differences.
 *
 * \author Robert Link
 */
class AbsoluteCostLogit : public IDiscreteChoice {
    friend class CalibrateShareWeightVisitor;
public:
    AbsoluteCostLogit();
    virtual ~AbsoluteCostLogit();

    static const std::string& getXMLNameStatic();

    // IParsable methods
    virtual bool XMLParse( const xercesc::DOMNode *aNode );

    // IRoundTrippable methods
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const;

    // IDiscreteChoice methods
    virtual const std::string& getXMLName() const { return getXMLNameStatic(); }

    virtual void toDebugXML( const int aPeriod, std::ostream& aOut,
                             Tabs *aTabs ) const;

    virtual double calcUnnormalizedShare( const double aShareWeight, const double aCost,
                                          const int aPeriod ) const;

    virtual double calcShareWeight( const double aShare, const double aCost, const double aAnchorShare,
                                    const double aAnchorCost, const int aPeriod ) const;

    virtual void setBaseCost( const double aBaseCost, const std::string &aFailMsg );

protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        IDiscreteChoice,
                            
        //! The logit exponents by period.
        CREATE_ARRAY_VARIABLE( mLogitExponent, objects::PeriodVector<double>, "logit-exponent" ),

        //! scale factor for cost -- may be parsed or set in calibration
        CREATE_SIMPLE_VARIABLE( mBaseCost, double, "base-cost" ),

        //! flag indicating whether a base cost was set in the XML input
        CREATE_SIMPLE_VARIABLE( mParsedBaseCost, bool, "is-base-cost-parsed" )
    )
};

#endif // _ABSOLUTE_COST_LOGIT_HPP_
