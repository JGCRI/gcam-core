#ifndef _RELATIVE_COST_LOGIT_HPP_
#define _RELATIVE_COST_LOGIT_HPP_
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
 * \file relative_cost_logit.hpp
 * \ingroup Objects
 * \brief RelativeCostLogit class declaration file
 * \author Robert Link
 */

#include "functions/include/idiscrete_choice.hpp"
#include "util/base/include/time_vector.h"


/*!
 * \ingroup Objects
 * \brief The relative cost logit based discrete choice function.
 * \details A more technical description for the relative cost logit function is
 *          the Weibull variant of the logit, as described in Clarke &
 *          Edmonds (1993).
 *
 *          <b>XML specification for RelativeCostLogit</b>
 *          - XML name: \c RelativeCostLogit::getXMLNameStatic()
 *          - Contained by: Sector and Subsector
 *          - Parsing inherited from class: None
 *          - Attributes: None
 *          - Elements:
 *              - \c "logit-exponent" RelativeCostLogit::mLogitExponent
 *                   The logit exponent used to describe the width of the distribution.
 *
 * \warning In this formulation of the logit function negative prices can not be
 *          tolerated see RelativeCostLogit::calcUnnormalizedShare for how these
 *          situations are handled.
 * \author Robert Link
 */
class RelativeCostLogit : public IDiscreteChoice {
public:
    RelativeCostLogit();
    virtual ~RelativeCostLogit();

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

    virtual void setBaseCost( const double aBaseCost );

private:
    //! The logit exponents by period.
    objects::PeriodVector<double> mLogitExponent;

    static double getMinCostThreshold();
};

#endif // _RELATIVE_COST_LOGIT_HPP_
