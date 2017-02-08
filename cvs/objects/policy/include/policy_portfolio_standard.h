#ifndef _POLICY_PORTFOLIO_STANDARD_H_
#define _POLICY_PORTFOLIO_STANDARD_H_
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
* \file policy_portfolio_standard.h
* \ingroup Objects
* \brief The PolicyPortfolioStandard class header file.
* \author Sonny Kim
*/

#include <xercesc/dom/DOMNode.hpp>
#include <string>
#include <boost/core/noncopyable.hpp>

#include "util/base/include/inamed.h"
#include "util/base/include/iround_trippable.h"
#include "util/base/include/time_vector.h"
#include "util/base/include/data_definition_util.h"

/*! 
* \ingroup Objects
* \brief Class which defines a portfolio standard policy. 
* \author Sonny Kim
*/
class PolicyPortfolioStandard: public INamed, public IRoundTrippable, private boost::noncopyable {
public:
    PolicyPortfolioStandard();

    const std::string& getName() const;
    const std::string& getXMLName() const;
    static const std::string& getXMLNameStatic();
    void XMLParse( const xercesc::DOMNode* node );
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    void completeInit( const std::string& aRegionName );
protected:

    DEFINE_DATA(
        // PolicyPortfolioStandard is the only member of this container hierarchy.
        DEFINE_SUBCLASS_FAMILY( PolicyPortfolioStandard ),
        
        //! Policy name
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),
        
        //! Name of the market
        DEFINE_VARIABLE( SIMPLE, "market", mMarket, std::string ),
        
        //! Type of policy (tax or subsidy)
        DEFINE_VARIABLE( SIMPLE, "policyType", mPolicyType, std::string ),
        
        //! Boolean to use share of total or quantity constraint
        DEFINE_VARIABLE( SIMPLE, "isShareBased", mIsShareBased, bool ),
        
        //! Quantity constraint by year
        DEFINE_VARIABLE( ARRAY, "constraint", mConstraint, objects::PeriodVector<double> ),
        
        //! Fixed tax on Fuel by year($/GJ)
        DEFINE_VARIABLE( ARRAY, "fixedTax", mFixedTax, objects::PeriodVector<double> ),
        
        //! Share of total or sectoral output
        DEFINE_VARIABLE( ARRAY, "share-of-sector-output", mShareOfSectorOutput, objects::PeriodVector<double> ),
        
        //! The minimum price below which the constraint is considered non-binding.
        DEFINE_VARIABLE( ARRAY, "min-price", mMinPrice, objects::PeriodVector<double> )
    )
};

#endif // _POLICY_PORTFOLIO_STANDARD_H_
