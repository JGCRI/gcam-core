#ifndef _LINKED_GHG_POLICY_H_
#define _LINKED_GHG_POLICY_H_
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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */


/*! 
 * \file linked_ghg_policy.h
 * \ingroup Objects
 * \brief The LinkedGHGPolicy class header file.
 * \author Pralit Patel
 */

#include "policy/include/policy_ghg.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"

/*!
 * \ingroup Objects
 * \brief A class which will link a policy to another GHGPolicy which really
 *        drives the policy.
 * \details This policy does not set a constraint or price itself, it assumes the
 *          policy it links to will.  It merely allows additional gasses to be
 *          included into the policy it links to.  This class allows a price
 *          multiplier to adjust the price of the linked policy which could be to
 *          adjust for global warming potential or just turn "off" price feedbacks.
 *          Similarly a multiplier exists for the quantity value of the gas as well.
 *          Note that these multipliers can change over time to simulate a "phase in"
 *          of the gas into the policy at a later time.
 *          In addition this type of policy has the ability to switch from linking
 *          from one policy to another at a specified start-year.  Such a use case
 *          would be to specify region specific constraints in the near term and switch
 *          to a global market for target finding in later model periods. Please see
 *          Marketplace::createLinkedMarket for additional details about how changing
 *          market links over time works as well as some caveats about it's use.
 *
 *          <b>XML specification for LinkedGHGPolicy</b>
 *          - XML name: \c LinkedGHGPolicy::getXMLNameStatic()
 *          - Contained by: region
 *          - Parsing inherited from class: None
 *          - Attributes: \c name LinkedGHGPolicy::mName
 *          - Elements:
 *              - \c market LinkedGHGPolicy::mMarket
 *                   The name of the market to create blocks of regions which share this
 *                   policy.
 *              - \c linked-policy LinkedGHGPolicy::mLinkedPolicyName
 *                   The name of the ghg policy to link to.  This name and the current
 *                   region's name will be used to look up the market and link to it.
 *              - \c price-unit LinkedGHGPolicy::mPriceUnits
 *                   A label for the units of the price of this market for reporting
 *                   which may differ from the linked market if the price adjust is
 *                   converting units.
 *              - \c output-unit LinkedGHGPolicy::mOutputUnits
 *                   A label for the units of the quantity of this market for reporting
 *                   which may differ from the linked market if the demand adjust is
 *                   converting units.
 *              - \c price-adjust LinkedGHGPolicy::mPriceAdjust
 *                   A multiplier on the price of the linked market to allow for GWP
 *                   unit conversion or to turn "off" price feedbacks.
 *              - \c price-adjust LinkedGHGPolicy::mPriceAdjust
 *                   A multiplier on the quantity added to the linked market to allow
 *                   for GWP unit conversion or to turn "off" inclusion in a constraint.
 *              - \c start-year LinkedGHGPolicy::mStartYear
 *                   The first year this linked policy should be used (-1 indicates all years).
 *                   This is the parameter which allows us to switch markets over time.
 *              - \c policy-name LinkedGHGPolicy::mPolicyName
 *                   The name this policy will use to create the linked market.  Note
 *                   this is typically the same as mName however may differ if for
 *                   instance a user wanted to switch markets over time.
 *
 * \sa Marketplace::createLinkedMarket
 * \sa LinkedMarket
 * \author Pralit Patel
 */
class LinkedGHGPolicy: public GHGPolicy {
public:
    LinkedGHGPolicy();

    virtual GHGPolicy* clone() const;
    virtual const std::string& getName() const;
    virtual const std::string& getXMLName() const;
    static const std::string& getXMLNameStatic();
    virtual void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual void completeInit( const std::string& aRegionName );
    virtual bool isApplicable( const std::string& aRegion ) const;
    virtual void setConstraint( const std::vector<double>& aConstraint );
protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        GHGPolicy,

        //! The name of the policy to link to.
        DEFINE_VARIABLE( SIMPLE, "linked-policy", mLinkedPolicyName, std::string ),

        //! A label for the price units of this market
        DEFINE_VARIABLE( SIMPLE, "price-unit", mPriceUnits, std::string ),

        //! A label for the units of this market
        DEFINE_VARIABLE( SIMPLE, "output-unit", mOutputUnits, std::string ),
        
        //! A price adjustment factor by period.  Could be useful for unit conversion or
        //! "turning off" price feedbacks from the linked market.
        DEFINE_VARIABLE( ARRAY, "price-adjust", mPriceAdjust, objects::PeriodVector<Value> ),
        
        //! A demand adjustment factor by period.  Could be useful for unit conversion (GWP) or
        //! "turning off" participation in the linked market.
        DEFINE_VARIABLE( ARRAY, "demand-adjust", mDemandAdjust, objects::PeriodVector<Value> ),
        
        //! The first year this linked policy should be used (-1 indicates all years).
        //! This is the parameter which allows us to switch markets over time.
        DEFINE_VARIABLE( SIMPLE, "start-year", mStartYear, int ),
        
        //! The name this policy will use to create the linked market.  Note this is typically
        //! the same as mName however may differ if for instance a user wanted to switch markets
        //! over time.
        DEFINE_VARIABLE( SIMPLE, "policy-name", mPolicyName, std::string )
    )
    
    void copy( const LinkedGHGPolicy& aOther );
};

#endif // _LINKED_GHG_POLICY_H_
