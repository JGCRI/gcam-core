#ifndef _IDISCRETE_CHOICE_HPP_
#define _IDISCRETE_CHOICE_HPP_
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
 * \file idiscrete_choice.hpp
 * \ingroup Objects
 * \brief IDiscreteChoice class declaration file
 * \author Robert Link
 */

#include "util/base/include/iparsable.h"
#include "util/base/include/iround_trippable.h"


/*!
 * \ingroup Objects
 * \brief Base class for discrete choice model class hierarchy
 * \details Subclasses of this class describe different formulae for
 *          imputing shares to competing options with different
 *          prices.  The classes gather in a single place the
 *          functions for calculating the discrete choice function and
 *          computing share weights 
 * \note The calcAveragePrice method is currently only used in the land-allocator.
 *       The sectors and and subsectors are currently not using this and instead
 *       are calculating a straight average cost.
 */
class IDiscreteChoice : public IParsable, public IRoundTrippable {
public:
    /*!
     * \brief Constructor.
     * \details Inlined constructor to avoid compiler problems with abstract
     *          base classes.
     */
    inline IDiscreteChoice();

    /*!
     * \brief Destructor.
     * \details Inlined destructor to avoid compiler problems with abstract base
     *          classes and allow deletion through the base class pointer.
     */
    virtual inline ~IDiscreteChoice();

	/*!
     * \brief Write data from this object in an XML format for debugging.
	 * \param aPeriod Period for which to write data.
	 * \param aOut Filestream to which to write.
	 * \param aTabs Object responsible for writing the correct number of tabs.
     */
	virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const = 0;

    /*!
     * \brief Return the actual XML name of the object.
     */
    virtual const std::string& getXMLName() const = 0;
  
    /*!
     * \brief Compute the unnormalized share given the cost and share weight of the
     *        give option.
     * \param aShareWeight The weighting term used in the share calculation.
     * \param aCost The cost of the option.
     * \param aPeriod The current model period.
     * \return The unnormalized share.
     */
    virtual double calcUnnormalizedShare( const double aShareWeight, const double aCost,
                                          const int aPeriod ) const = 0;

    /*!
     * \brief Compute the mean mean according the the discrete choice function's
     *        parameterization.
     * \param aUnnormalizedShareSum The sum of all of the shares as calculated by
     *                              exp( calcUnnormalizedShare ) which will be used to
     *                              calculate the mean cost.
     * \param aLogShareFac A log( unnormalized share ) factor that has been factored
     *                     out of aUnnormalizedShareSum.  Doing this allows for calculating
     *                     the average cost in a numerically stable way.
     * \param aPeriod The current model period.
     * \return The average cost.
     */
    virtual double calcAverageCost( const double aUnnormalizedShareSum,
                                    const double aLogShareFac,
                                    const int aPeriod ) const = 0;

    /*!
     * \brief Compute the share weight by inverting the discrete choice function
     *        given the values of the other terms in the equation.
     * \details Given the discrete choice is calculating unnormalized shares we must
     *          anchor this calculation which is done by assuming some "anchor" option
     *          receives a share weight of 1.
     * \param aShare The share the option should receive.
     * \param aCost The cost of the option.
     * \param aAnchorShare The share of the anchoring option.
     * \param aAnchorCost The cost of the anchoring option.
     * \param aPeriod The current model period.
     * \return The share weight.
     */
    virtual double calcShareWeight( const double aShare, const double aCost, const double aAnchorShare,
                                    const double aAnchorCost, const int aPeriod ) const = 0;

    virtual double calcShareWeight( const double aShare, const double aCost, const int aPeriod ) const = 0;

    virtual double calcImpliedCost( const double aShare, const double aCost, const int aPeriod ) const = 0;

    virtual void setOutputCost( const double aCost ) = 0;

    /*!
     * \brief Sets the base cost to use in the choice formulation if necessary.
     * \warning This value may be ignored if a user explicitly parsed a value to
     *          use instead.
     * \param aBaseCost The base cost to set.
     * \param aFailMsg String to prepend to log message, if an illegal value is passed for aBaseCost.
     */
    virtual void setBaseCost( const double aBaseCost, const std::string &aFailMsg ) = 0;
};

// Inline function definitions.
inline IDiscreteChoice::IDiscreteChoice(){
}

inline IDiscreteChoice::~IDiscreteChoice(){
}

#endif // _IDISCRETE_CHOICE_HPP_
