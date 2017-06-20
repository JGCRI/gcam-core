#ifndef _FOODDEMANDSYSTEM_HPP_
#define _FOODDEMANDSYSTEM_HPP_
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
 * \file fooddemandsystem.hpp
 * \ingroup Objects
 * \brief Header file for FoodDemandSystem, a demand system for staple
 * and nonstaple food.
 * \author Robert Link
 */

#include "functions/include/idemandsystem.hpp"


class Demographic;
class GDP;

/*!
 * \ingroup Objects 
 * \brief The Edmonds, et al. (2016) food demand system

 * \details This class implements the demand system for representing
 *          staple and nonstaple foods, as published by Edmonds, et
 *          al. (2016) in _Climate Change Economics_ (full cite to
 *          follow once the paper is accepted).  This demand system is
 *          a two-component model for demand.  The components are
 *          "staple foods", which are assumed to behave as inferior
 *          goods, and "nonstaple foods", which are assumed to behave
 *          as normal goods, but with demand eventually saturating at
 *          high per capita income levels.  The model also tracks
 *          internally "materials", a stand-in for everything else
 *          that consumers spend their budget on.  Materials demand is
 *          calculated as the residual of consumers' budget after
 *          their food needs are met.  GCAM doesn't use the materials
 *          demand, and the price for the materials component is fixed
 *          as a parameter of the model (i.e., it cannot be set as an
 *          input).  Therefore, we do not expose this third component
 *          to the rest of GCAM.
 *
 *          The model has 10 parameters: the 9 parameters given in
 *          Table 3 of the paper (in the same order as given in the
 *          table), and a tenth regional bias correction factor, as
 *          described in the Regional Bias Correction section of the
 *          paper.  (Note that since each region will have its own
 *          food demand sector, with its own instance of this class,
 *          it is only necessary to include the bias correction factor
 *          for a single region.)  The parameters are pushed into the
 *          parameter vector in the order given in the XML input;
 *          there is currently no mechanism for reading them in by
 *          name or otherwise reordering them.  The parameter list
 *          will be checked at the conclusion of the input parsing,
 *          and the parser will raise an exception if it has not
 *          parsed exactly 10 model parameters.
 *
 *          The equations in this demand system make use of the budget
 *          fractions, \f$\alpha\f$, for the goods in the demand
 *          system.  These budget fractions are themselves computed
 *          fromt the demand quantities output by the system.  In
 *          stand-alone implementations of the demand system we use a
 *          nonlinear equation solver to solve for a self-consistent
 *          set of demands, but in GCAM we use the Trial Value
 *          mechanism.  During the `completeInit()` method we set up a
 *          trial value market for each of the two goods (staple and
 *          nonstaple foods) in the system.  The GCAM solver mechanism
 *          will use these to solve for self-consistent demand
 *          values.
 *
 *          The \f$\alpha\f$ values can range from 0 to 1; however,
 *          there is no way to constrain the solver to values within
 *          this range.  Therefore, we transform the input trial value
 *          \f$\alpha_t\f$ using the formula \f$\alpha = \frac{1}{2}
 *          (1 + \tanh(\alpha_t))\f$.  This ensures that any
 *          trial value produced by the solver is a valid input for
 *          the demand system.
 *
 *          Output is returned as a vector of 2 values, representing
 *          staple demand and nonstaple demand, in that order.  These
 *          demands are in units of thousands of dietary calories, per
 *          capita, per day.
 *
 *          Although the demand system described in the paper is
 *          readily generalizable to an arbitrary number of
 *          components, we have not attempted to make this
 *          implementation similarly generalizable.  This is partly
 *          because having an arbitrary number of components
 *          complicates the parsing, but mostly because we would then
 *          also need to come up with some way of specifying the
 *          behavior of each demand component.  That is, is it a
 *          normal good or an inferior good?  Does the demand saturate
 *          at high income levels?  And so on.  None of these problems
 *          are insurmountable, but the payoff didn't seem to justify
 *          the effort, for the time being.
 */
class FoodDemandSystem : public IDemandSystem {
public:

    //! Constructor
    FoodDemandSystem(void) : IDemandSystem(2) {} // 2 components, see explanation above 
    //! Destructor
    virtual ~FoodDemandSystem(void) {}

    /*!
     * \brief Calculate demand from inputs 
     * \details This function does the calculations for the demand
     *          system described above and in the paper.  As a side
     *          effect, it sets the "demand" side of the trial value
     *          markets for staple and nonstaple budget shares.
     */
    virtual void calcDemand( const std::string & aRegionName,
                             const Demographic & aDemographics,
                             const GDP & aGDP,
                             const std::vector<double> & aprices,
                             int aPeriod,
                             std::vector<double> &aDemandOutput ) const;

    virtual void reportDemand( std::vector<double> &aDemand ) const;
    virtual void reportUnits(std::vector<std::string> &aUnits) const;
    

    /*!
     * \brief Complete the demand system's initialization, prior to
     *        the start of the first model period.
     *
     * \details The main thing that happens in this function is the
     *          setup for the trial value markets.
     */
    virtual void completeInit( const std::string &aRegionName,
                               const std::string & aSectorName );


    //! Get the name used to refer to this class in XML inputs
    virtual const std::string &getXMLName(void) const {return getXMLNameStatic();} 

    //! Static XML name
    static const std::string &getXMLNameStatic(void);

    //! INamed interface
    virtual const std::string& getName(void) const {return mName;}

    //! IParsable interface
    virtual bool XMLParse( const xercesc::DOMNode *aNode );

    //! IRoundTrippable interface
    virtual void toInputXML( std::ostream &aOut, Tabs *aTabs ) const;

private:
    //! Price conversion factor
    const static double mprice_conversion_fac;
    //! Output quantity conversion factor
    const static double mqty_conversion_fac;

    //! Name of this object
    std::string mName;

    //! Names of trial value markets for budget fractions
    std::vector<std::string> mTrialValueMktNames;

    //! Some bit of internal state for the trial value markets that we have to
    //! keep track of ourselves, because of reasons. 
    // NB: This member is mutable because it isn't really our data;
    //     therefore, it makes sense to allow the class that *really*
    //     owns it to make changes to it, even when called from one of
    //     our const methods.
    mutable std::vector<double> mLastValues;

    /*!
     * \brief Demand system parameters 
     * \note Possibly we should consider moving these up into the base
     *       class, since most subclasses will have something similar.
     */
    std::vector<double> mParams;

    /*!
     * \brief Fetch trial budget fraction from the trial value market. 
     * \param aRegion[in] Region for this sector
     * \param aPeriod[in] Period to fetch (usually the current period).
     * \param acomp[in] Component for which to fetch the trial value.
     */
    double getTrialBudgetFrac(const std::string &aRegion, int aPeriod,
                              int acomp) const;

    /*!
     * \brief Set actual budget fractions into the trial value markets
     *        for the solver to compare
     * \param aRegion[in] Region for this sector
     * \param aPeriod[in] Period to set (usually the current period).
     * \param acomp[in] Component for which to set the trial value.     
     * \param alpha[in] Budget fraction value
     */
    void setActualBudgetFrac( const std::string &aRegion, int aPeriod,
                              int acomp, double alpha ) const;

    /*!
     * \brief Income dependence function for staple foods
     * \param lam[in] Lambda parameter from Edmonds et al.
     * \param kap[in] Kappa parameter from Edmonds et al.
     * \param x[in] Normalized pcGDP (= pcGDP/Pm)
     * \param qis[out] Income factor for staple demand.
     * \param etas[out] Income elasticity for staple demand. 
     * \details The staple demand function separates into three multiplicative
     *          components: an own-price factor, a cross-price factor, and an
     *          income factor.  This function calculates the income factor and
     *          the income elasticity.  The latter is needed in order to
     *          calculate the exponents in the price components.
     * \notes kappa in this function is the version defined in the paper. It
     *        should be somewhere around 2.5 (vice the values of 12-15 for the
     *        'k' parameter defined in the R implementation).
     */
    static void stapleIncomeFunc( double lam, double kap, double x, double &qis,
                                  double &etas ); 

    /*!
     * \brief Income dependence function for nonstaple foods.
     * \param nu[in] Nu parameter from Edmonds et al.
     * \param x[in] Normalized pcGDP (= pcGDP/Pm)
     * \param qin[out] Income factor for nonstaple demand.
     * \param etan[out] Income elasticity for nonstaple demand.
     * \details The nonstaple demand function separates into three multiplicative
     *          components: an own-price factor, a cross-price factor, and an
     *          income factor.  This function calculates the income factor and
     *          the income elasticity.  The latter is needed in order to
     *          calculate the exponents in the price components.
     * \notes nu in this function should be the version defined in the paper.  It
     *        differs by a factor of 2 from the version defined in the R code.
     *        Typical values will be somewhere around 1.
     */
    static void nonStapleIncomeFunc( double nu, double x, double &qin, double
                                     &etan );
};

#endif
