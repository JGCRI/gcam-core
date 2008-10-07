#ifndef _IMARKET_TYPE_H_
#define _IMARKET_TYPE_H_
#if defined(_MSC_VER_)
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
 * \file imarket_type.h
 * \ingroup Objects
 * \brief The IMarketType class header file.
 * \author Josh Lurz
 */

/*!
 * \ingroup Objects
 * \brief An interface which specifies the various types of markets.
 * \note This was separated from market.h to reduce dependencies. 
 * \author Josh Lurz
 */

class IMarketType
{
public:
    /*!
     * \brief An enumeration of the types of markets used throughout the model.
     * \note Not all market types can be created from outside the marketplace.
     *       Check the documentation for the specific type of market for
     *       guidance.
     * \note The Market::createTypeFromString function must be updated to
     *       reflect changes in this enum for debugging output to be correct.
     */
    enum Type {
      
      /*!
       * \brief Standard supply-demand-price market.
       * \details The standard market used for supply and demand of good
       *          throughout the model. The market may or may not be solved.
       *          Supply must increase with price and demand must decrease.
       */
      NORMAL,

      /*!
       * \brief A market used to calibrate variable that is determined
       *        parametrically.
       * \details This type of market facilitates the calibration of a value
       *          that is determined indirectly through an independent
       *          parameter. For a variable V that is a function of y, V = F(y),
       *          y is varied until the trial value of V matches the target
       *          value. The function F(y) needs to be implemented appropriately
       *          in the code. The price in the market is the parameter y, which
       *          is used as a trial variable to calculate the result F(y) for
       *          each iteration, which is stored as the market supply. The
       *          market demand should be set to the target value. The
       *          calculated value must increase as the price variable
       *          increases.
       */
      CALIBRATION,

      /*!
       * \brief A market used to calibrate variable that is determined
       *        parametrically, in which the calculated value has an inverse
       *        response to the trial value.
       * \details This type of market facilitates the calibration of a value
       *          that is determined indirectly through an independent
       *          parameter. For a variable V that is a function of y, V = F(y),
       *          y is varied until the trial value of V matches the target
       *          value. The function F(y) needs to be implemented appropriately
       *          in the code. The price in the market is the parameter y, which
       *          is used as a trial variable to calculate the result F(y) for
       *          each iteration, which is stored as the market demand. The
       *          market supply should be set to the target value. The
       *          calculated value must decrease as the price variable
       *          increases.
       */
      INVERSE_CALIBRATION,

      /*!
       * \brief A constraint market.
       * \details The price parameter represents the tax on the gas or fuel. The market
       *          may be a fixed tax market which is not solved, or a market
       *          which solves for a constraint on emissions or fuels. If it is a
       *          constraint market, supply should be set to the constraint and
       *          demand should be updated to the current emissions. Demand
       *          should decrease with price. Previously, type was GHG.
       */
      TAX,

      /*!
       * \brief A portfolio standard market.
       * \details The price parameter represents the tax on the gas. The market
       *          may be a fixed tax market which is not solved, or a market
       *          which solves for a constraint on emissions. If it is a
       *          constraint market, supply should be set to the constraint and
       *          demand should be updated to the current emissions. Demand
       *          should decrease with price.
       */
      SUBSIDY,

      /*!
       * \brief A market which provides a trial value for a parameter.
       * \details A trial value market allows for the access to a variable which
       *          would otherwise be unknown until after it is needed. The trial
       *          value is accessed through the price variable and the demand
       *          should be updated to reflect the current calculated value. 
       */
      TRIAL_VALUE,

      /*! 
       * \brief A market used internally by the marketplace for a trial demand
       *        value.
       * \details This market is used internally by the marketplace when a
       *          normal market is separated into a price and demand market
       *          pair. The pairing allows trial values for demand and price to
       *          be retrieved during an iteration in a manner that is
       *          transparent to the model outside of the market.
       * \note Demand markets are only created internally in the marketplace and
       *       cannot be created from the rest of the model.
       */
      DEMAND,

      /*! 
       * \brief A market used internally by the marketplace for a trial price
       *        value.
       * \details This market is used internally by the marketplace when a
       *          normal market is separated into a price and demand market
       *          pair. The pairing allows trial values for demand and price to
       *          be retrieved during an iteration in a manner that is
       *          transparent to the model outside of the market.
       * \note Price markets are only created internally in the marketplace and
       *       cannot be created from the rest of the model.
       */
      PRICE,

      /*!
       * \brief End enum marker.
       * \note This is not a market type but a flag to denote the end of the
       *       enum. Always add market types before this.
       */
      END
    };
};

#endif // _IMARKET_TYPE_H_
