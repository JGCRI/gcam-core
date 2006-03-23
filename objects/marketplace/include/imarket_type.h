#ifndef _IMARKET_TYPE_H_
#define _IMARKET_TYPE_H_
#if defined(_MSC_VER_)
#pragma once
#endif

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
       * \brief A market used to calibrate a parameter to a target value.
       * \details The price parameter of the market should be used as the trial
       *          value to calculate the result for the iteration, which is
       *          stored in the supply variable. The demand variable should be
       *          set to the known value. The calculated value must increase as
       *          price increases.
       */
      CALIBRATION,

      /*!
       * \brief A market used to calibrate a parameter to a target value in
       *        which the calculated value has an inverse response to price.
       * \details The price parameter of the market should be used as the trial
       *          value to calculate the result for the iteration, which is
       *          stored in the demand variable. The supply variable should be
       *          set to the known value. The calculated value must decrease
       *          price increases.
       */
      INVERSE_CALIBRATION,

      /*!
       * \brief Greenhouse gas constraint market.
       * \details The price parameter represents the tax on the gas. The market
       *          may be a fixed tax market which is not solved, or a market
       *          which solves for a constraint on emissions. If it is a
       *          constraint market, supply should be set to the constraint and
       *          demand should be updated to the current emissions. Demand
       *          should decrease with price.
       */
      GHG,

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
