#ifndef _ICARBON_STOCK_H_
#define _ICARBON_STOCK_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

/*! 
 * \file icarbon_stock.h
 * \ingroup Objects
 * \brief ICarbonStock class header file.
 * \author Jim Naslund and Ming Chang
 */

#include "util/base/include/ivisitable.h"
#include "util/base/include/iparsable.h"
#include "util/base/include/iround_trippable.h"
#include "ccarbon_model/include/carbon_model_utils.h"

class EnvironmentalInfo;

/*!
 * \brief Interface for a carbon stock.
 * \details A carbon stock is a store of carbon that resides in a carbon container.
 *          Its amount can increase and decrease throughout the course of the
 *          model run.
 */
class ICarbonStock : public IParsable,
                     public IVisitable,
                     public IRoundTrippable {

public:
    //! \brief Inline default constructor.
    ICarbonStock(){}
    //ICarbonStock( const ICarbonStock& aCarbonStock );
    virtual ~ICarbonStock(){}

    virtual ICarbonStock* clone() const = 0;

    // IParsable Interface
    virtual bool XMLParse( const xercesc::DOMNode* aNode ) = 0;
    // IRoundTripable Interface
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const = 0;
    virtual void toDebugXML( const int aPeriod, std::ostream& aOut,
                             Tabs* aTabs ) const = 0;
    // IVisitable Interface
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const = 0;

    /*!
     * \brief Depletes the appropriate amount from the stock.
     * \return the amount depleted.
     */
    virtual double transfer( const EnvironmentalInfo* aEnvInfo,
                             FlowType aFlowType, const int aYear ) = 0;

    /*!
     * \brief Adds the passed in amount to the stock.
     * \param aValueToAdd the value to add.
     */
    virtual void addToStock( double aValueToAdd, const int aYear ) = 0;

    /*!
     * \brief Returns the current stock.
     * \return the current stock.
     */
    virtual const double getStock( const int aYear ) = 0;

    /*!
     * \brief Sets the current stock for the specified year.
     * \details Sets the current stock variable to the carbon stock in the
     *          year before the specified year.
     */
    virtual void setCurrentStock( const int aYear ) = 0;

    /*!
     * \brief Returns the current NPP per unit area .
     * \return the current NPP/area.
     */
    virtual double getNPPOverAreaRatio( const int aYear ) = 0;

    /*!
     * \brief deduct the specified value from the carbon stock 
     * \details deduct the specified value fromt he carbon stock. This is meant 
     *  for use where an explicit deduction is needed outside of the transfer 
     *  functionality. For example, the special case for when NPP needs to be 
     *  deducted from the atmosphere box
     * \param  double aDeductValue, value to deduct
     * \param  int aYear, the year of the carbon stock to operate on
     */

    virtual void deductFromStock( const double aDeductValue, const int aYear ) = 0;

    virtual void modifyCarbonStock( const double aNewValue, const int aYear ) = 0;

    virtual double getTurnoverTimescale() = 0;

    virtual void copyBoxType( BoxType aCarbonBoxType ) = 0;
    virtual BoxType returnBoxType() = 0;
};

#endif // _ICARBON_STOCK_H_
