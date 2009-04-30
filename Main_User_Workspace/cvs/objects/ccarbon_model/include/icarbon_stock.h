#ifndef _ICARBON_STOCK_H_
#define _ICARBON_STOCK_H_
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
    virtual double getStock( const int aYear ) = 0;

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
