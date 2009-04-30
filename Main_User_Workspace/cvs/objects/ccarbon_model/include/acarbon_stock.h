#ifndef _ACARBON_STOCK_H_
#define _ACARBON_STOCK_H_
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
 * \file acarbon_stock.h
 * \ingroup Objects
 * \brief ACarbonStock class header file.
 * \author Jim Naslund and Ming Chang
 */

#include "ccarbon_model/include/icarbon_stock.h"
#include "util/base/include/time_vector.h"

/*!
 * \brief ICarbonStock Interface
 * \details A basic implementation of the ICarbonStock Interface that supports
 *          all interface functions.  This class is essentially a wrapper around
 *          a double representing a carbon stock.
 */
class ACarbonStock : public ICarbonStock {

public:
    ACarbonStock();
    ACarbonStock( const ACarbonStock& aCarbonStock );
    virtual ACarbonStock* clone() const = 0;

    virtual ~ACarbonStock();

    // IParsable Interface
    virtual bool XMLParse( const xercesc::DOMNode* aNode ) = 0;
    // IRoundTripable Interface
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const;
    virtual void toDebugXML( const int aPeriod, std::ostream& aOut,
                             Tabs* aTabs ) const;
    // IVisitable Interface
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;

    virtual double transfer( const EnvironmentalInfo* aEnvInfo,
                             FlowType aFlowType, const int aYear ) = 0;
    virtual void addToStock( double aValueToAdd, const int aYear );
    virtual double getStock( const int aYear );
    virtual void setCurrentStock( const int aYear );
    virtual double getNPPOverAreaRatio( const int aYear );
    virtual void deductFromStock( const double aDeductValue, const int aYear );
    virtual void modifyCarbonStock( const double aNewValue, const int aYear );
    virtual double getTurnoverTimescale();
    virtual void copyBoxType( BoxType aCarbonBoxType );
    virtual BoxType returnBoxType();
protected:
    //! initial stock value
    double mInitialStock;
    //! Carbon stock for each year.
    objects::YearVector<double> mCarbonStock;
    //! CarbonBox type associated with the stock
    BoxType mCarbonBoxType;

};

#endif // _ACARBON_STOCK_H_
