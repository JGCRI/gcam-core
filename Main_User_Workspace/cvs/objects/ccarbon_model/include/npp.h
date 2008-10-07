#ifndef _NPP_H_
#define _NPP_H_
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
 * \file npp.h
 * \ingroup Objects
 * \brief NPP class header file.
 * \author Jim Naslund and Ming Chang
 */

#include "ccarbon_model/include/icarbon_stock.h"

/*!
 * \brief An implementation of the ICarbonStock Interface that represents
 *        NPP.
 * \details An implementation of the ICarbonStock Interface. NPP is a special
 *          type of stock that never loses its initial value.  It also cannot
 *          receive transfers.
 */
class NPP : public ICarbonStock {

public:
    NPP();
    NPP( const NPP& aNPP );
    virtual NPP* clone() const;
    ~NPP();

    // IParsable Interface
    virtual bool XMLParse( const xercesc::DOMNode* aNode );
    static const std::string& getXMLNameStatic();
    // IRoundTripable Interface
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const;
    virtual void toDebugXML( const int aPeriod, std::ostream& aOut,
                             Tabs* aTabs ) const;
    // IVisitable Interface
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;

    virtual double transfer( const EnvironmentalInfo* aEnvInfo,
                             FlowType aFlowType, const int aYear );
    virtual void addToStock( double aValueToAdd, const int aYear );
    virtual const double getStock( const int aYear );
    virtual void setCurrentStock( const int aYear );
    virtual double getNPPOverAreaRatio( const int aYear );
    virtual void deductFromStock( const double aDeductValue, const int aYear );
    virtual void modifyCarbonStock( const double aNewValue, const int aYear );
    virtual double getTurnoverTimescale();
    virtual void copyBoxType( BoxType aCarbonBoxType );
    virtual BoxType returnBoxType();

private:
    //! The initial NPP value.
    double mInitialNPP;
    //! The current value
    double mCurrentNPP;
    //! Carbon stock for each year.
    objects::YearVector<double> mNPP;
    //! The NPP/area ratio
    objects::YearVector<double> mNPPOverArea;
    //! CarbonBox Name associate with the NPP
    BoxType mCarbonBoxType;
    //! Condition for NPP unit/are
    bool mUnitPerArea;

};

#endif // _NPP_H_
