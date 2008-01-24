#ifndef _NPP_H_
#define _NPP_H_
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
