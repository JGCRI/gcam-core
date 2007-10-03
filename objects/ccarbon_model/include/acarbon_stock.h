#ifndef _ACARBON_STOCK_H_
#define _ACARBON_STOCK_H_
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
 * \file acarbon_stock.h
 * \ingroup Objects
 * \brief ACarbonStock class header file.
 * \author Jim Naslund and Ming Chang
 */

#include "ccarbon_model/include/icarbon_stock.h"
#include "util/base/include/time_vector.h"

/*!
 * \brief An implementation of the ICarbonStock Interface.
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
    //! The current stock.
    double mCurrentStock;
	/****** uncertain change ******/
	//! initial stock value
	double mInitialStock;
    //! Carbon stock for each year.
    objects::YearVector<double> mCarbonStock;
	//! CarbonBox Name associate with the stock
	BoxType mCarbonBoxType;

};

#endif // _ACARBON_STOCK_H_
