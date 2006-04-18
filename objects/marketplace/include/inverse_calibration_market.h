#ifndef _INVERSE_CALIBRATION_MARKET_H_
#define _INVERSE_CALIBRATION_MARKET_H_
#if defined(_MSC_VER_)
#pragma once
#endif

/*! 
* \file inverse_calibration_market.h
* \ingroup Objects
* \brief The InverseCalibrationMarket class header file.
* \author Josh Lurz
*/

#include "marketplace/include/market.h"

/*!
* \ingroup Objects
* \brief A class which defines a market for solving calibration markets which
*        have an inverse response to the solved term.
* \details Inverse calibration markets allow the calibration of values where as
*          the solved or price term increases, the trial quantity decreases.
*          This is the inverse behavior of the CalibrationMarket. To use a
*          InverseCalibrationMarket initially set for each period supply to
*          equal the known or target value of the variable. Use the price term
*          of the market to calculate the iteration value, which should be
*          stored in the demand side of the market.
* \note The implementation is very similar to a CalibrationMarket. The functions
*       which differ are nullSupply, nullDemand and
*       meetsSpecialSolutionCriteria. These differ due to the constraint being
*       stored in the supply variable instead of the demand variable.
* \author Josh Lurz
*/

class InverseCalibrationMarket: public Market {
public:
    InverseCalibrationMarket( const std::string& goodNameIn, const std::string& regionNameIn, const int periodIn );
    ~InverseCalibrationMarket();
    virtual IMarketType::Type getType() const;

    virtual void initPrice();
    virtual void setPrice( const double priceIn );
    virtual void setPriceFromLast( const double lastPriceIn );
    virtual double getPrice() const;

    virtual void addToDemand( const double demandIn );
    virtual double getDemand() const;

    virtual void nullSupply();
    virtual void nullDemand();
    virtual double getSupply() const;
    virtual double getSupplyForChecking() const;
    virtual void addToSupply( const double supplyIn );
    virtual bool meetsSpecialSolutionCriteria() const;

    virtual bool shouldSolve() const;
    virtual bool shouldSolveNR() const;
protected:
    virtual void toDebugXMLDerived( std::ostream& out, Tabs* tabs ) const;
};

#endif // _INVERSE_CALIBRATION_MARKET_H_
