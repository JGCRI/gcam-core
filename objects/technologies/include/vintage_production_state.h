/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
 */

#ifndef _VINTAGE_PRODUCTION_STATE_H_
#define _VINTAGE_PRODUCTION_STATE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file vintage_production_state.h
 * \ingroup Objects
 * \brief The VintageProductionState header file.
 * \author Josh Lurz
 */

#include "technologies/include/iproduction_state.h"
#include "util/base/include/value.h"

/*! 
 * \ingroup Objects
 * \brief The production state of a Technology after its initial period of
 *        production but before it is retired.
 * \details Technologies in a vintage production state produce output equal to
 *          their base period output reduced by any deprecation or short term
 *          shutdown decisions. 
 * \author Josh Lurz
 */
class VintageProductionState: public IProductionState
{
    friend class ProductionStateFactory;
public:


    // ISimpleComponent methods.
    virtual VintageProductionState* clone() const;
    virtual bool isSameType( const std::string& aType ) const;
    virtual const std::string& getName() const;
	virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const;

    // IProductionState methods.
    virtual double calcProduction( const std::string& aRegionName,
                                   const std::string& aSectorName,
                                   const double aVariableOutput,
                                   const MarginalProfitCalculator* aMarginalProfitCalc,
                                   const double aFixedOutputScaleFactor,
                                   const std::vector<IShutdownDecider*>& aShutdownDeciders,
                                   const int aPeriod ) const;

    virtual void setBaseOutput( const double aBaseOutput,
                                const int aBaseYear );

    virtual bool isNewInvestment() const;

    virtual bool isOperating() const;

protected:
    //! The output level of the vintage in its initial period.
    Value mBaseOutput;

    //! The initial year of the vintage.
    int mInitialYear;

    /*
     * \brief Get the static name of this object.
     * \return The static name of this object.
     */
    static const std::string& getXMLNameStatic();

    /*!
     * \brief Protected constructor which may only be called from the
     *        ProductionStateFactory.
     */
    VintageProductionState();

    double calcShutdownCoefficient( const std::string& aRegionName,
								    const std::string& aSectorName,
                                    const std::vector<IShutdownDecider*>& aShutdownDeciders,
                                    const MarginalProfitCalculator* aMarginalProfitCalc,
                                    const int aPeriod ) const;
};

#endif // _VINTAGE_PRODUCTION_STATE_H_
