#ifndef _FIXED_PRODUCTION_STATE_H_
#define _FIXED_PRODUCTION_STATE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
 */

/*! 
 * \file fixed_production_state.h
 * \ingroup Objects
 * \brief The FixedProductionState header file.
 * \author Josh Lurz
 */

#include "technologies/include/iproduction_state.h"
#include "util/base/include/value.h"

/*! 
 * \ingroup Objects
 * \brief The production state of a Technology which has a predefined output
 *        level in its initial period.
 * \details Technologies in a fixed production state produce output equal to
 *          their predefined output level.
 * \author Josh Lurz
 */
class FixedProductionState: public IProductionState
{
    friend class ProductionStateFactory;
public:

    // ISimpleComponentMethods
    FixedProductionState* clone() const;
    bool isSameType( const std::string& aType ) const;
    const std::string& getName() const;
	virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const;

    // IProductionState methods
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
    /*!
     * \brief Protected constructor which may only be called from the
     *        ProductionStateFactory.
     */
    FixedProductionState();
    
    /*
     * \brief Get the static name of this object.
     * \return The static name of this object.
     */
    static const std::string& getXMLNameStatic();

    //! The level of fixed output.
    Value mFixedOutput;
};

#endif // _FIXED_PRODUCTION_STATE_H_
