/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
 */

#ifndef _RETIRED_PRODUCTION_STATE_H_
#define _RETIRED_PRODUCTION_STATE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file retired_production_state.h
 * \ingroup Objects
 * \brief The RetiredProductionState header file.
 * \author Josh Lurz
 */

#include "technologies/include/iproduction_state.h"

/*! 
 * \ingroup Objects
 * \brief The production state for a retired Technology.
 * \details This production state is used by Technology's which have already
 *          fully retired. This state is reached when the Technology's lifetime
 *          was completed before the current year. This does not include
 *          technologies which have made a short-term shutdown decision or which
 *          have deprecated to near zero output. Retired technologies produce
 *          zero output.
 * \author Josh Lurz
 */
class RetiredProductionState: public IProductionState
{
    friend class ProductionStateFactory;
public:
    // ISimpleComponent methods.
    RetiredProductionState* clone() const;
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
    /*!
     * \brief Protected constructor which may only be called from the
     *        ProductionStateFactory.
     */
    RetiredProductionState();

    /*
     * \brief Get the static name of this object.
     * \return The static name of this object.
     */
    static const std::string& getXMLNameStatic();
};

#endif // _RETIRED_PRODUCTION_STATE_H_
