/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial
 * Institute. Battelle has certain unperfected rights in the software which
 * should not be copied or otherwise disseminated outside your organization
 * without the express written authorization from Battelle. All rights to the
 * software are reserved by Battelle. Battelle makes no warranty, express or
 * implied, and assumes no liability or responsibility for the use of this
 * software.
 */

#ifndef _VARIABLE_PRODUCTION_STATE_H_
#define _VARIABLE_PRODUCTION_STATE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file variable_production_state.h
 * \ingroup Objects
 * \brief The VariableProductionState header file.
 * \author Josh Lurz
 */

#include "technologies/include/iproduction_state.h"

/*! 
 * \ingroup Objects
 * \brief This production state calculates the production for a variable output
 *        technology in its initial period.
 * \details Technologies in a variable production state produce the quantity the
 *          subsector requests they produce.
 * \author Josh Lurz
 */
class VariableProductionState: public IProductionState
{
    friend class ProductionStateFactory;
public:
    // ISimpleComponentMethods
    VariableProductionState* clone() const;
    virtual bool isSameType( const std::string& aType ) const;
    virtual const std::string& getName() const;
	virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const;

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
    VariableProductionState();

    /*
     * \brief Get the static name of this object.
     * \return The static name of this object.
     */
    static const std::string& getXMLNameStatic();
};

#endif // _VARIABLE_PRODUCTION_STATE_H_
