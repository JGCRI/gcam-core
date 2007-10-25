#ifndef _INPUT_EMISSIONS_COEF_H_
#define _INPUT_EMISSIONS_COEF_H_
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
 * \file input_emissions_coef.h
 * \ingroup Objects
 * \brief InputEmissionsCoef header file.
 * \author Jim Naslund
 */

#include "emissions/include/aemissions_coef.h"

class IInfo;

/*! 
 * \ingroup Objects
 * \brief A class that represents an input emissions.
 * \details This class represents an emissions coefficient that is determined by a read-in
 *          level of emissions (input emissions).
 * \author Jim Naslund
 */
class InputEmissionsCoef : public AEmissionsCoef{

public:
    InputEmissionsCoef( const double aInputEmissions );
    virtual InputEmissionsCoef* clone() const;

    virtual void updateCoef( const double adjEmissDriver );
    virtual void initCalc( const IInfo* aSubsectorInfo, const std::string& aName, const int aPeriod );
    virtual double getInputEmissions() const;
    virtual double calcMaxCntrl( const double aFinalEmissCoef, const double aB,
                                 const double aMultiplier ) const;
    virtual bool needsCalcForAdjustment() const;

    virtual const std::string& getXMLName() const;
    virtual double getXMLValue() const;
private:
    double mInputEmissions;
};


#endif // _INPUT_EMISSIONS_COEF_H_
