#ifndef _TEMPERATURE_TARGET_H_
#define _TEMPERATURE_TARGET_H_
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
* \file temperature_target.h
* \ingroup Objects
* \brief The TemperatureTarget class header file.
* \author Josh Lurz
*/

#include "target_finder/include/itarget.h"
#include <string>

class IClimateModel;

/*! \brief A class representing a temperature target.
* \details A target for a temperature for a set of gases in the
*          end period of the model.
*/
class TemperatureTarget: public ITarget {
public:
    TemperatureTarget( const IClimateModel* aClimateModel,
                       const double aTargetValue );

    virtual TrialStatus getStatus( const double aTolerance,
                                   const double aYear ) const;

    static const std::string& getXMLNameStatic();
private:
    //! The climate model.
    const IClimateModel* mClimateModel;

    //! The target value.
    double mTargetValue;
};

#endif // _TEMPERATURE_TARGET_H_
