#ifndef _CONCENTRATION_TARGET_H_
#define _CONCENTRATION_TARGET_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
* This software, which is provided in confidence, was prepared by employees of
* Pacific Northwest National Labratory operated by Battelle Memorial Institute.
* Battelle has certain unperfected rights in the software which should not be
* copied or otherwise disseminated outside your organization without the express
* written authorization from Battelle. All rights to the software are reserved
* by Battelle. Battelle makes no warranty, express or implied, and assumes no
* liability or responsibility for the use of this software.
*/

/*!
* \file concentration_target.h
* \ingroup Objects
* \brief The ConcentrationTarget class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "target_finder/include/itarget.h"
#include <string>

class IClimateModel;

/*! \brief A class representing a concentration target for a particular gas.
* \details A target for a concentration of a particular gas in the end
*          period of the model.
*/
class ConcentrationTarget: public ITarget {
public:
    ConcentrationTarget( const IClimateModel* aClimateModel,
                         const double aTargetValue );

    virtual TrialStatus getStatus( const double aTolerance,
                                   const unsigned int aYear ) const;

    const std::string& getTaxName() const;

    static const std::string& getXMLNameStatic();
private:

    //! The name of the target gas.
    std::string mTargetGas;

    //! The climate model.
    const IClimateModel* mClimateModel;

    //! The target value.
    double mTargetValue;

};
#endif // _CONCENTRATION_TARGET_H_
