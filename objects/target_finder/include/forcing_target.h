#ifndef _FORCING_TARGET_H_
#define _FORCING_TARGET_H_
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
* \file forcing_target.h
* \ingroup Objects
* \brief The ForcingTarget class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "target_finder/include/itarget.h"
#include <string>

class IClimateModel;

/*! \brief A class representing a forcing target.
* \details A target for a total climate forcing for a set of gases.
*/
class ForcingTarget: public ITarget {
public:
    ForcingTarget( const IClimateModel* aClimateModel,
                   const unsigned int aInitialPeriod,
                   const unsigned int aFinalPeriod,
                   const double aTargetValue );

    virtual TrialStatus getStatus( const double aTolerance,
                                   const unsigned int aPeriod ) const;
    
    const std::string& getTaxName() const;

    static const std::string& getXMLNameStatic();
private:
    //! The climate model.
    const IClimateModel* mClimateModel;

    //! The initial period of the target.
    const unsigned int mInitialPeriod;
    
    //! The final period of the target.
    const unsigned int mFinalPeriod;

    //! The target value.
    double mTargetValue;
};

#endif // _FORCING_TARGET_H_
