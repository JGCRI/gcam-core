/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Labratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */
#ifndef _EMISSIONS_STABALIZATION_TARGET_H_
#define _EMISSIONS_STABALIZATION_TARGET_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
 * \file forcing_target.h
 * \ingroup Objects
 * \brief The EmissionsStabalizationTarget class header file.
 * \author Josh Lurz
 */

#include "target_finder/include/itarget.h"
#include <string>

class IClimateModel;

/*!
 * \brief An emissions stabalization target.
 * \details A target for stabalizing emissions such that industrial emissions
 *          in the target period are equal to the net land use emissions plus
 *          the rate of ocean uptake.
 */
class EmissionsStabalizationTarget: public ITarget {
public:
    EmissionsStabalizationTarget( const IClimateModel* aClimateModel,
                                  const double aTargetValue );

    virtual TrialStatus getStatus( const double aTolerance,
                                   const double aYear ) const;

    static const std::string& getXMLNameStatic();
private:
    //! The name of the target gas.
    std::string mTargetGas;

    //! The climate model.
    const IClimateModel* mClimateModel;
};

#endif // _EMISSIONS_STABALIZATION_TARGET_H_
