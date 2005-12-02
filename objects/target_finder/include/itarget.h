#ifndef _ITARGET_H_
#define _ITARGET_H_
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
* \file itarget.h
* \ingroup Objects
* \brief The ITarget interface file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

class IClimateModel;
class Modeltime;

/*! \brief Interface to represent a target.*/
class ITarget {
public:
    //! Enum representation of the trial.
    enum TrialStatus {
        //! Trial value is too high.
        HIGH,
        //! Trial value is too low.
        LOW,
        //! Trial value is within the tolerance.
        SOLVED,
        //! Trial status is unknown.
        UNKNOWN
    };

    /*! \brief Get the status of the last trial for a given period.
    * \details Returns whether the last trial was over the target, under the
    *          target, or solved the target for a given period.
    * \param aTolerance Solution tolerance.
    * \param aPeriod Period in which to check the target.
    * \return The status of the last trial.
    */
    virtual TrialStatus getStatus( const double aTolerance,
                                   const unsigned int aPeriod ) const = 0;

    /*! \brief Get the name of the tax which will cause the target to be
    *          reached.
    * \return The name of the tax for the policy target.
    */
    virtual const std::string& getTaxName() const = 0;
};

#endif // _ITARGET_H_
