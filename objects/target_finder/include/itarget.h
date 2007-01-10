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
 */

#include <cassert>

/*!
 * \brief Interface to represent a target.
 */
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

    /*!
     * \brief Convert a TrialStatus to a string.
     * \param aTrialStatus Trial status enum.
     * \return String representation of the enum.
     */
    static const std::string& toString( const TrialStatus aTrialStatus ){
        assert( aTrialStatus >= 0 );

        static const std::string names[] = {
            "High",
            "Low",
            "Solved"
            "Unknown"
        };

        assert( aTrialStatus < sizeof( names ) / sizeof( names[ 0 ] ) );

        return names[ aTrialStatus ];
    }

    /*!
     * \brief Get the status of the last trial for a given period.
     * \details Returns whether the last trial was over the target, under the
     *          target, or solved the target for a given period.
     * \param aTolerance Solution tolerance.
     * \param aYear Year in which to check the target.
     * \return The status of the last trial.
     */
    virtual TrialStatus getStatus( const double aTolerance,
                                   const double aYear ) const = 0;
};

#endif // _ITARGET_H_
