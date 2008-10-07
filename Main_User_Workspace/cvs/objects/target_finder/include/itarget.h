#ifndef _ITARGET_H_
#define _ITARGET_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * LEGAL NOTICE
 * This computer software was prepared by Battelle Memorial Institute,
 * hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
 * with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
 * CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
 * sentence must appear on any copies of this computer software.
 * 
 * EXPORT CONTROL
 * User agrees that the Software will not be shipped, transferred or
 * exported into any country or used in any manner prohibited by the
 * United States Export Administration Act or any other applicable
 * export laws, restrictions or regulations (collectively the "Export Laws").
 * Export of the Software may require some form of license or other
 * authority from the U.S. Government, and failure to obtain such
 * export control license may result in criminal liability under
 * U.S. laws. In addition, if the Software is identified as export controlled
 * items under the Export Laws, User represents and warrants that User
 * is not a citizen, or otherwise located within, an embargoed nation
 * (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
 *     and that User is not otherwise prohibited
 * under the Export Laws from receiving the Software.
 * 
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
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
