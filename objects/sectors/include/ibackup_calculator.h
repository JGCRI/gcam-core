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

#ifndef _IBACKUP_CALCULATOR_H_
#define _IBACKUP_CALCULATOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
 * \file ibackup_calculator.h
 * \ingroup Objects
 * \brief The IBackupCalculator interface header file.
 * \author Marshall Wise, Josh Lurz
 */

#include "util/base/include/istandard_component.h"

/*!
 * \ingroup Objects
 * \brief Interface which defines methods for calculating an average and
 *        marginal amount of backup capacity required per unit of output.
 * \details Defines an interface to an object which determines the backup
 *          capacity required for a Sector. The backup capacity is determined
 *          per unit of output, but may use trial values to allow computation
 *          based on the total output. Backup requirements are specific to
 *          sectors that produce electricity.
 * \author Josh Lurz
 */
class IBackupCalculator : public IParsedComponent {
public:
	// Clone operator must be declared explicitly even though it is inherited
    // from IStandardComponent so that the return type can be changed. Since
    // this class is a subtype of IStandardComponent, this is legal and referred
    // to as a covariant return type.
	virtual IBackupCalculator* clone() const = 0;

	/*!
     * \brief Compute backup required for the marginal unit of energy output.
	 * \details Compute backup required per resource energy output on the margin
	 *          (since energy output is what the modeled market is based on).
	 * \param aSector The name of the sector which requires backup capacity.
	 * \param aElectricSector The name of the electricity sector into which the
	 *        sector having a backup amount calculated for will feed.
	 * \param aResource The name of the resource the sector consumes.
	 * \param aRegion Name of the containing region.
	 * \param aReserveMargin Reserve margin for the electricity sector.
	 * \param aAverageGridCapacityFactor The average electricity grid capacity
	 *        factor.
	 * \param aPeriod Model period.
	 * \return Reserve capacity per marginal intermittent electricity resource
     *         output.
     */
    virtual double getMarginalBackupCapacity( const std::string& aSector,
                                              const std::string& aElectricSector,
                                              const std::string& aResource,
                                              const std::string& aRegion,
                                              const double aReserveMargin,
                                              const double aAverageGridCapacityFactor,
                                              const int aPeriod ) const = 0;

	/*!
     * \brief Compute the average backup required per unit for the intermittent
     *        subsector.
	 * \details Computes the average quantity of backup capacity required per
     *          unit of energy output.
	 * \param aSector The name of the sector which requires backup capacity.
	 * \param aElectricSector The name of the electricity sector into which the
	 *        sector having a backup amount calculated for will feed.
	 * \param aResource The name of the resource the sector consumes.
	 * \param aRegion Name of the containing region.
	 * \param aReserveMargin Reserve margin for the electricity sector.
	 * \param aAverageGridCapacityFactor The average electricity grid capacity
	 *        factor.
	 * \param aPeriod Model period.
	 * \return The average backup capacity required per unit of output.
     */
    virtual double getAverageBackupCapacity( const std::string& aSector,
                                             const std::string& aElectricSector,
                                             const std::string& aResource,
                                             const std::string& aRegion,
                                             const double aReserveMargin,
                                             const double aAverageGridCapacityFactor,
                                             const int aPeriod ) const = 0;
};

#endif // _IBACKUP_CALCULATOR_H_
