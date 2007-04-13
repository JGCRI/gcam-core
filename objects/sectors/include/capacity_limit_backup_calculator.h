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

#ifndef _CAPACITY_LIMIT_BACKUP_CALCULATOR_H_
#define _CAPACITY_LIMIT_BACKUP_CALCULATOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
 * \file capacity_limit_backup_calculator.h
 * \ingroup Objects
 * \brief The CapacityLimitBackupCalculator class header file.
 * \author Josh Lurz
 */

#include <string>
#include "sectors/include/ibackup_calculator.h"
/*!
 * \ingroup Objects
 * \brief The backup calculator for a capacity limited resource.
 * \details Calculates the amount of backup required per unit of output. The
 *          resource is assumed to only be able to penetrate up to the share of
 *          intermediate electricity in the model. Above this point the resource
 *          is required to have a one-to-one ratio of backup capacity to output.
 *          This is represented as an "S" shaped curve, so that the second
 *          derivatives are continuous at zero and one, and there is a smooth
 *          transformation in-between.<br>
 *
 *          Backup capacity required per unit of output is calculated as:
 *          \f[ b_c = \frac{b_{frac}}{\frac{EJ}{GWH} * \frac{hours}{year} * CF_i} \f]
 *
 *          where
 *              - \f$b_{frac}\f$ is the backup fraction calculated as:
 *
 *          \f[ b_{frac} = \left\{ \begin{array}{ll}1 & (S_i>C_{limit}) \\
 *              ( \alpha * (\frac{S_i}{C_{limit}})^\beta - \beta * (\frac{S_i}{C_{limit}})^\alpha)/(\alpha-\beta) & (otherwise)
 *              \end{array} \right. \f]
 *
 *          and
 *              - \f$S_i\f$ is share of the capacity of the electricity sector supplied by
 *                the intermittent sector, calculated as:
 *
 *          \f[S_i = \frac{O_i/CF_i}{O_e/CF_e}\f]
 *
 *          and
 *              - \f$C_{limit}\f$ is the read-in capacity limit.
 *              - \f$\alpha\f$ is a shaping constant.
 *              - \f$\beta\f$ is a shaping constant.
 *              - \f$O_i\f$ is the output of the intermittent sector.
 *              - \f$CF_i\f$ is the capacity factor of the intermittent sector.
 *              - \f$O_e\f$ is the output of the electricity sector.
 *              - \f$CF_e\f$ is the capacity factor of the electricity sector.
 *
 *          <b>XML specification for CapacityLimitBackupCalculator</b>
 *          - XML name: \c capacity-limit-backup-calculator
 *          - Contained by: IntermittentSubsector
 *          - Parsing inherited from class: None
 *          - Attributes: None
 *          - Elements:
 *              - \c capacity-limit CapacityLimitBackupCalculator::mCapacityLimit
 *
 * \author Josh Lurz
 */
class CapacityLimitBackupCalculator: public IBackupCalculator {
    friend class BackupCalculatorFactory;
public:
    virtual CapacityLimitBackupCalculator* clone() const;
    virtual bool isSameType( const std::string& aType ) const;
    virtual const std::string& getName() const;
    virtual bool XMLParse( const xercesc::DOMNode* aNode );
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const;
    virtual void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const;
    
    virtual double getMarginalBackupCapacity( const std::string& aSector,
                                              const std::string& aElectricSector,
                                              const std::string& aResource,
                                              const std::string& aRegion,
                                              const double aReserveMargin,
                                              const double aAverageGridCapacityFactor,
                                              const int aPeriod ) const;
    
    virtual double getAverageBackupCapacity( const std::string& aSector,
                                             const std::string& aElectricSector,
                                             const std::string& aResource,
                                             const std::string& aRegion,
                                             const double aReserveMargin,
                                             const double aAverageGridCapacityFactor,
                                             const int aPeriod ) const;
protected:
    static const std::string& getXMLNameStatic();
    CapacityLimitBackupCalculator();

    double getMarginalBackupCapacityFraction( const std::string& aSector,
                                              const std::string& aElectricSector,
                                              const std::string& aResource,
                                              const std::string& aRegion,
                                              const double aReserveMargin,
                                              const double aAverageGridCapacityFactor,
                                              const int aPeriod ) const;

    double calcIntermittentShare( const std::string& aSector,
                                  const std::string& aElectricSector,
                                  const std::string& aResource,
                                  const std::string& aRegion,
                                  const double aReserveMargin,
                                  const double aAverageGridCapacityFactor,
                                  const int aPeriod ) const;

    //! Capacity limit which determines when a backup to output ratio of one to
    //! one is required.
    double mCapacityLimit;

    //! Parameter for limiting equation. A must be greater than B.
    static const int A = 15;

    //! Parameter for limiting equation. A must be greater than B.
    static const int B = 5;
};

#endif // _CAPACITY_LIMIT_BACKUP_CALCULATOR_H_
