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

#ifndef _WIND_BACKUP_CALCULATOR_H_
#define _WIND_BACKUP_CALCULATOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
 * \file wind_backup_calculator.h
 * \ingroup Objects
 * \brief The wind backup calculator class header file.
 * \author Marshall Wise, Josh Lurz
 */

#include <string>
#include "sectors/include/ibackup_calculator.h"

/*!
 * \ingroup Objects
 * \brief The backup calculator for the wind subsector.
 * \details Calculates the amount of backup required per unit of output.
 *          Required backup capacity is computed based on a reserve requirement
 *          and the variance of the wind resource.<br>
 *
 *          Backup capacity required per unit of output is calculated as:
 *
 *          \f[ b_c = \frac{b_{frac}}{\frac{EJ}{GWH} * \frac{hours}{year} * CF_i} \f]
 *
 *          where
 *              - \f$b_{frac}\f$ is the backup fraction calculated as:
 *
 *          \f[ b_{frac} = R_{total} * ( \sqrt{1+\sigma * \frac{C_{i}^2}{R_{total}^2}} - 1 ) \f]
 *
 *          and
 *              - \f$R_{total}\f$ is total reserve capacity calculated as:
 *
 *          \f[R_{total} = \frac{RM * O_e}{\frac{hours}{year} * \frac{EJ}{GWH} * CF_e}\f]
 *
 *          and
 *              - \f$C_{i}\f$ is the capacity of the intermittent sector calculated as:
 *          \f[C_{i} = \frac{O_{i}}{\frac{EJ}{GWH} * \frac{hours}{year} * CF_i}\f]
 *
 *          and
 *              - \f$\sigma\f$ is the intermittent sector variance.
 *              - \f$RM\f$ is the reserve margin.
 *              - \f$O_i\f$ is the output of the intermittent sector.
 *              - \f$CF_i\f$ is the capacity factor of the intermittent sector.
 *              - \f$O_e\f$ is the output of the electricity sector.
 *              - \f$CF_e\f$ is the capacity factor of the electricity sector.
 *
 *          <b>XML specification for WindBackupCalculator</b>
 *          - XML name: \c wind-backup-calculator
 *          - Contained by: IntermittentSubsector
 *          - Parsing inherited from class: None
 *          - Attributes: None
 *          - Elements: None
 *
 * \author Marshall Wise, Josh Lurz
 */
class WindBackupCalculator: public IBackupCalculator {
    friend class BackupCalculatorFactory;
public:
    virtual WindBackupCalculator* clone() const;
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

    double getBackupCapacityFraction( const std::string& aSector,
                                      const std::string& aElectricSector,
                                      const std::string& aResource,
                                      const std::string& aRegion,
                                      const double aReserveMargin,
                                      const double aAverageGridCapacityFactor,
                                      const int aPeriod ) const;

    double getReserveTotal( const std::string& aElectricSector,
                            const std::string& aRegion,
                            const double aReserveMargin,
                            const double aAverageGridCapacityFactor,
                            const int aPeriod ) const;

    double getSectorCapacity( const std::string& aRegion,
                              const std::string& aSector,
                              const std::string& aResource,
                              const int aPeriod ) const;

    WindBackupCalculator();
};

#endif // _WIND_BACKUP_CALCULATOR_H_
