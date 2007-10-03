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

#ifndef _CSP_BACKUP_CALCULATOR_H_
#define _CSP_BACKUP_CALCULATOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
 * \file CSP_backup_calculator.h
 * \ingroup Objects
 * \brief The CSPBackupCalculator class header file.
 * \author Marshall Wise
 */

#include <string>
#include "sectors/include/ibackup_calculator.h"
/*!
 * \ingroup Objects
 * \brief The backup calculator for the CSP (concentrated solar trough) technology.
 * \details Calculates the amount of backup energy required per unit of output. This
 *          differs from the capacity_limit_backup_calculator in that the CSP
 *          backup retunrs energy reuqired rather than capacity.<br>
 *
 *
 *          <b>XML specification for CSPBackupCalculator</b>
 *          - XML name: \c CSP-backup-calculator
 *          - Contained by: IntermittentSubsector
 *          - Parsing inherited from class: None
 *          - Attributes: None
 *          - Elements: None
 *
 * \author Marshall Wise
 */
class CSPBackupCalculator: public IBackupCalculator {
    friend class BackupCalculatorFactory;
public:
    virtual CSPBackupCalculator* clone() const;
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
    CSPBackupCalculator();

    double calcIntermittentShare( const std::string& aSector,
                                  const std::string& aElectricSector,
                                  const std::string& aResource,
                                  const std::string& aRegion,
                                  const double aReserveMargin,
                                  const double aAverageGridCapacityFactor,
                                  const int aPeriod ) const;

    
    // Parameters hard-coded in constructor for first step
    //! Maximum Backup Fraction is the maximum backup fraction required.
    double mMaxBackupFraction;
    //! Fraction of electric sector that is intermediate and peak
    double mIPFraction;
    //! exponent parameter a
    double A;

};

#endif // _CSP_BACKUP_CALCULATOR_H_
