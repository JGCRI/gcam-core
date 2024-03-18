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
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


#ifndef _VALUE_FACTOR_CALCULATOR_H_
#define _VALUE_FACTOR_CALCULATOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
 * \file value_factor_calculator.h
 * \ingroup Objects
 * \brief The ValueFactorCalculator class header file.
 * \author Matthew Binsted, Matt Mowers
 */

#include <string>
#include "sectors/include/ibackup_calculator.h"


// Forward declaration
class IInfo;

/*!
 * \ingroup Objects
 * \brief Interface which defines methods for calculating value factor
 *        of a technology depending on its market share.
 * \details Defines an interface to an object which determines the value
 *          factor for a technology. The value factor may use trial values of
 *          market share. Value factors are specific to electricity sector
 *          technologies.
 * \author Matthew Binsted, Matt Mowers
 */
class ValueFactorCalculator : public IBackupCalculator {
public:
    ValueFactorCalculator();
    virtual ValueFactorCalculator* clone() const;
    static const std::string& getXMLNameStatic();
    virtual const std::string& getXMLName() const;
    virtual bool isSameType(const std::string& aType) const;
    virtual const std::string& getName() const;
    virtual void toDebugXML(const int aPeriod, std::ostream& aOut, Tabs* aTabs) const;
    virtual void initCalc(const IInfo* aTechInfo);

    virtual double getMarginalBackupCapacity(const std::string& aSector,
        const std::string& aElectricSector,
        const std::string& aResource,
        const std::string& aRegion,
        const double aTechCapacityFactor,
        const double aReserveMargin,
        const double aAverageGridCapacityFactor,
        const int aPeriod) const;

    virtual double getAverageBackupCapacity(const std::string& aSector,
        const std::string& aElectricSector,
        const std::string& aResource,
        const std::string& aRegion,
        const double aTechCapacityFactor,
        const double aReserveMargin,
        const double aAverageGridCapacityFactor,
        const int aPeriod) const;

    virtual double getValueFactor(const std::string& aSector,
        const std::string& aElectricSector,
        const std::string& aRegion,
        const int aPeriod) const;

protected:

    double calcIntermittentShare(const std::string& aSector,
        const std::string& aElectricSector,
        const std::string& aResource,
        const std::string& aRegion,
        const double aTechCapacityFactor,
        const double aReserveMargin,
        const double aAverageGridCapacityFactor,
        const int aPeriod) const;
        
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        IBackupCalculator,

        //! TODO:  leave an explanatory comment
        DEFINE_VARIABLE( SIMPLE, "value-factor-intercept", mValueFactorIntercept, double ),

        DEFINE_VARIABLE( SIMPLE, "value-factor-slope", mValueFactorSlope, double )

    )
};

#endif // _VALUE_FACTOR_CALCULATOR_H_
