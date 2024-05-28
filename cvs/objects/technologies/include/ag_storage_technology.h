#ifndef _AG_STORAGE_TECHNOLOGY_H_
#define _AG_STORAGE_TECHNOLOGY_H_
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
 * Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
 * Distributed as open-source under the terms of the Educational Community
 * License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
 *
 * For further details, see: http://www.globalchange.umd.edu/models/gcam/
 *
 */


/*!
 * \file ag_storage_technology.h
 * \ingroup Objects
 * \brief The AgStorageTechnology class header file.
 * \author Ellie Lochner
 */
class IVisitor;

#include "technologies/include/technology.h"

/*!
 * \ingroup Objects
 * \brief A technology capture some amount of pass through quantity flow and store 
 *        it to use as supply in the next model timestep.
 * \details This technology will operate in exactly two vintages.  In new vintage mode
 *          it will use a two-choice logit to determine the share of a quantity that will
 *          be stored vs passed through for consumption.  In vintaged mode it will only
 *          supply the amount stored in the previous model period (minus losses) for consumption.
 *
 * \author Ellie Lochner
 */
class AgStorageTechnology: public Technology
{
   friend class XMLDBOutputter;
public:
    AgStorageTechnology();
    AgStorageTechnology( const std::string& aName, const int aYear );
    virtual ~AgStorageTechnology();

    static const std::string& getXMLNameStatic();

    // ITechnology methods
    virtual const std::string& getXMLName() const;

    virtual AgStorageTechnology* clone() const;

    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               const std::string& aSubsectorName,
                               const IInfo* aSubsectorIInfo,
                               ILandAllocator* aLandAllocator );

    virtual void initCalc(const std::string& aRegionName,
        const std::string& aSectorName,
        const IInfo* aSubsectorInfo,
        const Demographic* aDemographics,
        PreviousPeriodInfo& aPrevPeriodInfo,
        const int aPeriod);

    virtual double getFixedOutput(const std::string& aRegionName,
        const std::string& aSectorName,
        const bool aHasRequiredInput,
        const std::string& aRequiredInput,
        const double aMarginalRevenue,
        const int aPeriod) const;

    virtual void production( const std::string& aRegionName,
                             const std::string& aSectorName,
                             double aVariableDemand,
                             double aFixedOutputScaleFactor,
                             const int aPeriod );

    virtual void calcCost(const std::string& aRegionName,
                          const std::string& aSectorName,
                          const int aPeriod);


protected:
    virtual void toDebugXMLDerived( const int aPeriod, std::ostream& aout, Tabs* aTabs ) const;

    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        Technology,

        //! Amount in stock at the start of the year used for calibration
        DEFINE_VARIABLE(SIMPLE, "opening-stock", mOpeningStock, Value),

        //! Expected price of food crop
        DEFINE_VARIABLE(SIMPLE | NOT_PARSABLE, "expected-price", mAdjExpectedPrice, Value),

        //! Amount in storage at the end of the year used for calibration
        DEFINE_VARIABLE(SIMPLE, "closing-stock", mClosingStock, Value),

        //! Amount put in storage
        DEFINE_VARIABLE(SIMPLE | STATE | NOT_PARSABLE, "stored-value", mStoredValue, Value),

        //! Logit exponent for storage decision
        DEFINE_VARIABLE(SIMPLE, "logit-exponent", mLogitExponent, Value), 

        //! Fraction not lost in storage
        DEFINE_VARIABLE(SIMPLE, "loss-coefficient", mLossCoefficient, Value),
    
        //! Commodity consumption in current period
        DEFINE_VARIABLE(SIMPLE | STATE | NOT_PARSABLE, "consumption", mConsumption, Value),

        //! Total usage in a period (consumption + storage)
        DEFINE_VARIABLE(SIMPLE | STATE | NOT_PARSABLE, "total", mTotal, Value),

        //! Storage cost 
        DEFINE_VARIABLE(SIMPLE, "storage-cost", mStorageCost, Value)
    )
    
    void copy( const AgStorageTechnology& aOther );
    virtual void setProductionState(const int aPeriod);

};

#endif // _AG_STORAGE_TECHNOLOGY_H_

