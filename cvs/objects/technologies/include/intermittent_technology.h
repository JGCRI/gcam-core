#ifndef _ITERMITTENT_TECHNOLOGY_H_
#define _ITERMITTENT_TECHNOLOGY_H_
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
* \file intermittent_technology.h
* \ingroup Objects
* \brief The IntermittentTechnology class header file.
* \author Marshall Wise, Sonny Kim
*/

#include <string>
#include "technologies/include/technology.h"
#include "util/base/include/value.h"
#include "sectors/include/ibackup_calculator.h"

class IInfo;
/*
 * \ingroup Objects
 * \brief A Technology which represents production from an intermittent
 *        resource.
 * \details An intermittent subsector represents the production of a good, such
 *          as electricity, from an intermittent resource, such as wind or
 *          solar. An intermittent subsector has a pair of technologies. One
 *          Technology consumes the intermittent resource and produces the
 *          majority of the output, and the other Technology produces the backup
 *          required. The backup Technology may produce a small amount of
 *          output, and emissions. The intermittent and backup technologies do
 *          not compete. The intermittent subsector has a backup calculator,
 *          which is responsible for determining the average and marginal quantity
 *          of backup capacity required. The backup calculator sets the shares
 *          of the technologies using the marginal backup requirements. These
 *          shares are used for the cost calculation, but not the output
 *          calculation. Output, and therefore emissions, is based on the
 *          average backup required.
 * \note An intermittent subsector must have two and only two Technologies, one
 *       consuming an intermittent resource and one which is the backup.
 * \note If a backup calculator is not read in, the backup requirement is
 *       assumed to be zero and this subsector will operate exactly the same as
 *       a standard Subsector with one Technology.
 *          <b>XML specification for IntermittentTechnology</b>
 *          - XML name: \c intermittent-technology
 *          - Contained by: Subsector
 *          - Parsing inherited from class: Technology
 *          - Elements:
 *              - \c electric-sector-name mElectricSectorName
 *              - \c wind-backup-calculator WindBackupCalculator
 *              - \c capacity-limit-backup-calculator CapacityLimitBackupCalculator
 *
 * \author Marshall Wise, Josh Lurz
 */
class IntermittentTechnology: public Technology {
public:
    static const std::string& getXMLNameStatic();

    IntermittentTechnology( const std::string& aName,
                            const int aYear );
    IntermittentTechnology();
    virtual ~IntermittentTechnology();
    
    virtual IntermittentTechnology* clone() const;

    virtual const std::string& getXMLName() const;
    
    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               const std::string& aSubsectorName,
                               const IInfo* aSubsectorInfo,
                               ILandAllocator* aLandAllocator );

    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const IInfo* aSubsectorIInfo,
                           const Demographic* aDemographics,
                           PreviousPeriodInfo& aPrevPeriodInfo,
                           const int aPeriod );
    
    virtual void postCalc( const std::string& aRegionName,
                           const int aPeriod );

    virtual void production( const std::string& aRegionName,
                             const std::string& aSectorName, 
                             double aVariableDemand,
                             double aFixedOutputScaleFactor,
                             const GDP* aGDP,
                             const int aPeriod );

    
    virtual void calcCost( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const int aPeriod );
protected:
    typedef std::vector<IInput*>::iterator InputIterator;
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        Technology,

        //! A calculator which determines the amount of backup per unit output.
        DEFINE_VARIABLE( CONTAINER, "backup-calculator", mBackupCalculator, IBackupCalculator* ),

        //! Name of the electricity sector which this Technology will supply.
        DEFINE_VARIABLE( SIMPLE, "electric-sector-name", mElectricSectorName, std::string ),
        
        DEFINE_VARIABLE( SIMPLE, "electric-sector-market", mElectricSectorMarket, std::string ),

        //! Name of trial market associated with this Intermittent Technology.
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "real-trial-market-name", mTrialMarketName, std::string ),

        //! Name of trial market readin for this Intermittent Technology.
        DEFINE_VARIABLE( SIMPLE, "trial-market-name", mTrialMarketNameParsed, std::string ),

        //! Cached input containing the resource.
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "resource-input-pointer", mResourceInput, InputIterator ),

        //! Cached input containing the backup.
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "backup-input-pointer", mBackupInput, InputIterator ),

        //! Cached input containing the capital costs for backup.
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "backup-cap-cost-input-pointer", mBackupCapCostInput, InputIterator ),

        //! Cached input containing the technology costs.
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "tech-cost-input-pointer", mTechCostInput, InputIterator ),

        //! Backup capacity factor read in at the Sector level.
        DEFINE_VARIABLE( SIMPLE, "backup-capacity-factor", mBackupCapacityFactor, Value ),

        //! Backup capital cost.
        DEFINE_VARIABLE( SIMPLE, "backup-capital-cost", mBackupCapitalCost, Value ),

        //! Electric reserve cost read in at the Sector level.
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "electricity-reserve-margin", mElecReserveMargin, Value ),

        //! Average grid capacity factor read in at the Sector level.
        //todo dynamically calculate average grid capacity factor
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "average-grid-capacity-factor", mAveGridCapacityFactor, Value ),

        //! State value necessary to track tech output ration
        DEFINE_VARIABLE( SIMPLE | STATE | NOT_PARSABLE, "tech-output-ratio", mIntermitOutTechRatio, Value )
    )
    
    //! Info object used to pass parameter information into backup calculators.
    std::auto_ptr<IInfo> mIntermittTechInfo;
    
    void copy( const IntermittentTechnology& aOther );

    void setCoefficients( const std::string& aRegionName,
                          const std::string& aSectorName,
                          const int aPeriod );

    virtual double getResourceToEnergyRatio( const std::string& aRegionName,
                                             const std::string& aSectorName,
                                             const int aPeriod );

    double getBackupCapacityPerEnergyOutput( const std::string& aRegionName,
                                             const std::string& aSectorName,
                                             const int aPeriod ) const;

    double getMarginalBackupCapCost( const std::string& aRegionName,
                                     const std::string& aSectorName,
                                     const int aPeriod ) const;

    void initializeInputLocations( const std::string& aRegionName,
                                   const std::string& aSectorName,
                                   const int aPeriod );

    double getMarginalBackupCapacity( const std::string& aRegionName,
                                      const std::string& aSectorName,
                                      const int aPeriod ) const;

    double getAverageBackupCapacity( const std::string& aRegionName,
                                     const std::string& aSectorName,
                                     const int aPeriod ) const;

    double calcEnergyFromBackup() const;

    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;


    virtual const std::string& getBackupCapCostName( ) const;

    virtual const std::string& getTechCostName( ) const;
};

#endif // _ITERMITTENT_TECHNOLOGY_H_
