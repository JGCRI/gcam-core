#ifndef _INTERMITTENT_TECHNOLOGY_H_
#define _INTERMITTENT_TECHNOLOGY_H_
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
* \author Marshall Wise, Sonny Kim, Matthew Binsted, Matt Mowers
*/

#include <string>
#include "technologies/include/technology.h"
#include "util/base/include/value.h"
#include "sectors/include/ibackup_calculator.h"
#include "sectors/include/value_factor_calculator.h"

class IInfo;
/*
 * \ingroup Objects
 * \brief A Technology which represents production from an intermittent
 *        resource.
 * \details An intermittent technology represents the production of a good, such
 *          as electricity, from an intermittent resource, such as wind or
 *          solar. These technologies will have adjusted costs to reflect
 *          a reduction of their value as a function of market share.
 *          <b>XML specification for IntermittentTechnology</b>
 *          - XML name: \c intermittent-technology
 *          - Contained by: Subsector
 *          - Parsing inherited from class: Technology
 *          - Elements:
 *              - \c electric-sector-name mElectricSectorName
 *              - \c value-factor-calculator ValueFactorCalculator
 *
 * \author Marshall Wise, Josh Lurz, Matthew Binsted, Matt Mowers
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
    
    virtual void completeInit( const gcamstr& aRegionName,
                               const gcamstr& aSectorName,
                               const gcamstr& aSubsectorName,
                               const IInfo* aSubsectorInfo,
                               ILandAllocator* aLandAllocator );

    virtual void initCalc( const gcamstr& aRegionName,
                           const gcamstr& aSectorName,
                           const IInfo* aSubsectorIInfo,
                           const Demographic* aDemographics,
                           PreviousPeriodInfo& aPrevPeriodInfo,
                           const int aPeriod );
    
    virtual void postCalc( const gcamstr& aRegionName,
                           const int aPeriod );

    virtual void production( const gcamstr& aRegionName,
                             const gcamstr& aSectorName, 
                             double aVariableDemand,
                             double aFixedOutputScaleFactor,
                             const int aPeriod );

    
    virtual void calcCost( const gcamstr& aRegionName,
                           const gcamstr& aSectorName,
                           const int aPeriod );
protected:
    typedef std::vector<IInput*>::iterator InputIterator;
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        Technology,

        //! A calculator which determines the value factor of the technology
        DEFINE_VARIABLE(CONTAINER, "value-factor-calculator", mValueFactorCalculator, ValueFactorCalculator*),

        //! Name of the electricity sector which this Technology will supply.
        DEFINE_VARIABLE( SIMPLE, "electric-sector-name", mElectricSectorName, gcamstr ),
        
        DEFINE_VARIABLE( SIMPLE, "electric-sector-market", mElectricSectorMarket, gcamstr ),

        //! Name of trial market associated with this Intermittent Technology.
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "real-trial-market-name", mTrialMarketName, gcamstr ),

        //! Name of trial market readin for this Intermittent Technology.
        DEFINE_VARIABLE( SIMPLE, "trial-market-name", mTrialMarketNameParsed, gcamstr ),

        //! Cached input containing the resource.
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "resource-input-pointer", mResourceInput, InputIterator ),

        //! Cached input containing the technology costs.
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "tech-cost-input-pointer", mTechCostInput, InputIterator ),

        //! State value necessary to track tech output ratio
        DEFINE_VARIABLE( SIMPLE | STATE | NOT_PARSABLE, "tech-output-ratio", mIntermitOutTechRatio, Value )
    )
    
    //! Info object used to pass parameter information into backup calculators.
    std::unique_ptr<IInfo> mIntermittTechInfo;
    
    void copy( const IntermittentTechnology& aOther );

    virtual double getResourceToEnergyRatio(const gcamstr& aRegionName,
        const gcamstr& aSectorName,
        const int aPeriod);

    void initializeInputLocations( const gcamstr& aRegionName,
                                   const gcamstr& aSectorName,
                                   const int aPeriod );

    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;

    virtual const std::string& getTechCostName( ) const;
};

#endif // _INTERMITTENT_TECHNOLOGY_H_
