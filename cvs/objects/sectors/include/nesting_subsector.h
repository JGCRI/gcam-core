#ifndef _NESTING_SUBSECTOR_H_
#define _NESTING_SUBSECTOR_H_
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
* \file nesting_subsector.h
* \ingroup Objects
* \brief The NestingSubsector class header file.
* \author Pralit Patel
*/

#include "sectors/include/subsector.h"

/*! 
* \ingroup Objects
* \brief A class which defines subsector which only contains other subsectors.
* \details This class allows infinite nesting of subsectors so users can set up
*          logit competitions that best suite the sector without worrying about
*          the number of levels of nesting it creates.
* \author Pralit Patel
*/

class NestingSubsector: public Subsector
{
    friend class CalibrateShareWeightVisitor;
protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        Subsector,

        //! subsector name
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "depth", mNestingDepth, int ),

        //! region name
        DEFINE_VARIABLE( CONTAINER, "nested-subsector", mSubsectors, std::vector<Subsector*> )

    )
    
    const std::vector<double> calcChildShares( const GDP* aGDP, const int aPeriod ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;

public:
    NestingSubsector();

    virtual ~NestingSubsector();

    static const std::string& getXMLNameStatic();
    
    virtual void setNames( const std::string& aRegionName, const std::string& aSectorName );
    
    virtual const std::string& getXMLName() const;

    virtual void completeInit( const IInfo* aSectorInfo,
                               ILandAllocator* aLandAllocator );
    
    virtual void initCalc( NationalAccount* aNationalAccount,
                           const Demographic* aDemographics,
                           const int aPeriod );

    virtual double getPrice( const GDP* aGDP, const int aPeriod ) const;
    virtual bool allOutputFixed( const int period ) const;
    virtual bool containsOnlyFixedOutputTechnologies( const int period ) const;
    virtual double getAverageFuelPrice( const GDP* aGDP, const int aPeriod ) const;

    virtual void calcCost( const int aPeriod );

    virtual void setOutput( const double aVariableDemand,
                            const double aFixedOutputScaleFactor,
                            const GDP* aGDP,
                            const int aPeriod );

    virtual bool isAllCalibrated( const int aPeriod, double aCalAccuracy, const bool aPrintWarnings ) const;
    virtual double getFixedOutput( const int Period, const double aMarginalRevenue ) const;

    virtual double getTotalCalOutputs( const int period ) const;

    virtual double getOutput( const int period ) const;

    virtual double getEnergyInput( const int aPeriod ) const;

    virtual void postCalc( const int aPeriod );
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
};

#endif // _NESTING_SUBSECTOR_H_

