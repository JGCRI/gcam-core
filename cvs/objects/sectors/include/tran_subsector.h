#ifndef _TRAN_SUBSECTOR_H_
#define _TRAN_SUBSECTOR_H_
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
* \file tran_subsector.h
* \ingroup Objects
* \brief The TranSubsector header file. 
* \author Marshall Wise, Sonny Kim, Josh Lurz
*/

#include <string>
#include "sectors/include/subsector.h"
#include "util/base/include/time_vector.h"
#include "util/base/include/value.h"

// Forward declarations
class GDP;
class Demographic;
class IInfo;

/*! 
* \ingroup Objects
* \brief A derived subsector representing a mode of transportation.
* \author Sonny Kim, Josh Lurz, Steve Smith, Marshall Wise
*/


class TranSubsector: public Subsector{
    friend class XMLDBOutputter;
public:
    TranSubsector();
    static const std::string& getXMLNameStatic();
    const std::string& getXMLName() const;

    virtual void completeInit( const IInfo* aSectorInfo,
                               ILandAllocator* aLandAllocator );
    
    virtual void initCalc( NationalAccount* aNationalAccount,
                           const Demographic* aDemographics,
                           const int aPeriod );
    double getPrice( const GDP* aGDP, const int aPeriod ) const;

    virtual void setOutput( const double aVariableSubsectorDemand,
                            const double aFixedOutputScaleFactor,
                            const GDP* aGDP,
                            const int aPeriod );
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        Subsector,

        //! Speed of Mode in Miles/hour
        DEFINE_VARIABLE( ARRAY, "speed", mSpeed, objects::PeriodVector<Value> ),

        //! copy of population from demographics
        DEFINE_VARIABLE( ARRAY | NOT_PARSABLE, "population", mPopulation, objects::PeriodVector<double> ),

        //! Population Density Elasticity of mode
        DEFINE_VARIABLE( ARRAY, "popDenseElasticity", mPopDenseElasticity, objects::PeriodVector<double> ),

        //! Time value multiplier
        DEFINE_VARIABLE( ARRAY, "time-value-multiplier", mTimeValueMult, objects::PeriodVector<Value> ),

        //! add value of time to price term
        DEFINE_VARIABLE( SIMPLE, "addTimeValue", mAddTimeValue, bool )
    )
    
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;

    double getTimeValue( const GDP* aGDP, const int aPeriod ) const;
    double getTimeInTransit( const int aPeriod ) const;
    double getServicePerCapita( const int aPeriod ) const;
    double getGeneralizedPrice( const GDP* aGDP, const int aPeriod ) const;
};


#endif // _TRAN_SUBSECTOR_H_
