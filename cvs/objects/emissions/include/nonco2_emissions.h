#ifndef _NONCO2_EMISSIONS_H_
#define _NONCO2_EMISSIONS_H_
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
 * \file nonco2_emissions.h
 * \ingroup Objects
 * \brief NonCO2Emissions class header file.
 * \author Kate Calvin
 */

#include <memory>

#include "emissions/include/aghg.h"
#include "util/base/include/time_vector.h"
#include "util/base/include/value.h"

// Forward declaration
class IEmissionsDriver;
class AEmissionsControl;
class IInfo;

/*! 
 * \ingroup Objects
 * \brief An class that represents Non-CO2 emissions.
 * \author Kate Calvin
 */
class NonCO2Emissions: public AGHG {

public:
    NonCO2Emissions();
    
    virtual ~NonCO2Emissions();
    
    virtual NonCO2Emissions* clone() const;

    virtual void copyGHGParameters( const AGHG* aPrevGHG );
    
    static const std::string& getXMLNameStatic();
    
    virtual const std::string& getXMLName() const;

    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               const IInfo* aTechIInfo );

    virtual void initCalc( const std::string& aRegionName,
                           const IInfo* aTechIInfo,
                           const int aPeriod );

    virtual double getGHGValue( const std::string& aRegionName,
                                const std::vector<IInput*>& aInputs,
                                const std::vector<IOutput*>& aOutputs,
                                const ICaptureComponent* aSequestrationDevice,
                                const int aPeriod ) const;

	virtual void calcEmission( const std::string& aRegionName, 
                               const std::vector<IInput*>& aInputs,
                               const std::vector<IOutput*>& aOutputs,
					           const GDP* aGDP,
					           ICaptureComponent* aSequestrationDevice,
                               const int aPeriod );
    
    virtual void doInterpolations( const int aYear, const int aPreviousYear,
                                   const int aNextYear, const AGHG* aPreviousGHG,
                                   const AGHG* aNextGHG );

    double getAdjustedEmissCoef( const int aPeriod ) const;
    
protected:
    NonCO2Emissions( const NonCO2Emissions& aOther );
    NonCO2Emissions& operator=( const NonCO2Emissions& aOther );
    
    virtual void toDebugXMLDerived( const int period, std::ostream& aOut, Tabs* aTabs ) const;
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        AGHG,

        //! The emissions coefficient.
        DEFINE_VARIABLE( SIMPLE | STATE, "emiss-coef", mEmissionsCoef, Value ),

        //! Emissions to calibrate to if provided.
        DEFINE_VARIABLE( SIMPLE, "input-emissions", mInputEmissions, Value ),
                                
        //! Set of emissions controls
        DEFINE_VARIABLE( CONTAINER, "emissions-control", mEmissionsControls, std::vector<AEmissionsControl*> ),
        
        //! Emissions driver delegate
        DEFINE_VARIABLE( CONTAINER, "emissions-driver", mEmissionsDriver, IEmissionsDriver* ),
                                
        //! Stored Emissions Coefficient (needed for some control technologies)
        //! The emissions coefficient is the current ratio of emissions to driver, accounting for any controls   
        DEFINE_VARIABLE( ARRAY | STATE | NOT_PARSABLE, "control-adjusted-emiss-coef", mAdjustedEmissCoef, objects::TechVintageVector<Value> )
    )

    //! A flag to indicate if mInputEmissions should be used recalibrate mEmissionsCoef
    //! in the current model period.
    bool mShouldCalibrateEmissCoef;

    //! A weark reference to the regional GDP object which needs to be stashed to be
    //! able to calculate the emissions controls.
    const GDP* mGDP;

    // typdef to help simplify code
    typedef std::vector<AEmissionsControl*>::const_iterator CControlIterator;
    
    void clear();

    void copy( const NonCO2Emissions& aOther );
};

#endif // _NONCO2_EMISSIONS_H_
