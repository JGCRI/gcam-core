#ifndef _LINEAR_CONTROL_H_
#define _LINEAR_CONTROL_H_
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
 * \file linear_control.h
 * \ingroup Objects
 * \brief LinearControl class header file.
 * \author Steve Smith
 */

#include "emissions/include/aemissions_control.h"
#include "util/base/include/value.h"

/*! 
 * \ingroup Objects
 * \brief An class that impliments a linear reduction in the emissions factor
 * \author Steve Smith
 */
class LinearControl: public AEmissionsControl {
public:
    LinearControl();
    
    virtual ~LinearControl();
    
    virtual LinearControl* clone() const;
    
    static const std::string& getXMLNameStatic();
    
    virtual const std::string& getXMLName() const;
    
    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               const IInfo* aTechIInfo );

    virtual void initCalc( const std::string& aRegionName,
                           const IInfo* aTechInfo,
                           const NonCO2Emissions* aParentGHG,
                           const int aPeriod );

protected: 
    LinearControl( const LinearControl& aOther );
    LinearControl& operator=( const LinearControl& aOther );
    
    virtual void toDebugXMLDerived( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const;

    virtual void calcEmissionsReduction( const std::string& aRegionName, const int aPeriod, const GDP* aGDP );

    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        AEmissionsControl,
        
        //! Target year for final emissions factor. Does not have to be a model period.
        DEFINE_VARIABLE( SIMPLE, "end-year", mTargetYear, int ),
        
        //! Year to start emission factor decline. Must be a model year.
        //! The default is the final calibration year.
        DEFINE_VARIABLE( SIMPLE, "start-year", mStartYear, int ),
        
        //! Final emissions coefficient
        DEFINE_VARIABLE( SIMPLE, "final-emissions-coefficient", mFinalEmCoefficient, Value ),
        
        //!  Emissions redution percentage (alternative to specifying emissions coefficient)
        DEFINE_VARIABLE( SIMPLE, "control-percentage", mControlFraction, Value ),
        
        //! Flag if wish to allow emissions factor increase
        DEFINE_VARIABLE( SIMPLE, "allow-ef-increase", mAllowIncrease, bool )
    )

private:
    void copy( const LinearControl& aOther );

    virtual void calcEmissionsReductionInternal( const double aBaseEmissionsCoef, const int aPeriod );

};

#endif // _LINEAR_CONTROL_H_
