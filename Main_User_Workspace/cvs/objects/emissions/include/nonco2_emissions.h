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

#include "emissions/include/aghg.h"
#include "util/base/include/value.h"

class AEmissionsDriver;
class AEmissionsControl;

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

    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               const IInfo* aTechIInfo );

    virtual void initCalc( const std::string& aRegionName,
                           const IInfo* aLocalInfo,
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
protected: 
    NonCO2Emissions( const NonCO2Emissions& aOther );
    NonCO2Emissions& operator=( const NonCO2Emissions& aOther );
    
    virtual const std::string& getXMLName() const;
    virtual bool XMLDerivedClassParse( const std::string& aNodeName, const xercesc::DOMNode* aCurrNode );
    virtual void toInputXMLDerived( std::ostream& aOut, Tabs* aTabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& aOut, Tabs* aTabs ) const;

private:    
    //! The emissions coefficient.
    Value mEmissionsCoef;

    //! Emissions to calibrate to if provided.
    Value mInputEmissions;

    //! A flag to indicate if mInputEmissions should be used recalibrate mEmissionsCoef
    //! in the current model period.
    bool mShouldCalibrateEmissCoef;

    //! A weark reference to the regional GDP object which needs to be stashed to be
    //! able to calculate the emissions controls.
    const GDP* mGDP;

    //! Emissions driver delegate
    std::auto_ptr<AEmissionsDriver> mEmissionsDriver;

    //! Set of emissions controls
    std::vector<AEmissionsControl*> mEmissionsControls;

    // typdef to help simplify code
    typedef std::vector<AEmissionsControl*>::const_iterator CControlIterator;

    void clear();

    void copy( const NonCO2Emissions& aOther );
};

#endif // _NONCO2_EMISSIONS_H_
