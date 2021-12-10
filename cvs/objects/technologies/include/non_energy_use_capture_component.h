#ifndef _NON_ENERGY_USE_CAPTURE_COMPONENT_H_
#define _NON_ENERGY_USE_CAPTURE_COMPONENT_H_
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
 * \file non_energy_use_capture_component.h
 * \ingroup Objects
 * \brief NonEnergyUseCaptureComponent class header file.
 * \author Josh Lurz
 */

#include <string>
#include <vector>

#include "technologies/include/icapture_component.h"
#include "util/base/include/time_vector.h"
#include "util/base/include/value.h"

/*! 
 * \ingroup Objects
 * \brief This object sequesters emissions which occur from industrial sectors
 *        that use fossil fuels to produce non-energy products, such as plastics.
 * \details This object is added on to Technologies so that they can capture
 *          their non-energy emissions instead of emitting them. Non-energy use
 *          of fuels does not require a charge or efficiency penalty to capture
 *          emissions, as the emissions are sequestered in the standard
 *          production process. A storage sink is not required as the sink is the
 *          product itself.
 *          
 *          <b>XML specification for NonEnergyUseCaptureComponent</b>
 *          - XML name: \c non-energy-use-capture-component
 *          - Contained by: Technology
 *          - Parsing inherited from class: None
 *          - Attributes: None
 *          - Elements:
 *              - \c remove-fraction NonEnergyUseCaptureComponent::mRemoveFraction
 *     
 * \author Josh Lurz
 */
class NonEnergyUseCaptureComponent: public ICaptureComponent {
public:
    NonEnergyUseCaptureComponent();
    
    virtual ~NonEnergyUseCaptureComponent();
    
    // Documentation is inherited from ICaptureComponent.
    virtual NonEnergyUseCaptureComponent* clone() const;
    
    static const std::string& getXMLNameStatic();
    
    virtual const std::string& getXMLName() const;

    virtual bool isSameType( const std::string& aType ) const;
    
    virtual const std::string& getName() const;

    virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const;

    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName );
    
    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const std::string& aFuelName,
                           const int aPeriod );

    double getStorageCost( const std::string& aRegionName,
                           const std::string& aGHGName,
                           const int aPeriod ) const;

    double getRemoveFraction( const std::string& aGHGName ) const;
    
    virtual double getSequesteredAmount( const std::string& aGHGName,
                                         const bool aGetGeologic,
                                         const int aPeriod ) const;

    virtual double calcSequesteredAmount( const std::string& aRegionName,
                                          const std::string& aGHGName,
                                          const double aTotalEmissions,
                                          const int aPeriod );

    virtual void adjustInputs( const std::string& aRegionName,
                               std::vector<IInput*>& aInputs,
                               const int aPeriod ) const;
protected:
    void copy( const NonEnergyUseCaptureComponent& aOther );

    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        ICaptureComponent,

        //! Sequestered quantity by period.
        DEFINE_VARIABLE( ARRAY | STATE | NOT_PARSABLE, "sequestered-amount", mSequesteredAmount, objects::TechVintageVector<Value> ),

        //! The name of the gas which will be sequestered.
        DEFINE_VARIABLE( SIMPLE, "target-gas", mTargetGas, std::string ),

        //! Fraction of carbon removed from the emissions stream.
        DEFINE_VARIABLE( SIMPLE, "remove-fraction", mRemoveFraction, double )
    )
};

#endif // _NON_ENERGY_USE_CAPTURE_COMPONENT_H_
