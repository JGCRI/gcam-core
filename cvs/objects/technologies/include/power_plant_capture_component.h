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


#ifndef _POWER_PLANT_CAPTURE_COMPONENT_H_
#define _POWER_PLANT_CAPTURE_COMPONENT_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file power_plant_capture_component.h
 * \ingroup Objects
 * \brief PowerPlantCaptureComponent class header file.
 * \author Josh Lurz
 */

#include <string>
#include <vector>
#include "technologies/include/icapture_component.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"

/*! 
 * \ingroup Objects
 * \brief This object is responsible for controlling and calculating the cost
 *        for sequestered emissions.
 * \details This object is added on to Technologies so that they can sequester
 *          their emissions instead of emitting them.<br>
 *
 *          Effective efficiency is calculated as:
 *          \f[ H_{effective} = \frac{O - \delta * O/H_{technology} * C_{fuel} * RF}{I} \f]
 *
 *          which reduces to:
 *          \f[ H_{effective} = H_{technology} - \delta * C_{fuel} * RF \f]
 *
 *          Total non-energy cost with capture applied is calculated as:
 *          \f[ NE_{total} = (NE_{technology} + a * NE_{adder}) * H_{technology} / H_{effective} \f]
 *          \f[ a= 1/H_{technology} * C_{fuel} * RF \f]
 *
 *          Total captured emissions are calculated as:
 *          \f[ E_{captured} = RF * (C_{fuel} * I - C_{product} * O) \f]
 *
 *          where
 *              - \f$I\f$ is the input to the technology.
 *              - \f$O\f$ is the output of the technology.
 *              - \f$\delta\f$ is the energy required to capture one unit of emissions.
 *              - \f$H_{technology}\f$ is the efficiency of the technology.
 *              - \f$NE_{technology}\f$ is the non-energy cost of the technology.
 *              - \f$NE_{adder}\f$ is the non-energy cost adder.
 *              - \f$C_{fuel}\f$ is the emissions coefficient of the fuel.
 *              - \f$C_{product}\f$ is the emissions coefficient of the product.
 *              - \f$RF\f$ is the fraction of emissions captured.
 *
 *          <b>XML specification for PowerPlantCaptureComponent</b>
 *          - XML name: \c non-energy-use-capture-component
 *          - Contained by: Technology
 *          - Parsing inherited from class: None
 *          - Attributes: None
 *          - Elements:
 *              - \c storage-market PowerPlantCaptureComponent::mStorageMarket
 *              - \c remove-fraction PowerPlantCaptureComponent::mRemoveFraction
 *              - \c capture-energy PowerPlantCaptureComponent::mCaptureEnergy
 *              - \c non-energy-penalty PowerPlantCaptureComponent::mNonEnergyCostPenalty
 *
 * \author Josh Lurz
 */
class PowerPlantCaptureComponent: public ICaptureComponent {
public:
    PowerPlantCaptureComponent();
    
    virtual ~PowerPlantCaptureComponent();
    
    // Documentation is inherited from ICaptureComponent.
    virtual PowerPlantCaptureComponent* clone() const;
    
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

    double getRemoveFraction( const std::string& aGHGNam ) const;

	double calcSequesteredAmount( const std::string& aRegionName,
                                  const std::string& aGHGName,
								  const double aTotalEmissions,
                                  const int aPeriod );

	double getSequesteredAmount( const std::string& aGHGName,
                                 const bool aGetGeologic,
                                 const int aPeriod ) const;

    
    void calcSequesteredAmount( const std::string& aRegionName,
                                const std::string& aGHGName,
                                const double aInput,
                                const double aOutput,
                                const double aInputCoef,
                                const double aOutputCoef,
                                const int aPeriod );

    void adjustInputs( const std::string& aRegionName,
                       std::vector<IInput*>& aInputs,
                       const int aPeriod ) const;

protected:
    
    void copy( const PowerPlantCaptureComponent& aOther );
    
    void adjustEnergyInput( IInput* aEnergyInput,
                            const int aPeriod ) const;
    
    void adjustNonEnergyInput( IInput* aNonEnergyInput,
                               const std::string& aRegionName,
                               const double aBaseEnergyIntensity,
                               const double aEffectiveEnergyIntensity,
                               const double aFuelEmissCoef,
                               const int aPeriod ) const;
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        ICaptureComponent,

        //! Sequestered quantity by period.
        DEFINE_VARIABLE( ARRAY | STATE | NOT_PARSABLE, "sequestered-amount", mSequesteredAmount, objects::TechVintageVector<Value> ),

        //! Name of the storage market.
        DEFINE_VARIABLE( SIMPLE, "storage-market", mStorageMarket, std::string ),

        //! The name of the gas which will be sequestered.
        DEFINE_VARIABLE( SIMPLE, "target-gas", mTargetGas, std::string ),

         //! Fraction of carbon removed from the emissions stream.
        DEFINE_VARIABLE( SIMPLE, "remove-fraction", mRemoveFraction, double ),

        //! The amount of energy required to capture one unit of the emitted gas.
        DEFINE_VARIABLE( SIMPLE, "capture-energy", mCaptureEnergy, double ),

        //! Non-energy cost penalty.
        DEFINE_VARIABLE( SIMPLE, "non-energy-penalty", mNonEnergyCostPenalty, double )
    )
};

#endif // _POWER_PLANT_CAPTURE_COMPONENT_H_
