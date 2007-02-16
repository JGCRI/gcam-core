#ifndef _NON_ENERGY_USE_CAPTURE_COMPONENT_H_
#define _NON_ENERGY_USE_CAPTURE_COMPONENT_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

/*! 
 * \file non_energy_use_capture_component.h
 * \ingroup Objects
 * \brief NonEnergyUseCaptureComponent class header file.
 * \author Josh Lurz
 */

#include <string>
#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include "technologies/include/icapture_component.h"

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
    friend class CaptureComponentFactory;
public:
    // Documentation is inherited from ICaptureComponent.
    virtual NonEnergyUseCaptureComponent* clone() const;

    virtual bool isSameType( const std::string& aType ) const;
    
    virtual const std::string& getName() const;
   
    virtual bool XMLParse( const xercesc::DOMNode* aNode );
    
    virtual void toInputXML( std::ostream& aOut,
                             Tabs* aTabs ) const;

    virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const;

    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               DependencyFinder* aDependencyFinder );

    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const std::string& aFuelName,
                           const int aPeriod );

    virtual double getStorageCost( const std::string& aRegionName,
                                   const int aPeriod ) const;

    virtual double getRemoveFraction() const;
    
    virtual void calcSequesteredAmount( const std::string& aRegionName,
                                        const std::string& aGHGName,
                                        const double aInput,
                                        const double aOutput,
                                        const double aInputCoef,
                                        const double aOutputCoef,
                                        const int aPeriod );

    virtual double getSequesteredAmount( const std::string& aGHGName,
                                         const bool aGetGeologic,
                                         const int aPeriod ) const;
    
    virtual double getEffectiveEfficiency( const double aTechnologyEfficiency,
                                           const int aPeriod ) const;

    virtual double getTotalNonEnergyCost( const double aTechnologyEfficiency,
                                          const double aTechnologyNonEnergyCost,
                                          const int aPeriod ) const;
protected:
    NonEnergyUseCaptureComponent();

    static const std::string& getXMLNameStatic();

    //! Sequestered quantity by period.
    std::vector<double> mSequesteredAmount;

     //! Fraction of carbon removed from the emissions stream.
    double mRemoveFraction;
};

#endif // _NON_ENERGY_USE_CAPTURE_COMPONENT_H_
