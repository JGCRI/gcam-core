/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

#ifndef _STANDARD_CAPTURE_COMPONENT_H_
#define _STANDARD_CAPTURE_COMPONENT_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file standard_capture_component.h
 * \ingroup Objects
 * \brief StandardCaptureComponent class header file.
 * \author Josh Lurz
 */

#include <string>
#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include "technologies/include/icapture_component.h"

/*! 
 * \ingroup Objects
 * \brief This object is added on to Technologies so that they can sequester
 *          their emissions instead of emitting them. 
 * \details This object is responsible for controlling and calculating the cost
 *          for sequestered emissions using simple multiplicative penalties on
 *          efficiency and non-energy cost. The standard capture component
 *          applies multiplicative penalties to the non-energy costs and the
 *          Technology efficiency. A separate storage cost per unit of
 *          sequestered emissions is also calculated, this is determined by a
 *          storage market if one was read in and exists, or a fallback read-in
 *          per unit cost. The fraction of emissions sequestered is determined by
 *          a read-in value.<br>
 *
 *          Effective efficiency is calculated as:
 *          \f[ H_{effective} = H_{technology} * (1-H_{penalty}) \f]
 *
 *          where
 *              - \f$H_{technology}\f$ is the efficiency of the technology.
 *              - \f$H_{penalty}\f$ is the efficiency penalty due to capture.
 *
 *          Total non-energy cost with capture applied is calculated as:
 *          \f[ NE_{total} = (NE_{technology} + (1 + NE_{adder}) \f]
 *
 *          where
 *              - \f$NE_{technology}\f$ is the non-energy cost of the technology.
 *              - \f$NE_{adder}\f$ is the non-energy cost adder.
 *
 *          Total captured emissions are calculated as:
 *          \f[ E_{captured} = RF * (C_{fuel} * I - C_{product} * O) \f]
 *
 *          where
 *              - \f$RF\f$ is the fraction of emissions captured.
 *              - \f$C_{fuel}\f$ is the emissions coefficient of the fuel.
 *              - \f$I\f$ is the input consumed by the technology.
 *              - \f$C_{product}\f$ is the emissions coefficient of the product.
 *              - \f$O\f$ is the output of the technology.
 *
 *          <b>XML specification for StandardCaptureComponent</b>
 *          - XML name: \c standard-capture-component
 *          - Contained by: Technology
 *          - Parsing inherited from class: None
 *          - Attributes: None
 *          - Elements:
 *              - \c storage-market StandardCaptureComponent::mStorageMarket
 *              - \c storage-cost StandardCaptureComponent::mStorageCost
 *              - \c remove-fraction StandardCaptureComponent::mRemoveFraction
 *              - \c efficiency-penalty StandardCaptureComponent::mEfficiencyPenalty
 *              - \c non-energy-penalty StandardCaptureComponent::mNonEnergyCostPenalty
 *     
 *
* \author Josh Lurz
*/
class StandardCaptureComponent: public ICaptureComponent {
    friend class CaptureComponentFactory;
public:
    // Documentation inherits.
    virtual StandardCaptureComponent* clone() const;
    
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
    StandardCaptureComponent();
    
    static const std::string& getXMLNameStatic();

    //! Sequestered quantity by period.
    std::vector<double> mSequesteredAmount;

    //! Name of the storage market.
    std::string mStorageMarket;

     //! Fraction of carbon removed from fuel.
    double mRemoveFraction;
    
    //! Storage cost associated with the remove fraction.
    double mStorageCost;

    //! Multiplicative energy efficiency penalty.
    double mEfficiencyPenalty;

    //! Multiplicative non-energy cost penalty.
    double mNonEnergyCostPenalty;
};

#endif // _STANDARD_CAPTURE_COMPONENT_H_
