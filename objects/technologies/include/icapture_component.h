#ifndef _ICAPTURE_COMPONENT_H_
#define _ICAPTURE_COMPONENT_H_
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
 * \file icapture_component.h
 * \ingroup Objects
 * \brief ICaptureComponent interface header file.
 * \author Josh Lurz
 */
#include "util/base/include/istandard_component.h"
class DependencyFinder;

/*! 
 * \ingroup Objects
 * \brief This object is responsible for determining the quantity and cost of
 *        sequestered emissions for a Technology.
 * \details This object can be added on to Technologies so that they can
 *          sequester their emissions instead of emitting them. The capture
 *          component is responsible for determining the fraction of emissions
 *          captured, the cost and efficiency loss for capturing the emissions,
 *          and how the emissions are disposed.
 * \author Josh Lurz
*/
class ICaptureComponent : public IParsedComponent { 
public:
    // Clone operator must be declared explicitly even though it is inherited
    // from IStandardComponent so that the return type can be changed. Since
    // this class is a subtype of IStandardComponent, this is legal and referred
    // to as a covariant return type.
    virtual ICaptureComponent* clone() const = 0;
    
    /*!
     * \brief Complete the initialization of the capture component.
     * \param aRegionName Region name.
     * \param aSectorName Sector name.
     * \param aDependencyFinder Regional dependency finder.
     */
    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               DependencyFinder* aDependencyFinder ) = 0;

    /*!
     * \brief Initialize the capture component for a given period.
     * \param aRegionName Region name.
     * \param aSectorName Sector name.
     * \param aFuelName Name of the fuel being consumed.
     * \param aPeriod Model period.
     */
    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const std::string& aFuelName,
                           const int aPeriod ) = 0;

    /*!
     * \brief Get the cost of storing one unit of carbon.
     * \details Calculates and returns the cost of storing a unit of carbon.
     *          This may either be based on a storage market or a read-in value.
     *          There is no switch to turn off sequestration if there is no tax,
     *          as this causes the calculation of the total social cost of a
     *          policy to be incorrect.
     * \param aRegionName Name of the region containing the capture component.
     * \param aPeriod Period in which the carbon is being stored.
     * \return The cost of storing one unit of carbon.
     */
    virtual double getStorageCost( const std::string& aRegionName,
                                   const int aPeriod ) const = 0;
    
    /*! 
     * \brief Get the fraction of emissions captured by the capture component.
     * \return The fraction of emissions captured.
     */
    virtual double getRemoveFraction() const = 0;
    
    /*!
     * \brief Calculate  the amount of emissions that are sequestered.
     * \param aRegionName Name of the region.
     * \param aGHGName Name of the GHG to sequester.
     * \param aInput Quantity of input.
     * \param aOutput Quantity of output.
     * \param aInputCoef Input coefficient.
     * \param aOutputCoef Output coefficient.
     * \param aPeriod Model period.
     */
    virtual void calcSequesteredAmount( const std::string& aRegionName,
                                        const std::string& aGHGName,
                                        const double aInput,
                                        const double aOutput,
                                        const double aInputCoef,
                                        const double aOutputCoef,
                                        const int aPeriod ) = 0;
    
    /*!
     * \brief Get the amount of the emissions sequestered.
     * \param aGHGName Name of the GHG to capture.
     * \param aGetGeologic Whether to get geologically sequestered emissions.
     * \param aPeriod Period for which to get emissions.
     * \pre The sequestered amount has been calculated.
     * \pre calcSequesteredAmount
     */
    virtual double getSequesteredAmount( const std::string& aGHGName, 
                                         const bool aGetGeologic,
                                         const int aPeriod ) const = 0;
    
    /*!
     * \brief Get the efficiency of the Technology with the capture component
     *        applied.
     * \param aRegionName Name of the containing region.
     * \param aFuelName Name of the fuel being consumed.
     * \param aTechnologyEfficiency The efficiency of the technology before
     *        capture is added.
     * \param aPeriod Model period.
     * \return The capture component of the Technology with the capture
     *         component applied.
     */
    virtual double getEffectiveEfficiency( const double aTechnologyEfficiency,
                                           const int aPeriod ) const = 0;
    
    /*!
     * \brief Get the total-non energy cost of the Technology with the capture
     *        component applied.
     * \param aTechnologyEfficiency The efficiency of the technology before
     *        capture is added.
     * \param aTechnologyNonEnergyCost The non-energy cost of the technology
     *        without sequestration.
     * \param aPeriod Model period.
     * \return The total non-energy cost of the Technology with the capture
     *         component applied.
     */
    virtual double getTotalNonEnergyCost( const double aTechnologyEfficiency,
                                          const double aTechnologyNonEnergyCost,
                                          const int aPeriod ) const = 0;
};

#endif // _ICAPTURE_COMPONENT_H_
