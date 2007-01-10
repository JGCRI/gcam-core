#ifndef _IOUTPUT_H_
#define _IOUTPUT_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Labratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

/*! 
 * \file ioutput.h
 * \ingroup Objects
 * \brief IOutput interface header file.
 * \author Josh Lurz
 */

#include <string>
#include <xercesc/dom/DOMNode.hpp>

class Tabs;
class DependencyFinder;
class ICaptureComponent;

#include "util/base/include/ivisitable.h"
#include "util/base/include/iparsable.h"
#include "util/base/include/iround_trippable.h"
/*! 
* \ingroup Objects
* \brief Represents a single generic output of a Technology.
* \details The output interface represents a single output of a technology. All
*          MiniCAM technologies must have a primary output that implements this
*          interface. The primary output level is determined by the technology.
*          The output levels of other outputs may be determined by the scale of
*          the primary output, or through other means. Outputs may have positive
*          or negative monetary value, which is incorporated into the costs of
*          operating the technology. Outputs may also have associated carbon
*          content and emissions. These are accounted for in the emissions cost
*          and quantity calculations.
* \author Josh Lurz
*/
class IOutput: public IVisitable,
               public IParsable,
               public IRoundTrippable 
{ 
public:
    /*! 
     * \brief Constructor.
     * \details Inlined constructor to avoid compiler problems with abstract
     *          base classes. 
     */
    inline IOutput();

    /*!
     * \brief Destructor.
     * \details Inlined destructor to avoid compiler problems with abstract base
     *          classes and allow deletion through the base class pointer. 
     */
    inline virtual ~IOutput();
    
    /*!
     * \brief Creates an exact copy.
     * \return An exact copy. 
     */
    virtual IOutput* clone() const = 0;

    /*!
     * \brief Returns whether the type of the object is the same as the passed
     *        in type.
     * \param aType Type to check the object's type against.
     * \return Whether the type of the object is the same as the passed in type.
     */
    virtual bool isSameType( const std::string& aType ) const = 0;
    
    /*!
     * \brief Return the name of the output.
     * \return The name of the output.
     */
    virtual const std::string& getName() const = 0;

    // Documentation is inherited.
    virtual bool XMLParse( const xercesc::DOMNode* aNode ) = 0;
    
    // Documentation is inherited.
    virtual void toInputXML( std::ostream& aOut,
                             Tabs* aTabs ) const = 0;
    
    // Documentation is inherited.
    virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const = 0;

    /*!
     * \brief Complete the initialization of the output.
     * \param aSectorName Sector name.
     * \param aDependencyFinder The input dependency finder, which may be null.
     * \param aIsTechOperating Whether the Technology can operate.
     */
    virtual void completeInit( const std::string& aSectorName,
                               DependencyFinder* aDependencyFinder,
                               const bool aIsTechOperating ) = 0;

    /*!
     * \brief Initialize an output for a given period.
     * \param aRegionName Name of the containing region.
     * \param aPeriod Model period.
     */
    virtual void initCalc( const std::string& aRegionName,
                           const int aPeriod ) = 0;

    /*!
     * \brief Perform any final operations for a output in a given period.
     * \param aRegionName Name of the containing region.
     * \param aPeriod Model period.
     */
    virtual void postCalc( const std::string& aRegionName,
                           const int aPeriod ) = 0;

    /*! 
     * \brief Scale the output coefficient by a specified value.
     * \details Scales the output coefficient by a specified value. Outputs
     *          are not required to support this operation.
     * \note This is currently a workaround for biproducts not having
             independent coefficients.
     * \param aCoefficientScaler Coefficient scaler.
     */
    virtual void scaleCoefficient( const double aScaler ) = 0;

    /*!
     * \brief Calculate and return the physical output determined by the
     *        specified primary output of the Technology.
     * \details Determine the level of output from the known primary output of
     *          the Technology. Does not add the output to the marketplace.
     * \param aPrimaryOutput Output of the primary good.
     * \param aRegionName Region name.
     * \param aPeriod Period.
     * \return Physical output given the level of primary output.
     * \sa setPhysicalOutput
     */
    virtual double calcPhysicalOutput( const double aPrimaryOutput,
                                       const std::string& aRegionName,
                                       const ICaptureComponent* aCaptureComponent,
                                       const int aPeriod ) const = 0;

    /*!
     * \brief Set the physical output determined by the specified primary output
     *        of the Technology.
     * \details Determine the level of output from the known primary output of
     *          the Technology and add the output to the marketplace.
     * \param aPrimaryOutput Output of the primary good.
     * \param aRegionName Region name.
     * \param aPeriod Period.
     * \sa calcPhysicalOutput
     */
    virtual void setPhysicalOutput( const double aPrimaryOutput,
                                    const std::string& aRegionName,
                                    ICaptureComponent* aCaptureComponent,
                                    const int aPeriod ) = 0;

    /*!
     * \brief Get the quantity of physical output.
     * \param aPeriod Model period.
     * \return The physical output.
     */
    virtual double getPhysicalOutput( const int aPeriod ) const = 0;

    /*!
     * \brief Get the monetary value of a single unit of output.
     * \param aRegionName Name of the region containing the output.
     * \param aPeriod Period.
     * \return The value in the given period.
     */
    virtual double getValue( const std::string& aRegionName,
                             const ICaptureComponent* aCaptureComponent,
                             const int aPeriod ) const = 0;

    /*!
     * \brief Get the emissions of a given gas per unit of primary output.
     * \details Return the emissions of the gas per a unit of primary output.
     *          For the primary output, this is the same as the emissions
     *          coefficient. For other outputs, this is determined by their
     *          emissions coefficient and their current ratio of output to the
     *          primary output.
     * \param aGHGName The name of the gas.
     * \param aPeriod Model period
     * \return The emissions of the gas per unit of output.
     */
    virtual double getEmissionsPerOutput( const std::string& aGHGName,
                                          const int aPeriod ) const = 0;

    // Documentation is inherited.
    virtual void accept( IVisitor* aVisitor,
                        const int aPeriod ) const = 0;
};

// Inline function definitions.
IOutput::IOutput(){
}

IOutput::~IOutput(){
}

#endif // _IOUTPUT_H_
