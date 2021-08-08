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


#ifndef _ICOEFFICIENT_H_
#define _ICOEFFICIENT_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file ICoefficient.h
 * \ingroup Objects
 * \brief ICoefficient interface header file.
 * \author Josh Lurz
 */

#include <iosfwd>
#include <string>
#include <boost/core/noncopyable.hpp>

#include "util/base/include/inamed.h"
#include "util/base/include/data_definition_util.h"

class Tabs;

// Need to forward declare the subclasses as well.
class Efficiency;
class Intensity;

/*! 
 * \ingroup Objects
 * \brief Represents a single coefficient of a production function.
 * \details This interface represents a coefficient for a single input in a
 *          production function. In a leontief production function this
 *          coefficient is equal to the intensity of the input. For more complex
 *          production functions, the output is more complexly related to this
 *          coefficient.
 * \author Josh Lurz
 */
class ICoefficient : public INamed,
                     private boost::noncopyable
{
public:
    /*!
     * \brief Constructor.
     * \details Inlined constructor to avoid compiler problems with abstract
     *          base classes. 
     */
    ICoefficient();

    /*!
     * \brief Destructor.
     * \details Inlined destructor to avoid compiler problems with abstract base
     *          classes. 
     */
    virtual ~ICoefficient();
    
    /*!
     * \brief Creates an exact copy of the coefficient.
     * \return An exact copy of the coefficient. 
     */
    virtual ICoefficient* clone() const = 0;
    
    /*!
     * \brief Get the XML name appropriate for the subclass.
     * \return The XML name that can be used for input/output.
     */
    virtual const std::string& getXMLName() const = 0;

    /*!
     * \brief Complete the initialization of the coefficient.
     */
    virtual void completeInit() = 0;
    
    /*!
     * \brief Write data from this object in an XML format for debugging.
     * \param aPeriod Period for which to write data.
     * \param aOut Filestream to which to write.
     * \param aTabs Object responsible for writing the correct number of tabs.
     */
    virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const = 0;

    /*!
     * \brief Get the coefficient.
     * \details Returns the coefficienct for use of the input in the production
     *          function. For leontief production functions this is equal to the
     *          intensity, or output divided by input. For more complex
     *          production functions this condition does not hold.
     * \return The coefficient.
     */
    virtual double getCoefficient() const = 0;
    
protected:
    DEFINE_DATA(
        /* Declare all subclasses of ICoefficient to allow automatic traversal of the
         * hierarchy under introspection.
         */
        DEFINE_SUBCLASS_FAMILY( ICoefficient, Efficiency, Intensity )
    )
};

// Inline function definitions.
inline ICoefficient::ICoefficient(){
}

inline ICoefficient::~ICoefficient(){
}

#endif // _ICOEFFICIENT_H_
