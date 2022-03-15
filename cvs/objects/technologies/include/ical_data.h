#ifndef _ICAL_DATA_H_
#define _ICAL_DATA_H_
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
 * \file ical_data.h
 * \ingroup Objects
 * \brief The ICalData interface header file.
 * \author James Blackwood
 */

#include <boost/core/noncopyable.hpp>

#include "util/base/include/data_definition_util.h"

// Forward declarations
class Demographic;
class Tabs;

// Need to forward declare the subclasses as well.
class CalDataOutput;
class CalDataOutputPercap;

/*!
 * \brief An interface to an object which represents the calibration value of a
 *        Technology's primary input and output for its initial period.
 * \details This interface allows a Technology to abstract whether an output or
 *          input calibration value is read in. Conversion between input and
 *          output can be performed by this interface using the Technology's
 *          single fixed efficiency. Only one calibration value may be read in
 *          per Technology.
 */
class ICalData : private boost::noncopyable {
public:
    ICalData();
    virtual ~ICalData();

    // TODO: Inherit from a common base class so the following functions have
    // inherited documentation.
    virtual ICalData* clone() const = 0;

    virtual void toDebugXML( std::ostream& aOut, Tabs* aTabs ) const = 0;

    /*!
     * \brief Complete the initialization of the object for a period.
     * \param aDemographics Regional demographics container.
     * \param aPeriod Model period
     */
    virtual void initCalc( const Demographic* aDemographics,
                           const int aPeriod ) = 0;
    
    /*
     * \brief Complete the initialization of the calibration object.
     */
    virtual void completeInit() = 0;

    /*! \brief Get the calibration output value.
    * \param aEfficiency Technology efficiency.
    * \return Calibration output value.
    */
    virtual double getCalOutput() = 0;
    
protected:
    
    DEFINE_DATA(
        /* Declare all subclasses of ICalData to allow automatic traversal of the
         * hierarchy under introspection.
         */
        DEFINE_SUBCLASS_FAMILY( ICalData, CalDataOutput, CalDataOutputPercap )
    )
};

// Inline function definitions.

//! Constructor.
inline ICalData::ICalData(){
}

//! Destructor.
inline ICalData::~ICalData(){
}

#endif // _ICAL_DATA_H_
