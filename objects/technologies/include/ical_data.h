#ifndef _ICAL_DATA_H_
#define _ICAL_DATA_H_
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
 * \file ical_data.h
 * \ingroup Objects
 * \brief The ICalData interface header file.
 * \author James Blackwood
 */

#include <xercesc/dom/DOMNode.hpp>

// Forward declarations
class Demographic;
class Tabs;

/*!
 * \brief An interface to an object which represents the calibration value of a
 *        Technology's primary input and output for its initial period.
 * \details This interface allows a Technology to abstract whether an output or
 *          input calibration value is read in. Conversion between input and
 *          output can be performed by this interface using the Technology's
 *          single fixed efficiency. Only one calibration value may be read in
 *          per Technology.
 */
class ICalData {
public:
    ICalData();
    virtual ~ICalData();

    // TODO: Inherit from a common base class so the following functions have
    // inherited documentation.
    virtual ICalData* clone() const = 0;

    virtual void XMLParse( const xercesc::DOMNode* aNode ) = 0;
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const = 0;
    virtual void toDebugXML( std::ostream& aOut, Tabs* aTabs ) const = 0;

    /*!
     * \brief Complete the initialization of the object for a period.
     * \param aDemographics Regional demographics container.
     * \param aPeriod Model period
     */
    virtual void initCalc( const Demographic* aDemographics,
                           const int aPeriod ) = 0;
    
    /*!
     * \brief Get the calibration input value.
     * \param aEfficiency Technology efficiency.
     * \return Calibration input value.
     */
    virtual double getCalInput( const double aEfficiency ) = 0;

    /*!
     * \brief Get the calibration output value.
     * \param aEfficiency Technology efficiency.
     * \return Calibration output value.
     */
    virtual double getCalOutput( const double aEfficiency ) = 0;

    /*!
     * \brief Scale the calibration value by a constant factor.
     * \param aScaleFactor Scale the calibration value by a constant factor.
     */
    virtual void scaleValue( const double aScaleFactor ) = 0;
};

// Inline function definitions.

//! Constructor.
inline ICalData::ICalData(){
}

//! Destructor.
inline ICalData::~ICalData(){
}

#endif // _ICAL_DATA_H_
