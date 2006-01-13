#ifndef _ICAL_DATA_H_
#define _ICAL_DATA_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file cal_data.h
* \ingroup Objects
* \brief The CalData class header file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>

// Forward declarations
class Demographic;
class Tabs;

class ICalData {
public:
    inline ICalData();
    inline virtual ~ICalData();
    virtual ICalData* clone() const = 0;

    virtual void XMLParse( const xercesc::DOMNode* aNode ) = 0;
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const = 0;
    virtual void toDebugXML( std::ostream& aOut, Tabs* aTabs ) const = 0;

    /*! \brief Complete the initialization of the object for a period.
    * \param aDemographics Regional demographics container.
    * \param aPeriod Model period
    */
    virtual void initCalc( const Demographic* aDemographics,
                           const int aPeriod ) = 0;
    
    /*! \brief Get the calibration input value.
    * \param aEfficiency Technology efficiency.
    * \return Calibration input value.
    */
    virtual double getCalInput( const double aEfficiency ) = 0;

    /*! \brief Get the calibration output value.
    * \param aEfficiency Technology efficiency.
    * \return Calibration output value.
    */
    virtual double getCalOutput( const double aEfficiency ) = 0;

    /*! \brief Scale the calibration value by a constant factor.
    * \param aScaleFactor Scale the calibration value by a constant factor.
    */
    virtual void scaleValue( const double aScaleFactor ) = 0;
};

// Inline function definitions.

//! Constructor.
ICalData::ICalData(){
}

//! Destructor.
ICalData::~ICalData(){
}

#endif // _ICAL_DATA_H_
