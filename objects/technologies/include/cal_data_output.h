#ifndef _CAL_DATA_OUTPUT_H_
#define _CAL_DATA_OUTPUT_H_
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
 * \file cal_data_output.h
 * \ingroup Objects
 * \brief The CalDataOutput class header file.
 * \author James Blackwood
 */

#include <xercesc/dom/DOMNode.hpp>
#include "technologies/include/ical_data.h"

/*!
 * \brief Represents a single output calibration for a MiniCAM technology.
 * \details This class is a wrapper around a single output calibration value. It
 *          reads in a single calibration output value, which represents the
 *          calibration value of the fuel output of the Technology in the first
 *          period of the Technology.
 *
 *          <b>XML specification for CalDataOutput</b>
 *          - XML name: \c CalDataOutput
 *          - Contained by: Technology
 *          - Parsing inherited from class: None.
 *          - Attributes: None.
 *          - Elements:
 *              - \c calOutputValue CalDataOutput::mCalOutputValue
 *
 */
class CalDataOutput : public ICalData {
public:
    CalDataOutput();
    virtual CalDataOutput* clone() const;

    virtual void XMLParse( const xercesc::DOMNode* aNode );
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const;
    virtual void toDebugXML( std::ostream& aOut, Tabs* aTabs ) const;
    static const std::string& getXMLNameStatic();

    virtual void initCalc( const Demographic* aDemographics,
                           const int aPeriod );

    virtual double getCalInput( const double aEfficiency );
    virtual double getCalOutput( const double aEfficiency );
    virtual void scaleValue( const double aScaleFactor );
private:
    //! Calibrated output value.
    double mCalOutputValue;
};

#endif // _CAL_DATA_OUTPUT_H_
