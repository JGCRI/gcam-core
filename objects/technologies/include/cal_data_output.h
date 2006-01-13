#ifndef _CAL_DATA_OUTPUT_H_
#define _CAL_DATA_OUTPUT_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file cal_data_output.h
* \ingroup Objects
* \brief The CalDataOutput class header file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include "technologies/include/ical_data.h"

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
    virtual void scaleValue( const double aScaleValue );
private:
    //! Calibrated output value.
    double mCalOutputValue;
};

#endif // _CAL_DATA_OUTPUT_H_
