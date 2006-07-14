#ifndef _CAL_DATA_OUTPUT_PERCAP_H_
#define _CAL_DATA_OUTPUT_PERCAP_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file cal_data_output_percap.h
* \ingroup Objects
* \brief The CalDataOutputPercap class header file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include "technologies/include/ical_data.h"

class CalDataOutputPercap : public ICalData {
public:
    CalDataOutputPercap();
    virtual CalDataOutputPercap* clone() const;

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
    //! Calibrated output on a per capita basis.
    double mCalOutputPercapValue;

    /*! \brief Cached population for the period of the calibration value.
    * \todo If population becomes dynamic this will have to change.
    */
    double mPopulation;
};

#endif // _CAL_DATA_OUTPUT_PERCAP_H_
