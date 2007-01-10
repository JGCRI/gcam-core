#ifndef _MODEL_TIME_H_
#define _MODEL_TIME_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file model_time.h
* \ingroup Objects
* \brief The Modeltime class header file.
* \author Sonny Kim
*/

#include <vector>
#include <map>
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/iround_trippable.h"
/*! 
* \ingroup Objects
* \brief A class which defines the time information neccessary for the model to run.
* \todo This class needs to be cleaned up and documented. 
* \author Sonny Kim
*/

class Modeltime: public IRoundTrippable
{
private:
    int startYear; //!< Model start year (read-in).
    int interYear1; //!< First intermediate year.
    int interYear2; //!< Second intermediate year.
    int endYear; //!< Model end year (read-in).
    
    //! The final year in which calibration occurs.
    unsigned int mFinalCalibrationYear;

    int maxPeriod; //!< Maximum number of model periods (calculated).

    int timeStep1; //!< Timestep from start to first intermediate year.
    int timeStep2; //!< Timestep from first to second intermediate year.
    int timeStep3; //!< Timestep from second intermediate to end year.
    int numberOfPeriods1;  //!< Number of periods in first time interval.
    int numberOfPeriods1a; //!< One more in first time interval for remainder year.
    int numberOfPeriods2;  //!< Number of periods in second time interval.
    int numberOfPeriods2a; //!< One more in second time interval for remainder year.
    int numberOfPeriods3;  //!< Number of periods in third time interval.
    int numberOfPeriods3a; //!< One more in third time interval for remainder year.
    std::vector<int> periodToTimeStep; //!< Index of timesteps.

    std::vector<int> modelPeriodToYear; //!< Model period to year.
    
    std::map<int,int> yearToModelPeriod; //!< Year to model period map object.
    static const std::string XML_NAME; //!< node name for toXML methods

    // member functions
    void initElementalMembers();
    const std::string& getXMLName() const;
public:
    Modeltime();
    void XMLParse( const xercesc::DOMNode* node );
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;

    static const std::string& getXMLNameStatic();
    void set(); // calculates parameters
    int getBasePeriod() const;
    int getStartYear() const;
    int getEndYear() const;
    int gettimestep( const int period ) const { return periodToTimeStep[ period ]; } // years from last to current per
    int getmaxper() const { return maxPeriod; }  // max modeling periods

    int getper_to_yr( const int period ) const;
    int getyr_to_per( const int year ) const;

    int getFinalCalibrationPeriod() const;
};

#endif // _MODEL_TIME_H_

