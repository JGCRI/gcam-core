#ifndef _MODELTIME_H_
#define _MODELTIME_H_
#pragma once

/*! 
* \file modeltime.h
* \ingroup CIAM
* \brief The Modeltime class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <map>
#include <xercesc/dom/DOM.hpp>

using namespace std;
using namespace xercesc;

/*! 
* \ingroup CIAM
* \brief A class which defines the time information neccessary for the model to run.
* \author Sonny Kim
* \date $ Date $
* \version $ Revision $
*/

class Modeltime
{
private:
	int startYear; //!< Model start year (read-in).
	int interYear1; //!< First intermediate year.
	int interYear2; //!< Second intermediate year.
	int endYear; //!< Model end year (read-in).
	int popStartYear; //!< Start year for population data (read-in).
	int dataEndYear; //!< Last year for general data (read-in). 
	int maxPeriod; //!< Maximum number of model periods (calculated).
	int maxDataPeriod; //!< Maximum number of data points (read-in).
	int maxPopData; //!< Maximum number of data points for population (read-in).
	int dataTimeStep; //!< Timestep for data points.
	int timeStep1; //!< Timestep from start to first intermediate year.
	int timeStep2; //!< Timestep from first to second intermediate year.
	int timeStep3; //!< Timestep from second intermediate to end year.
	int numberOfPeriods1;  //!< Number of periods in first time interval.
	int numberOfPeriods1a; //!< One more in first time interval for remainder year.
	int numberOfPeriods2;  //!< Number of periods in second time interval.
	int numberOfPeriods2a; //!< One more in second time interval for remainder year.
	int numberOfPeriods3;  //!< Number of periods in third time interval.
	int numberOfPeriods3a; //!< One more in third time interval for remainder year.
	vector<int> periodToTimeStep; //!< Index of timesteps.
	vector<int> dataPeriodToModelPeriod; //!< Index of data to model period.
	vector<int> popDataToVariable; //!< Index of population data to variable.
	vector<int> dataOffset; //!< Index of timesteps.
	vector<int> modelPeriodToYear; //!< Model period to year.
	vector<int> modelPeriodToPopPeriod; //!< Population period to year.
	vector<int> popPeriodToYear; //!< Index of population timesteps.
	map<int,int> yearToModelPeriod; //!< Year to model period map object.
	
	// member functions
	void initElementalMembers();

public:
	Modeltime();
	void clear();
	void XMLParse( const DOMNode* node );
	void toXML( ostream& out ) const;
	void toDebugXML( const int period, ostream& out ) const;
	void set(); // calculates parameters
	int getstartyr() const { return startYear; }
	int getendyr() const { return endYear; }
	int getPopStartYear() const { return popStartYear; }
	int getdataendyr() const { return dataEndYear; }
	int gettimestep( const int period ) const{ return periodToTimeStep[ period ]; } // years from last to current per
	int getmaxper() const { return maxPeriod; }  // max modeling periods
	int getmaxdataper() const { return maxDataPeriod; } // data points for reading in
	int getdtimestep() const { return dataTimeStep; } // timestep for data points
	int getmaxpopdata() const { return maxPopData; } // number population data points
	int getdataoffset( const int dataPeriod ) const { return dataOffset[ dataPeriod ]; }  // data to model timestep
	int getyr_to_per( const int year ) const { return ( yearToModelPeriod.find( year ) )->second; }  // year to model period
	int getper_to_yr( const int period ) const { return modelPeriodToYear[ period ]; }  // year to model period
	int getPopPeriodToYear( const int period ) const { return popPeriodToYear[ period ]; }
	int getdata_to_mod( const int dataPeriod ) const { return dataPeriodToModelPeriod[ dataPeriod ]; } // data per to model per 
	int getmod_to_pop( const int period ) const { return modelPeriodToPopPeriod[ period ]; } // model per to pop per 
	int getpopdata_popvar( const int popDataPeriod ) { return popDataToVariable[ popDataPeriod ]; } // model per to pop per 
};

#endif // _MODELTIME_H_