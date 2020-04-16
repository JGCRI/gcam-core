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
* \file util.cpp 
* \ingroup Objects
* \brief Source file for the util functions.
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include "util/base/include/util.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "util/base/include/configuration.h"
#include "util/logger/include/ilogger.h"

#include <string>
#include <ctime>

using namespace std;

extern Scenario* scenario;

namespace objects {
    /*!
     * \brief Linearly interpolate or extrapolate a Y value for a given X value,
     *        given two other points.
     * \param aX The X value for which to determine a Y value.
     * \param aX1 X value of the first point.
     * \param aX2 X value of the second point.
     * \param aY1 Y value of the first point.
     * \param aY2 Y value of the second point.
     * \return The Y value of the requested X point.
     * \warning aX1 and aX2 must not define the same point. If they do, the only
     *          Y value that can be returned is the Y value of that single
     *          point.
     */
    double linearInterpolateY( const double aX,
                               const double aX1,
                               const double aX2,
                               const double aY1,
                               const double aY2 )
    {
        // If the given points have the same value, a line segment cannot
        // be created and the Y value of the given X point cannot be
        // determined unless the requested X value is the same point.
        if( util::isEqual( aX1, aX2 ) ){
            assert( isEqual( aX, aX1 ) );
            assert( isEqual( aY1, aY2 ) );
            return aY1;
        }
        return ( aX - aX1 ) * ( aY2 - aY1 ) / ( aX2 - aX1 ) + aY1;
    }

    /*!
     * \brief Appends the scenario name to the given file name.
     * \details Very simple implementation where we search for the
     *          last '.' and insert the scenario->getName() before that.
     *          Should no '.' be found we simply append it to the end.
     * \return The modified file name which includes the scenario name.
     */
    string appendScenarioToFileName( const string& aFileName ) {
        size_t dotPos = aFileName.find_last_of( '.' );
        string modifiedFileName( aFileName );
        if( dotPos == string::npos ) {
            modifiedFileName.append( scenario->getName() );
        }
        else {
            modifiedFileName.insert( dotPos, scenario->getName() );
        }
        return modifiedFileName;
    }

    
    /*! \brief A function to replace spaces with underscores.
    * \details Returns a string equivalent to the string passed into the
    *          function but with spaces replaced with underscores. Each space
    *          will be replaced by exactly one underscore, multiple spaces are
    *          not concatenated. Other whitespace characters are not replaced.
    * \param aString The string in which spaces should be replaced by
    *        underscores.
    * \return String with spaces replaced with underscores.
    */
    string replaceSpaces( const string& aString ) {
        string result = aString;
        while( result.find_first_of( " " ) != basic_string<char>::npos ) {
            basic_string<char>::size_type index = result.find_first_of( " " );
            result.replace( index, 1, "_" );
        }
        return result;
    }

    /*! \brief Create a Minicam style run identifier.
    * \details Creates a run identifier by combining the current date and time,
    *          including the number of seconds so that is is always unique.
    * \param aTime The initialized time object.
    * \return A unique run identifier.
    */
    long createMinicamRunID( const time_t& aTime ){
        tm* timeInfo = getLocalTime( aTime );
        const long runID = timeInfo->tm_sec + timeInfo->tm_min * 100 + timeInfo->tm_hour * 10000
            + timeInfo->tm_mday * 1000000 
            + ( timeInfo->tm_mon * 100000000 + 100000000);
        return runID;
    }

    /*! \brief Function which creates an XML compliant date time string.
    *
    * This function takes as an argument a time_t object and returns a string
    * containing the date and time in the following format:
    * yyyy-mm-dd-Thh:mm-GMTOFFSET ie: 2003-01-11T09:30:47-05:00
    * \param time time_t to convert to XML string form.
    * \return string The time converted to XML date string format.
    */
    string XMLCreateDate( const time_t& time ) {
        stringstream buffer;
        string retString;

        tm* timeInfo = getLocalTime( time );
        // need to store the hour and minutes to calculate
        // the time zone offset
        int currHour = timeInfo->tm_hour;
        int currMin = timeInfo->tm_min;

        // Create the string
        buffer << ( timeInfo->tm_year + 1900 ); // Set the year
        buffer << "-";
        buffer << timeInfo->tm_mday; // Set the day
        buffer << "-";
        buffer << ( timeInfo->tm_mon + 1 ); // Month's in ctime range from 0-11
        buffer << "T";
        if( currHour < 10 ) {
            buffer << "0";
        }
        buffer << timeInfo->tm_hour;
        buffer << ":";
        if( currMin < 10 ) {
            buffer << "0";
        }
        buffer << timeInfo->tm_min;
        buffer << ":";
        if( timeInfo->tm_sec < 10 ) {
            buffer << "0";
        }
        buffer << timeInfo->tm_sec;

        // calculate the time zone offset
        // note that this assignment is superfluous since
        // getGMTime will return the same struct
        timeInfo = getGMTime( time );

        int umtHourDiff = currHour - timeInfo->tm_hour;
        int umtMinDiff = currMin - timeInfo->tm_min;

        if( umtMinDiff < 0 && umtHourDiff > 0 ) {
            --currHour;
        }

        if( umtHourDiff > 0 || ( umtHourDiff == 0 && umtMinDiff > 0 ) ) {
            buffer << "+";
        }
        else {
            buffer << "-";
        }
        if( umtHourDiff < 10 ) {
            buffer << "0";
        }
        buffer << abs( umtHourDiff ) << ":";
        if( umtMinDiff < 10 ) {
            buffer << "0";
        }
        buffer << abs( umtMinDiff );
        // Completed creating the string;
        buffer >> retString;

        return retString;
    }

    /*! 
     * \brief Get an initialized time struct with the GM time information.
     * \param aTime Current time object.
     * \warning The returned structure is statically allocated and shared by the functions
     *          getGMTime and getLocalTime.  Each time either one of these functions is
     *          called the contents of this structure is overwritten.
     * \return GM time struct.
     */
    tm* getGMTime( const time_t& aTime ){
        
        // VC 8 complains when the older unsafe versions of the time functions
        // are used. The are not available on older compilers or other
        // platforms.
#if( !defined(_MSC_VER) || _MSC_VER < 1400 )
        return gmtime( &aTime );
#else
        static tm timeInfo;
        errno_t error = gmtime_s( &timeInfo, &aTime );
        // 0 means the call was successful.
        assert( error == 0 );
        return &timeInfo;
#endif
    }

    /*! 
     * \brief Get an initialized time struct with the local time information.
     * \param aTime Current time object.
     * \warning The returned structure is statically allocated and shared by the functions
     *          getGMTime and getLocalTime.  Each time either one of these functions is
     *          called the contents of this structure is overwritten.
     * \return Local time struct.
     */
    tm* getLocalTime( const time_t& aTime ){
    // VC 8 complains when the older unsafe versions of the time functions are
    // used. The are not available on older compilers or other platforms.
#if( !defined(_MSC_VER) || _MSC_VER < 1400 )
        return localtime( &aTime );
#else
        static tm timeInfo;
        errno_t error = localtime_s( &timeInfo, &aTime );
        // 0 means the call was successful.
        assert( error == 0 );
        return &timeInfo;
#endif
    }

    /*! 
     * \brief Print the current time to an output stream.
     * \param aTime Current time object.
     * \param aStream Output stream to print into.
     */
void printTime( const time_t& aTime, ostream& aOut ){
        // VC 8 complains when the older unsafe versions of the time functions
        // are used. The are not available on older compilers or other
        // platforms.
#if( !defined(_MSC_VER) || _MSC_VER < 1400 )
    aOut << ctime( &aTime );
#else
    const size_t BUFFER_SIZE = 26;
    char* buffer = new char[ BUFFER_SIZE ];
    errno_t error = ctime_s( buffer, BUFFER_SIZE, &aTime );
    // 0 means the call was successful.
    assert( error == 0 );

    aOut << buffer;
    delete[] buffer;
#endif
    }

/*!
 * \brief Gets the appropriate value to pass to runScenarios by checking the configuration for
 *        a stop/restart -period or -year.
 * \details This method will reconcile between -period and -year using the following rules:
 *            - If year is not set then then the period is used (even if it is -1 which indicates RUN_ALL_YEARS).
 *            - If no model time is available, which may happen when running in batch mode, then
 *              Scenario::UNINITIALIZED_RUN_PERIODS is returned.
 *            - If year is set but not a valid year then Scenario::UNINITIALIZED_RUN_PERIODS
 *            - If only year is set then it will be converted to period using the model time and be used.
 *            - If both are set the year will be converted to period and used but a warning will be issued if
 *              they are inconsistent.
 * \param aKey The base key to look up from the configuration, i.e. "start" to check for "start-period" and
 *             "start-year".
 * \return The appropriate period value to use.
 */
int getConfigRunPeriod( const string aKey ) {
    // Get the main log file.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::WARNING );
    
    // Get the run period and year, defaulting to -1 which indicates not set.
    const int configPeriod = Configuration::getInstance()->getInt( aKey + "-period", -1, false );
    const int configYear = Configuration::getInstance()->getInt( aKey + "-year", -1, false );
    
    // Get the model time, note getting it this way is safer then through the scenario
    // object as this method could potentially be called before a scenario is initialized.
    const Modeltime* modeltime = Modeltime::getInstance();
    
    // Declare the new period
    int newPeriod = configPeriod;
    
    // Check whether ONLY aPeriod is set.
    if ( configYear == -1 ) {
        // Use aPeriod, whether it was set or not.
        newPeriod = configPeriod;
    }
    else if( !modeltime->isInitialized() ) {
        // The model time is not yet initialized.  This can happen if we are running
        // in batch mode for instance.  We will return uninitialized for now and give
        // the batch runner a second chance.  If the uninitialized value persists 
        // to the point of use a warning will be generated at that point.
        newPeriod = Scenario::UNINITIALIZED_RUN_PERIODS;
    }
    else if( !modeltime->isModelYear( configYear ) ) {
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Invalid Year Specified:  " << configYear << endl;
        newPeriod = Scenario::UNINITIALIZED_RUN_PERIODS;
    }
    else if ( configPeriod == -1 ) {
        // Only aYear was set, so use that.
        newPeriod = modeltime->getyr_to_per( configYear );
    }
    else {
        // Both aYear and aPeriod were set.
        // We will use aYear, but warn if they are inconsistent
        newPeriod = modeltime->getyr_to_per( configYear );
        if ( newPeriod != configPeriod ) {
            // If the two specifications are inconsistent, print a notice, but use aYear.
            mainLog.setLevel( ILogger::NOTICE );
            mainLog << "Year and period are specified inconsistently. Using year." << endl;
        }
    }
    
    return newPeriod;
}


}

