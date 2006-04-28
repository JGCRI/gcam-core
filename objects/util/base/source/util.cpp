/*! 
* \file util.cpp 
* \ingroup Objects
* \brief Source file for the util functions.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include "util/base/include/util.h"

#include <string>
#include <ctime>

using namespace std;

namespace objects {
    double linearInterpolateY( const double aX,
                               const double aX1,
                               const double aX2,
                               const double aY1,
                               const double aY2 )
    {
        if( util::isEqual( aX1, aX2 ) ){
            assert( isEqual( aX, aX1 ) );
            assert( isEqual( aY1, aY2 ) );
            return aY1;
        }
        return ( aX - aX1 ) * ( aY2 - aY1 ) / ( aX2 - aX1 ) + aY1;
    }
    
	/*! \brief A function to replace spaces with underscores.
	* \details Returns a string equivalent to the string passed into the
	*          function but with spaces replaced with underscores. Each space
    *          will be replaced by exactly one underscore, multiple spaces are
    *          not concatonated. Other whitespace characters are not replaced.
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
		tm* lt = localtime( &aTime ); // struct for time components
		const long runID = lt->tm_sec + lt->tm_min * 100 + lt->tm_hour * 10000
			+ lt->tm_mday * 1000000 
			+ ( lt->tm_mon * 100000000 + 100000000);
		return runID;
	}

	/*! \brief Function which creates an XML compliant date time string.
	*
	* This function takes as an argument a time_t object and returns a string
	* containing the date and time in the following format:
	* yyyy-mm-dd-Thh:mm-GMTOFFSET ie: 2003-01-11T09:30:47-05:00
	* \param time time_t to convert to XML string form.
	* \return string The time converted to XML date string format.
	* \bug GMT offset does not work properly.
	*/
	string XMLCreateDate( const time_t& time ) {
		stringstream buffer;
		string retString;
		struct tm* timeInfo;
		struct tm* umtTimeInfo;

		timeInfo = localtime( &time );
		umtTimeInfo = gmtime( &time );

		// Create the string
		buffer << ( timeInfo->tm_year + 1900 ); // Set the year
		buffer << "-";
		buffer << timeInfo->tm_mday; // Set the day
		buffer << "-";
		buffer << ( timeInfo->tm_mon + 1 ); // Month's in ctime range from 0-11
		buffer << "T";
		buffer << timeInfo->tm_hour;
		buffer << ":";
		buffer << timeInfo->tm_min;
		buffer << ":";
		buffer << timeInfo->tm_sec;
		buffer << "-";

		int umtDiff = timeInfo->tm_hour - umtTimeInfo->tm_hour;
		if( umtDiff < 10 ) {
			buffer << "0";
		}
		buffer << umtDiff;
		buffer << ":00";
		// Completed creating the string;
		buffer >> retString;

		return retString;
	}
}
