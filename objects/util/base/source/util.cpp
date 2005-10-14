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

namespace util {
	/*! \brief A function to replace spaces with underscores.
	* \details This function will replace all spaces in a string with
	*          underscores. Each space will be replaced by exactly one
	*          underscore, multiple spaces are not concatonated. Other
	*          whitespace characters are not replaced.
	* \param stringIn The string in which spaces should be replaced by
	*        underscores.
	*/
	void replaceSpaces( string& stringIn ) {
		basic_string<char>::size_type index;
		while( stringIn.find_first_of( " " ) != basic_string<char>::npos ) {
			index = stringIn.find_first_of( " " );
			stringIn.replace( index, 1, "_" );
		}
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
