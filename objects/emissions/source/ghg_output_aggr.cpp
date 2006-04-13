/*! 
* \file ghg_output_aggr.cpp
* \ingroup Objects
* \brief GhgOutputAggr class source file.
* \author Steve Smith
*/

#include "util/base/include/definitions.h"
#include "emissions/include/ghg_output_aggr.h"
#include "emissions/include/total_sector_emissions.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/iinfo.h"

using namespace std;

//! Constructor.
GhgOutputAggr::GhgOutputAggr(){
}

/*! \brief Clone the GHG.
* \return A deep copy of the GHG.
*/
GhgOutputAggr* GhgOutputAggr::clone() const {
    return new GhgOutputAggr( *this );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& GhgOutputAggr::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& GhgOutputAggr::getXMLNameStatic() {
    const static string XML_NAME = "GHG_OUTPUT_AGGR"; // All caps is non-standard.
    return XML_NAME;
}

//! Perform initializations that only need to be done once per period
void GhgOutputAggr::initCalc( const IInfo* aLocalInfo ) {

    // Check for aggregate emissions factor.
    if( aLocalInfo->hasValue( TotalSectorEmissions::aggrEmissionsPrefix() + name ) ){
        emissCoef = aLocalInfo->getDouble( TotalSectorEmissions::aggrEmissionsPrefix() + name, true );

        // -1 is the default value for input emissions.
        if( inputEmissions != -1 ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING);
            mainLog << "Aggregate GHG object " << name << " also had an input emissions read in. " 
                    << "Input emissions deleted" << endl;
            inputEmissions = -1;
        }
    }
    else {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Aggregate GHG object " << name << " has no emissions data supplied." << endl;
    }

    Ghg::initCalc( aLocalInfo );
}
