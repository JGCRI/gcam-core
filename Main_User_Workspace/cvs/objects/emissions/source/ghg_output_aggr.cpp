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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

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
void GhgOutputAggr::initCalc( const IInfo* aLocalInfo,
                              const int aPeriod )
{
    // TODO: Emissions coefficients are not properly setup in the base period.
    if( aPeriod == 0 ){
        emissCoef = 0;
    }

    // Check for aggregate emissions factor.
    else if( aLocalInfo->hasValue( TotalSectorEmissions::aggrEmissionsPrefix() + name ) ){
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

    Ghg::initCalc( aLocalInfo, aPeriod );
}
