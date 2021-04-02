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
 * \file mac_control.cpp
 * \ingroup Objects
 * \brief MACControl class source file.
 * \author Kate Calvin
 */

#include "util/base/include/definitions.h"

#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "emissions/include/gdp_control.h"
#include "containers/include/scenario.h"
#include "containers/include/gdp.h"
#include "util/base/include/xml_helper.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/model_time.h"
//#include "containers/include/iinfo.h"
//#include "technologies/include/ioutput.h"
//#include "functions/include/function_utils.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//! Default constructor.
GDPControl::GDPControl():
AEmissionsControl()
{
}

//! Default destructor.
GDPControl::~GDPControl(){
}

//! Copy constructor.
GDPControl::GDPControl( const GDPControl& aOther )
: AEmissionsControl( aOther )
{
    copy( aOther );
}

//! Clone operator.
GDPControl* GDPControl::clone() const {
    return new GDPControl( *this );
}

//! Assignment operator.
GDPControl& GDPControl::operator=( const GDPControl& aOther ){
    if( this != &aOther ){
        // If there was a destructor it would need to be called here.
        AEmissionsControl::operator=( aOther );
        copy( aOther );
    }
    return *this;
}

//! Copy helper function.
void GDPControl::copy( const GDPControl& aOther ){
    mMaxReduction = aOther.mMaxReduction;
    mSteepness = aOther.mSteepness;
}

/*!
 * \brief Get the XML node name for output to XML.
 * \details This public function accesses the private constant string, XML_NAME.
 *          This way the tag is always consistent for both read-in and output and can be easily changed.
 *          This function may be virtual to be overridden by derived class pointers.
 * \author Jim Naslund
 * \return The constant XML_NAME.
 */
const string& GDPControl::getXMLName() const {
    return getXMLNameStatic();
}

const string& GDPControl::getXMLNameStatic(){
    static const string XML_NAME = "gdp-control";
    return XML_NAME;
}

bool GDPControl::XMLDerivedClassParse( const string& aNodeName, const DOMNode* aCurrNode ){
    
    if ( aNodeName == "max-reduction" ){
        mMaxReduction = XMLHelper<Value>::getValue( aCurrNode );
    }
    else if ( aNodeName == "steepness" ){
        mSteepness = XMLHelper<Value>::getValue( aCurrNode );
    }
    else{
        return false;
    }
       
    return true;
}

void GDPControl::toDebugXMLDerived( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteElement( mMaxReduction, "max-reduction", aOut, aTabs);
    XMLWriteElement( mSteepness, "steepness", aOut, aTabs);
}

void GDPControl::completeInit( const string& aRegionName, const string& aSectorName,
                               const IInfo* aTechIInfo )
{

}

void GDPControl::initCalc( const string& aRegionName,
                           const IInfo* aTechInfo,
                           const NonCO2Emissions* aParentGHG,
                           const int aPeriod )
{
    // TODO: Figure out what gas this is & print more meaningful information
    if ( !mMaxReduction.isInited() || !mSteepness.isInited() ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "GDP control function has not been parameterized. " << endl;
    }    
}

void GDPControl::calcEmissionsReduction( const std::string& aRegionName, const int aPeriod, const GDP* aGDP ) {
    double reduction = 0.0;
    
    // Calculate reduction
    // we only make adjustments in future model periods
    int finalCalibPer = scenario->getModeltime()->getFinalCalibrationPeriod();
    if( aPeriod > finalCalibPer ) {
        double baseGDP = aGDP->getGDPperCap( finalCalibPer );
        double currGDP = aGDP->getGDPperCap( aPeriod );
        reduction = 1 - ( 1.0 / ( 1.0 + ( currGDP - baseGDP ) / mSteepness ));
            
        // Ensure reduction doesn't exceed maximum allowed
        const double maxReductionFraction = mMaxReduction / 100.0;
        if ( reduction > maxReductionFraction ) {
            reduction = maxReductionFraction;
        }
        
        // Also ensure that reduction is not negative. This can happen if GDP declines significantly
        if ( reduction < 0.0 ) {
            reduction = 0.0;
        }
    }
    
    setEmissionsReduction( reduction );
}
