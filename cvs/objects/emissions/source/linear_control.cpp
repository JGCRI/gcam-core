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
 * \brief LinearControl class source file.
 * \author Steve Smith
 */

#include "util/base/include/definitions.h"

#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "emissions/include/linear_control.h"
#include "emissions/include/nonco2_emissions.h"
#include "containers/include/scenario.h"
#include "util/base/include/xml_helper.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/model_time.h"
#include "containers/include/iinfo.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//! Default constructor.
LinearControl::LinearControl():
AEmissionsControl(),
mTargetYear( 0 ),
mStartYear( 0 ),
mAllowIncrease( false ),
mBaseEmissionsCoef( -1 )
{
}

//! Default destructor.
LinearControl::~LinearControl(){
}

//! Copy constructor.
LinearControl::LinearControl( const LinearControl& aOther )
: AEmissionsControl( aOther )
{
    copy( aOther );
}

//! Clone operator.
LinearControl* LinearControl::clone() const {
    return new LinearControl( *this );
}

//! Assignment operator.
LinearControl& LinearControl::operator=( const LinearControl& aOther ){
    if( this != &aOther ){
        // If there was a destructor it would need to be called here.
        AEmissionsControl::operator=( aOther );
        copy( aOther );
    }
    return *this;
}

//! Copy helper function.
void LinearControl::copy( const LinearControl& aOther ){
    mTargetYear = aOther.mTargetYear;
    mStartYear = aOther.mStartYear;
    mFinalEmCoefficient = aOther.mFinalEmCoefficient;
    mAllowIncrease = aOther.mAllowIncrease;
    mBaseEmissionsCoef = aOther.mBaseEmissionsCoef;
}

/*!
 * \brief Get the XML node name for output to XML.
 * \details This public function accesses the private constant string, XML_NAME.
 *          This way the tag is always consistent for both read-in and output and can be easily changed.
 *          This function may be virtual to be overridden by derived class pointers.
 * \author Jim Naslund
 * \return The constant XML_NAME.
 */
const string& LinearControl::getXMLName() const {
    return getXMLNameStatic();
}

const string& LinearControl::getXMLNameStatic(){
    static const string XML_NAME = "linear-control";
    return XML_NAME;
}

bool LinearControl::XMLDerivedClassParse( const string& aNodeName, const DOMNode* aCurrNode ){
    
    if ( aNodeName == "end-year" ){
        mTargetYear = XMLHelper<Value>::getValue( aCurrNode );
    }
    else if ( aNodeName == "start-year" ){
        mStartYear = XMLHelper<Value>::getValue( aCurrNode );
    }
    else if ( aNodeName == "final-emissions-coefficient" ){
        mFinalEmCoefficient = XMLHelper<Value>::getValue( aCurrNode );
    }
    else if ( aNodeName == "allow-ef-increase" ){
        mAllowIncrease = true;
    }
    else{
        return false;
    }
       
    return true;
}

void LinearControl::toInputXMLDerived( ostream& aOut, Tabs* aTabs ) const {
    
    XMLWriteElement( mFinalEmCoefficient, "final-emissions-coefficient", aOut, aTabs);
    XMLWriteElement( mTargetYear, "end-year", aOut, aTabs);
    XMLWriteElementCheckDefault( mStartYear, "start-year", aOut, aTabs, 0 );
    XMLWriteElementCheckDefault( mAllowIncrease, "allow-ef-increase", aOut, aTabs, false );
}

void LinearControl::toDebugXMLDerived( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    toInputXMLDerived( aOut, aTabs );
    XMLWriteElement( mBaseEmissionsCoef, "base-emissions-coefficient", aOut, aTabs);
}


void LinearControl::completeInit( const string& aRegionName, const string& aSectorName,
                               const IInfo* aTechInfo )
{

    if ( ( mTargetYear == 0 ) || !mFinalEmCoefficient.isInited() ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Linear control function " << getName() << " has not been parameterized. " << endl;
    }
    
}

void LinearControl::initCalc( const string& aRegionName,
                           const IInfo* aTechInfo,
                           const NonCO2Emissions* aParentGHG,
                           const int aPeriod )
{
    int finalCalibPer = scenario->getModeltime()->getFinalCalibrationPeriod();
    double baseYear = scenario->getModeltime()->getper_to_yr( finalCalibPer );

    if ( mTargetYear <= baseYear ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Linear control function improperly parameterized. Target year <= last calibration year." << endl;
    }
    
    // Reset start year if needed
    // Note that this will be reset to the calibration year, if default value is used.
    int finalCalibYr  = scenario->getModeltime()->getper_to_yr( finalCalibPer );
    if ( mStartYear == 0 ) {
        mStartYear = finalCalibYr;
    }
    
    // Make sure start year is not before final calibration year
    if ( mStartYear < finalCalibYr ) {
        mStartYear = finalCalibYr;
    }
    
    // Need to get the emissions coefficient from start period to serve as starting point 
    // for linear decline.
    int startPeriod = scenario->getModeltime()->getyr_to_per( mStartYear );
    if ( aPeriod ==  ( startPeriod + 1 ) ) {
        mBaseEmissionsCoef = aParentGHG->mSavedEmissionsCoef[ startPeriod ];
    }
    
    // Note, the emissions driver in NonCO2Emissions::calcEmission for input driver is 
    // defined as the sum of all physical inputs (e.g. getPhysicalDemandSum). This means 
    // that the emissions factor has an unusual definition for quantities with more than 
    // one input. This function should be used with caution in these cases.
    // Fortunately, these cases are currently rare. If multiple inputs become more common 
    // changes, a means of specifying the appropriate input would need to be added.
    // Electricity inputs, for example, should never be associated with non-CO2 emissions.
}

void LinearControl::calcEmissionsReduction( const std::string& aRegionName, const int aPeriod,
                                            const GDP* aGDP ) {
    double reduction = 0.0;
    
    if ( mStartYear >= scenario->getModeltime()->getEndYear() ) {
        return;
    }
    
    double thisYear = scenario->getModeltime()->getper_to_yr( aPeriod );
    
    // Don't bother if no emissions or haven't passed starting point yet
    if ( mBaseEmissionsCoef > 0 && thisYear > mStartYear && mFinalEmCoefficient.isInited() ) {
        
        // Derivation of emission reduction formula below
        // newEF = baseEF - (baseEF - targetEF) * ( year - baseYear ) / ( targetYear - baseYear )
        // newEF = baseEF * ( 1 - reduction )  therefore reduction = 1 - newEF / baseEF
        // reduction = ( 1 - targetEF / baseEF ) * ( year - baseYear ) / ( targetYear - baseYear )
        
        // This is the final reduction
        reduction = ( 1 - mFinalEmCoefficient / mBaseEmissionsCoef );
        
        // If not at final year yet, phase this in linearly
        if ( thisYear < mTargetYear ) {
            reduction *= static_cast<double>( thisYear -  mStartYear ) /
                         static_cast<double>( mTargetYear - mStartYear );
        }
        
        // Ensure that reduction is not negative unless user specifically requires this
        if ( reduction < 0.0 && !mAllowIncrease ) {
            reduction = 0.0;
        }
    }
 
    setEmissionsReduction( reduction );
}
