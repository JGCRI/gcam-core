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

#include <cmath>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "emissions/include/mac_control.h"
#include "containers/include/scenario.h"
#include "util/base/include/xml_helper.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"
#include "containers/include/market_dependency_finder.h"
#include "util/curves/include/point_set_curve.h"
#include "util/curves/include/explicit_point_set.h"
#include "util/curves/include/xy_data_point.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//! Default constructor.
MACControl::MACControl():
AEmissionsControl(),
mNoZeroCostReductions( false ),
mCovertPriceValue( 1 ),
mPriceMarketName( "CO2" ),
mZeroCostPhaseInTime( 25 ),
mMacCurve( new PointSetCurve( new ExplicitPointSet() ) )
{
}

//! Default destructor.
MACControl::~MACControl(){
}

//! Copy constructor.
MACControl::MACControl( const MACControl& aOther )
: AEmissionsControl( aOther ) {
    copy( aOther );
}

//! Clone operator.
MACControl* MACControl::clone() const {
    return new MACControl( *this );
}

//! Assignment operator.
MACControl& MACControl::operator=( const MACControl& aOther ){
    if( this != &aOther ){
        // If there was a destructor it would need to be called here.
        AEmissionsControl::operator=( aOther );
        copy( aOther );
    }
    return *this;
}

//! Copy helper function.
void MACControl::copy( const MACControl& aOther ){
    mMacCurve.reset( aOther.mMacCurve->clone() );
    mNoZeroCostReductions = aOther.mNoZeroCostReductions;
    mZeroCostPhaseInTime = aOther.mZeroCostPhaseInTime;
    mCovertPriceValue = aOther.mCovertPriceValue;
    mPriceMarketName = aOther.mPriceMarketName;
}

/*!
 * \brief Get the XML node name for output to XML.
 * \details This public function accesses the private constant string, XML_NAME.
 *          This way the tag is always consistent for both read-in and output and can be easily changed.
 *          This function may be virtual to be overridden by derived class pointers.
 * \author Jim Naslund
 * \return The constant XML_NAME.
 */
const string& MACControl::getXMLName() const {
    return getXMLNameStatic();
}

const string& MACControl::getXMLNameStatic(){
    static const string XML_NAME = "mac-control";
    return XML_NAME;
}

bool MACControl::XMLDerivedClassParse( const string& aNodeName, const DOMNode* aCurrNode ){
 
    if ( aNodeName == "mac-reduction" ){
        double taxVal = XMLHelper<double>::getAttr( aCurrNode, "tax" );
        double reductionVal = XMLHelper<double>::getValue( aCurrNode );
        XYDataPoint* currPoint = new XYDataPoint( taxVal, reductionVal );
        mMacCurve->getPointSet()->addPoint( currPoint );
    }
    else if ( aNodeName == "no-zero-cost-reductions" ){
        mNoZeroCostReductions = true;
    }
    else if ( aNodeName == "zero-cost-phase-in-time" ){
        mZeroCostPhaseInTime = XMLHelper<Value>::getValue( aCurrNode );
    }
    else if ( aNodeName == "mac-price-conversion" ){
        mCovertPriceValue = XMLHelper<Value>::getValue( aCurrNode );
    }
    else if ( aNodeName == "market-name" ){
        mPriceMarketName = XMLHelper<string>::getValue( aCurrNode );
    }
    else{
        return false;
    }    
    return true;
}


void MACControl::toInputXMLDerived( ostream& aOut, Tabs* aTabs ) const {
    
    const vector<pair<double,double> > pairs = mMacCurve->getSortedPairs();
    typedef vector<pair<double, double> >::const_iterator PairIterator;
    map<string, double> attrs;
    for( PairIterator currPair = pairs.begin(); currPair != pairs.end(); ++currPair ) {
        attrs[ "tax" ] = currPair->first;
        XMLWriteElementWithAttributes( currPair->second, "mac-reduction", aOut, aTabs, attrs );
    }
    XMLWriteElementCheckDefault( mZeroCostPhaseInTime, "zero-cost-phase-in-time", aOut, aTabs, 20 );
    XMLWriteElementCheckDefault( mNoZeroCostReductions, "no-zero-cost-reductions", aOut, aTabs, false );    
    XMLWriteElement( mCovertPriceValue, "mac-price-conversion", aOut, aTabs );
    XMLWriteElement( mPriceMarketName, "market-name", aOut, aTabs );
}

void MACControl::toDebugXMLDerived( const int period, ostream& aOut, Tabs* aTabs ) const {
    toInputXMLDerived( aOut, aTabs );
    
}

void MACControl::completeInit( const string& aRegionName, const string& aSectorName,
                               const IInfo* aTechInfo )
{
    scenario->getMarketplace()->getDependencyFinder()->addDependency( aSectorName, aRegionName, "CO2", aRegionName );

    if ( mMacCurve->getMaxX() == -DBL_MAX ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "MAC Curve " << getName() << " appears to have no data. " << endl;
    }
    
}

void MACControl::initCalc( const string& aRegionName,
                           const IInfo* aLocalInfo,
                           const NonCO2Emissions* parentGHG,
                           const int aPeriod )
{
}

void MACControl::calcEmissionsReduction( const std::string& aRegionName, const int aPeriod, const GDP* aGDP ) {
    // Check first if MAC curve operation should be turned off
    if ( mCovertPriceValue < 0 ) { // User flag to turn off MAC curves
        setEmissionsReduction( 0 );
        return;
    }
    
    const Marketplace* marketplace = scenario->getMarketplace();
    double emissionsPrice = marketplace->getPrice( mPriceMarketName, aRegionName, aPeriod, false );
    if( emissionsPrice == Marketplace::NO_MARKET_PRICE ) {
        emissionsPrice = 0;
    }
    
    emissionsPrice *= mCovertPriceValue;

    double reduction = getMACValue( emissionsPrice );
    
    if( mNoZeroCostReductions && emissionsPrice <= 0.0 ) {
        reduction = 0.0;
    }

   /*!  Adjust to smoothly phase-in "no-cost" emission reductions
     Some MAC curves have non-zero abatement at zero emissions price. Unless the users sets
     mNoZeroCostReductions, this reduction will occur even without an emissions price. This
     code smoothly phases in this abatement so that a sudden change in emissions does not
     occur. The phase-in period has a default value that can be altered
     by the user. This code also reduces this phase-in period if there is a emissions-price,
     which avoids an illogical situation where a high emissions price is present and mitigation
     is maxed out, but the "no-cost" reductions are not fully phased in.
*/
    const int lastCalYear = scenario->getModeltime()->getper_to_yr( 
                            scenario->getModeltime()->getFinalCalibrationPeriod() );
    int modelYear = scenario->getModeltime()->getper_to_yr( aPeriod );

    // Amount of zero-cost reduction
    double zeroCostReduction = getMACValue( 0 );

    if ( ( reduction > 0.0 ) && ( zeroCostReduction > 0.0 ) &&
        ( modelYear <= ( lastCalYear + mZeroCostPhaseInTime ) ) ) {
        const double maxEmissionsTax = mMacCurve->getMaxX();

		// Fraction of zero cost that is removed from original reduction value
		// Equal to 1 at last calibration year and zero at the zero cost phase in time
		double multiplier = ( static_cast<double>( lastCalYear ) + mZeroCostPhaseInTime
		                    - static_cast<double>( modelYear ) ) / mZeroCostPhaseInTime;

		// If emissions price is not zero, accelerate the phase in for consistency if there are 
		// zero cost reductions to phase in
        double adjEmissionsPrice = min( emissionsPrice, maxEmissionsTax );
        multiplier *= ( maxEmissionsTax - adjEmissionsPrice ) / maxEmissionsTax;
		
		reduction = reduction - zeroCostReduction * multiplier;
    }
    
    setEmissionsReduction( reduction );
}

/*! \brief Get MAC curve value
 *  Wrapper function that takes care of error handling for MAC curve values.
 *  If there is an error, a value of zero is returned and a message is logged.
 * \param aCarbonPrice carbon price
 */
double MACControl::getMACValue( const double aCarbonPrice ) const {
    const double maxCO2Tax = mMacCurve->getMaxX();
    
    // so that getY function won't interpolate beyond last value
    double effectiveCarbonPrice = min( aCarbonPrice, maxCO2Tax );

    double reduction = mMacCurve->getY( effectiveCarbonPrice );

    // If no mac curve read in then reduction should be zero.
    // This is a legitimate option for a user to remove a mac curve
    if ( ( mMacCurve->getMinX() == mMacCurve->getMaxX() ) && ( mMacCurve->getMaxX() == 0 ) ) {
         reduction = 0;
    }
    // Check to see if some other error has occurred
    else if ( reduction == -DBL_MAX ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << " An error occured when evaluating MAC curve for a GHG." << endl;
        reduction = 0;
    }
    
    return reduction;
}
