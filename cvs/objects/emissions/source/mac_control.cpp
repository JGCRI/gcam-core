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
AEmissionsControl()
{
    mNoZeroCostReductions = false;
    mMacCurve = new PointSetCurve( new ExplicitPointSet() );
}

//! Default destructor.
MACControl::~MACControl(){
    delete mMacCurve;
}

//! Copy constructor.
MACControl::MACControl( const MACControl& aOther )
: AEmissionsControl( aOther ) {
    mMacCurve = 0;
    copy( aOther );
}

//! Clone operator.
MACControl* MACControl::clone() const {
    return new MACControl( *this );
}

//! Assignment operator.
MACControl& MACControl::operator=( const MACControl& aOther ){
    if( this != &aOther ){
        // Free memory before copying.  Since this is just a single
        // variable I am just deleting it directly here.
        delete mMacCurve;
        mMacCurve = 0;
        AEmissionsControl::operator=( aOther );
        copy( aOther );
    }
    return *this;
}

//! Copy helper function.
void MACControl::copy( const MACControl& aOther ){
    /*!
     * \pre mMacCurve should be null otherwise we have a memory leak.
     */
    assert( !mMacCurve );
    mMacCurve = aOther.mMacCurve->clone();
    mNoZeroCostReductions = aOther.mNoZeroCostReductions;
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
}

void MACControl::toDebugXMLDerived( const int period, ostream& aOut, Tabs* aTabs ) const {
    toInputXMLDerived( aOut, aTabs );
    XMLWriteElement( mNoZeroCostReductions, "no-zero-cost-reductions", aOut, aTabs);
}

void MACControl::completeInit( const string& aRegionName, const string& aSectorName,
                               const IInfo* aTechInfo )
{
    scenario->getMarketplace()->getDependencyFinder()->addDependency( aSectorName, aRegionName, "CO2", aRegionName );
}

void MACControl::initCalc( const string& aRegionName,
                           const IInfo* aLocalInfo,
                           const int aPeriod )
{
    // TODO: Figure out what gas this is & print more meaningful information
    if ( mMacCurve->getMaxX() == -DBL_MAX ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "MAC Curve appears to have no data. " << endl;
    }
}

void MACControl::calcEmissionsReduction( const std::string& aRegionName, const int aPeriod, const GDP* aGDP ) {
    const Marketplace* marketplace = scenario->getMarketplace();
    double effectiveCarbonPrice = marketplace->getPrice( "CO2", aRegionName, aPeriod, false );
    if( effectiveCarbonPrice == Marketplace::NO_MARKET_PRICE ) {
        effectiveCarbonPrice = 0;
    }
       
    double reduction = getMACValue( effectiveCarbonPrice );
    
    if( mNoZeroCostReductions && effectiveCarbonPrice == 0.0 ) {
        reduction = 0.0;
    }
    
    setEmissionsReduction( reduction );
}

/*! \brief Get MAC curve value
 *  Wrapper function that takes care of error handling for MAC curve values.
 *  If there is an error, a value of zero is returned and a message is logged.
 *  Errors can happen if no MAC curve values are read in, although perhaps other error situations can occur.
 * \param aCarbonPrice carbon price
 */
double MACControl::getMACValue( const double aCarbonPrice ) const {
    const double maxCO2Tax = mMacCurve->getMaxX();
    
    // so that getY function won't interpolate beyond last value
    double effectiveCarbonPrice = min( aCarbonPrice, maxCO2Tax );

    double reduction = mMacCurve->getY( effectiveCarbonPrice );

    // Check to see if an error has occurred.
    if ( reduction == -DBL_MAX ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << " An error occured when evaluating MAC curve for a GHG." << endl;
        reduction = 0;
    }
    
    return reduction;
}
