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
* \file relative_cost_logit.cpp
* \ingroup objects
* \brief RelativeCostLogit class source file
* \author Robert Link
*/

#include "util/base/include/definitions.h"
#include <stdlib.h>
#include <math.h>
#include <cassert>
#include <string>

#include "functions/include/relative_cost_logit.hpp"
#include "util/base/include/xml_helper.h"
#include "util/logger/include/ilogger.h"

using namespace std;

//! default constructor:  arg value <= 0 will get filled in with the default
RelativeCostLogit::RelativeCostLogit():
mLogitExponent( 1.0 ),
mBaseValue( 0.0 ),
mParsedBaseValue( false )
{
}

//! destructor: nothing to clean up
RelativeCostLogit::~RelativeCostLogit() {
}

const string& RelativeCostLogit::getXMLNameStatic() {
    const static string XML_NAME = "relative-cost-logit";
    return XML_NAME;
}

void RelativeCostLogit::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElement( mLogitExponent[ aPeriod ], "logit-exponent", aOut, aTabs );
    XMLWriteElement( mBaseValue, "base-value", aOut, aTabs );
    XMLWriteElement( mParsedBaseValue, "parsed-base-value", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void RelativeCostLogit::initCalc( const string& aRegionName, const string& aContainerName,
                                  const bool aShouldShareIncreaseWithValue, const int aPeriod)
{
    if( mLogitExponent[ aPeriod ] != 0.0 && aShouldShareIncreaseWithValue ^ ( mLogitExponent[ aPeriod ] > 0.0 ) ) {
        ILogger& mainlog = ILogger::getLogger( "main_log" );
        mainlog.setLevel( ILogger::WARNING );
        mainlog << "Inversed sign on logit exponent: " << mLogitExponent[ aPeriod ]
                << " when we expect the unormalized share to increase with an increase in value is " << aShouldShareIncreaseWithValue
                << endl;
    }
    
    if( !mParsedBaseValue && mBaseValue == getMinValueThreshold() ) {
        // Illegal value.  For relative cost logit it will not affect sharing and
        // we can continue operating the model normally so just generate a warning
        ILogger &calibrationLog = ILogger::getLogger("calibration_log");
        ILogger::WarningLevel oldlvl = calibrationLog.setLevel(ILogger::WARNING);
        calibrationLog << "In " << aRegionName << ", " << aContainerName<< ":  invalid or uninitialized base value parameter  "
                       << mBaseValue << endl;
        calibrationLog.setLevel(oldlvl);
    }
}


/*!
 * \brief Relative value logit discrete choice function.
 * \details Calculate the log of the numerator of the discrete choice (i.e., the unnormalized version) 
 *          function being used to calculate subsector shares in this sector.  The normalization 
 *          factor will be calculated later.
 * \param aShareWeight share weight for the choice for which the share is being calculated.
 * \param aValue value for the choice for which the share is being calculated.
 * \param aPeriod model time period for the calculation.
 * \return log of the unnormalized share.
 * \warning Negative values can not be used in this logit formulation.  Instead the value
 *          the value is capped at RelativeCostLogit::getMinValueThreshold.  This implies
 *          no behavior once values have crossed this threshold.
 */
double RelativeCostLogit::calcUnnormalizedShare( const double aShareWeight, const double aValue,
                                                 const int aPeriod ) const
{
    // Zero share weight implies no share which is signaled by negative infinity.
    const double minInf = -std::numeric_limits<double>::infinity();
    double logShareWeight = aShareWeight > 0.0 ? log( aShareWeight ) : minInf;

    // Negative values are not allowed so they are instead capped at getMinValueThreshold()
    double cappedValue = std::max( aValue, getMinValueThreshold() );
    
    return logShareWeight + mLogitExponent[ aPeriod ] * log( cappedValue );
    // This log is the difference between the relative value    --^
    // logit and the absolute value logit.
}

double RelativeCostLogit::calcAverageValue( const double aUnnormalizedShareSum,
                                           const double aLogShareFac,
                                           const int aPeriod ) const
{
    double ret;
    if( mLogitExponent[ aPeriod ] == 0.0 ) {
        // TODO: what to do with zero logit?
        ret = aUnnormalizedShareSum * exp( aLogShareFac ) * mBaseValue;
    }
    else if( aUnnormalizedShareSum == 0 && mLogitExponent[ aPeriod ] < 0 ) {
        // No Valid options and negative logit so return a large value so a nested
        // logit would not want to choose this nest.
        ret = util::getLargeNumber();
    }
    else if( aUnnormalizedShareSum == 0 && mLogitExponent[ aPeriod ] > 0 ) {
        // No Valid options and positive logit so return a large negative value
        // so a nested logit would not want to choose this nest.
        ret = -util::getLargeNumber();
    }
    else {
        ret = exp( aLogShareFac / mLogitExponent[ aPeriod ] )
              * pow( aUnnormalizedShareSum, 1.0 / mLogitExponent[ aPeriod ] );
    }

    return ret;
}

/*!
 * \brief Share weight calculation for the relative value logit.
 * \details  Given an "anchor" choice with observed share and price and another choice
 *           also with observed share and price, compute the inverse of the discrete choice function
 *           to produce a share weight.
 * \param aShare observed share for the current choice.
 * \param aValue observed value for the current choice.
 * \param aAnchorShare observed share for the anchor choice.
 * \param aAnchorValue observed value for the anchor choice.
 * \param aPeriod model time period for the calculation.
 * \return share weight for the current choice.
 */
double RelativeCostLogit::calcShareWeight( const double aShare, const double aValue, const double aAnchorShare,
                                           const double aAnchorValue, const int aPeriod ) const
{
    // Negative values are not allowed so they are instead capped at getMinValueThreshold()
    double cappedValue = std::max( aValue, getMinValueThreshold() );
    double cappedAnchorValue = std::max( aAnchorValue, getMinValueThreshold() );
    
    // guard against numerical instability in the pow when the share was zero anyway
    return aShare == 0.0 ? 0.0 : ( aShare / aAnchorShare ) * pow( cappedAnchorValue / cappedValue, mLogitExponent[ aPeriod ] );
}

/*!
 * \brief A base cost is not used in the relative value formulation.  However we may use it
 *        out of convenience to set the scale of share weights which is particularly important
 *        if users will call the calcAverageValue method.
 * \details Note the value will not be set if a user has explicitly set via XMLParse the value
 *          they would like to use explicitly.
 * \param aBaseValue The value to set.
 */
void RelativeCostLogit::setBaseValue( const double aBaseValue ) {
    if( !mParsedBaseValue ) {
        mBaseValue = std::max( aBaseValue, getMinValueThreshold() );
    } // This function is a no-op if mParsedBaseValue is set.
}

/*!
 * \brief Get the minimum value threshold value that may be used in this logit share
 *        equation.
 * \return The threshold value.
 */
double RelativeCostLogit::getMinValueThreshold() {
    const double MIN_VALUE = 0.001;
    return MIN_VALUE;
}
