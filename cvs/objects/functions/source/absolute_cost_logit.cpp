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
* \file absolute_cost_logit.cpp
* \ingroup objects
* \brief AbsoluteCostLogit class source file
* \author Robert Link
*/

#include "util/base/include/definitions.h"
#include <stdlib.h>
#include <math.h>
#include <cassert>
#include <string>
#include <numeric>

#include "functions/include/absolute_cost_logit.hpp"
#include "util/base/include/xml_helper.h"
#include "util/logger/include/ilogger.h"

using namespace std;

//! constructor: arg values <= 0 (including default) will get filled in with the default
AbsoluteCostLogit::AbsoluteCostLogit():
mLogitExponent( 1.0 )
{
    mBaseValue = 0.001;
    mParsedBaseValue = false;
}

//! destructor: nothing to clean up
AbsoluteCostLogit::~AbsoluteCostLogit() {
}

const string& AbsoluteCostLogit::getXMLNameStatic() {
    const static string XML_NAME = "absolute-cost-logit";
    return XML_NAME;
}

void AbsoluteCostLogit::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElement( mLogitExponent[ aPeriod ], "logit-exponent", aOut, aTabs );
    XMLWriteElement( mBaseValue, "base-value", aOut, aTabs );
    XMLWriteElement( mParsedBaseValue, "parsed-base-value", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void AbsoluteCostLogit::initCalc( const string& aRegionName, const string& aContainerName,
                                  const bool aShouldShareIncreaseWithValue, const int aPeriod)
{
    if( mLogitExponent[ aPeriod ] != 0.0 && aShouldShareIncreaseWithValue ^ ( mLogitExponent[ aPeriod ] > 0.0 ) ) {
        ILogger& mainlog = ILogger::getLogger( "main_log" );
        mainlog.setLevel( ILogger::WARNING );
        mainlog << "Inversed sign on logit exponent: " << mLogitExponent[ aPeriod ]
                << " when we expect the unormalized share to increase with an increase in value is " << aShouldShareIncreaseWithValue
                << endl;
    }
    
    if( !mParsedBaseValue && mBaseValue <= 0.0 ) {
        bool isFuturePer = aPeriod > scenario->getModeltime()->getFinalCalibrationPeriod();
        // Illegal value.  Since this will affect sharing in the model we will
        // not be able to proceed and will abort.
        ILogger &calibrationLog = ILogger::getLogger("calibration_log");
        calibrationLog.setLevel(isFuturePer ? ILogger::SEVERE : ILogger::WARNING);
        calibrationLog << "In " << aRegionName << ", " << aContainerName << ":  invalid or uninitialized base value parameter  "
                       << mBaseValue << endl;
        if( isFuturePer ) {
            calibrationLog << "This value will generate invalid sharing with " << getXMLName()
                           << ", please parse a reasonable value instead." << endl;
            abort();
        }
    }
}

/*!
 * \brief Absolute cost logit discrete choice function.
 * \details Calculate the log of the numerator of the discrete choice (i.e., the unnormalized version) 
 *          function being used to calculate subsector shares in this sector.  The normalization 
 *          factor will be calculated later.
 * \param aShareWeight share weight for the choice for which the share is being calculated.
 * \param aValue value for the choice for which the share is being calculated.
 * \param aPeriod model time period for the calculation.
 * \return log of the unnormalized share.
 */
double AbsoluteCostLogit::calcUnnormalizedShare( const double aShareWeight, const double aValue,
                                                 const int aPeriod ) const
{
    /*!
     * \pre A valid base cost has been set.
     */
    assert( mBaseValue > 0 );

    // Zero share weight implies no share which is signaled by negative infinity.
    const double minInf = -std::numeric_limits<double>::infinity();
    double logShareWeight = aShareWeight > 0.0 ? log( aShareWeight ) : minInf; // log(alpha)
    //           v--- log(alpha * exp(beta*p/p0))  ---v
    return logShareWeight + mLogitExponent[ aPeriod ] * aValue / mBaseValue;
}

double AbsoluteCostLogit::calcAverageValue( const double aUnnormalizedShareSum,
                                           const double aLogShareFac,
                                           const int aPeriod ) const
{
    double ret;
    if( mLogitExponent[ aPeriod ] == 0.0 ) {
        // TODO: what to do with zero logit?
        ret = mBaseValue * aUnnormalizedShareSum * exp( aLogShareFac );
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
        ret = ( aLogShareFac + log( aUnnormalizedShareSum ) )
              * ( mBaseValue / mLogitExponent[ aPeriod ] ) + mBaseValue;
    }

    return ret;
}

/*!
 * \brief Share weight calculation for the absolute cost logit.
 * \details Given an an "anchor" subsector with observed share and value and another choice
 *          also with observed share and value, compute the inverse of the discrete choice function
 *          to produce a share weight.
 * \param aShare observed share for the current choice.
 * \param aValue observed value for the current choice.
 * \param aAnchorShare observed share for the anchor choice.
 * \param aAnchorValue observed value for the anchor choice.
 * \param aPeriod model time period for the calculation.
 * \return share weight for the current choice.
 */
double AbsoluteCostLogit::calcShareWeight( const double aShare, const double aValue, const double aAnchorShare,
                                           const double aAnchorValue, const int aPeriod ) const
{
    double coef = mLogitExponent[ aPeriod ] / mBaseValue;
    return ( aShare / aAnchorShare ) * exp( coef * ( aAnchorValue - aValue ) );
}

/*!
 * \brief Set the value scale for the logit choice function
 * \details This parameter determines the value range in which the
 *          logit parameter will have the same behavior as a logit
 *          exponent with the same numerical value in the
 *          relative-cost variant.  The purpose of this parameter is
 *          to allow us to easily work out the numerical values of the
 *          choice function parameters that will give behavior similar
 *          to a relative-cost-logit with known parameters (at least,
 *          over a limited range of value values).  This function is
 *          called by the calibration subroutines, which use a set of
 *          heuristics to set the value scale automatically.  If a
 *          value for the value scale parameter was specified
 *          explicitly in the input, then the value suggested by the
 *          calibration subroutine is ignored, and the parsed value is
 *          used instead.
 * \param aBaseValue Value to set as the value scale parameter.
 */
void AbsoluteCostLogit::setBaseValue( const double aBaseValue ) {
  if( !mParsedBaseValue ) {
      // both zero and negative base values are problematic
      // however a zero value will generate NaNs while a negative
      // value will still calibrate just fine however it will cause
      // inversed sharing behavior once we move beyond calibration
      // thus we will replace explicitly zero values with a negative
      // and in initCalc check for negative base values (only generate
      // warnings if we solve at a negative value).
      mBaseValue = aBaseValue == 0.0 ? -1.0 : aBaseValue;
  } // This function is a no-op if mParsedBaseValue is set.
}
