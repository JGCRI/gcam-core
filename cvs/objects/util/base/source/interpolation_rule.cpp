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
 * \file interpolation_rule.cpp
 * \ingroup Objects
 * \brief InterpolationRule class source file.
 * \author Pralit Patel
 */

#include "util/base/include/definitions.h"
#include <string>

#include "util/base/include/interpolation_rule.h"
#include "util/base/include/fixed_interpolation_function.h" // for the fixed hack
#include "util/base/include/linear_interpolation_function.h"
#include "util/base/include/s_curve_interpolation_function.h"
#include "util/curves/include/xy_data_point.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/xml_parse_helper.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "util/base/include/factory.h"

using namespace std;
using namespace objects;

extern Scenario* scenario;

InterpolationRule::InterpolationRule()
{
    mFromYear = -1;
    mToYear = -1;
    mOverwritePolicy = ALWAYS;
    mWarnWhenOverwritting = false;
    mIsFixedFunction = false;
    mInterpolationFunction = 0;
}

InterpolationRule::~InterpolationRule() {
    delete mInterpolationFunction;
}

InterpolationRule* InterpolationRule::clone() const {
    InterpolationRule* clone = new InterpolationRule();
    clone->copy( *this );
    return clone;
}

void InterpolationRule::copy( const InterpolationRule& aOther ) {
    mFromYear = aOther.mFromYear;
    mFromValue = aOther.mFromValue;
    mToYear = aOther.mToYear;
    mToValue = aOther.mToValue;
    mApplyTo = aOther.mApplyTo;
    mOverwritePolicy = aOther.mOverwritePolicy;
    mWarnWhenOverwritting = aOther.mWarnWhenOverwritting;
    mIsFixedFunction = aOther.mIsFixedFunction;
    
    delete mInterpolationFunction;
    mInterpolationFunction = aOther.mInterpolationFunction ? aOther.mInterpolationFunction->clone() : 0;
}

const string& InterpolationRule::getXMLNameStatic() {
    const static string XML_NAME = "interpolation-rule";
    return XML_NAME;
}

/*!
 * \brief Return the constant flag that indicates a user wanted to interpolate
 *        to the last model year.
 * \return Constant flag used in the input data to be replaced with the last
 *         model year.
 */
const int InterpolationRule::getLastModelYearConstant() {
    const static int END_MODEL_YEAR_FLAG = 9999;
    return END_MODEL_YEAR_FLAG;
}

bool InterpolationRule::XMLParse( rapidxml::xml_node<char>* & aNode ) {
    // replace to year if it is equal to the last model year flag
    if( mToYear == getLastModelYearConstant() ) {
        mToYear = scenario->getModeltime()->getEndYear();
    }
    
    string nodeName = XMLParseHelper::getNodeName( aNode );
    map<string, string> attrs = XMLParseHelper::getAllAttrs( aNode );
    
    if( nodeName == IInterpolationFunction::getXMLNameStatic() ) {
        string functionName = attrs["name"];
        IInterpolationFunction* tempFn = Factory<IInterpolationFunction::SubClassFamilyVector>::createType(functionName);
        // parse child nodes
        ParseChildData parseChildHelper(aNode, attrs);
        parseChildHelper.setContainer(tempFn);
        ExpandDataVector<IInterpolationFunction::SubClassFamilyVector> getDataVector;
        tempFn->doDataExpansion( getDataVector );
        getDataVector.getFullDataVector(parseChildHelper);

        // only set valid functions
        if( tempFn ) {
            mIsFixedFunction = functionName == FixedInterpolationFunction::getXMLNameStatic();
            delete mInterpolationFunction;
            mInterpolationFunction = tempFn;
        }
    }
    else if( nodeName == "overwrite-policy" ) {
        mWarnWhenOverwritting = XMLParseHelper::getValue<bool>( attrs["warn"] );

        const string overwritePolicyStr = XMLParseHelper::getValue<string>( aNode );
        if( overwritePolicyStr == overwritePolicyEnumToStr( NEVER ) ) {
            mOverwritePolicy = NEVER;
        }
        else if( overwritePolicyStr == overwritePolicyEnumToStr( INTERPOLATED ) ) {
            mOverwritePolicy = INTERPOLATED;
        }
        else if( overwritePolicyStr == overwritePolicyEnumToStr( ALWAYS ) ) {
            mOverwritePolicy = ALWAYS;
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized overwrite policy: " << overwritePolicyStr << " found while parsing "
                    << getXMLNameStatic() << "." << endl;
        }
    }
    else {
        return false;
    }
    return true;
}
    

void InterpolationRule::toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const {
    aTabs->writeTabs( aOut );
    aOut << "<" << getXMLNameStatic() << " apply-to=\"" << mApplyTo
         << "\" from-year=\"" << mFromYear
         << "\" to-year=\"" <<  mToYear
         << "\">" << endl;
    aTabs->increaseIndent();

    // only write out a value to this element if one was parsed
    if( mFromValue.isInited() ) {
        XMLWriteElement( mFromValue, "from-value", aOut, aTabs );
    }

    // only write out a value to this element if one was parsed
    if( mToValue.isInited() ) {
        XMLWriteElement( mToValue, "to-value", aOut, aTabs );
    }

    mInterpolationFunction->toDebugXML( aPeriod, aOut, aTabs );

    map<string, bool> attrs;
    attrs[ "warn" ] = mWarnWhenOverwritting;
    XMLWriteElementWithAttributes( overwritePolicyEnumToStr( mOverwritePolicy ),
        "overwrite-policy", aOut, aTabs, attrs );

    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

/*!
 * \brief Perform any potential interpolations according to the parsed rules.
 * \details Apply interpolations for any years that are with in the ranged
 *          defined by this rule by calling the interpolation function contained
 *          in this rule.  The overwrite policy will determine the behavoir when
 *          trying to interpolate a value which has already been set in
 *          aValuesToInterpolate.  If the overwrite policy is set to INTERPOLATED
 *          then aParsedValues will be utilized to determine how a value was set.
 * \param aValuesToInterpolate The period vector which contains values which may
 *                             need to be interpolated.
 * \param aParsedValues The period vector which contains the original values which
 *                      were parsed.
 */
void InterpolationRule::applyInterpolations( PeriodVector<Value>& aValuesToInterpolate,
                                             const PeriodVector<Value>& aParsedValues ) const
{
    // perform error checking before attempting interpolations
    if( !mInterpolationFunction ) {
        // abort no interpolation function set
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "No interpolation function set" << endl;
        exit( 1 );
    }
    if( mToYear <= mFromYear ) {
        // abort to <= from
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "to-year is before or equal to from-year" << endl;
        exit( 1 );
    }
    const Modeltime* modeltime = scenario->getModeltime();
    int fromPer = 0;
    if( modeltime->isModelYear( mFromYear ) ) {
        // the left bracket is a model year so the fromPer can be converted
        // directly
        fromPer = modeltime->getyr_to_per( mFromYear );
        if( !mFromValue.isInited() && !aValuesToInterpolate[ fromPer ].isInited() ) {
            // if we were to get the from-value from the given period vector
            // then it should have been set already
            // abort from-value has not been set
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Could not find a value to interpolate from." << endl;
            exit( 1 );
        }
    }
    else {
        if( !mFromValue.isInited() ) {
            // if the from-year is not a model year then the from-value should
            // have been set
            // abort no from-value has been set for non-model year
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "A from value must be parsed when interpolating from a non-model year." << endl;
            exit( 1 );
        }
        // we need to find the closest model period to from-year
        for( ; fromPer < modeltime->getmaxper() && modeltime->getper_to_yr( fromPer ) < mFromYear; ++fromPer ) {
        }
        if( fromPer == modeltime->getmaxper() ) {
            // abort could not find a model year after mFromYear
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "There are no model years after " << mFromYear << "." << endl;
            exit( 1 );
        }

        // subtract 1 from fromPer since it is currently the first period that we would
        // want to interpolate
        --fromPer;
    }

    int toPer = modeltime->getmaxper() - 1;
    if( modeltime->isModelYear( mToYear ) ) {
        // the right bracket is a model year so the toPer can be converted
        // directly
        toPer = modeltime->getyr_to_per( mToYear );

        if( !mIsFixedFunction && !mToValue.isInited() && !aValuesToInterpolate[ toPer ].isInited() ) {
            // if we were to get the to-value from the given period vector
            // then it should have been set already
            // abort to-value has not been set
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Could not find a value to interpolate to." << endl;
            exit( 1 );
        }
    }
    else {
        if( !mToValue.isInited() && !mIsFixedFunction ) {
            // if the to-year is not a model year then the to-value should
            // have been set
            // abort no to-value has been set for non-model year
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "A to value must be parsed when interpolating to a non-model year." << endl;
            exit( 1 );
        }
        // we need to find the closest model period to to-year
        for( ; toPer >= 0 && modeltime->getper_to_yr( toPer ) > mToYear; --toPer ) {
        }
        if( toPer < 0 ) {
            // abort could not find a model year before mToYear
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "There are no model years before " << mToYear << "." << endl;
            exit( 1 );
        }

        // add 1 to toPer since it is currently the last period that we would
        // want to interpolate
        ++toPer;
    }

    // all error checking has occured so we can set the left and right brackets
    // and perform any interpolations
    XYDataPoint leftBracket( mFromYear, mFromValue.isInited() ? mFromValue : aValuesToInterpolate[ fromPer ] );
    XYDataPoint rightBracket( mToYear, mToValue.isInited() || mIsFixedFunction ? mToValue : aValuesToInterpolate[ toPer ] );

    // increment one to interpolate or set the terminal period for the fixed function.
    if( mIsFixedFunction ){ ++toPer; } 

    for( int per = fromPer + 1; per < toPer; ++per ) {
        // determine if we can set a value in this period
        if( !aValuesToInterpolate[ per ].isInited() // no value as been set yet
            || ( mOverwritePolicy == INTERPOLATED && !aParsedValues[ per ].isInited() ) // can overwrite interpolated
            || mOverwritePolicy == ALWAYS // always overwrite
            ) {
                // interpolate a new value for the current period
                const double newValue = mInterpolationFunction->interpolate(
                    &leftBracket, &rightBracket, modeltime->getper_to_yr( per ) );

                // print a warning if the flag was set and we are indeed overwriting
                // a value
                if( aValuesToInterpolate[ per ].isInited() && mWarnWhenOverwritting ) {
                    ILogger& mainLog = ILogger::getLogger( "main_log" );
                    mainLog.setLevel( ILogger::WARNING );
                    mainLog << "Overwriting value " << aValuesToInterpolate[ per ] << " with interpolated value "
                        << newValue << " in year " << modeltime->getper_to_yr( per ) << endl;
                    // TODO: be able to say where we are.
                }
                aValuesToInterpolate[ per ].set( newValue );
        }
    }
}

/*!
 * \brief Convert the enumerated OverwritePolicy values to a string which 
 *        is suitable for use during XMLParse.
 * \param aPolicy An enumerated value to convert.
 * \return The string which represents that value.
 */
string InterpolationRule::overwritePolicyEnumToStr( const OverwritePolicy aPolicy ) const {
    const static string names[] = {
        "NEVER",
        "INTERPOLATED",
        "ALWAYS"
    };

    return names[ aPolicy ];
}
