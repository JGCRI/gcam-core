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
* \file degree_days_feedback.cpp
* \ingroup Objects
* \brief The DegreeDaysFeedback class source file.
* \author Pralit Patel
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include <vector>

#include "containers/include/degree_days_feedback.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"

#include "util/base/include/gcam_fusion.hpp"
#include "util/base/include/gcam_data_containers.h"

using namespace std;
using namespace xercesc;

DegreeDaysFeedback::DegreeDaysFeedback()
:mHDDCoef( 0 ),
mCDDCoef( 0 ),
mBaseYearValue( 0 ),
mCurrDDScaler( 0 )
{
}

DegreeDaysFeedback::~DegreeDaysFeedback() {
}

const string& DegreeDaysFeedback::getXMLNameStatic() {
    const static string XML_NAME = "degree-day-feedback";
    return XML_NAME;
}

const string& DegreeDaysFeedback::getName() const {
    return mName;
}

bool DegreeDaysFeedback::XMLParse( const DOMNode* aNode ) {
    /*! \pre Make sure we were passed a valid node. */
    assert( aNode );
    
    // get the name attribute.
    mName = XMLHelper<string>::getAttr( aNode, XMLHelper<void>::name() );

    // get all child nodes.
    DOMNodeList* nodeList = aNode->getChildNodes();
    
    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
        
        if( nodeName == XMLHelper<void>::text() ) {
            continue;
        }
        else if( nodeName == "hdd-coef" ) {
            mHDDCoef = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "cdd-coef" ) {
            mCDDCoef = XMLHelper<double>::getValue( curr );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Unknown element " << nodeName << " encountered while parsing " << getXMLNameStatic() << endl;
        }
    }
    
    return true;
}

void DegreeDaysFeedback::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );

    XMLWriteElement( mHDDCoef, "hdd-coef", aOut, aTabs );
    XMLWriteElement( mCDDCoef, "cdd-coef", aOut, aTabs );
    
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void DegreeDaysFeedback::calcFeedbacksBeforePeriod( Scenario* aSceanrio, const IClimateModel* aClimateModel,
                                                    const int aPeriod )
{
    // this feedback will occur after the period has run
}

void DegreeDaysFeedback::calcFeedbacksAfterPeriod( Scenario* aScenario, const IClimateModel* aClimateModel,
                                                   const int aPeriod )
{
    const Modeltime* modeltime = aScenario->getModeltime();
    if( aPeriod < modeltime->getFinalCalibrationPeriod() ) {
        // not feedbacks will occur during historical years
        return;
    }
    vector<FilterStep*> emissFilterSteps = parseFilterString( "world/region/sector/subsector/technology" );
    emissFilterSteps.push_back( new FilterStep( "period", new YearFilter( new IntLessThanEq( modeltime->getper_to_yr( aPeriod ) ) ) ) );
    emissFilterSteps.push_back( new FilterStep( "ghg", new NamedFilter( new StringEquals( "CO2" ) ) ) );
    emissFilterSteps.push_back( new FilterStep( "emissions", new IndexFilter( new IntEquals( aPeriod ) ) ) );
    GatherEmiss gatherEmissProc;
    GCAMFusion<GatherEmiss> gatherEmiss( gatherEmissProc, emissFilterSteps );
    gatherEmiss.startFilter( aScenario );
    for( auto filterStep : emissFilterSteps ) {
        delete filterStep;
    }
    double currGlobalEmiss = gatherEmissProc.mEmiss;
    cout << "Curr global emissions are " << currGlobalEmiss << " in period " << aPeriod << endl;
    if( aPeriod == modeltime->getFinalCalibrationPeriod() ) {
        // just store the base year value
        mBaseYearValue = currGlobalEmiss;
    }
    // scale heating and cooling degree days for the next period
    mCurrDDScaler = 1.0 / ( currGlobalEmiss / mBaseYearValue ) * mHDDCoef;
    vector<FilterStep*> ddFilterSteps = parseFilterString( "world/region/consumer/nodeInput/nodeInput/nodeInput[NamedFilter,StringRegexMatches,heating]" );
    ddFilterSteps.push_back( new FilterStep( "degree-days", new IndexFilter( new IntEquals( aPeriod + 1 ) ) ) );
    GCAMFusion<DegreeDaysFeedback> scaleHDD( *this, ddFilterSteps );
    scaleHDD.startFilter( aScenario );
    
    mCurrDDScaler = ( currGlobalEmiss / mBaseYearValue ) * mCDDCoef;
    delete ddFilterSteps[ ddFilterSteps.size() - 2 ];
    ddFilterSteps[ ddFilterSteps.size() - 2 ] = new FilterStep( "nodeInput", new NamedFilter( new StringRegexMatches( "cooling" ) ) );
    GCAMFusion<DegreeDaysFeedback> scaleCDD( *this, ddFilterSteps );
    scaleCDD.startFilter( aScenario );
    for( auto filterStep : ddFilterSteps ) {
        delete filterStep;
    }
}

template<typename T>
void DegreeDaysFeedback::GatherEmiss::processData( T& aData ) {
    cout << "Off" << endl;
    assert( false );
}

template<>
void DegreeDaysFeedback::GatherEmiss::processData<double>( double& aData ) {
    //cout << "On " << aData << endl;
    mEmiss += aData;
}

template<typename T>
void DegreeDaysFeedback::processData( T& aData ) {
    cout << "DD off" << endl;
    assert( false );
}
template<>
void DegreeDaysFeedback::processData<Value>( Value& aData ) {
    aData *= mCurrDDScaler;
}
