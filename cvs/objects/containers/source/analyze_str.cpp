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
 * \file analyze_str.cpp
 * \ingroup util
 * \brief AnalyzeStr class source file.
 * \author Pralit Patel
 */

#include <cstring>

#include "containers/include/analyze_str.hpp"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/gcam_fusion.hpp"
#include "util/base/include/gcam_data_containers.h"

using namespace std;

extern Scenario* scenario;

template<typename A, typename B>
std::pair<B,A> flip_pair(const std::pair<A,B> &p)
{
    return std::pair<B,A>(p.second, p.first);
}

template<typename A, typename B>
std::multimap<B,A> flip_map(const std::map<A,B> &src)
{
    std::multimap<B,A> dst;
    std::transform(src.begin(), src.end(), std::inserter(dst, dst.begin()),
                   flip_pair<A,B>);
    return dst;
}

void AnalyzeStr::calcFeedbacksBeforePeriod( Scenario* aScenario, const IClimateModel* aClimateModel, const int aPeriod ) {
    // do nothing
}


void AnalyzeStr::calcFeedbacksAfterPeriod( Scenario* aScenario, const IClimateModel* aClimateModel, const int aPeriod ) {
    // do stuff
    mNumStr = 0;
    mTotalSize = 0;
    mStrCount.clear();

    vector<FilterStep*> collectStateSteps( 2, 0 );
    collectStateSteps[ 0 ] = new FilterStep( "" );
    collectStateSteps[ 1 ] = new FilterStep( "", DataFlags::SIMPLE );
    // DoCollect will handle all fusion callbacks thus their template boolean parameter
    // are set to true.
    GCAMFusion<AnalyzeStr, false, false, true> gatherState( *this, collectStateSteps );
    gatherState.startFilter( aScenario );
    
    // DoCollect has now gathered all active state into the mStateValues list to
    // allow faster/easier processing for the remaining tasks at hand.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::SEVERE );
    mainLog << "sizeof(string::value_type): " << sizeof(string::value_type) << endl;
    mainLog << "sizeof(string): " << sizeof(string) << endl;
    mainLog << "Num str: " << mNumStr << endl;
    mainLog << "Total size of str: " << mTotalSize << endl;
    mainLog << "Unique str: " << mStrCount.size() << endl;
    mainLog << "Top 10 str and num occurrences:" << endl;
    
    std::multimap<size_t, string> flipped = flip_map( mStrCount );
    auto iter = flipped.rbegin();
    for( int i = 0; i < 10; ++i ) {
        mainLog << (*iter).second << " = " << (*iter).first << endl;
        ++iter;
    }

    // clean up GCAMFusion related memory
    for( auto filterStep : collectStateSteps ) {
        delete filterStep;
    }
}

template<typename DataType>
void AnalyzeStr::processData( DataType& aData ) {
    // ignore
}

template<>
void AnalyzeStr::processData<string>( string& aData ) {
    ++mNumStr;
    mTotalSize += sizeof(string) + sizeof(string::value_type) * aData.size();
    
    auto iter = mStrCount.find( aData );
    if(iter == mStrCount.end()) {
        iter = mStrCount.insert(make_pair( aData, 0)).first;
    }

    ++(*iter).second;
}

