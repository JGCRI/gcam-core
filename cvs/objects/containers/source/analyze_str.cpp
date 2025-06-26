#if DEBUG_STATE

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
#include "containers/include/scenario.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/gcamstr.h"
#include "util/base/include/gcam_fusion.hpp"
#include "util/base/include/gcam_data_containers.h"

using namespace std;

extern Scenario* scenario;

// some helper functions so that we can flip our map[string] -> instance count
// to multimap[instance count] -> string which we can quickly sort to produce the
// top N most frequent strings
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
    // do introspection and report results at the end of a model period in case any new strings get instantiated
    // during this period
    
    // clear out counts
    mNumActualStr = 0;
    mNumStr = 0;
    mTotalSize = 0;
    mStrCount.clear();

    // we want to search for any string Data types (std::string or gcamstr) however we do not
    // presently have any GCAMFusion predicates to do this kind of search
    // instead we will search for SIMPLE data and only handle `processData` for the desired types
    vector<FilterStep*> collectStateSteps( 2, 0 );
    collectStateSteps[ 0 ] = new FilterStep( "" );
    collectStateSteps[ 1 ] = new FilterStep( "", DataFlags::SIMPLE );
    // only interested in the "data", not how we got there so only processData is set to true
    GCAMFusion<AnalyzeStr, false, false, true> gatherState( *this, collectStateSteps );
    gatherState.startFilter( aScenario );
    
    // This is un-ideal, however we need to identify the proper holder type to access the DebugFactory
    // (which AnalyzeStr is a friend of) to check the size of the internal string pool.  This is a compiler
    // generated derived type which we could ideally access directly from GCAMStrBase however the boost implementation
    // declared the derived type private, so we need to replicate it here.
    using core_type = boost::flyweights::detail::flyweight_core<boost::flyweights::detail::default_value_policy<std::string>,
        mpl_::na, boost::flyweights::no_tracking, DebugFactory, boost::flyweights::no_locking, boost::flyweights::static_holder>;
    
    // we will have finished gathering all of the statistics at this point so go ahead
    // and report results
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "String pool size: " << core_type::factory().cont.size() << endl;
    mainLog << "sizeof(string::value_type): " << sizeof(string::value_type) << endl;
    mainLog << "sizeof(string): " << sizeof(string) << endl;
    mainLog << "Num std::string: " << mNumActualStr << endl;
    mainLog << "Num str: " << mNumStr << endl;
    mainLog << "Total size of str: " << mTotalSize << endl;
    mainLog << "Unique str: " << mStrCount.size() << endl;
    mainLog << "Top 10 str and num occurrences:" << endl;
    
    // invert the string reference count map so that the number of instances is the key
    // and store that in a multimap which will then implicitly sort them in increasing order
    std::multimap<size_t, string> flipped = flip_map( mStrCount );
    // we want the top 10 most references so we should iterate backwards
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
    // an instance of std::string so increase the actual str count
    ++mNumActualStr;
    
    // as well as the total count and calculate the total memory size this string holds
    // which is the direct size + the number of bytes for however long the string is
    ++mNumStr;
    mTotalSize += sizeof(string) + sizeof(string::value_type) * aData.size();
    
    // update the string reference count
    auto iter = mStrCount.find( aData );
    if(iter == mStrCount.end()) {
        iter = mStrCount.insert(make_pair( aData, 0)).first;
    }
    ++(*iter).second;
}

template<>
void AnalyzeStr::processData<gcamstr>( gcamstr& aData ) {
    // an instance of gcamstr
    // here update just the total count and calculate the total memory size this string holds
    // which is the direct size + the number of bytes for however long the string is
    ++mNumStr;
    mTotalSize += sizeof(string) + sizeof(string::value_type) * aData.get().size();
    
    // update the string reference count
    auto iter = mStrCount.find( aData.get() );
    if(iter == mStrCount.end()) {
        iter = mStrCount.insert(make_pair( aData.get(), 0)).first;
    }
    ++(*iter).second;
}

#endif // DEBUG_STATE
