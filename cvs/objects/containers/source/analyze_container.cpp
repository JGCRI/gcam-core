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
 * \file analyze_container.cpp
 * \ingroup util
 * \brief AnalyzeContainer class source file.
 * \author Pralit Patel
 */

#include <cstring>

#include "containers/include/analyze_container.hpp"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/gcam_fusion.hpp"
#include "util/base/include/gcam_data_containers.h"
#include "solution/util/include/calc_counter.h"

// Create a type def of the data structure used to collect results to clean up syntax
// note we define it here instead of the class as it is needed in some helper routines
// we create as well.
typedef std::map<std::string, std::tuple<std::string, size_t, size_t, size_t> > ResultMap;

using namespace std;

extern Scenario* scenario;

void AnalyzeContainer::calcFeedbacksBeforePeriod( Scenario* aScenario, const IClimateModel* aClimateModel, const int aPeriod ) {
    // do nothing
}


void AnalyzeContainer::calcFeedbacksAfterPeriod( Scenario* aScenario, const IClimateModel* aClimateModel, const int aPeriod ) {
    // do introspection and report results at the end of a model period in case any new classes get instantiated
    // to run this period
    
    // clear out any previous results
    mContainerStats.clear();

    // search for any CONTAINER type regardless of where it is in the nesting structure
    vector<FilterStep*> collectStateSteps( 2, 0 );
    collectStateSteps[ 0 ] = new FilterStep( "" );
    collectStateSteps[ 1 ] = new FilterStep( "", DataFlags::CONTAINER );
    // only interested in the "data", not how we got there so only processData is set to true
    GCAMFusion<AnalyzeContainer, false, false, true> gatherState( *this, collectStateSteps );
    gatherState.startFilter( aScenario );
    
    // we have finished introspection and collected all the results in mContainerStats
    // now we simply report results
    ILogger& mainLog = ILogger::getLogger( "container_size_log.csv" );
    mainLog.setLevel( ILogger::NOTICE );
    // write in a CSV format (thus try to write the header once) so that we can easily
    // parse it for analysis
    if(aPeriod == 0) {
        mainLog << "period,container,base,size,min_size,num_instances" << endl;
    }
    for(auto instPair : mContainerStats) {
        const string container = instPair.first;
        mainLog << aPeriod << "," << instPair.first << "," << get<0>(instPair.second)
                << "," << get<1>(instPair.second) << "," << get<2>(instPair.second)
                << "," << get<3>(instPair.second) << endl;
    }
    
    // clean up GCAMFusion related memory
    for( auto filterStep : collectStateSteps ) {
        delete filterStep;
    }
}

/*
 * \brief A helper function to merge results
 * \details Given the recursive nature of the way we will be processing
 *          results we will need to continually fold in new results as
 *          we move up the structure.  Merging is straightforward, just
 *          join on the map keys and sum the number of instances (the rest
 *          are the same as there is no way they wouldn't be).
 * \param aSource The output map to aggregate results into.
 * \param aCurr The current set of results that need to get folded in.
 */
void merge(ResultMap& aSource, const ResultMap& aCurr) {
    // loop over all new results in aCurr
    for(auto curr : aCurr) {
        // join on the map key
        auto iter = aSource.find(curr.first);
        if(iter == aSource.end()) {
            // newly discovered class, just copy over results
            aSource[curr.first] = curr.second;
        }
        else {
            // we need to "merge" results which really just means
            // adding the number of class instances
            get<3>((*iter).second) += get<3>(curr.second);
        }
    }
}

/*!
 * \brief A helper class to calculate the "minimum" class size.
 * \details Here we will get a complete data vector including base class member variables
 *          and will calculate the min size as simply summing the `sizeof` each of those
 *          types.
 */
struct CalcMemberDataHelper {
    //! The output of this helper struct which can be checked after callbacks are complete
    size_t mMinSize = 0;
    template<typename DataVectorType>
    void processDataVector(DataVectorType aDataVector) {
        mMinSize = 0;
        boost::fusion::for_each(aDataVector, [this] (auto aData) {
            this->mMinSize += sizeof(typename decltype(aData)::value_type);
        });
    }
};

BOOST_MPL_HAS_XXX_TRAIT_DEF( SubClassFamilyVector );

// some Data types to ignore
ResultMap calc(std::vector<bool>& aData) {
    return ResultMap();
}

ResultMap calc(const Modeltime* & aData) {
    return ResultMap();
}

ResultMap calc(gcamstr& aData) {
    return ResultMap();
}

// we are only checking dynamically allocated memory so we can ignore these
template<typename DataType>
typename boost::enable_if<
    boost::mpl::and_<
        boost::mpl::not_<has_iterator<DataType> >,
        boost::mpl::not_<has_SubClassFamilyVector<typename boost::remove_pointer<DataType>::type> >
    >,
ResultMap>::type calc(DataType& aData) {
    cout << "Here" << typeid(DataType).name();
    return ResultMap();
}

// calculate the size of a single instance of a CONTAINER class
template<typename DataType>
typename boost::enable_if<
    has_SubClassFamilyVector<typename boost::remove_pointer<DataType>::type>,
ResultMap>::type calc(DataType& aData) {
    // if the pointer is null we can just skip
    if(!aData) {
        return ResultMap();
    }
    
    // use the data declaration to infer the base class name
    using data_type = typename boost::remove_pointer<DataType>::type;
    string baseClassName = typeid(data_type).name();
    
    // rely on the expand data vector utility to figure out the actually instantiated
    // subclass type and then loop over the vector summing up the sizeof each element
    ExpandDataVector<typename data_type::SubClassFamilyVector> expander;
    aData->doDataExpansion(expander);
    string dataName;
    size_t dataSize;
    boost::fusion::for_each( expander.mSubClassPtrMap, [&dataName, &dataSize] ( auto& aPair ) {
        // a pair of (data type, instance)
        // the instance is non-null when it is the actually instantiated type
        if(aPair.second) {
            using currType = typename boost::remove_pointer<decltype(aPair.second)>::type;
            // grab the class name
            dataName = typeid(currType).name();
            // and the sizeof that class
            dataSize = sizeof(currType);
        }
    });
    // use a helper struct to handle the call back to grab the data vector and calculate the "min" size
    CalcMemberDataHelper memberDataHelper;
    expander.getFullDataVector(memberDataHelper);
    
    // finally organize it in our ResultMap data structure and return it
    ResultMap ret;
    ret[dataName] = make_tuple(baseClassName, dataSize, memberDataHelper.mMinSize, static_cast<size_t>(1));
    return ret;
}

// calculate the size of a vector of a CONTAINER class
template<typename DataType>
typename boost::enable_if<
    boost::mpl::and_<has_iterator<DataType>, boost::mpl::not_<has_key_type<DataType> > >,
ResultMap>::type calc(DataType& aData) {
    // to process simply loop over the vector and calc each individually
    // merging results into a single result map as we go
    ResultMap ret;
    for(auto iter = aData.begin(); iter != aData.end(); ++iter) {
        ResultMap curr = calc(*iter);
        merge(ret, curr);
    }
    
    // and return the partial results
    return ret;
}

// calculate the size of a map containing instances of a CONTAINER class
template<typename DataType>
typename boost::enable_if<
    has_key_type<DataType>,
ResultMap>::type calc(DataType& aData) {
    // to process simply loop over the map and calc each value individually
    // merging results into a single result map as we go
    ResultMap ret;
    for(auto iter = aData.begin(); iter != aData.end(); ++iter) {
        ResultMap curr = calc((*iter).second);
        merge(ret, curr);
    }
    
    // and return the partial results
    return ret;
}

// GCAMFusion call back which is fired any time a CONTAINER Data is found
template<typename DataType>
void AnalyzeContainer::processData( DataType& aData ) {
    // we will rely on the calc methods to figure out the exact structure
    // holding the instances of CONTAINER data and return a partial result
    ResultMap curr = calc(aData);
    // merge that partial result into our final data structure
    merge(mContainerStats, curr);
}

#endif // DEBUG_STATE
