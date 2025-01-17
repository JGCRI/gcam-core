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

typedef std::map<std::string, std::tuple<std::string, size_t, size_t, size_t> > ResultMap;

using namespace std;

extern Scenario* scenario;

void AnalyzeContainer::calcFeedbacksBeforePeriod( Scenario* aScenario, const IClimateModel* aClimateModel, const int aPeriod ) {
    // do nothing
}


void AnalyzeContainer::calcFeedbacksAfterPeriod( Scenario* aScenario, const IClimateModel* aClimateModel, const int aPeriod ) {
    // do stuff
    /*mContainerCount.clear();
    mContainerSize.clear();
    mContainerMinSize.clear();*/
    mContainerStats.clear();

    vector<FilterStep*> collectStateSteps( 2, 0 );
    collectStateSteps[ 0 ] = new FilterStep( "" );
    collectStateSteps[ 1 ] = new FilterStep( "", DataFlags::CONTAINER );
    // DoCollect will handle all fusion callbacks thus their template boolean parameter
    // are set to true.
    GCAMFusion<AnalyzeContainer, false, false, true> gatherState( *this, collectStateSteps );
    gatherState.startFilter( aScenario );
    
    // DoCollect has now gathered all active state into the mStateValues list to
    // allow faster/easier processing for the remaining tasks at hand.
    ILogger& mainLog = ILogger::getLogger( "size_log" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Period: " << aPeriod << endl;
    mainLog << "Container,Base,size,min_size,num_instances," << endl;
    for(auto instPair : mContainerStats) {
        const string container = instPair.first;
        mainLog << instPair.first << "," << get<0>(instPair.second) << "," << get<1>(instPair.second) << "," << get<2>(instPair.second) << "," << get<3>(instPair.second) << endl;
    }
    
    // clean up GCAMFusion related memory
    for( auto filterStep : collectStateSteps ) {
        delete filterStep;
    }
}

void merge(ResultMap& aSource, const ResultMap& aCurr) {
    for(auto curr : aCurr) {
        auto iter = aSource.find(curr.first);
        if(iter == aSource.end()) {
            aSource[curr.first] = curr.second;
        }
        else {
            get<3>((*iter).second) += get<3>(curr.second);
        }
    }
}

struct Aasdf {
    size_t mMinSize;
    template<typename DataVectorType>
    void processDataVector(DataVectorType aDataVector) {
        mMinSize = 0;
        boost::fusion::for_each(aDataVector, [this] (auto aData) {
            this->mMinSize += sizeof(typename decltype(aData)::value_type);
        });
    }
};

BOOST_MPL_HAS_XXX_TRAIT_DEF( SubClassFamilyVector );

ResultMap calc(std::vector<bool>& aData) {
    return ResultMap();
}

ResultMap calc(const Modeltime* & aData) {
    return ResultMap();
}

ResultMap calc(gcamstr& aData) {
    return ResultMap();
}

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

template<typename DataType>
typename boost::enable_if<
    has_SubClassFamilyVector<typename boost::remove_pointer<DataType>::type>,
ResultMap>::type calc(DataType& aData) {
    if(!aData) {
        return ResultMap();
    }
    using data_type = typename boost::remove_pointer<DataType>::type;
    string baseClassName = typeid(data_type).name();
    ExpandDataVector<typename data_type::SubClassFamilyVector> expander;
    aData->doDataExpansion(expander);
    string dataName;
    size_t dataSize;
    boost::fusion::for_each( expander.mSubClassPtrMap, [&dataName, &dataSize] ( auto& aPair ) {
        if(aPair.second) {
            using currType = typename boost::remove_pointer<decltype(aPair.second)>::type;
            dataName = typeid(currType).name();
            dataSize = sizeof(currType);
        }
    });
    Aasdf aasdf;
    expander.getFullDataVector(aasdf);
    ResultMap ret;
    ret[dataName] = make_tuple(baseClassName, dataSize, aasdf.mMinSize, static_cast<size_t>(1));
    return ret;
}

template<typename DataType>
typename boost::enable_if<
    boost::mpl::and_<has_iterator<DataType>, boost::mpl::not_<has_key_type<DataType> > >,
ResultMap>::type calc(DataType& aData) {
    ResultMap ret;
    for(auto iter = aData.begin(); iter != aData.end(); ++iter) {
        ResultMap curr = calc(*iter);
        merge(ret, curr);
    }
    return ret;
}

template<typename DataType>
typename boost::enable_if<
    has_key_type<DataType>,
ResultMap>::type calc(DataType& aData) {
    ResultMap ret;
    for(auto iter = aData.begin(); iter != aData.end(); ++iter) {
        ResultMap curr = calc((*iter).second);
        merge(ret, curr);
    }
    return ret;
}

/*template<typename DataType, typename Enable = void>
struct GetActualContainerType;

template<typename DataType>
struct GetActualContainerType<DataType, typename boost::disable_if<
    has_iterator<DataType>
>::type> {
    using data_type = typename boost::remove_pointer<DataType>::type;
    size_t mDataSize;
    std::string mDataName;
    std::string mBaseClassName;
    size_t mNumInst;
    GetActualContainerType(DataType& aData) {
        mBaseClassName = typeid(data_type).name();
        mNumInst = 1;
    }
    static constexpr size_t getDataSize() {
        return sizeof(data_type);
    }
    
    static constexpr auto getTypeName() {
        return typeid(data_type).name();
    }
    
    static size_t getNumInstances( DataType& aData ) {
        return 1;
    }
};

template<typename DataType>
struct GetActualContainerType<DataType, typename boost::enable_if<
    boost::mpl::and_<has_iterator<DataType>, boost::mpl::not_<has_key_type<DataType> > >
>::type> {
    using data_type = typename boost::remove_pointer<typename DataType::value_type>::type;
    static constexpr size_t getDataSize() {
        return sizeof(data_type);
    }
    
    static constexpr auto getTypeName() {
        return typeid(data_type).name();
    }
    
    static size_t getNumInstances( DataType& aData ) {
        return aData.size();
    }
};

template<typename DataType>
struct GetActualContainerType<DataType, typename boost::enable_if<
    has_key_type<DataType>
>::type> {
    using data_type = typename boost::remove_pointer<typename DataType::mapped_type>::type;
    static constexpr size_t getDataSize() {
        return sizeof(data_type);
    }
    
    static constexpr auto getTypeName() {
        return typeid(data_type).name();
    }
    
    static size_t getNumInstances( DataType& aData ) {
        return aData.size();
    }
};*/

template<typename DataType>
void AnalyzeContainer::processData( DataType& aData ) {
    /*const string data_name = GetActualContainerType<DataType>::getTypeName();
    size_t data_size = GetActualContainerType<DataType>::getDataSize();
    size_t numInst = GetActualContainerType<DataType>::getNumInstances(aData);
    auto iter = mContainerCount.find(data_name);
    if(iter == mContainerCount.end()) {
        mContainerCount[data_name] = numInst;
        mContainerSize[data_name] = data_size;
    } else {
        (*iter).second += numInst;
    }*/
    ResultMap curr = calc(aData);
    merge(mContainerStats, curr);
}

