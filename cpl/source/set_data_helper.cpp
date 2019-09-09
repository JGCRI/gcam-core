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
#include "../include/set_data_helper.h"

#include "util/base/include/gcam_fusion.hpp"
#include "util/base/include/gcam_data_containers.h"

using namespace std;

class StringVecEquals : public AMatchesValue {
public:
    StringVecEquals( const vector<string>& aStr, size_t& row ):mStr( aStr ), mRow( row ) {}
    virtual ~StringVecEquals() {}
    virtual bool matchesString( const std::string& aStrToTest ) const {
        return mStr[mRow] == aStrToTest;
    }
private:
    const vector<string>& mStr;
    size_t& mRow;
};

class IntVecEquals : public AMatchesValue {
public:
    IntVecEquals( const vector<int>& aInt, size_t& row ):mInt( aInt ), mRow( row ) {}
    virtual ~IntVecEquals() {}
    virtual bool matchesInt( const int aIntToTest ) const {
        return mInt[mRow] == aIntToTest;
    }
private:
    const vector<int>& mInt;
    size_t& mRow;
};

void SetDataHelper::run(Scenario* aScenario) {
  GCAMFusion<SetDataHelper> fusion(*this, mFilterSteps);
    mRow = 0;
    for(auto row : mDataVector) {
        fusion.startFilter(aScenario);
        mRow++;
    }
}

SetDataHelper::~SetDataHelper() {
  for(auto step : mFilterSteps) {
    delete step;
  }
}

template<>
void SetDataHelper::processData(double& aData) {
    aData = mDataVector[mRow];
}
template<>
void SetDataHelper::processData(Value& aData) {
    aData = mDataVector[mRow];
}
template<>
void SetDataHelper::processData(int& aData) {
    aData = mDataVector[mRow];
}
template<typename T>
void SetDataHelper::processData(T& aData) {
    // TODO: what is error handling strategy?
    abort();
}

FilterStep* SetDataHelper::parseFilterStepStr( const std::string& aFilterStepStr, int& aCol ) {
    auto openBracketIter = std::find( aFilterStepStr.begin(), aFilterStepStr.end(), '[' );
    if( openBracketIter == aFilterStepStr.end() ) {
        // no filter just the data name
        return new FilterStep( aFilterStepStr );
    }
    else {
        std::string dataName( aFilterStepStr.begin(), openBracketIter );
        bool isRead = *(openBracketIter + 1) == '+';
        int filterOffset = isRead ? 2 : 1;
        std::string filterStr( openBracketIter + filterOffset, std::find( openBracketIter, aFilterStepStr.end(), ']' ) );
        
        AMatchesValue* matcher = 0;
        FilterStep* filterStep = 0;
        if( filterStr == "name" && aCol == 0) {
            matcher = new StringVecEquals( mRegionColumn, mRow );
            filterStep = new FilterStep( dataName, new NamedFilter( matcher ) );
        }
        else if( filterStr == "name" && aCol == 1) {
            matcher = new StringVecEquals( mLandTechColumn, mRow );
            filterStep = new FilterStep( dataName, new NamedFilter( matcher ) );
        }
        else if( filterStr == "year" ) {
            matcher = new IntVecEquals( mYearColumn, mRow );
            filterStep = new FilterStep( dataName, new YearFilter( matcher ) );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unknown subclass of AMatchesValue: " << filterStr << std::endl;
        }
        
        if(isRead) {
            ++aCol;
        }
        return filterStep;
    }
}

/*!
 * \brief Parse a string to create a list of FilterSteps.
 * \details The string is split on the '/' character so that the contents of each is
 *          assumed to be one FilterStep definition.  Each split string is therefore
 *          parsed further using the helper function parseFilterStepStr.
 * \param aFilterStr A string representing a series of FilterSteps.
 * \return A list of FilterSteps parsed from aFilterStr as detailed above.
 */
std::vector<FilterStep*> SetDataHelper::parseFilterString(const std::string& aFilterStr ) {
    std::vector<std::string> filterStepsStr;
    boost::split( filterStepsStr, aFilterStr, boost::is_any_of( "/" ) );
    std::vector<FilterStep*> filterSteps( filterStepsStr.size() );
    int col = 0;
    for( size_t i = 0; i < filterStepsStr.size(); ++i ) {
        filterSteps[ i ] = parseFilterStepStr( filterStepsStr[ i ], col );
    }
    return filterSteps;
}

