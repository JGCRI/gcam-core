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

#include "../include/get_data_helper.h"
#include "../include/remap_data.h"
#include "util/base/include/gcam_fusion.hpp"
#include "util/base/include/gcam_data_containers.h"

using namespace std;

class MatchesAny : public AMatchesValue {
public:
    virtual bool matchesString( const std::string& aStrToTest ) const {
        return true;
    }
    virtual bool matchesInt( const int aIntToTest ) const {
        return true;
    }
};

class AMatcherWrapper : public AMatchesValue {
public:
    AMatcherWrapper(AMatchesValue* aToWrap):mToWrap(aToWrap)
    {
    }
    virtual ~AMatcherWrapper() {
        delete mToWrap;
    }
    virtual void recordPath() = 0;

protected:
    AMatchesValue* mToWrap;
};

class StrMatcherWrapper : public AMatcherWrapper {
public:
  StrMatcherWrapper(AMatchesValue* aToWrap, std::vector<std::string>& aCurrRowValue, const size_t aIndex):AMatcherWrapper(aToWrap), mCurrRowValue( aCurrRowValue ), mIndex( aIndex )
  {
  }
    virtual bool matchesString( const std::string& aStrToTest ) const {
        bool matches = mToWrap->matchesString(aStrToTest);
        if(matches) {
          const_cast<StrMatcherWrapper*>(this)->mCurrValue = aStrToTest;
        }
        return matches;
    }
    virtual void recordPath() {
        mCurrRowValue[mIndex] = mCurrValue;
    }
    private:
    string mCurrValue;
    std::vector<string>& mCurrRowValue;
    size_t mIndex;
};

class IntMatcherWrapper : public AMatcherWrapper {
public:
  IntMatcherWrapper(AMatchesValue* aToWrap, int& aCurrRowValue):AMatcherWrapper(aToWrap), mCurrRowValue(aCurrRowValue)
  {
  }
    virtual bool matchesInt( const int aIntToTest ) const {
        bool matches = mToWrap->matchesInt(aIntToTest);
        if(matches) {
            const_cast<IntMatcherWrapper*>(this)->mCurrValue = aIntToTest;
        }
        return matches;
    }
    virtual void recordPath() {
        mCurrRowValue = mCurrValue;
    }
    
    private:
    int mCurrValue;
    int& mCurrRowValue;
};

void GetDataHelper::run(Scenario* aScenario) {
  GCAMFusion<GetDataHelper> fusion(*this, mFilterSteps);
  fusion.startFilter(aScenario);
}

GetDataHelper::~GetDataHelper() {
  // note mPathTracker's memory is managed by mFilterSteps
  for(auto step : mFilterSteps) {
    delete step;
  }
}

template<>
void GetDataHelper::processData(double& aData) {
    mCurrDataValue = aData;
    for(auto path: mPathTracker) {
        path->recordPath();
    }
    mDataMapper.setData(mCurrColValues, mCurrYearValue, mCurrDataValue);
}
template<>
void GetDataHelper::processData(Value& aData) {
    mCurrDataValue = aData;
    for(auto path: mPathTracker) {
        path->recordPath();
    }
    mDataMapper.setData(mCurrColValues, mCurrYearValue, mCurrDataValue);
}
template<>
void GetDataHelper::processData(int& aData) {
    mCurrDataValue = aData;
    for(auto path: mPathTracker) {
        path->recordPath();
    }
    mDataMapper.setData(mCurrColValues, mCurrYearValue, mCurrDataValue);
}
template<>
void GetDataHelper::processData(std::vector<int>& aData) {
    vectorDataHelper(aData);
}
template<>
void GetDataHelper::processData(std::vector<double>& aData) {
    vectorDataHelper(aData);
}
template<>
void GetDataHelper::processData(std::vector<Value>& aData) {
    vectorDataHelper(aData);
}
template<>
void GetDataHelper::processData(objects::PeriodVector<int>& aData) {
    vectorDataHelper(aData);
}
template<>
void GetDataHelper::processData(objects::PeriodVector<double>& aData) {
    vectorDataHelper(aData);
}
template<>
void GetDataHelper::processData(objects::PeriodVector<Value>& aData) {
    vectorDataHelper(aData);
}
template<>
void GetDataHelper::processData(objects::TechVintageVector<int>& aData) {
  vectorDataHelper(aData);
}
template<>
void GetDataHelper::processData(objects::TechVintageVector<double>& aData) {
  vectorDataHelper(aData);
}
template<>
void GetDataHelper::processData(objects::TechVintageVector<Value>& aData) {
  vectorDataHelper(aData);
}
template<typename VecType>
void GetDataHelper::vectorDataHelper(VecType& aDataVec) {
    for(auto iter = aDataVec.begin(); iter != aDataVec.end(); ++iter) {
        mCurrYearValue = (GetIndexAsYear::convertIterToYear(aDataVec, iter));
        processData(*iter);
    }
}

template<typename T>
void GetDataHelper::processData(T& aData) {
  // TODO: what is error handling strategy?
  abort();
}

FilterStep* GetDataHelper::parseFilterStepStr( const std::string& aFilterStepStr, int& aCol ) {
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
    std::vector<std::string> filterOptions;
    boost::split( filterOptions, filterStr, boost::is_any_of( "," ) );
    // [0] = filter type (name, year, index)
    // [1] = match type
    // [2:] = match type options
    AMatchesValue* matcher = 0;
        if( filterOptions[ 1 ] == "StringEquals" ) {
            matcher = new StringEquals( filterOptions[ 2 ] );
        }
        else if( filterOptions[ 1 ] == "StringRegexMatches" ) {
            matcher = new StringRegexMatches( filterOptions[ 2 ] );
        }
        else if( filterOptions[ 1 ] == "IntEquals" ) {
            matcher = new IntEquals( boost::lexical_cast<int>( filterOptions[ 2 ] ) );
        }
        else if( filterOptions[ 1 ] == "IntGreaterThan" ) {
            matcher = new IntGreaterThan( boost::lexical_cast<int>( filterOptions[ 2 ] ) );
        }
        else if( filterOptions[ 1 ] == "IntGreaterThanEq" ) {
            matcher = new IntGreaterThanEq( boost::lexical_cast<int>( filterOptions[ 2 ] ) );
        }
        else if( filterOptions[ 1 ] == "IntLessThan" ) {
            matcher = new IntLessThan( boost::lexical_cast<int>( filterOptions[ 2 ] ) );
        }
        else if( filterOptions[ 1 ] == "IntLessThanEq" ) {
            matcher = new IntLessThanEq( boost::lexical_cast<int>( filterOptions[ 2 ] ) );
        }
        else if( filterOptions[ 1 ] == "MatchesAny" ) {
            matcher = new MatchesAny();
        }
    else {
      ILogger& mainLog = ILogger::getLogger( "main_log" );
      mainLog.setLevel( ILogger::WARNING );
      mainLog << "Unknown subclass of AMatchesValue: " << filterStr << std::endl;
    }

    FilterStep* filterStep = 0;
        if( filterOptions[ 0 ] == "IndexFilter" ) {
            if(isRead) {
                AMatcherWrapper* wrap = new IntMatcherWrapper(matcher, mCurrYearValue);
                mPathTracker.push_back(wrap);
                mColNames.push_back("index");
                matcher = wrap;
            }

            filterStep = new FilterStep( dataName, new IndexFilter( matcher ) );
        }
        else if( filterOptions[ 0 ] == "NamedFilter" ) {
            if(isRead) {
                mCurrColValues.push_back("");
                AMatcherWrapper* wrap = new StrMatcherWrapper(matcher, mCurrColValues, mCurrColValues.size() -1);
                mColNames.push_back(dataName);
                mPathTracker.push_back(wrap);
                matcher = wrap;
            }

            filterStep = new FilterStep( dataName, new NamedFilter( matcher ) );
        }
        else if( filterOptions[ 0 ] == "YearFilter" ) {
            if(isRead) {
                AMatcherWrapper* wrap = new IntMatcherWrapper(matcher, mCurrYearValue);
                mColNames.push_back("year");
                mPathTracker.push_back(wrap);
                matcher = wrap;
            }

            filterStep = new FilterStep( dataName, new YearFilter( matcher ) );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unknown filter: " << filterOptions[ 0 ] << std::endl;
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
void GetDataHelper::parseFilterString(const std::string& aFilterStr ) {
  std::vector<std::string> filterStepsStr;
  boost::split( filterStepsStr, aFilterStr, boost::is_any_of( "/" ) );
  std::vector<FilterStep*> filterSteps( filterStepsStr.size() );
  mFilterSteps.resize(filterStepsStr.size());
  int col = 0;
  for( size_t i = 0; i < filterStepsStr.size(); ++i ) {
    mFilterSteps[ i ] = parseFilterStepStr( filterStepsStr[ i ], col );
  }
  mColNames.push_back(mFilterSteps.back()->mDataName);
}


