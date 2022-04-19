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
 * \file analyze_tech_mem.cpp
 * \ingroup util
 * \brief AnalyzeTechMem class source file.
 * \author Pralit Patel
 */

#include <cstring>

#include "containers/include/analyze_tech_mem.hpp"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/gcam_fusion.hpp"
#include "util/base/include/gcam_data_containers.h"

using namespace std;

extern Scenario* scenario;

void AnalyzeTechMem::calcFeedbacksBeforePeriod( Scenario* aScenario, const IClimateModel* aClimateModel, const int aPeriod ) {
    // do nothing
}


void AnalyzeTechMem::calcFeedbacksAfterPeriod( Scenario* aScenario, const IClimateModel* aClimateModel, const int aPeriod ) {
    // do stuff
    mInTech = false;
    mCurrTechLifetme = -1;
    mNumTech = 0;
    mNumInTech = 0;
    mTotalSizeInTech = 0;
    mDeadSizeInTech = 0;
    mNumOutTech = 0;
    mTotalSizeOutTech = 0;
    mNumTechVec = 0;
    mTotalSizeInTechVec = 0;
    mNumLUCArr = 0;
    mTotalSizeLUCArr = 0;
    mNumValueClasses = 0;

    vector<FilterStep*> collectStateSteps( 2, 0 );
    collectStateSteps[ 0 ] = new FilterStep( "" );
    //collectStateSteps[ 1 ] = new FilterStep( "", DataFlags::ARRAY );
    collectStateSteps[ 1 ] = new FilterStep( "", DataFlags::SIMPLE );
    // DoCollect will handle all fusion callbacks thus their template boolean parameter
    // are set to true.
    GCAMFusion<AnalyzeTechMem, true, true, true> gatherState( *this, collectStateSteps );
    gatherState.startFilter( aScenario );
    
    // DoCollect has now gathered all active state into the mStateValues list to
    // allow faster/easier processing for the remaining tasks at hand.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::SEVERE );
    mainLog << "sizeof(double): " << sizeof(double) << ", sizeof(Value): " << sizeof(Value) << endl;
    mainLog << "sizeof(vector<double>): " << sizeof(std::vector<double>) << ", sizeof(vector<Value>): " << sizeof(std::vector<Value>) << endl;
    mainLog << "sizeof(PeriodVector<double>): " << sizeof(objects::PeriodVector<double>) << ", sizeof(PeriodVector<Value>): " << sizeof(objects::PeriodVector<Value>) << endl;
    mainLog << "sizeof(TechVintageVector<double>): " << sizeof(objects::TechVintageVector<double>) << ", sizeof(TechVintageVector<Value>): " << sizeof(objects::TechVintageVector<Value>) << endl;
    mainLog << "Num techs: " << mNumTech << endl;
    mainLog << "Num array in tech: " << mNumInTech << endl;
    mainLog << "Total size in tech: " << mTotalSizeInTech << endl;
    mainLog << "Tech dead space: " << mDeadSizeInTech << endl;
    mainLog << "Num array outside of tech: " << mNumOutTech << endl;
    mainLog << "Total size outside of tech: " << mTotalSizeOutTech << endl;
    mainLog << "Num TechVintageVector arrays: " << mNumTechVec << endl;
    mainLog << "Total size in TechVintageVector: " << mTotalSizeInTechVec << endl;
    mainLog << "Num LUC arrays: " << mNumLUCArr << endl;
    mainLog << "Total size in LUC arrays: " << mTotalSizeLUCArr << endl;
    mainLog << "Num Value classes: " << mNumValueClasses << endl;

    // clean up GCAMFusion related memory
    for( auto filterStep : collectStateSteps ) {
        delete filterStep;
    }
}

template<typename DataType>
void AnalyzeTechMem::processData( DataType& aData ) {
    // ignore
}

template<>
void AnalyzeTechMem::processData<objects::PeriodVector<double> >( objects::PeriodVector<double>& aData ) {
    size_t dataSize = sizeof( aData ) + aData.size() * sizeof(double);
    if(mInTech) {
        ++mNumInTech;
        mTotalSizeInTech += dataSize;
        if( mCurrTechLifetme <= 0 ) {
            cout << "didn't set tech lifetime." << endl;
        } else {
            // TODO: hard coding sizes
            mDeadSizeInTech += (aData.size() - mCurrTechLifetme ) * sizeof(double);
        }
    } else {
        ++mNumOutTech;
        mTotalSizeOutTech += dataSize;
    }
}

template<>
void AnalyzeTechMem::processData<objects::PeriodVector<Value> >( objects::PeriodVector<Value>& aData ) {
    size_t dataSize = sizeof( aData ) + aData.size() * sizeof(Value);
    mNumValueClasses += aData.size();
    if(mInTech) {
        ++mNumInTech;
        mTotalSizeInTech += dataSize;
        if( mCurrTechLifetme <= 0 ) {
            cout << "didn't set tech lifetime." << endl;
        } else {
            // TODO: hard coding sizes
            mDeadSizeInTech += ( aData.size() - mCurrTechLifetme) * sizeof(Value);
        }
    } else {
        ++mNumOutTech;
        mTotalSizeOutTech += dataSize;
    }
}

template<>
void AnalyzeTechMem::processData<std::vector<double> >( std::vector<double>& aData ) {
    size_t dataSize = sizeof( aData ) + aData.size() * sizeof(double);
    if(mInTech) {
        ++mNumInTech;
        mTotalSizeInTech += dataSize;
        if( mCurrTechLifetme <= 0 ) {
            cout << "didn't set tech lifetime." << endl;
        } else {
            // TODO: hard coding sizes
            mDeadSizeInTech += (aData.size() -  mCurrTechLifetme) * sizeof(double);
        }
    } else {
        ++mNumOutTech;
        mTotalSizeOutTech += dataSize;
    }
}

template<>
void AnalyzeTechMem::processData<std::vector<Value> >( std::vector<Value>& aData ) {
    size_t dataSize = sizeof( aData ) + aData.size() * sizeof(Value);
    mNumValueClasses += aData.size();
    if(mInTech) {
        ++mNumInTech;
        mTotalSizeInTech += dataSize;
        if( mCurrTechLifetme <= 0 ) {
            cout << "didn't set tech lifetime." << endl;
        } else {
            // TODO: hard coding sizes
            mDeadSizeInTech += (aData.size() - mCurrTechLifetme ) * sizeof(Value);
        }
    } else {
        ++mNumOutTech;
        mTotalSizeOutTech += dataSize;
    }
}

template<>
void AnalyzeTechMem::processData<objects::TechVintageVector<double> >( objects::TechVintageVector<double>& aData ) {
    size_t dataSize = sizeof( aData ) + aData.size() * sizeof(double);
    ++mNumTechVec;
    mTotalSizeInTechVec += dataSize;
}

template<>
void AnalyzeTechMem::processData<objects::TechVintageVector<Value> >( objects::TechVintageVector<Value>& aData ) {
    size_t dataSize = sizeof( aData ) + aData.size() * sizeof(Value);
    mNumValueClasses += aData.size();
    ++mNumTechVec;
    mTotalSizeInTechVec += dataSize;
}

template<>
void AnalyzeTechMem::processData<objects::PeriodVector<objects::YearVector<Value>*> >( objects::PeriodVector<objects::YearVector<Value>*>& aData ) {
    size_t dataSize = sizeof( aData ) + aData.size() * sizeof(objects::YearVector<Value>*);
    ++mNumLUCArr;
    mTotalSizeLUCArr += dataSize;
    for( int i = 0; i < aData.size(); ++i) {
        if( aData[ i ] ) {
            mNumValueClasses += (*aData[i]).size();
            dataSize = sizeof( *aData[i] ) + (*aData[i]).size() * sizeof(Value);
            ++mNumLUCArr;
            mTotalSizeLUCArr += dataSize;
        }
    }
}

template<>
void AnalyzeTechMem::processData<objects::YearVector<double> >( objects::YearVector<double>& aData ) {
    size_t dataSize = sizeof( aData ) + aData.size() * sizeof(double);
    ++mNumLUCArr;
    mTotalSizeLUCArr += dataSize;
}

template<>
void AnalyzeTechMem::processData<objects::YearVector<Value> >( objects::YearVector<Value>& aData ) {
    size_t dataSize = sizeof( aData ) + aData.size() * sizeof(Value);
    mNumValueClasses += aData.size();
    ++mNumLUCArr;
    mTotalSizeLUCArr += dataSize;
}

template<>
void AnalyzeTechMem::processData<Value>( Value& aData ) {
    ++mNumValueClasses;
}

template<typename DataType>
void AnalyzeTechMem::pushFilterStep( const DataType& aData ) {
    // ignore most steps
}

template<typename DataType>
void AnalyzeTechMem::popFilterStep( const DataType& aData ) {
    // ignore most steps
}

template<>
void AnalyzeTechMem::pushFilterStep<ITechnology*>( ITechnology* const& aData ) {
    mInTech = true;
    ++mNumTech;
    const Modeltime* modeltime = scenario->getModeltime();
    mCurrTechLifetme = 0;
    int currPer = modeltime->getyr_to_per( aData->getYear() );
    for( int year = aData->getYear(); currPer < modeltime->getmaxper() && year < (aData->getYear() + /*aData->getLifetimeYears()*/5 ); ) {
        ++mCurrTechLifetme;
        ++currPer;
        if( currPer < modeltime->getmaxper() ) {
            year = modeltime->getper_to_yr( currPer );
        }
    }
}

/*template<>
void AnalyzeTechMem::pushFilterStep<ICarbonCalc*>( ICarbonCalc* const& aData ) {
    size_t dataSize = sizeof( vector<double> );
    if( aData->getMatureAge() > 1 ) {
        dataSize += ( CarbonModelUtils::getEndYear() - CarbonModelUtils::getStartYear() + 1 ) * sizeof(double);
    }
    ++mNumLUCArr;
    mTotalSizeLUCArr += dataSize;
}*/

template<>
void AnalyzeTechMem::popFilterStep<ITechnology*>( ITechnology* const& aData ) {
    mInTech = false;
    mCurrTechLifetme = -1;
}

