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
* \file calc_base_price.cpp
* \ingroup Objects
* \brief CalcBasePrice class source file.
* \author Pralit Patel
*/

#include "util/base/include/definitions.h"
#include <cassert>

#include "containers/include/calc_base_price.h"
#include "util/base/include/gcam_fusion.hpp"
#include "util/base/include/gcam_data_containers.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_parse_helper.h"
#include "util/logger/include/ilogger.h"

using namespace std;

extern Scenario* scenario;

CalcBasePrice::CalcBasePrice()
{
}

CalcBasePrice::~CalcBasePrice() {
}

const string& CalcBasePrice::getXMLNameStatic() {
    // This is the string you will use to refer to this object
	// in input files.
    const static string XML_NAME = "calc-base-price";
    return XML_NAME;
}

const string& CalcBasePrice::getXMLName() const {
    return getXMLNameStatic();
}

const string& CalcBasePrice::getName() const {
    return mName;
}

bool CalcBasePrice::XMLParse( rapidxml::xml_node<char>* & aNode ) {
    for( ; aNode; aNode = aNode->next_sibling() ) {
        if( aNode->type() == rapidxml::node_element) {
            string nodeName = XMLParseHelper::getNodeName(aNode);
            if( nodeName == "sector-map" ) {
                map<string, string> attrs = XMLParseHelper::getAllAttrs(aNode);
                string fromSector = attrs["from-name"];
                string toSector = attrs["to-name"];
                mSectorNameMap[ fromSector ] = toSector;
            }
        }
    }
    return true;
}

void CalcBasePrice::calcFeedbacksAfterPeriod( Scenario* aScenario, 
                                                   const IClimateModel* aClimateModel,
                                                   const int aPeriod )
{
    // we can calculate this during calibration periods, however it is only going to be
    // used in future model periods and ignored otherwise, therefore only do it for the
    // final calibration period
    const Modeltime* modeltime = aScenario->getModeltime();
    if(modeltime->getFinalCalibrationPeriod() == aPeriod) {
        mPeriod = aPeriod;
        string finalCalYear = util::toString( modeltime->getper_to_yr( aPeriod ) );
        // search for any currency output which is only calculated for sectors
        // that we track for macro purposes
        vector<FilterStep*> query = parseFilterString( "world/region/sector/subsector/technology/period[YearFilter,IntEquals,"+
            finalCalYear+"]/output/currency-output" );
        GCAMFusion<CalcBasePrice, true, true, true> setPrices( *this, query );
        setPrices.startFilter( aScenario );
        for( FilterStep* step : query ) {
            delete step;
        }
    }
}

template<typename ContainerType>
void CalcBasePrice::pushFilterStep( const ContainerType& aData ) {
    // ignore most types
}

template<typename ContainerType>
void CalcBasePrice::popFilterStep( const ContainerType& aData ) {
    // ignore most types
}

template<>
void CalcBasePrice::pushFilterStep<Region*>( Region* const & aData ) {
    // stash the current region as we process down the nesting structure
    mCurrRegionName = aData->getName();
}

template<>
void CalcBasePrice::popFilterStep<Region*>( Region* const & aData ) {
    // we will want to set the same base-price into all the GCAM sectors which were mapped to an
    // aggregate sector
    for( auto mappedSectorIter = mSectorNameMap.begin(); mappedSectorIter != mSectorNameMap.end(); ++mappedSectorIter ) {
        // some error checking to see if a user parsed a sector to aggregate but GCAMFusion didn't
        // actually find any data for it
        auto mappedSectorValueIter = mMappedSectorOutputs.find( (*mappedSectorIter).second );
        if( mappedSectorValueIter == mMappedSectorOutputs.end() ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Warning couldn't find values for: " << (*mappedSectorIter).second  << "in " << mCurrRegionName << endl;
        }
        else {
            IInfo* marketInfo = scenario->getMarketplace()->getMarketInfo( (*mappedSectorIter).first, mCurrRegionName, mPeriod, false );
            if( marketInfo ) {
                // store the base price which may be used for accounting purposes later
                // note we need to ensure this price does not include time-value
                marketInfo->setDouble( "base-price", (*mappedSectorValueIter).second.second == 0.0 ? -1.0 : (*mappedSectorValueIter).second.first / (*mappedSectorValueIter).second.second );
            }
        }
    }
    // clear the current values to get ready for the next region
    mCurrRegionName = "";
    mMappedSectorOutputs.clear();
}

template<>
void CalcBasePrice::pushFilterStep<Sector*>( Sector* const & aData ) {
    // stash the current sector as we process down the nesting structure
    mCurrSectorName = aData->getName();
}

template<>
void CalcBasePrice::popFilterStep<Sector*>( Sector* const & aData ) {
    // we found tracking information in this sector we can either calculate and store
    // the base-price now if this sector does not need to map to an aggregate sector
    // to just aggregate and wait to calculate the base-price when we are done processing
    // the region
    if(mDidSetValue) {
        auto mapSectorIter = mSectorNameMap.find( mCurrSectorName );
        if( mapSectorIter == mSectorNameMap.end() ) {
            IInfo* marketInfo = scenario->getMarketplace()->getMarketInfo( mCurrSectorName, mCurrRegionName, mPeriod, true );
            if( marketInfo ) {
                // store the base price which may be used for accounting purposes later
                // note we need to ensure this price does not include time-value
                marketInfo->setDouble( "base-price", mPhysicalOutput == 0.0 ? -1.0 : mCurrencyOutput / mPhysicalOutput );
            }
        }
        else {
            // pass through sector, we will need to just save it for now and update the marketplace
            // at the region level where we are sure we have gotten all the contained sectors
            auto currValueIter = mMappedSectorOutputs.find( (*mapSectorIter).second );
            if( currValueIter == mMappedSectorOutputs.end() ) {
                mMappedSectorOutputs[ (*mapSectorIter).second ] = make_pair( mCurrencyOutput, mPhysicalOutput );
            }
            else {
                (*currValueIter).second.first += mCurrencyOutput;
                (*currValueIter).second.second += mPhysicalOutput;
            }
        }
    }
    // clear all tracking information for this sector
    mCurrSectorName = "";
    mDidSetValue = false;
    mCurrencyOutput = 0.0;
    mPhysicalOutput = 0.0;
}

template<>
void CalcBasePrice::pushFilterStep<ITechnology*>( ITechnology* const & aData ) {
    // stash the current tech as we process down the nesting structure
    mCurrTech = aData;
}

template<>
void CalcBasePrice::popFilterStep<ITechnology*>( ITechnology* const & aData ) {
    // clear the current tech
    mCurrTech = 0;
}

template<typename DataType>
void CalcBasePrice::processData( DataType& aData ) {
    // ignore most types
}

template<>
void CalcBasePrice::processData<objects::TechVintageVector<Value> >( objects::TechVintageVector<Value>& aData ) {
    // we found a currency output data, stash the values
    mDidSetValue = true;
    mCurrencyOutput += aData[ mPeriod ];
    mPhysicalOutput += mCurrTech->getOutput( mPeriod );
}

