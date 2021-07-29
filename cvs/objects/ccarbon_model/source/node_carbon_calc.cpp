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
 * \file node_carbon_carbon_calc.cpp
 * \ingroup Objects
 * \brief NodeCarbonCalc class source file.
 * \author Pralit Patel
 */

#include "util/base/include/definitions.h"
#include <cassert>

#include "ccarbon_model/include/node_carbon_calc.h"
#include "ccarbon_model/include/no_emiss_carbon_calc.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include "util/base/include/util.h"
#include "util/base/include/xml_helper.h"
#include "land_allocator/include/land_use_history.h"
#include "land_allocator/include/land_leaf.h"
#include "util/logger/include/ilogger.h"

using namespace std;
using namespace objects;

extern Scenario* scenario;

//! Default constructor
NodeCarbonCalc::NodeCarbonCalc():
mHasCalculatedHistoricEmiss( false )
{
}

//! Default destructor
NodeCarbonCalc::~NodeCarbonCalc() {
    // this class does not own the memory of the contained carbon calcs objects.
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
 *
 * This public function accesses the private constant string, XML_NAME.
 * This way the tag is always consistent for both read-in and output and can be easily changed.
 * The "==" operator that is used when parsing, required this second function to return static.
 * \note A function cannot be static and virtual.
 * \author James Blackwood
 * \return The constant XML_NAME as a static.
 */
const string& NodeCarbonCalc::getXMLNameStatic() {
    const static string XML_NAME = "node-carbon-calc";
    return XML_NAME;
}

void NodeCarbonCalc::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void NodeCarbonCalc::startVisitNoEmissCarbonCalc( const NoEmissCarbonCalc* aNoEmissCarbonCalc, const int aPeriod ) {
    mCarbonCalcs.push_back( const_cast<NoEmissCarbonCalc*>( aNoEmissCarbonCalc ) );
}

/*!
 * \brief Complete any remaining initializations.
 */
void NodeCarbonCalc::completeInit() {
    // Create indicies for the carbon calcs in terms of smallest to largest carbon
    // densities and vice versa.
    const int startYear = scenario->getModeltime()->getStartYear();
    for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
        vector<size_t>::iterator insertIt = mIndLowToHigh.begin();
        while( insertIt < mIndLowToHigh.end() && mCarbonCalcs[ *insertIt ]->getActualAboveGroundCarbonDensity( startYear )
               < mCarbonCalcs[ i ]->getActualAboveGroundCarbonDensity( startYear ) )
        {
            ++insertIt;
        }
        mIndLowToHigh.insert( insertIt, i );
    }
    
    mIndHighToLow = mIndLowToHigh;
    reverse( mIndHighToLow.begin(), mIndHighToLow.end() );
}

/*!
 * \brief Initializations prior to calculating the given period.
 * \details Back out the previsouly calculated emissions from the saved future
 *          values in case we are re-running this model period.
 */
void NodeCarbonCalc::initCalc( const int aPeriod ) {
    bool shouldReverseCalc = false;
    for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
        shouldReverseCalc |= mCarbonCalcs[ i ]->shouldReverseCalc( aPeriod );
    }
    
    if( aPeriod > 0 && shouldReverseCalc ) {
        calc( aPeriod, CarbonModelUtils::getEndYear(), ICarbonCalc::eReverseCalc );
    }
}

/*!
 * \brief Do the carbon calculations for the historical period.
 * \note While these calculations are very similar to those of model year there are
 *       slight differences.  Some of this code could be pulled into a common function
 *       however that would sacrifice performance in an already time consuming section
 *       of code.
 */
void NodeCarbonCalc::calcLandUseHistory()
{
    if( mHasCalculatedHistoricEmiss ) {
        /*!
         * \warning Land-use history emissions can only be calculated once regardless
         *          of how many times the model will be run.
         */
        return;
    }
    
    // stash carbon densities for quick access
    vector<double> aboveGroundCarbonDensity( mCarbonCalcs.size() );
    vector<double> belowGroundCarbonDensity( mCarbonCalcs.size() );
    for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
        aboveGroundCarbonDensity[ i ] = mCarbonCalcs[ i ]->mLandUseHistory->getHistoricAboveGroundCarbonDensity();
        belowGroundCarbonDensity[ i ] = mCarbonCalcs[ i ]->mLandUseHistory->getHistoricBelowGroundCarbonDensity();
    }

    vector<double> prevLand( mCarbonCalcs.size() );
    vector<double> currLand( mCarbonCalcs.size() );
    vector<double> diffLand( mCarbonCalcs.size() );
    vector<double> carbonStock( mCarbonCalcs.size() );
    double prevLandTotal = 0;
    for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
        double land = mCarbonCalcs[ i ]->mLandUseHistory->getAllocation( CarbonModelUtils::getStartYear() - 1 );
        prevLand[ i ] = land;
        prevLandTotal += land;
        carbonStock[ i ] = land * aboveGroundCarbonDensity[ i ];
    }
    // Calculated emissions over the entire historical period.
    for( int year = CarbonModelUtils::getStartYear(); year <= mCarbonCalcs[ 0 ]->mLandUseHistory->getMaxYear(); ++year ) {
        // compute changes in land area
        double currLandTotal = 0;
        for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
            double land = mCarbonCalcs[ i ]->mLandUseHistory->getAllocation( year );
            currLand[ i ] = land;
            currLandTotal += land;
            diffLand[ i ] = land - prevLand[ i ];
        }
        double diffLandTotal = currLandTotal - prevLandTotal;

        // First allocate changes in total land area preferentially adding land to the
        // most carbon dense land first when the total increase and conversely removing
        // land from the least carbon dense lands first when it decreases.
        vector<size_t>::const_iterator cdenIt = diffLandTotal > 0 ? mIndHighToLow.begin() : mIndLowToHigh.begin();
        vector<size_t>::const_iterator cdenItEnd = diffLandTotal > 0 ? mIndHighToLow.end() : mIndLowToHigh.end();
        for( ; cdenIt < cdenItEnd && diffLandTotal != 0; ++cdenIt ) {
            size_t i = *cdenIt;
            if( hasSameSign( diffLandTotal, diffLand[ i ] ) ) {
                double newDiff = abs( diffLand[ i ] ) < abs( diffLandTotal ) ? diffLand[ i ] : diffLandTotal;
                diffLand[ i ] -= newDiff;
                diffLandTotal -= newDiff;
                assert( diffLand[ i ] == 0 || diffLandTotal == 0 );
                mCarbonCalcs[ i ]->calcAboveGroundCarbonEmission( carbonStock[ i ], prevLand[ i ], prevLand[ i ] + newDiff,
                                                                  aboveGroundCarbonDensity[ i ], year,
                                                                  CarbonModelUtils::getEndYear(), mCarbonCalcs[ i ]->mTotalEmissionsAbove );
                mCarbonCalcs[ i ]->calcBelowGroundCarbonEmission( -1 * newDiff * belowGroundCarbonDensity[ i ], year,
                                                                  CarbonModelUtils::getEndYear(), mCarbonCalcs[ i ]->mTotalEmissionsBelow );
            }
            // Adjust carbon stock for any emissions that occurred from this change.
            carbonStock[ i ] -= mCarbonCalcs[ i ]->mTotalEmissionsAbove[ year ];
        }
        // The difference in total land area change should have all been allocated
        // across the various land types.
        assert( util::isEqual( diffLandTotal, 0.0 ) );

        // Now the differences in land area are all between the land options with in 
        // this node carbon calc.  We must then allocate the change in carbon between
        // those options.
        double carbonPrevBelow = 0;
        double carbonStockMoved = 0;
        double totalLandGain = 0;
        double landCheck = 0;
        for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
            if( diffLand[ i ] > 0 ) {
                totalLandGain += diffLand[ i ];
            }
            else if( diffLand[ i ] == 0 ) {
                // Explicit check where the land did not change since if prevLand
                // was also zero we may generate NaN.
            }
            else {
                // Calculate the carbon that will be moved out of this land type by
                // computing the average from the current carbon stock.
                double currCarbonStockMoved = -1 * carbonStock[ i ] * diffLand[ i ] / prevLand[ i ];
                carbonStockMoved += currCarbonStockMoved;
                // Remove the carbon that is changing land type from the carbon stock without
                // emissions.
                carbonStock[ i ] -= currCarbonStockMoved;
                carbonPrevBelow -= diffLand[ i ] * belowGroundCarbonDensity[ i ];
            }
            landCheck += diffLand[ i ];
        }
        // Some land options increase in size and some decrease but in total the
        // change in land should be zero at this point.
        assert( util::isEqual( landCheck, 0.0 ) );

        // Allocate the change in carbon.  We do this by assigning it to the
        // options that increased in land.
        for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
            if( diffLand[ i ] > 0 ) {
                double emissBeforeMove = mCarbonCalcs[ i ]->mTotalEmissionsAbove[ year ];
                // Calculate the difference in carbon densities which would drive any
                // emissions or uptake.
                double fractionOfGain = diffLand[ i ] / totalLandGain;
                double currCarbonMove = fractionOfGain * carbonStockMoved;
                double carbonDiffAboveDensity = -1 * ( currCarbonMove / diffLand[ i ] - aboveGroundCarbonDensity[ i ] );
                double carbonDiffBelow = -1 * ( diffLand[ i ] * belowGroundCarbonDensity[ i ] - fractionOfGain * carbonPrevBelow );
                mCarbonCalcs[ i ]->calcAboveGroundCarbonEmission( 0, 0, diffLand[ i ], carbonDiffAboveDensity, year,
                                                                  CarbonModelUtils::getEndYear(), mCarbonCalcs[ i ]->mTotalEmissionsAbove );
                mCarbonCalcs[ i ]->calcBelowGroundCarbonEmission( carbonDiffBelow, year, CarbonModelUtils::getEndYear(),
                                                                  mCarbonCalcs[ i ]->mTotalEmissionsBelow );
                // Adjust carbon stock to include the carbon being moved in minus any emissions because of moving
                // the carbon.
                carbonStock[ i ] += currCarbonMove - ( mCarbonCalcs[ i ]->mTotalEmissionsAbove[ year ] - emissBeforeMove );
            }
            mCarbonCalcs[ i ]->mTotalEmissions[ year ] = mCarbonCalcs[ i ]->mTotalEmissionsAbove[ year ] + mCarbonCalcs[ i ]->mTotalEmissionsBelow[ year ];
        }
        
        prevLand = currLand;
        prevLandTotal = currLandTotal;
    }

    // Make sure future year calculations start from the correct historical carbon stock.
    for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
        mCarbonCalcs[ i ]->mCarbonStock[ mCarbonCalcs[ i ]->mLandUseHistory->getMaxYear() ] = carbonStock[ i ];
    }

    mHasCalculatedHistoricEmiss = true;
}

void NodeCarbonCalc::calc( const int aPeriod, const int aEndYear, const ICarbonCalc::CarbonCalcMode aCalcMode ) {
    const Modeltime* modeltime = scenario->getModeltime();

    // If this is a land-use history year...
    if( aPeriod == 0 ) {
        calcLandUseHistory();

    }
    else {
        // using model calculated allocations
        const int modelYear = modeltime->getper_to_yr( aPeriod );
        const int modelTimestep = modeltime->gettimestep( aPeriod );
        const int prevModelYear = modelYear - modelTimestep;
        int year = prevModelYear + 1;

        // clear the previously calculated emissions first
        vector<YearVector<double>*> currEmissionsAbove( mCarbonCalcs.size() );
        vector<YearVector<double>*> currEmissionsBelow( mCarbonCalcs.size() );
        for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
            currEmissionsAbove[ i ] = new YearVector<double>( year, aEndYear, 0.0 );
            currEmissionsBelow[ i ] = new YearVector<double>( year, aEndYear, 0.0 );
        }
        
        // stash carbon densities for quick access
        year = prevModelYear;
        vector<double> aboveGroundCarbonDensity( mCarbonCalcs.size() );
        vector<double> belowGroundCarbonDensity( mCarbonCalcs.size() );
        for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
            aboveGroundCarbonDensity[ i ] = mCarbonCalcs[ i ]->getActualAboveGroundCarbonDensity( year );
            belowGroundCarbonDensity[ i ] = mCarbonCalcs[ i ]->getActualBelowGroundCarbonDensity( year );
        }

        // compute changes in land area
        vector<double> prevLandByTimestep( mCarbonCalcs.size() );
        vector<double> currLandByTimestep( mCarbonCalcs.size() );
        vector<double> diffLandByTimestep( mCarbonCalcs.size() );
        double prevLandTotal = 0;
        for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
            // we need to be careful about accessing the land allocation from a previous timestep
            // when we are intending to calculate in eReverseCalc as the previous timestep may have
            // already calculated in eStoreResults
            double land = aPeriod == 1 ? mCarbonCalcs[ i ]->mLandUseHistory->getAllocation( prevModelYear ) :
                aCalcMode != ICarbonCalc::eReverseCalc ?
                    mCarbonCalcs[ i ]->mLandLeaf->getLandAllocation( mCarbonCalcs[ i ]->mLandLeaf->getName(), aPeriod - 1 ) :
                    mCarbonCalcs[ i ]->mSavedLandAllocation[ aPeriod - 1 ];
            prevLandByTimestep[ i ] = land;
            prevLandTotal += land;
        }
        double currLandTotal = 0;
        for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
            double land = mCarbonCalcs[ i ]->mLandLeaf->getLandAllocation( mCarbonCalcs[ i ]->mLandLeaf->getName(), aPeriod );
            currLandByTimestep[ i ] = land;
            currLandTotal += land;
            diffLandByTimestep[ i ] = land - prevLandByTimestep[ i ];
        }
        double diffLandTotal = currLandTotal - prevLandTotal;
        vector<double> diffLandFromExternalByYear( mCarbonCalcs.size(), 0 );
        vector<double> diffLandFromInternalByYear( mCarbonCalcs.size(), 0 );
        double totalInternalLandGainByYear = 0;
        double totalInternalCarbonAboveMovedByYear = 0;
        double totalInternalCarbonBelowMovedByYear = 0;
        vector<double> internalCarbonAboveMovedByYear( mCarbonCalcs.size(), 0 );
        double diffLandInternalCheck = 0;

        // First allocate changes in total land area preferentially adding land to the
        // most carbon dense land first when the total increase and conversely removing
        // land from the least carbon dense lands first when it decreases.
        vector<size_t>::const_iterator cdenIt = diffLandTotal > 0 ? mIndHighToLow.begin() : mIndLowToHigh.begin();
        vector<size_t>::const_iterator cdenItEnd = diffLandTotal > 0 ? mIndHighToLow.end() : mIndLowToHigh.end();
        for( ; cdenIt < cdenItEnd; ++cdenIt ) {
            size_t i = *cdenIt;
            if( hasSameSign( diffLandTotal, diffLandByTimestep[ i ] ) ) {
                double newDiff = abs( diffLandByTimestep[ i ] ) < abs( diffLandTotal ) ? diffLandByTimestep[ i ] : diffLandTotal;
                diffLandByTimestep[ i ] -= newDiff;
                diffLandTotal -= newDiff;
                assert( diffLandByTimestep[ i ] == 0 || diffLandTotal == 0 );
                diffLandFromExternalByYear[ i ] = newDiff / modelTimestep;
            }

            // Any differences in land for this land type are due to changes in area
            // internal to the node.
            diffLandInternalCheck += diffLandByTimestep[ i ];
            diffLandFromInternalByYear[ i ] = diffLandByTimestep[ i ] / modelTimestep;

            // Store some information that will be useful for calculating changes in
            // carbon.
            if( diffLandFromInternalByYear[ i ] > 0 ) {
                totalInternalLandGainByYear += diffLandFromInternalByYear[ i ];
            }
            else if( diffLandFromInternalByYear[ i ] == 0 ) {
                // Explicit check where the land did not change since if prevLand
                // was also zero we may generate NaN.
                internalCarbonAboveMovedByYear[ i ] = 0;
            }
            else {
                // Calculate the carbon that will be moved out of this land type by
                // computing the average from the current carbon stock.
                
                // we need to be careful about accessing the carbon stock from a previous timestep
                // when we are intending to calculate in eReverseCalc as the previous timestep may have
                // already calculated in eStoreResults
                double currCarbonMovedByYear = -1 * ( aCalcMode == ICarbonCalc::eReverseCalc ?
                                                        mCarbonCalcs[ i ]->mSavedCarbonStock[ aPeriod - 1 ] :
                                                        mCarbonCalcs[ i ]->mCarbonStock[ prevModelYear ] )
                    * diffLandByTimestep[ i ] / prevLandByTimestep[ i ] / modelTimestep;
                totalInternalCarbonAboveMovedByYear += currCarbonMovedByYear;
                internalCarbonAboveMovedByYear[ i ] = currCarbonMovedByYear;
                totalInternalCarbonBelowMovedByYear -= diffLandFromInternalByYear[ i ] * belowGroundCarbonDensity[ i ];
            }
        }
        // The difference in total land area change should have all been allocated
        // across the various land types.
        assert( util::isEqual( diffLandTotal, 0.0 ) );
        // Some land options increase in size and some decrease but in total the
        // change in land internal to the node should be zero.
        assert( util::isEqual( diffLandInternalCheck, 0.0 ) );

        vector<double> prevLand( prevLandByTimestep );
        vector<double> currLand( mCarbonCalcs.size() );
        for( year = prevModelYear + 1; year <= modelYear; ++year ) {
            // Initialize the carbon stock in this year to the carbon stock of the previous year minus
            // any emissions that have already been allocated in this year from earlier year land decisions.
            if( aCalcMode != ICarbonCalc::eReverseCalc ) {
                for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
                    mCarbonCalcs[ i ]->mCarbonStock[ year ] = mCarbonCalcs[ i ]->mCarbonStock[ year - 1 ] -
                        ( mCarbonCalcs[ i ]->mTotalEmissionsAbove[ year ] + (*currEmissionsAbove[ i ])[ year ] );
                }
            }
            // Calculate emissions from changes in land that was removed/added from outside of this node.
            for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
                currLand[ i ] = prevLand[ i ] + diffLandFromExternalByYear[ i ];
                double carbonDiffBelowPerYear = -1 * diffLandFromExternalByYear[ i ] * belowGroundCarbonDensity[ i ];
                double prevEmiss = (*currEmissionsAbove[ i ])[ year ];
                // we need to be careful about accessing the carbon stock from a previous timestep
                // when we are intending to calculate in eReverseCalc as the previous timestep may have
                // already calculated in eStoreResults
                mCarbonCalcs[ i ]->calcAboveGroundCarbonEmission( aCalcMode == ICarbonCalc::eReverseCalc && (year - 1) == prevModelYear ?
                                                                        mCarbonCalcs[ i ]->mSavedCarbonStock[ aPeriod - 1 ] :
                                                                        mCarbonCalcs[ i ]->mCarbonStock[ year - 1 ],
                                                                  prevLand[ i ], currLand[ i ], aboveGroundCarbonDensity[ i ], year, aEndYear,
                                                                  *currEmissionsAbove[ i ] );
                mCarbonCalcs[ i ]->calcBelowGroundCarbonEmission( carbonDiffBelowPerYear, year, aEndYear, *currEmissionsBelow[ i ] );
                if( aCalcMode != ICarbonCalc::eReverseCalc ) {
                    mCarbonCalcs[ i ]->mCarbonStock[ year ] -= (*currEmissionsAbove[ i ])[ year ] - prevEmiss;
                }
            }
            // Calculate emissions from changes in land internal to this node.  Carbon can move internally
            // to the node with out emissions however if there is a difference in carbon densities then
            // emissions/uptake may still occur.  These emissions/uptake get accounted for in the options
            // that gain land.
            for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
                currLand[ i ] += diffLandFromInternalByYear[ i ];
                if( diffLandFromInternalByYear[ i ] > 0 ) {
                    double emissBeforeMove = (*currEmissionsAbove[ i ])[ year ];
                    // Calculate the difference in carbon densities which would drive any
                    // emissions or uptake.
                    double fractionOfGain = diffLandFromInternalByYear[ i ] / totalInternalLandGainByYear;
                    double currCarbonMove = fractionOfGain * totalInternalCarbonAboveMovedByYear;
                    double carbonDiffAboveDensity = -1 * ( currCarbonMove / diffLandFromInternalByYear[ i ] - aboveGroundCarbonDensity[ i ] );
                    double carbonDiffBelow = -1 * ( diffLandFromInternalByYear[ i ] * belowGroundCarbonDensity[ i ] -
                        fractionOfGain * totalInternalCarbonBelowMovedByYear );
                    mCarbonCalcs[ i ]->calcAboveGroundCarbonEmission( 0, 0, diffLandFromInternalByYear[ i ], carbonDiffAboveDensity,
                                                                      year, aEndYear, *currEmissionsAbove[ i ] );
                    mCarbonCalcs[ i ]->calcBelowGroundCarbonEmission( carbonDiffBelow, year, aEndYear, *currEmissionsBelow[ i ] );
                    // Adjust carbon stock to include the carbon being moved in minus any emissions because of moving
                    // the carbon.
                    if( aCalcMode != ICarbonCalc::eReverseCalc ) {
                        mCarbonCalcs[ i ]->mCarbonStock[ year ] += currCarbonMove - ( (*currEmissionsAbove[ i ])[ year ] - emissBeforeMove );
                    }
                }
                else {
                    // Remove the carbon that is changing land type from the carbon stock without
                    // emissions.
                    if( aCalcMode != ICarbonCalc::eReverseCalc ) {
                        mCarbonCalcs[ i ]->mCarbonStock[ year ] -= internalCarbonAboveMovedByYear[ i ];
                    }
                }
                prevLand[ i ] = currLand[ i ];
            }
        }

        // add current emissions to the total
        for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
            if( aCalcMode == ICarbonCalc::eStoreResults ) {
                for( year = prevModelYear + 1; year <= aEndYear; ++year ) {
                    mCarbonCalcs[ i ]->mTotalEmissionsAbove[ year ] += (*currEmissionsAbove[ i ])[ year ];
                    mCarbonCalcs[ i ]->mTotalEmissionsBelow[ year ] += (*currEmissionsBelow[ i ])[ year ];
                    mCarbonCalcs[ i ]->mTotalEmissions[ year ] = mCarbonCalcs[ i ]->mTotalEmissionsAbove[ year ] +
                        mCarbonCalcs[ i ]->mTotalEmissionsBelow[ year ];
                    mCarbonCalcs[ i ]->mSavedCarbonStock[ aPeriod - 1 ] = mCarbonCalcs[ i ]->mCarbonStock[ prevModelYear ];
                    mCarbonCalcs[ i ]->mSavedLandAllocation[ aPeriod - 1 ] = mCarbonCalcs[ i ]->mLandLeaf->getLandAllocation( mCarbonCalcs[ i ]->mLandLeaf->getName(), aPeriod - 1 );
                }
            }
            else if( aCalcMode == ICarbonCalc::eReverseCalc ) {
                for( year = prevModelYear + 1; year <= aEndYear; ++year ) {
                    mCarbonCalcs[ i ]->mTotalEmissionsAbove[ year ] -= (*currEmissionsAbove[ i ])[ year ];
                    mCarbonCalcs[ i ]->mTotalEmissionsBelow[ year ] -= (*currEmissionsBelow[ i ])[ year ];
                    mCarbonCalcs[ i ]->mTotalEmissions[ year ] = mCarbonCalcs[ i ]->mTotalEmissionsAbove[ year ] +
                        mCarbonCalcs[ i ]->mTotalEmissionsBelow[ year ];
                }
            }
            else if( aCalcMode == ICarbonCalc::eReturnTotal ) {
                mCarbonCalcs[ i ]->mStoredEmissions = mCarbonCalcs[ i ]->mTotalEmissionsAbove[ aEndYear ] +
                    mCarbonCalcs[ i ]->mTotalEmissionsBelow[ aEndYear ] +
                    (*currEmissionsAbove[ i ])[ aEndYear ] + (*currEmissionsBelow[ i ])[ aEndYear ];
            }
            // clean up memory now that we are done with it
            delete currEmissionsAbove[ i ];
            delete currEmissionsBelow[ i ];
        }
    }
}

/*!
 * \brief A simple utility function to check if the parameters are both positive
 *        or negative.
 * \param aValue1 The first value to check the sign of.
 * \param aValue2 The second value to check the sign of.
 * \return True when the two values are of the same sign.
 */
bool NodeCarbonCalc::hasSameSign( const double aValue1, const double aValue2 ) const {
    return ( aValue1 > 0 && aValue2 > 0 ) || ( aValue1 < 0 && aValue2 < 0 );
}
