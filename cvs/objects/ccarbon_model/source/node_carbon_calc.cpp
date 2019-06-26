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
    // KVC_IESM: This is problematic. It orders leafs based on carbon density in the first model year. If they
    //           switch in order in later years due to climate impacts, we aren't adjusting the ordering.
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
    
    // Stash carbon densities for quick access. Array is over the different land types in this node carbon calc (e.g., Forest and UnmanagedForest)
    vector<YearVector<double>*> aboveGroundCarbonDensity( mCarbonCalcs.size() );
    vector<YearVector<double>*> belowGroundCarbonDensity( mCarbonCalcs.size() );
    for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
        aboveGroundCarbonDensity[ i ] = new YearVector<double>( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear());
        belowGroundCarbonDensity[ i ] = new YearVector<double>( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear());
        for( int year = CarbonModelUtils::getStartYear(); year <= CarbonModelUtils::getEndYear(); ++year ) {
            if ( year < scenario->getModeltime()->getStartYear() ) {
                (*aboveGroundCarbonDensity[ i ])[ year ] = mCarbonCalcs[ i ]->mLandUseHistory->getHistoricAboveGroundCarbonDensity();
                (*belowGroundCarbonDensity[ i ])[ year ] = mCarbonCalcs[ i ]->mLandUseHistory->getHistoricBelowGroundCarbonDensity();
            }
            else {
                (*aboveGroundCarbonDensity[ i ])[ year ] = mCarbonCalcs[ i ]->getActualAboveGroundCarbonDensity( year );
                (*belowGroundCarbonDensity[ i ])[ year ] = mCarbonCalcs[ i ]->getActualBelowGroundCarbonDensity( year );
            }
        }
    }

     // Create vectors for previous land, current land, difference in land, and carbon stock by land type (e.g., Forest and UnmanagedForest)
    vector<double> prevLand( mCarbonCalcs.size() );
    vector<double> currLand( mCarbonCalcs.size() );
    vector<double> diffLand( mCarbonCalcs.size() );
    vector<double> fractDiffLand( mCarbonCalcs.size() );
    vector<double> carbonStock( mCarbonCalcs.size() );
    
    // Initialize previous land and carbon stock for the first carbon model time step. This means retrieving land allocation for the year
    // prior to the carbon model start year (i.e., if carbon model start year = 1750, then we need land allocation for 1749 ).
    double prevLandTotal = 0;
    for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
        double land = mCarbonCalcs[ i ]->mLandUseHistory->getAllocation( CarbonModelUtils::getStartYear() - 1 );
        prevLand[ i ] = land;
        prevLandTotal += land;
        carbonStock[ i ] = land * (*aboveGroundCarbonDensity[ i ])[ CarbonModelUtils::getStartYear() ]; // We are assuming terrestrial carbon cycle is in equilibrium prior to carbon model start year.

    }
    
    // Calculate emissions over the entire historical period.
    for( int year = CarbonModelUtils::getStartYear(); year <= mCarbonCalcs[ 0 ]->mLandUseHistory->getMaxYear(); ++year ) {
        // Compute changes in land area by land type
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
        // This is accomplished by changing the order of the vector (e.g., higher carbon density land type first when total increases).
        vector<size_t>::const_iterator cdenIt = diffLandTotal > 0 ? mIndHighToLow.begin() : mIndLowToHigh.begin();
        vector<size_t>::const_iterator cdenItEnd = diffLandTotal > 0 ? mIndHighToLow.end() : mIndLowToHigh.end();
        for( ; cdenIt < cdenItEnd && diffLandTotal != 0; ++cdenIt ) {
            size_t i = *cdenIt;
            
            // Calculate change in emissions if and only if land area for this type changed in the same direction of the total land area.
            // That is, if Forest increases, UnmanagedForest decreases, but total (Forest + UnmanagedForest) increase, this will only be
            // calculated for Forest. The conversion from UnmanagedForest to Forest occurs later. This calculation is handling expansion
            // or contraction of the total node.
            if( hasSameSign( diffLandTotal, diffLand[ i ] ) ) {
                double newDiff = abs( diffLand[ i ] ) < abs( diffLandTotal ) ? diffLand[ i ] : diffLandTotal;
                diffLand[ i ] -= newDiff;
                diffLandTotal -= newDiff;
                assert( diffLand[ i ] == 0 || diffLandTotal == 0 );
                // KVC_IESM: Need to separate land and c density in these methods. For above ground, this looks like it is already done --
                //           just need to stop passing carbon density. The calculate method should look it up.
                //           For below, we need to separate. Trivial in this case (future cases are hard).
                mCarbonCalcs[ i ]->calcAboveGroundCarbonEmission( carbonStock[ i ], prevLand[ i ], prevLand[ i ] + newDiff,
                                                                  *aboveGroundCarbonDensity[ i ], year,
                                                                  CarbonModelUtils::getEndYear(), mCarbonCalcs[ i ]->mTotalEmissionsAbove );
                mCarbonCalcs[ i ]->calcBelowGroundCarbonEmission( -1 * newDiff, *belowGroundCarbonDensity[ i ], year,
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
                // Assumes carbon is evenly distributed across land; that is, if you
                // move 10% of land you move 10% of carbon.
                double currCarbonStockMoved = -1 * carbonStock[ i ] * diffLand[ i ] / prevLand[ i ];
                carbonStockMoved += currCarbonStockMoved;
                // Remove the carbon that is changing land type from the carbon stock without
                // emissions. Carbon will be allocated to the other land types in subsequent steps.
                carbonStock[ i ] -= currCarbonStockMoved;
            }
            landCheck += diffLand[ i ];
        }
        // Some land options increase in size and some decrease but in total the
        // change in land should be zero at this point.
        assert( util::isEqual( landCheck, 0.0 ) );
        
        objects::YearVector<double> avgCarbonDensityReducedLand( year, CarbonModelUtils::getEndYear());
        for( int temp = year; temp <= CarbonModelUtils::getEndYear(); ++temp ){
            avgCarbonDensityReducedLand[ temp ] = 0.0;
            // Compute fraction of land reduced that comes from each land leaf.
            for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
                if ( diffLand[ i ] < 0 ) {
                    fractDiffLand[ i ] = -diffLand[ i ] / totalLandGain;
                    avgCarbonDensityReducedLand[ temp ] += fractDiffLand[ i ] * (*belowGroundCarbonDensity[ i ])[ temp ];
                }
            }
        }
        // Allocate the change in carbon computed above.  We do this by assigning it to the
        // options that increased in land. That is, the carbon difference computed above from land
        // areas that were contracting will now be allocated to expanding land areas.
        for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
            if( diffLand[ i ] > 0 ) {
                double emissBeforeMove = mCarbonCalcs[ i ]->mTotalEmissionsAbove[ year ];
                // Calculate the difference in carbon densities which would drive any
                // emissions or uptake.
                double fractionOfGain = diffLand[ i ] / totalLandGain;
                double currCarbonMove = fractionOfGain * carbonStockMoved;
                
                // For above ground carbon, carbon density is passed in instead of carbon stock. Use the difference
                // between the average carbon density of converted land and the equilibrium carbon density for this leaf
                
                // Change in below ground carbon stock is just the incremental gain (loss) due to differences in carbon densities. Land
                // areas between gain & loss are equal. That is, land area used to compute carbonPrevBelow is identical to totalLandGain (in abs value).
                // FractionOfGain only factors in if there are multiple land leafs in this node that are gaining land area.
                // KVC_IESM: Compute the carbon density to use for below ground carbon emissions.
                //           This should equal: carbon density of this land leaf minus average carbon density of leaves that have land reduced
                //           Need to do this for all years.
                objects::YearVector<double> adjAboveGroundCarbonDensity( year, CarbonModelUtils::getEndYear());
                objects::YearVector<double> adjBelowGroundCarbonDensity( year, CarbonModelUtils::getEndYear());
                for( int temp = year; temp <= CarbonModelUtils::getEndYear(); ++temp ){
                    adjAboveGroundCarbonDensity[ temp ] = (*aboveGroundCarbonDensity[ i ])[ temp ] - currCarbonMove / diffLand[ i ];
                    adjBelowGroundCarbonDensity[ temp ] = (*belowGroundCarbonDensity[ i ])[ temp ] - avgCarbonDensityReducedLand[ temp ];
                }
                
                // Allocate changes in carbon stock across time using the standard sigmoid (above) and exponential (below) methods.
                mCarbonCalcs[ i ]->calcAboveGroundCarbonEmission( 0, 0, diffLand[ i ], adjAboveGroundCarbonDensity, year,
                                                                  CarbonModelUtils::getEndYear(), mCarbonCalcs[ i ]->mTotalEmissionsAbove );
                mCarbonCalcs[ i ]->calcBelowGroundCarbonEmission( diffLand[ i ], adjBelowGroundCarbonDensity, year, CarbonModelUtils::getEndYear(),
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
        
        // Stash carbon densities for quick access.
        year = prevModelYear;
        vector<YearVector<double>*> aboveGroundCarbonDensity( mCarbonCalcs.size() );
        vector<YearVector<double>*> belowGroundCarbonDensity( mCarbonCalcs.size() );
        for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
            aboveGroundCarbonDensity[ i ] = new YearVector<double>( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear());
            belowGroundCarbonDensity[ i ] = new YearVector<double>( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear());
            for( int temp = CarbonModelUtils::getStartYear(); temp <= CarbonModelUtils::getEndYear(); ++temp ) {
                if ( temp < scenario->getModeltime()->getStartYear() ) {
                    (*aboveGroundCarbonDensity[ i ])[ temp ] = mCarbonCalcs[ i ]->mLandUseHistory->getHistoricAboveGroundCarbonDensity();
                    (*belowGroundCarbonDensity[ i ])[ temp ] = mCarbonCalcs[ i ]->mLandUseHistory->getHistoricBelowGroundCarbonDensity();
                }
                else {
                    (*aboveGroundCarbonDensity[ i ])[ temp ] = mCarbonCalcs[ i ]->getActualAboveGroundCarbonDensity( temp );
                    (*belowGroundCarbonDensity[ i ])[ temp ] = mCarbonCalcs[ i ]->getActualBelowGroundCarbonDensity( temp );
                }
             }
        }

        // compute changes in land area
        vector<double> prevLandByTimestep( mCarbonCalcs.size() );
        vector<double> currLandByTimestep( mCarbonCalcs.size() );
        vector<double> diffLandByTimestep( mCarbonCalcs.size() );
        
        // First determine land area in the previous year.
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
        
        // Now, determine current land area and compute the difference.
        double currLandTotal = 0;
        for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
            double land = mCarbonCalcs[ i ]->mLandLeaf->getLandAllocation( mCarbonCalcs[ i ]->mLandLeaf->getName(), aPeriod );
            currLandByTimestep[ i ] = land;
            currLandTotal += land;
            diffLandByTimestep[ i ] = land - prevLandByTimestep[ i ];
        }
        double diffLandTotal = currLandTotal - prevLandTotal;
        
        // Create variables to store annual difference in land area. This difference
        // will be apportioned due to whether it is a change in node size or a
        // reallocation across leafs within a node.
        vector<double> diffLandFromExternalByYear( mCarbonCalcs.size(), 0 );
        vector<double> diffLandFromInternalByYear( mCarbonCalcs.size(), 0 );
        double totalInternalLandGainByYear = 0;
        double totalInternalCarbonAboveMovedByYear = 0;
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

            // Net change in land area for the node is already accounted for above, so
            // any remaining differences in land for this land type are due to changes in area
            // internal to the node (i.e., shifting among leafs).
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
                // Calculate the carbon that will be moved out of this land type into the other land types
                // by computing the average carbon density from the current carbon stock. We do this instead
                // of using the read in carbon density because the land type may not have reached equilibrium.
                
                // we need to be careful about accessing the carbon stock from a previous timestep
                // when we are intending to calculate in eReverseCalc as the previous timestep may have
                // already calculated in eStoreResults
                double currCarbonMovedByYear = -1 * ( aCalcMode == ICarbonCalc::eReverseCalc ?
                                                        mCarbonCalcs[ i ]->mSavedCarbonStock[ aPeriod - 1 ] :
                                                        mCarbonCalcs[ i ]->mCarbonStock[ prevModelYear ] )
                    * diffLandByTimestep[ i ] / prevLandByTimestep[ i ] / modelTimestep;
                totalInternalCarbonAboveMovedByYear += currCarbonMovedByYear;
                internalCarbonAboveMovedByYear[ i ] = currCarbonMovedByYear;
            }
        }
        // The difference in total land area change should have all been allocated
        // across the various land types.
        assert( util::isEqual( diffLandTotal, 0.0 ) );
        // Some land options increase in size and some decrease but in total the
        // change in land internal to the node should be zero.
        assert( util::isEqual( diffLandInternalCheck, 0.0 ) );
        vector<double> fractDiffLand( mCarbonCalcs.size() );
        objects::YearVector<double> avgCarbonDensityReducedLand( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear());
        for( int temp = CarbonModelUtils::getStartYear(); temp <= CarbonModelUtils::getEndYear(); ++temp ){
            avgCarbonDensityReducedLand[ temp ] = 0.0;
            // Compute fraction of land reduced that comes from each land leaf.
            for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
                if ( diffLandByTimestep[ i ] < 0 ) {
                    fractDiffLand[ i ] = diffLandTotal != 0 ? -diffLandByTimestep[ i ] / diffLandTotal : 1.0;
                    avgCarbonDensityReducedLand[ temp ] += fractDiffLand[ i ] * (*belowGroundCarbonDensity[ i ])[ temp ];
                }
            }
        }


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
                double prevEmiss = (*currEmissionsAbove[ i ])[ year ];
                // we need to be careful about accessing the carbon stock from a previous timestep
                // when we are intending to calculate in eReverseCalc as the previous timestep may have
                // already calculated in eStoreResults
                // KVC_IESM: Need to change carbon density/stock to use time-varying information.
                mCarbonCalcs[ i ]->calcAboveGroundCarbonEmission( aCalcMode == ICarbonCalc::eReverseCalc && (year - 1) == prevModelYear ?
                                                                        mCarbonCalcs[ i ]->mSavedCarbonStock[ aPeriod - 1 ] :
                                                                        mCarbonCalcs[ i ]->mCarbonStock[ year - 1 ],
                                                                  prevLand[ i ], currLand[ i ], *aboveGroundCarbonDensity[ i ], year,
                                                                  aEndYear, *currEmissionsAbove[ i ] );
                
                mCarbonCalcs[ i ]->calcBelowGroundCarbonEmission( -1 * diffLandFromExternalByYear[ i ], *belowGroundCarbonDensity[ i ], year, aEndYear, *currEmissionsBelow[ i ] );
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
                    // emissions or uptake. Note: We are just shifting land among leafs so only
                    // changes in carbon densities among leafs drive emissions.
                    double fractionOfGain = diffLandFromInternalByYear[ i ] / totalInternalLandGainByYear;
                    double currCarbonMove = fractionOfGain * totalInternalCarbonAboveMovedByYear;
                    objects::YearVector<double> adjAboveGroundCarbonDensity( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear());
                    objects::YearVector<double> adjBelowGroundCarbonDensity( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear());
                    for( int temp = CarbonModelUtils::getStartYear(); temp <= CarbonModelUtils::getEndYear(); ++temp ){
                        adjAboveGroundCarbonDensity[ temp ] = (*aboveGroundCarbonDensity[ i ])[ temp ] - currCarbonMove / diffLandFromInternalByYear[ i ];
                        adjBelowGroundCarbonDensity[ temp ] = (*belowGroundCarbonDensity[ i ])[ temp ] - avgCarbonDensityReducedLand[ temp ];
                    }
                    
                    mCarbonCalcs[ i ]->calcAboveGroundCarbonEmission( 0, 0, diffLandFromInternalByYear[ i ], adjAboveGroundCarbonDensity,
                                                                     year, aEndYear, *currEmissionsAbove[ i ] );
                    mCarbonCalcs[ i ]->calcBelowGroundCarbonEmission( diffLandFromInternalByYear[ i ], adjBelowGroundCarbonDensity, year, aEndYear, *currEmissionsBelow[ i ] );
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
