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
#include "util/logger/include/ilogger.h"

using namespace std;
using namespace xercesc;
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

bool NodeCarbonCalc::XMLParse( const DOMNode* aCurr ) {
    // Assume we are passed a valid node.
    assert( aCurr );

    // get all the children.
    DOMNodeList* nodeList = aCurr->getChildNodes();

    for( unsigned int i = 0;  i < nodeList->getLength(); ++i ){
        const DOMNode* curr = nodeList->item( i );
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string " << nodeName
                << " found while parsing " << getXMLNameStatic() << "." << endl;
        }
    }

    // TODO: Improve error handling.
    return true;
}

void NodeCarbonCalc::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void NodeCarbonCalc::toInputXML( ostream& aOut, Tabs* aTabs ) const {
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
    double prevLandTotal = 0;
    for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
        double land = mCarbonCalcs[ i ]->mLandUseHistory->getAllocation( CarbonModelUtils::getStartYear() - 1 );
        prevLand[ i ] = land;
        prevLandTotal += land;
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
                mCarbonCalcs[ i ]->calcAboveGroundCarbonEmission( -1 * newDiff * aboveGroundCarbonDensity[ i ], year, CarbonModelUtils::getEndYear(), mCarbonCalcs[ i ]->mTotalEmissions );
                mCarbonCalcs[ i ]->calcBelowGroundCarbonEmission( -1 * newDiff * belowGroundCarbonDensity[ i ], year, CarbonModelUtils::getEndYear(), mCarbonCalcs[ i ]->mTotalEmissions );
            }
        }
        // The difference in total land area change should have all been allocated
        // across the various land types.
        assert( util::isEqual( diffLandTotal, 0.0 ) );

        // Now the differences in land area are all between the land options with in 
        // this node carbon calc.  We must then allocate the change in carbon between
        // those options.
        double carbonPrevAbove = 0;
        double carbonPrevBelow = 0;
        double carbonCurrAbove = 0;
        double carbonCurrBelow = 0;
        double landCheck = 0;
        for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
            if( diffLand[ i ] > 0 ) {
                carbonCurrAbove += diffLand[ i ] * aboveGroundCarbonDensity[ i ];
                carbonCurrBelow += diffLand[ i ] * belowGroundCarbonDensity[ i ];
            }
            else {
                carbonPrevAbove -= diffLand[ i ] * aboveGroundCarbonDensity[ i ];
                carbonPrevBelow -= diffLand[ i ] * belowGroundCarbonDensity[ i ];
            }
            landCheck += diffLand[ i ];
        }
        // Some land options increase in size and some decrease but in total the
        // change in land should be zero at this point.
        assert( util::isEqual( landCheck, 0.0 ) );

        // Allocate the change in carbon.  We do this by assigning it all to one of the
        // options that increased in land if carbon is gained or decreased in land
        // if the carbon decreased.  The carbon in total makes sense although this
        // assignment to the options is arbitrary.
        const double carbonDiffAbove = -1 * (carbonCurrAbove - carbonPrevAbove);
        const double carbonDiffBelow = -1 * (carbonCurrBelow - carbonPrevBelow);
        // If the change in carbon is zero then no need to do anything.
        bool hasAssignedCarbon = util::isEqual( carbonDiffAbove, 0.0 );
        for( size_t i = 0; i < mCarbonCalcs.size() && !hasAssignedCarbon; ++i ) {
            if( hasSameSign( carbonDiffAbove, diffLand[ i ] ) ) {
                mCarbonCalcs[ i ]->calcAboveGroundCarbonEmission( carbonDiffAbove, year, CarbonModelUtils::getEndYear(), mCarbonCalcs[ i ]->mTotalEmissions );
                mCarbonCalcs[ i ]->calcBelowGroundCarbonEmission( carbonDiffBelow, year, CarbonModelUtils::getEndYear(), mCarbonCalcs[ i ]->mTotalEmissions );
                hasAssignedCarbon = true;
            }
        }
        // The change in carbon must be assigned somewhere.
        assert( hasAssignedCarbon );

        prevLand = currLand;
        prevLandTotal = currLandTotal;
    }

    // Make sure future year calculations start from the correct historical land
    // areas.
    for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
        mCarbonCalcs[ i ]->setTotalLandUse( prevLand[ i ], 0 );
    }

    mHasCalculatedHistoricEmiss = true;
}

void NodeCarbonCalc::calc( const int aPeriod, const int aEndYear ) {
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
        vector<YearVector<double>*> currEmissions( mCarbonCalcs.size() );
        for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
            currEmissions[ i ] = mCarbonCalcs[ i ]->mStoredEmissions[ aPeriod ];
            for( year = prevModelYear + 1; year <= aEndYear; ++year ) {
                mCarbonCalcs[ i ]->mTotalEmissions[ year ] -= (*currEmissions[ i ])[ year ];
                (*currEmissions[ i ])[ year ] = 0.0;
            }
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
        vector<double> prevLand( mCarbonCalcs.size() );
        vector<double> currLand( mCarbonCalcs.size() );
        vector<double> diffLand( mCarbonCalcs.size() );
        double prevLandTotal = 0;
        for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
            double land = mCarbonCalcs[ i ]->mLandUse[ aPeriod - 1 ];
            prevLand[ i ] = land;
            prevLandTotal += land;
        }
        double currLandTotal = 0;
        for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
            double land = mCarbonCalcs[ i ]->mLandUse[ aPeriod ];
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
                double carbonDiffAbovePerYear = -1 * newDiff * aboveGroundCarbonDensity[ i ] / modelTimestep;
                double carbonDiffBelowPerYear = -1 * newDiff * belowGroundCarbonDensity[ i ] / modelTimestep;
                for( year = prevModelYear + 1; year <= modelYear; ++year ) {
                    mCarbonCalcs[ i ]->calcAboveGroundCarbonEmission( carbonDiffAbovePerYear, year, aEndYear, *currEmissions[ i ] );
                    mCarbonCalcs[ i ]->calcBelowGroundCarbonEmission( carbonDiffBelowPerYear, year, aEndYear, *currEmissions[ i ] );
                }
            }
        }
        // The difference in total land area change should have all been allocated
        // across the various land types.
        assert( util::isEqual( diffLandTotal, 0.0 ) );

        // Now the differences in land area are all between the land options with in 
        // this node carbon calc.  We must then allocate the change in carbon between
        // those options.
        double carbonPrevAbove = 0;
        double carbonPrevBelow = 0;
        double carbonCurrAbove = 0;
        double carbonCurrBelow = 0;
        double landCheck = 0;
        for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
            if( diffLand[ i ] > 0 ) {
                carbonCurrAbove += diffLand[ i ] * aboveGroundCarbonDensity[ i ];
                carbonCurrBelow += diffLand[ i ] * belowGroundCarbonDensity[ i ];
            }
            else {
                carbonPrevAbove -= diffLand[ i ] * aboveGroundCarbonDensity[ i ];
                carbonPrevBelow -= diffLand[ i ] * belowGroundCarbonDensity[ i ];
            }
            landCheck += diffLand[ i ];
        }
        // Some land options increase in size and some decrease but in total the
        // change in land should be zero at this point.
        assert( util::isEqual( landCheck, 0.0 ) );

        // Allocate the change in carbon.  We do this by assigning it all to one of the
        // options that increased in land if carbon is gained or decreased in land
        // if the carbon decreased.  The carbon in total makes sense although this
        // assignment to the options is arbitrary.
        double carbonDiffAbovePerYear = -1 * ( carbonCurrAbove - carbonPrevAbove ) / modelTimestep;
        double carbonDiffBelowPerYear = -1 * ( carbonCurrBelow - carbonPrevBelow ) / modelTimestep;
        // If the change in carbon is zero then no need to do anything.
        bool hasAssignedCarbon = util::isEqual( carbonDiffAbovePerYear, 0.0 );
        for( size_t i = 0; i < mCarbonCalcs.size() && !hasAssignedCarbon; ++i ) {
            if( hasSameSign( carbonDiffAbovePerYear, diffLand[ i ] ) ) {
                for( year = prevModelYear + 1; year <= modelYear; ++year ) {
                    mCarbonCalcs[ i ]->calcAboveGroundCarbonEmission( carbonDiffAbovePerYear, year, aEndYear, *currEmissions[ i ] );
                    mCarbonCalcs[ i ]->calcBelowGroundCarbonEmission( carbonDiffBelowPerYear, year, aEndYear, *currEmissions[ i ] );
                }
                hasAssignedCarbon = true;
            }
        }
        // The change in carbon must be assigned somewhere.
        assert( hasAssignedCarbon );

        // add current emissions to the total
        for( size_t i = 0; i < mCarbonCalcs.size(); ++i ) {
            for( year = prevModelYear + 1; year <= aEndYear; ++year ) {
                mCarbonCalcs[ i ]->mTotalEmissions[ year ] += (*currEmissions[ i ])[ year ];
            }
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
