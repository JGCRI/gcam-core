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
 * \file calibrate_share_weight_visitor.cpp
 * \ingroup Objects
 * \brief The CalibrateShareWeightVisitor class source file.
 * \author Pralit Patel
 */

#include "util/base/include/definitions.h"
#include <cassert>
#include "util/base/include/calibrate_share_weight_visitor.h"
#include "technologies/include/technology_container.h"
#include "technologies/include/itechnology.h"
#include "sectors/include/subsector.h"
#include "sectors/include/sector.h"
#include "containers/include/gdp.h"
#include "util/logger/include/ilogger.h"

using namespace std;

extern Scenario* scenario;

/*!
 * \brief Constructor
 * \param aRegionName Name of the region if starting the visiting below the
 *        region level.
 */
CalibrateShareWeightVisitor::CalibrateShareWeightVisitor( const string& aRegionName, const GDP* aGDP )
:mCurrentRegionName( aRegionName ), mGDP( aGDP )
{
}

void CalibrateShareWeightVisitor::startVisitSector( const Sector* aSector, const int aPeriod ) {
    mCurrentSectorName = aSector->getName();
}

void CalibrateShareWeightVisitor::endVisitSector( const Sector* aSector, const int aPeriod ) {
    // Doing subsector calibration in sector rather than in subsector because we will need access 
    // to all of the subsectors at the same time.  Also we need to do this in end visit sector
    // because the subsector can't be calculated until we have gotten the correct share weights
    // for the technologies.

    // Find out if we need to do calibration, make sure we do not have a mix of calibrated/non-calibrated
    // subsectors, and figure out the largest subsector to make the other subsectors anchored by it.
    int anchorIndex = -1;
    bool hasCalValues = false;
    double maxCalValue = 0;
    int numCalSubsectors = 0;
    double totalCalValue = 0;
    for( int subsectorIndex = 0; subsectorIndex < aSector->subsec.size(); ++subsectorIndex ) {
        double currCalValue = aSector->subsec[ subsectorIndex ]->getTotalCalOutputs( aPeriod );
        bool isAllFixed = aSector->subsec[ subsectorIndex ]->containsOnlyFixedOutputTechnologies( aPeriod )
            || aSector->subsec[ subsectorIndex ]->mShareWeights[ aPeriod ] == 0;

        // check if the subsector is calibrated
        if( hasCalValues && currCalValue == 0 && !isAllFixed ) {
            // warn that we mixed calibrated and variable technologies
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            ILogger& calibrationLog = ILogger::getLogger( "calibration_log" );
            mainLog.setLevel( ILogger::WARNING );
            calibrationLog.setLevel( ILogger::WARNING );
            mainLog << "Mixed calibrated and variable subsectors or read a zero calibration value in Region: "
                << mCurrentRegionName << " in sector: " << mCurrentSectorName
                << " for Subsector: " << aSector->subsec[ subsectorIndex ]->getName() << endl;
            calibrationLog << "Mixed calibrated and variable subsectors or read a zero calibration value in Region: "
                << mCurrentRegionName << " in sector: " << mCurrentSectorName
                << " for Subsector: " << aSector->subsec[ subsectorIndex ]->getName() << endl;
        }
        else if( currCalValue > 0 ) {
            hasCalValues = true;
        }

        // If the calibrated value > 0 then increase the number of subsectors to calibrate
        // and include it in the total sum.
        if( currCalValue > 0 ) {
            ++numCalSubsectors;
            totalCalValue += currCalValue;
        }

        // attempt to locate the largest subsector
        if( currCalValue > maxCalValue ) {
            maxCalValue = currCalValue;
            anchorIndex = subsectorIndex;
        }
    }

    // do calibration if we have cal values and there are more than one subsector in this nest
    if( hasCalValues && numCalSubsectors > 1 ) {	
        // we should have found a subsector to have share weights anchored
        assert( anchorIndex != -1 );
        const double scaledGdpPerCapita = mGDP->getBestScaledGDPperCap( aPeriod );
        const double anchorPrice = aSector->subsec[ anchorIndex ]->getPrice( mGDP, aPeriod );
        const double anchorShare = ( aSector->subsec[ anchorIndex ]->getTotalCalOutputs( aPeriod )
            / totalCalValue ) / pow( scaledGdpPerCapita, aSector->subsec[ anchorIndex ]->fuelPrefElasticity[ aPeriod ] );
        assert( anchorShare > 0 );

        for( int subsectorIndex = 0; subsectorIndex < aSector->subsec.size(); ++subsectorIndex ) {
            Subsector* currSubsector = aSector->subsec[ subsectorIndex ];
            double currShare = ( currSubsector->getTotalCalOutputs( aPeriod ) / totalCalValue )
                / pow( scaledGdpPerCapita, currSubsector->fuelPrefElasticity[ aPeriod ] );

            // only set the share weight for valid subsectors
            if( currShare > 0 ) {
                double currShareWeight = ( currShare / anchorShare )
                    * pow( anchorPrice / currSubsector->getPrice( mGDP, aPeriod ),
                          aSector->mSubsectorLogitExp[ aPeriod ] );
                currSubsector->mShareWeights[ aPeriod ] = currShareWeight;
            }
        }
    }

    mCurrentSectorName.clear();
}

void CalibrateShareWeightVisitor::startVisitSubsector( const Subsector* aSubsector, const int aPeriod ) {
    // Doing technology calibration in subsector rather than in technology because we will need access 
    // to all of the technologies at the same time.

    // First we need to make sure that the technologies have calculated their costs.
    // We will also find the largest technology in terms of output so that we can anchor the share weights
    // by that technology and we will also make sure that we do not have a inconsistent set of
    // technologies with and without calibration values.
    const bool hasRequired = false;
    const string requiredName = "";
    int anchorTechIndex = -1;
    bool hasCalValues = false;
    double maxCalValue = -1;
    int numlCalTechs = 0;
    double totalCalValue = 0;
    for( int techIndex = 0; techIndex < aSubsector->mTechContainers.size(); ++techIndex ) {
        ITechnology* currTech = aSubsector->mTechContainers[ techIndex ]->getNewVintageTechnology( aPeriod );
        // call calc cost to make sure they are up to date
        currTech->calcCost( mCurrentRegionName, mCurrentSectorName, aPeriod );

        double currCalValue = currTech->getCalibrationOutput( hasRequired, requiredName, aPeriod );
        bool isAvailable = currTech->isAvailable( aPeriod );

        // check if we have calibrated technologies
        if( hasCalValues && currCalValue == -1 && isAvailable ) {
            // log that we have inconsistent calibrated and variable technologies
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            ILogger& calibrationLog = ILogger::getLogger( "calibration_log" );
            mainLog.setLevel( ILogger::WARNING );
            calibrationLog.setLevel( ILogger::WARNING );
            mainLog << "Mixed calibrated and variable subsectors in Region: " << mCurrentRegionName
                << " in sector: " << mCurrentSectorName
                << " for technology: " << currTech->getName()  << endl;
            calibrationLog << "Mixed calibrated and variable subsectors in Region: " << mCurrentRegionName
                << " in sector: " << mCurrentSectorName
                << " for technology: " << currTech->getName()  << endl;
        }
        else if( isAvailable && currCalValue != -1 ) {
            hasCalValues = true;
        }

        // If the calibrated value > 0 then increase the number of technologies to calibrate
        // and include it in the total sum.
        if( currCalValue > 0 ) {
            ++numlCalTechs;
            totalCalValue += currCalValue;
        }

        // attempt to locate the largest technology
        if( currCalValue > maxCalValue ) {
            maxCalValue = currCalValue;
            anchorTechIndex = techIndex;
        }
    }

    // do calibration if we have cal values and there are more than one technologies in this nest
    if( hasCalValues && numlCalTechs > 1 ) {
        // we should have found a technology to have share weights anchored by
        assert( anchorTechIndex != -1 );
        const ITechnology* anchorTech = aSubsector->mTechContainers[ anchorTechIndex ]->getNewVintageTechnology( aPeriod );
        const double scaledGdpPerCapita = mGDP->getBestScaledGDPperCap( aPeriod );
        const double anchorPrice = anchorTech->getCost( aPeriod );
        const double anchorShare = ( anchorTech->getCalibrationOutput( hasRequired, requiredName, aPeriod )
            / totalCalValue ) / pow( scaledGdpPerCapita, anchorTech->calcFuelPrefElasticity( aPeriod ) );
        for( int techIndex = 0; techIndex < aSubsector->mTechContainers.size(); ++techIndex ) {
            ITechnology* currTech = aSubsector->mTechContainers[ techIndex ]->getNewVintageTechnology( aPeriod );
            double currShare = ( currTech->getCalibrationOutput( hasRequired, requiredName, aPeriod ) / totalCalValue )
                / pow( scaledGdpPerCapita, currTech->calcFuelPrefElasticity( aPeriod ) );

            // only set share weights for valid technologies
            if( currShare > 0 ) {
                double currShareWeight = ( currShare / anchorShare )
                    * pow( anchorPrice / currTech->getCost( aPeriod ), aSubsector->mTechLogitExp[ aPeriod ] );
                currTech->setShareWeight( currShareWeight );
            }
        }
    }
}
