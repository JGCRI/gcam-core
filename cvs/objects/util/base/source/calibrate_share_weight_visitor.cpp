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
#include <cmath>

#include "util/base/include/calibrate_share_weight_visitor.h"
#include "technologies/include/technology_container.h"
#include "technologies/include/itechnology.h"
#include "sectors/include/subsector.h"
#include "sectors/include/nesting_subsector.h"
#include "sectors/include/sector.h"
#include "containers/include/gdp.h"
#include "util/logger/include/ilogger.h"
#include "functions/include/idiscrete_choice.hpp"

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

/*!
 * \brief A generic method to calculate share-weights based off of calibrated outputs
 * \details This method will make share-weights relative to the child container that has
 *          has the largest share, which will be given a share-weight of 1.
 *
 *          Note that because we want to generalize this routine to avoid code duplication
 *          but we have to deal with ContainerType that do not share a common interface we
 *          will rely on the following adaptor methods being defined to supply the behavior
 *          appropriate for the derived types that this tempalted function gets instatiated
 *          with:
 *            - getChildren: to get a vector of child containers to calculate share-weights for
 *            - isAvailable: determine if this child is "available" (i.e. not fixed)
 *            - getCalValue: get calibrated output of child to determine share
 *            - getCost: get cost of child
 *            - getFuelPrefElasticity: get fuel preference elasticity of child
 *            - getDiscreteChoice: get the discrete choice function to use
 *            - setShareWeight: sets the calibrated share-weight into the child
 *
 * \param aContainer The container who's children need to have their share-weights calibrated.
 * \param aPeriod The current model period.
 */
template<typename ContainerType>
void CalibrateShareWeightVisitor::calibrateShareWeights( const ContainerType* aContainer, const int aPeriod ) const
{
    // Find out if we need to do calibration, make sure we do not have a mix of calibrated/non-calibrated
    // children, and figure out the largest child to make the other children anchored by it.
    int anchorIndex = -1;
    bool hasCalValues = false;
    double maxCalValue = 0;
    int numCalChildren = 0;
    double totalCalValue = 0;
    auto childrenVec = getChildren( aContainer, aPeriod );
    for( int childIndex = 0; childIndex < childrenVec.size(); ++childIndex ) {
        double currCalValue = getCalValue( childrenVec[ childIndex ], aPeriod );
        bool isAvail = isAvailable( childrenVec[ childIndex ], aPeriod );
        
        // check if the child is calibrated
        if( hasCalValues && currCalValue <= 0 && isAvail ) {
            // warn that we mixed calibrated and variable children
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            ILogger& calibrationLog = ILogger::getLogger( "calibration_log" );
            mainLog.setLevel( ILogger::WARNING );
            calibrationLog.setLevel( ILogger::WARNING );
            mainLog << "Mixed calibrated and variable children or read a zero calibration value in Region: "
                    << mCurrentRegionName << " in sector: " << mCurrentSectorName
                    << " for container: " << childrenVec[ childIndex ]->getName() << endl;
            calibrationLog << "Mixed calibrated and variable children or read a zero calibration value in Region: "
                    << mCurrentRegionName << " in sector: " << mCurrentSectorName
                    << " for container: " << childrenVec[ childIndex ]->getName() << endl;
        }
        else if( currCalValue > 0 ) {
            hasCalValues = true;
        }
        
        // If the calibrated value > 0 then increase the number of children to calibrate
        // and include it in the total sum.
        if( currCalValue > 0 ) {
            ++numCalChildren;
            totalCalValue += currCalValue;
        }
        
        // attempt to locate the largest child
        if( currCalValue > maxCalValue ) {
            maxCalValue = currCalValue;
            anchorIndex = childIndex;
        }
    }
    
    // If we are using the abosolute choice function, then we need to
    // set the base cost for the model.  We will calculate an appropriate
    // base cost and let the discrete choice function use it if it needs it.
    
    // The base cost is equal to the highest cost child that has a share if we
    // have calibration data.  Otherwise, it is equal to the highest cost child
    // that has a valid cost; otherwise, we may be in trouble if a user did not
    // parse a value.
    double baseCost = 0;
    for( int childIndex = 0; childIndex < childrenVec.size(); ++childIndex ) {
        double currCost = getCost( childrenVec[ childIndex ], aPeriod );
        if( !std::isnan( currCost )  && currCost > baseCost &&
           ( ( hasCalValues && getCalValue( childrenVec[ childIndex ], aPeriod ) > 0 ) || !hasCalValues ) )
        {
            baseCost = currCost;
        }
    }
    // In the case where there is only one child we will reset the base-price
    // to one as the sharing should not matter.
    if( baseCost == 0 && childrenVec.size() == 1 ) {
        baseCost = 1;
    }
    // baseCost may still be zero and if a user did not parse a base cost an error
    // will be generated.
    IDiscreteChoice* choiceFn = getDiscreteChoice( aContainer );
    choiceFn->setBaseValue( baseCost );
    
    // do calibration if we have cal values and there are more than one child in this nest
    if( hasCalValues && numCalChildren > 1 ) {
        // we should have found a child to have share weights anchored
        assert( anchorIndex != -1 );
        const double scaledGdpPerCapita = mGDP->getBestScaledGDPperCap( aPeriod );
        const double anchorCost = getCost( childrenVec[ anchorIndex ], aPeriod );
        const double anchorShare = ( getCalValue( childrenVec[ anchorIndex ], aPeriod )
                                    / totalCalValue ) / pow( scaledGdpPerCapita, getFuelPrefElasticity( childrenVec[ anchorIndex ], aPeriod ) );
        assert( anchorShare > 0 );
        
        for( int childIndex = 0; childIndex < childrenVec.size(); ++childIndex ) {
            auto currChild = childrenVec[ childIndex ];
            double currShare = ( getCalValue( currChild, aPeriod ) / totalCalValue )
                / pow( scaledGdpPerCapita, getFuelPrefElasticity( currChild, aPeriod ) );
            
            // only set the share weight for valid children
            if( currShare > 0 ) {
                double currCost = getCost( currChild, aPeriod );
                
                double currShareWeight = choiceFn->calcShareWeight( currShare, currCost,
                                                                    anchorShare, anchorCost, aPeriod );
                setShareWeight( currChild, currShareWeight, aPeriod );
            }
        }
    }
    
}

// adaptor methods that allows us to generically define calibrateShareWeights even
// though Sector/Subsector/Technology do not share a common interface
std::vector<Subsector*> CalibrateShareWeightVisitor::getChildren( const Sector* aSector, const int aPeriod ) const {
    return aSector->mSubsectors;
}
std::vector<Subsector*> CalibrateShareWeightVisitor::getChildren( const NestingSubsector* aSubsector, const int aPeriod ) const {
    return aSubsector->mSubsectors;
}
std::vector<ITechnology*> CalibrateShareWeightVisitor::getChildren( const Subsector* aSubsector, const int aPeriod ) const {
    vector<ITechnology*> ret( aSubsector->mTechContainers.size() );
    size_t index = 0;
    for( auto techContainer : aSubsector->mTechContainers ) {
        ret[index++] = techContainer->getNewVintageTechnology( aPeriod );
    }
    return ret;
}

bool CalibrateShareWeightVisitor::isAvailable( const Subsector* aSubsector, const int aPeriod ) const {
    return !( aSubsector->containsOnlyFixedOutputTechnologies( aPeriod )
        || aSubsector->mShareWeights[ aPeriod ] == 0 );
}
bool CalibrateShareWeightVisitor::isAvailable( const ITechnology* aTechnology, const int aPeriod ) const {
    return aTechnology->isAvailable( aPeriod );
}

double CalibrateShareWeightVisitor::getCalValue( const Subsector* aSubsector, const int aPeriod ) const {
    return aSubsector->getTotalCalOutputs( aPeriod );
}
double CalibrateShareWeightVisitor::getCalValue( const ITechnology* aTechnology, const int aPeriod ) const {
    const bool hasRequired = false;
    const string requiredName = "";
    return aTechnology->getCalibrationOutput( hasRequired, requiredName, aPeriod );
}

double CalibrateShareWeightVisitor::getCost( const Subsector* aSubsector, const int aPeriod ) const {
    return aSubsector->getPrice( mGDP, aPeriod );
}
double CalibrateShareWeightVisitor::getCost( const ITechnology* aTechnology, const int aPeriod ) const {
    return aTechnology->getCost( aPeriod );
}

double CalibrateShareWeightVisitor::getFuelPrefElasticity( const Subsector* aSubsector, const int aPeriod ) const {
    return aSubsector->mFuelPrefElasticity[ aPeriod ];
}
double CalibrateShareWeightVisitor::getFuelPrefElasticity( const ITechnology* aTechnology, const int aPeriod ) const {
    return aTechnology->calcFuelPrefElasticity( aPeriod );
}

IDiscreteChoice* CalibrateShareWeightVisitor::getDiscreteChoice( const Sector* aSector ) const {
    return aSector->mDiscreteChoiceModel;
}
IDiscreteChoice* CalibrateShareWeightVisitor::getDiscreteChoice( const Subsector* aSubsector ) const {
    return aSubsector->mDiscreteChoiceModel;
}

void CalibrateShareWeightVisitor::setShareWeight( Subsector* aSubsector,
                                                  const double aShareWeight,
                                                  const int aPeriod ) const
{
    aSubsector->mShareWeights[ aPeriod ] = aShareWeight;
}
void CalibrateShareWeightVisitor::setShareWeight( ITechnology* aTechnology,
                                                  const double aShareWeight,
                                                  const int aPeriod ) const
{
    aTechnology->setShareWeight( aShareWeight );
}

void CalibrateShareWeightVisitor::startVisitSector( const Sector* aSector, const int aPeriod ) {
    mCurrentSectorName = aSector->getName();
}

void CalibrateShareWeightVisitor::endVisitSector( const Sector* aSector, const int aPeriod ) {
    // Doing subsector calibration in sector rather than in subsector because we will need access 
    // to all of the subsectors at the same time.  Also we need to do this in end visit sector
    // because the subsector can't be calculated until we have gotten the correct share weights
    // for the technologies.
    
    calibrateShareWeights( aSector, aPeriod );

    mCurrentSectorName.clear();
}

void CalibrateShareWeightVisitor::startVisitSubsector( const Subsector* aSubsector, const int aPeriod ) {
    // Doing technology calibration in subsector rather than in technology because we will need access 
    // to all of the technologies at the same time.

    // First we need to make sure that the technologies have calculated their costs.
    for( auto techContainer : aSubsector->mTechContainers ) {
        techContainer->getNewVintageTechnology( aPeriod )->calcCost( mCurrentRegionName, mCurrentSectorName, aPeriod );
    }
    
    calibrateShareWeights( aSubsector, aPeriod );
}

void CalibrateShareWeightVisitor::endVisitNestingSubsector( const NestingSubsector* aSubsector, const int aPeriod ) {
    // We need to do this in end visit NestingSubsector because the child subsector
    // can't be calculated until we have gotten the correct share weights from the
    // bottom of the nesting strucutre first.
    
    calibrateShareWeights( aSubsector, aPeriod );
}
