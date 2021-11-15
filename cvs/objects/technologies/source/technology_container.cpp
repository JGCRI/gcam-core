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
 * \file technology_container.cpp
 * \ingroup Objects
 * \brief TechnologyContainer class source file.
 * \author Pralit Patel
 */
#include "util/base/include/definitions.h"
#include <string>
#include <cassert>

#include "util/base/include/util.h"
#include "technologies/include/technology_container.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/xml_parse_helper.h"
#include "util/base/include/interpolation_rule.h"
#include "technologies/include/stub_technology_container.h"
#include "util/base/include/factory.h"

// technology includes
#include "technologies/include/technology.h"
#include "technologies/include/itechnology.h"
#include "technologies/include/default_technology.h"
#include "technologies/include/intermittent_technology.h"
#include "technologies/include/wind_technology.h"
#include "technologies/include/solar_technology.h"
#include "technologies/include/nuke_fuel_technology.h"
#include "technologies/include/tran_technology.h"
#include "technologies/include/ag_production_technology.h"
#include "technologies/include/pass_through_technology.h"
#include "technologies/include/unmanaged_land_technology.h"
#include "technologies/include/resource_reserve_technology.h"
#include "technologies/include/empty_technology.h"

extern Scenario* scenario;

using namespace std;

//! Constructor
TechnologyContainer::TechnologyContainer()
{
    mInitialAvailableYear = -1;
    mFinalAvailableYear = -1;
    mCachedVintageRangePeriod = -1;
}

//! Destructor
TechnologyContainer::~TechnologyContainer() {
    for( CVintageIterator vintageIt = mVintages.begin(); vintageIt != mVintages.end(); ++vintageIt ) {
        delete ( *vintageIt ).second;
    }
    mVintages.clear();
    
    // just in case null out the period vector as well
    for( int period = 0; period < mVintagesByPeriod.size(); ++period ) {
        mVintagesByPeriod[ period ] = 0;
    }
    
    clearInterpolationRules();
}


/*!
 * \brief Create a deep clone of this technology container.
 * \note that initializations such as in completeInit may still need
 *          to be done.
 * \return A pointer to the cloned technology container.
 */
ITechnologyContainer* TechnologyContainer::clone() const {
    TechnologyContainer* clonedTechContainer = new TechnologyContainer();
    
    clonedTechContainer->mName = mName;
    clonedTechContainer->mInitialAvailableYear = mInitialAvailableYear;
    clonedTechContainer->mFinalAvailableYear = mFinalAvailableYear;
    
    for( CInterpRuleIterator ruleIter = mShareWeightInterpRules.begin(); ruleIter != mShareWeightInterpRules.end(); ++ruleIter ) {
        clonedTechContainer->mShareWeightInterpRules.push_back( ( *ruleIter )->clone() );
    }
    
    for( CVintageIterator vintageIter = mVintages.begin(); vintageIter != mVintages.end(); ++vintageIter ) {
        clonedTechContainer->mVintages[ ( *vintageIter ).first ] = ( *vintageIter ).second->clone();
    }
    
    return clonedTechContainer;
}

/*!
 * \brief Factory method to determine which technologies this container knows about.
 * \param aTechNodeName The xml name to check to see if it is a known technology
 *          type.
 * \return True if the given type is a known technology, false otherwise.
 * \note The list of known technologies here needs to be kept in sync with
 *       the ones found in TechnologyContainer::createAndParseVintage.
 */
bool TechnologyContainer::hasTechnologyType( const string& aTechNodeName ) {
    return ( aTechNodeName == DefaultTechnology::getXMLNameStatic() ||
             aTechNodeName == IntermittentTechnology::getXMLNameStatic() ||
             aTechNodeName == WindTechnology::getXMLNameStatic() ||
             aTechNodeName == SolarTechnology::getXMLNameStatic() ||
             aTechNodeName == NukeFuelTechnology::getXMLNameStatic() ||
             aTechNodeName == TranTechnology::getXMLNameStatic() ||
             aTechNodeName == AgProductionTechnology::getXMLNameStatic() ||
             aTechNodeName == PassThroughTechnology::getXMLNameStatic() ||
             aTechNodeName == UnmanagedLandTechnology::getXMLNameStatic() ||
             aTechNodeName == ResourceReserveTechnology::getXMLNameStatic() );
}

bool TechnologyContainer::XMLParse( rapidxml::xml_node<char>* & aNode) {
    string nodeName = XMLParseHelper::getNodeName(aNode);
    if( nodeName == Technology::getXMLVintageNameStatic() ) {
        rapidxml::xml_node<char>* parentNode = aNode->parent();
        string techType = parentNode ? XMLParseHelper::getNodeName(parentNode) : (*mVintages.begin()).second->getXMLName();
        using value_type = ITechnology*;
        using data_type = ITechnology;
        using FactoryType = Factory<ITechnology::SubClassFamilyVector>;
        
        //string nodeName(aNode->name(), aNode->name_size());
        map<string, string> attrs = XMLParseHelper::getAllAttrs(aNode);
        bool deleteFlagSet = XMLParseHelper::isAttrFlagSet( attrs, "delete" );
        bool noCreateFlagSet = XMLParseHelper::isAttrFlagSet( attrs, "nocreate" );
        attrs["name"] = mName;
        int year = boost::lexical_cast<int>(attrs["year"]);
        
        // We need to try to find in the array if the container exists
        auto dataIter = mVintages.find(year);
        bool found = dataIter != mVintages.end();
        /*bool found = false;
        for( auto currIter = mVintages.begin(); currIter != mVintages.end() && !found; ++currIter ) {
            if( GetFilterForContainer<data_type>::filter_type::matchesXMLAttr( (*currIter).second, attrs ) ) {
                found = true;
                dataIter = currIter;
            }
        }*/
        value_type currContainer = found ? (*dataIter).second : 0;
        if( !found ) {
            // The instance of the container has not been set yet.
            if( deleteFlagSet ) {
                // log delete set but container not found
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::ERROR );
                mainLog << "Could not delete node " << nodeName << " as it does not exist." << std::endl;
                return true;
            } else if( noCreateFlagSet ) {
                // log nocreate
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::NOTICE );
                mainLog << "Did not create node " << nodeName << " as the nocreate input flag was set." << std::endl;
                return true;
            }
            else {
                // No previous container so add a new one.
                
                // Some error checking to make sure the type of class that was created is
                // acutally a subclass of the type aData was declared as.  For instance
                // LandAllocator has a base class ALandAllocatorItem however in
                // RegionMiniCAM::mLandAllocator we want to ensure only the type LandAllocator
                // is created and not for instance a LandLeaf.
                /*typename FactoryType::FamilyBasePtr temp*/currContainer = FactoryType::createType( techType );
                /*currContainer = dynamic_cast<typename decltype( mVintages )::mapped_type>( temp );
                if( temp && !currContainer ) {
                    // log temp->getXMLName() is not a subclass of typename DataType::value_type::getXMLNameStatic()
                    ILogger& mainLog = ILogger::getLogger( "main_log" );
                    mainLog.setLevel( ILogger::ERROR );
                    mainLog << "Attempted to set incompatible type " << nodeName << " as " << typeid(ITechnology).name() << std::endl;
                    abort();
                }*/
                mVintages[ year/*boost::lexical_cast<int>( attrs[ GetFilterForContainer<data_type>::filter_type::getXMLAttrKey() ] )*/ ] = currContainer;
            }
        }
        else {
            // There is already an instance set
            if( deleteFlagSet ) {
                // when we get a delete flag we simply delete and ignore the rest.
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::DEBUG );
                mainLog << "Deleting node: " << nodeName << std::endl;
                delete currContainer;
                currContainer = 0;
                mVintages.erase( dataIter );
                return true;
            }
            else {
                // else we can just use this instance
                // TODO check to make sure the XML names are the same
            }
        }
        
        // parse child nodes
        ParseChildData parseChildHelper(aNode, attrs);
        parseChildHelper.setContainer(currContainer);
        ExpandDataVector<typename data_type::SubClassFamilyVector> getDataVector;
        currContainer->doDataExpansion( getDataVector );
        getDataVector.getFullDataVector(parseChildHelper);
        return true;
    }
    else if( nodeName == InterpolationRule::getXMLNameStatic() ) {
        // just handle the interpolation rule clear
        map<string, string> attrs = XMLParseHelper::getAllAttrs(aNode);
        if( attrs["apply-to"] == "share-weight" && attrs["delete"] == "1" ) {
            clearInterpolationRules();
        }
        InterpolationRule* tempRule = new InterpolationRule();
        ParseChildData parseChildHelper(aNode, attrs);
        parseChildHelper.setContainer(tempRule);
        ExpandDataVector<InterpolationRule::SubClassFamilyVector> getDataVector;
        tempRule->doDataExpansion( getDataVector );
        getDataVector.getFullDataVector(parseChildHelper);
        mShareWeightInterpRules.push_back( tempRule );
        return true;
    }
    return false;
}

void TechnologyContainer::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    for( CVintageIterator vintageIt = mVintages.begin(); vintageIt != mVintages.end(); ++vintageIt ) {
        ( *vintageIt ).second->toDebugXML( aPeriod, aOut, aTabs );
    }

    // Only write out if a rule is present
    if ( mShareWeightInterpRules.begin() != mShareWeightInterpRules.end() ) {
        for( CInterpRuleIterator ruleIt = mShareWeightInterpRules.begin(); ruleIt != mShareWeightInterpRules.end(); ++ruleIt ) {
            (*ruleIt)->toDebugXML( aPeriod, aOut, aTabs );
        }
    }
}

const string& TechnologyContainer::getName() const {
    return mName;
}

void TechnologyContainer::completeInit( const string& aRegionName,
                                        const string& aSectorName,
                                        const string& aSubsectorName,
                                        const IInfo* aSubsecInfo,
                                        ILandAllocator* aLandAllocator )
{
    // Setup the period to vintage vector with the parsed technologies.  Also check
    // for technologies that are not in the initial year to final year range if
    // specified.
    const Modeltime* modeltime = scenario->getModeltime();
    for( VintageIterator vintageIt = mVintages.begin(); vintageIt != mVintages.end(); ) {
        if( mInitialAvailableYear != -1 && ( *vintageIt ).first < mInitialAvailableYear ) {
            // warn a technology exists before the intial year
            /*ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::DEBUG );
            mainLog << "Removing technology " << mName << " vintage " << ( *vintageIt ).first
                << " since it is before the intial year " << mInitialAvailableYear << endl;
            delete ( *vintageIt ).second;
            
            // The erase will invalidate the iterator so we must use the post-increment
            // operator to get the next value before erasing the iterator.
            mVintages.erase( vintageIt++ );*/
            // We can get away with not deleting previous technologies and they will
            // be necessary if the mInitialAvailableYear needs to be interpolated.
            vintageIt++;
        }
        else if( mFinalAvailableYear != -1 && ( *vintageIt ).first > mFinalAvailableYear ) {
            // warn a technology exists after the final investment year
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::DEBUG );
            mainLog << "Removing technology " << mName << " vintage " << ( *vintageIt ).first
                << " since it is after the final investment year " << mFinalAvailableYear << endl;
            delete ( *vintageIt ).second;
            
            // The erase will invalidate the iterator so we must use the post-increment
            // operator to get the next value before erasing the iterator.
            mVintages.erase( vintageIt++ );
        }
        else {
            // Add technologies that are on model years to the vintages by period vector
            // for easy lookup.
            if( modeltime->isModelYear( ( *vintageIt ).first ) ) {
                mVintagesByPeriod[ modeltime->getyr_to_per( ( *vintageIt ).first ) ] = ( *vintageIt ).second;
            }
            
            // did not erase so increment the iterator
            ++vintageIt;
        }
    }
    
    // Finish filling the vintages by period vector with empty technologies or
    // interpolate for missing years.
    for( int period = 0; period < mVintagesByPeriod.size(); ++period ) {
        const int year = modeltime->getper_to_yr( period );
        if( ( mInitialAvailableYear != -1 && year < mInitialAvailableYear ) ||
            ( mFinalAvailableYear != -1 && year > mFinalAvailableYear ) ) {
            mVintagesByPeriod[ period ] = EmptyTechnology::getInstance();
        }
        else if( !mVintagesByPeriod[ period ] ) {
            if( period == 0 ) {
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::ERROR );
                mainLog << "Missing a necessary technology in the base year." << endl;
                mainLog << "Region: " << aRegionName << ", Sector: " << aSectorName
                        << ", Subsector: " << aSubsectorName << ", Technology: "
                        << mName << endl;
                abort();
            }
            
            // We can interpolate a technology to fill this year.
            CVintageIterator tempPrevTech, prevTech;
            tempPrevTech = prevTech = mVintages.lower_bound( 
                                      modeltime->getper_to_yr( period - 1 ) );
            // Use temporary iterator to get next tech to avoid changing prev tech.
            CVintageIterator nextTech = ++tempPrevTech;
            interpolateVintage( year, prevTech, nextTech );
        }
    }
    
    // Now that all interpolated technologies have been created we can call
    // completeInit.
    for( VintageIterator vintageIt = mVintages.begin(); vintageIt != mVintages.end(); ++vintageIt ) {
        // call complete init for all vintages, even those that are not a model year
        ( *vintageIt ).second->completeInit( aRegionName, aSectorName, aSubsectorName,
                                             aSubsecInfo, aLandAllocator );
    }
}

void TechnologyContainer::initCalc( const string& aRegionName, const string& aSectorName,
                                    const IInfo* aSubsecInfo, const Demographic* aDemographic,
                                    const int aPeriod )
{
    
    // Pass forward any emissions information
    if( aPeriod > 0 && mVintagesByPeriod[ aPeriod - 1 ] != EmptyTechnology::getInstance()
        && mVintagesByPeriod[ aPeriod ] != EmptyTechnology::getInstance() )
    {
        vector<string> ghgNames = mVintagesByPeriod[ aPeriod -1]->getGHGNames();
        int numberOfGHGs = mVintagesByPeriod[ aPeriod -1]->getNumbGHGs();
        for ( int j=0 ; j<numberOfGHGs; j++ ) {
            mVintagesByPeriod[ aPeriod ]->copyGHGParameters(
                mVintagesByPeriod[ aPeriod - 1 ]->getGHGPointer( ghgNames[j] ) );
        }
        // We must call initializeTechVintageVector again since we may have just newly
        // created some GHG objects which need to have their TechVintageVectors sized.
        mVintagesByPeriod[ aPeriod ]->initTechVintageVector();
    }
    
    // Initialize the previous period info as having no input set and
    // cumulative Hicks neutral, energy of 1 and it is the first tech.
    PreviousPeriodInfo prevPeriodInfo = { 0, 0, 1, true };
    // Warning: aPeriod is the current model period and not the technology vintage.
    // Currently calls initCalc on all vintages past and future.
    // TODO: Should not call initialization for all future technology vintages beyond the
    // current period but correction causing error (SHK).
    for( VintageIterator vintageIt = mVintages.begin(); vintageIt != mVintages.end(); ++vintageIt ) {
        ( *vintageIt ).second->initCalc( aRegionName, aSectorName, aSubsecInfo, aDemographic,
                                         prevPeriodInfo, aPeriod );
        prevPeriodInfo.mIsFirstTech = false;
    }
    
    // interpolate technology share weights
    interpolateShareWeights( aPeriod );
    
    // Cache the first and last technologies to those that are operating in this period
    // to avoid iterating over more technologies than necessary.  We must be careful to
    // check all past vintages in case the operating technologies are not contiguous.
    mCachedVintageRangePeriod = -1;
    mCachedTechRangeBegin = getVintageBegin( aPeriod );
    mCachedTechRangeEnd = mVintages.rend();
    for( TechRangeIterator it = mCachedTechRangeBegin; it != mVintages.rend(); ++it ) {
        if( mCachedTechRangeEnd != mVintages.rend() && (*it).second->isOperating( aPeriod ) ) {
            // We found a vintage that is still operating so we must reset the end
            // iterator and keep looking for an earlier end point.
            mCachedTechRangeEnd = mVintages.rend();
        }
        else if( mCachedTechRangeEnd == mVintages.rend() && !(*it).second->isOperating( aPeriod ) ) {
            // We have found a vintage that is no longer operating.  This could
            // potentially be our end iterator provided we don't find and earlier
            // vintage that is still operating.
            mCachedTechRangeEnd = it;
        }
    }
    mCachedVintageRangePeriod = aPeriod;
}

void TechnologyContainer::postCalc( const string& aRegionName, const int aPeriod ) {
    for( VintageIterator vintageIt = mVintages.begin(); vintageIt != mVintages.end(); ++vintageIt ) {
        ( *vintageIt ).second->postCalc( aRegionName, aPeriod );
    }
}

ITechnology* TechnologyContainer::getNewVintageTechnology( const int aPeriod ) {
    return mVintagesByPeriod[ aPeriod ];
}

const ITechnology* TechnologyContainer::getNewVintageTechnology( const int aPeriod ) const {
    return mVintagesByPeriod[ aPeriod ];
}

ITechnologyContainer::TechRangeIterator TechnologyContainer::getVintageBegin( const int aPeriod ) {
    // If the given period matches the cached period then we can use the cached
    // begin iterator and avoid having to find it.
    if( aPeriod == mCachedVintageRangePeriod ) {
        return mCachedTechRangeBegin;
    }
    const int year = scenario->getModeltime()->getper_to_yr( aPeriod );
    
    // Lower bound will give us the first technology which is not < year so we must
    // then make sure that it is not > year and is not beyond the final investment
    // year.  In those cases decrease the iterator to make sure we don't go include
    // a technology beyond aPeriod.
    VintageIterator vintageIter = mVintages.lower_bound( year );
    if( vintageIter == mVintages.end() ) {
        --vintageIter;
    }
    else if( ( *vintageIter ).first > year ) {
        return mVintages.rend();
    }
    
    // Converting a forward iterator to a reverse in not completely intuitive.  We
    // need to decrease one from the converted forward iterator to be in the same place.
    return --TechRangeIterator( vintageIter );
}

ITechnologyContainer::CTechRangeIterator TechnologyContainer::getVintageBegin( const int aPeriod ) const {
    // If the given period matches the cached period then we can use the cached
    // begin iterator and avoid having to find it.
    if( aPeriod == mCachedVintageRangePeriod ) {
        return mCachedTechRangeBegin;
    }
    const int year = scenario->getModeltime()->getper_to_yr( aPeriod );
    
    // Lower bound will give us the first technology which is not < year so we must
    // then make sure that it is not > year and is not beyond the final investment
    // year.  In those cases decrease the iterator to make sure we don't go include
    // a technology beyond aPeriod.
    CVintageIterator vintageIter = mVintages.lower_bound( year );
    if( vintageIter == mVintages.end() ) {
        --vintageIter;
    }
    else if( ( *vintageIter ).first > year ) {
        return mVintages.rend();
    }
    
    // Converting a forward iterator to a reverse in not completely intuitive.  We
    // need to decrease one from the converted forward iterator to be in the same place.
    return --CTechRangeIterator( vintageIter );
}

ITechnologyContainer::TechRangeIterator TechnologyContainer::getVintageEnd( const int aPeriod ) {
    // If the given period matches the cached period then we can use the cached
    // end iterator and avoid iterating over unnecessary technologies.
    return aPeriod == mCachedVintageRangePeriod ? mCachedTechRangeEnd : mVintages.rend();
}

ITechnologyContainer::CTechRangeIterator TechnologyContainer::getVintageEnd( const int aPeriod ) const {
    // If the given period matches the cached period then we can use the cached
    // end iterator and avoid iterating over unnecessary technologies.
    return aPeriod == mCachedVintageRangePeriod ? static_cast<CTechRangeIterator>( mCachedTechRangeEnd ) : mVintages.rend();
}

/*!
 * \brief A helper method to delete and clear the vector of interpolation rules.
 */
void TechnologyContainer::clearInterpolationRules() {
    for( CInterpRuleIterator ruleIter = mShareWeightInterpRules.begin(); ruleIter != mShareWeightInterpRules.end(); ++ruleIter ) {
        delete *ruleIter;
    }
    mShareWeightInterpRules.clear();
}

/*!
 * \brief Apply interpolation rules to fill share-weights for technologies.
 * \details Rules will only apply in the first period after calibration.  For
 *          technologies that were interpolated for time-steps that are left
 *          with uninitialized values after rules will have them linearly
 *          interpolated.  Any other uninitialized share-weights will be an error.
 * \param aPeriod The current model period.
 */
void TechnologyContainer::interpolateShareWeights( const int aPeriod ) {
    const Modeltime* modeltime = scenario->getModeltime();
    if( aPeriod != ( modeltime->getFinalCalibrationPeriod() + 1 ) ) {
        return;
    }
    
    objects::PeriodVector<Value> techShareWeights;
    objects::PeriodVector<Value> techParsedShareWeights;
    for( int period = 0; period < mVintagesByPeriod.size(); ++period ) {
        techParsedShareWeights[ period ] = mVintagesByPeriod[ period ]->getParsedShareWeight();
        if( period <= modeltime->getFinalCalibrationPeriod() ) {
            // be sure to get the calibrated share-weight
            techShareWeights[ period ] = mVintagesByPeriod[ period ]->getShareWeight();
        }
        else {
            // initialize the share-weights from the parsed ones
            techShareWeights[ period ] = techParsedShareWeights[ period ];
        }
    }
    for( CInterpRuleIterator ruleIter = mShareWeightInterpRules.begin(); ruleIter != mShareWeightInterpRules.end(); ++ruleIter ) {
        ( *ruleIter )->applyInterpolations( techShareWeights, techParsedShareWeights );
    }
    for( int period = 0; period < mVintagesByPeriod.size(); ++period ) {
        // All periods must have set a share weight value at this point, not having one is an error.
        if( period > modeltime->getFinalCalibrationPeriod() && !techShareWeights[ period ].isInited() ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Found uninitialized share weight in tech: " << mName
                << " in period " << aPeriod << endl;
            abort();
        }
        mVintagesByPeriod[ period ]->setShareWeight( techShareWeights[ period ] );
    }
}

/*!
 * \brief Interpolate a technology vintage given the previous and next vintage.
 * \details The vintage will be interpolated by cloning aPrevTech.  If aNextTech
 *          exists then doInterpolations will be called on the new vintage.  All
 *          datastructures will be updated with the new vintage.
 * \param aYear The year which needs to be interpolated.
 * \param aPrevTech The previous vintage to interpolate from.
 * \param aNextTech The next vintage to interpolate to.
 */
void TechnologyContainer::interpolateVintage( const int aYear, CVintageIterator aPrevTech,
                                              CVintageIterator aNextTech )
{
    /*!
     * \pre The given year does not already exist.
     */
    assert( mVintages.find( aYear ) == mVintages.end() );
    
    /*!
     * \pre The previous vintage must exist.
     */
    assert( aPrevTech != mVintages.end() );
    
    ITechnology* newTech = ( *aPrevTech ).second->clone();
    newTech->setYear( aYear );
    
    if( aNextTech != mVintages.end() ) {
        newTech->doInterpolations( static_cast<Technology*>( ( *aPrevTech ).second ),
                                   static_cast<Technology*>( ( *aNextTech ).second ) );
    }
    mVintages[ aYear ] = newTech;
    
    const Modeltime* modeltime = scenario->getModeltime();
    if( modeltime->isModelYear( aYear ) ) {
        mVintagesByPeriod[ modeltime->getyr_to_per( aYear ) ] = newTech;
    }
}

void TechnologyContainer::accept( IVisitor* aVisitor, const int aPeriod ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    CVintageIterator end;
    int periodToUse;
    // If the period is -1 this means to update output containers for all periods.
    if( aPeriod == -1 ){
        end = mVintages.end();
        periodToUse = modeltime->getmaxper() - 1;
    }
    else {
        int lastTechYearToVisit = modeltime->getper_to_yr( aPeriod );
        // If the technology should not exist yet for this period then there is
        // nothing to visit.
        if( lastTechYearToVisit < mInitialAvailableYear ) {
            return;
        }
        
        // If the technology was no longer available for investment in the current
        // period then just visit to the last available period.
        if( mFinalAvailableYear != -1 && lastTechYearToVisit > mFinalAvailableYear ) {
            end = mVintages.end();
        }
        else {
            end = mVintages.find( modeltime->getper_to_yr( aPeriod ) );
            
            // aPeriod must correspond to a valid technology
            assert( end != mVintages.end() );
            
            // Increase end by one so that it is pointing to the first technology
            // that we don't want to visit.
            ++end;
        }
        periodToUse = aPeriod;
    }
    
    for( CVintageIterator vintageIt = mVintages.begin(); vintageIt != end; ++vintageIt ) {
        ( *vintageIt ).second->accept( aVisitor, periodToUse );
    }
}
