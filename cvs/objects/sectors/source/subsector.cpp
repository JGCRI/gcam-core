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
* \file subsector.cpp
* \ingroup Objects
* \brief Subsector class source file.
* \author Sonny Kim, Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <vector>
#include <algorithm>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include <cmath>

#include "util/base/include/configuration.h"
#include "sectors/include/subsector.h"
#include "technologies/include/itechnology_container.h"
#include "technologies/include/technology_container.h"
#include "technologies/include/stub_technology_container.h"
#include "technologies/include/itechnology.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/gdp.h"
#include "containers/include/info_factory.h"
#include "containers/include/iinfo.h"
#include "util/base/include/ivisitor.h"
#include "sectors/include/sector_utils.h"
#include "util/base/include/interpolation_rule.h"
#include "functions/include/idiscrete_choice.hpp"
#include "functions/include/discrete_choice_factory.hpp"

using namespace std;
using namespace xercesc;
using namespace objects;

extern Scenario* scenario;

/*! \brief Default constructor.
*
* Constructor initializes member variables with default values, sets vector sizes, etc.
*
* \author Sonny Kim, Steve Smith, Josh Lurz
*/
Subsector::Subsector( const string& aRegionName, const string& aSectorName )
{
    mRegionName = aRegionName;
    mSectorName = aSectorName;
    mDiscreteChoiceModel = 0;
}

/*! \brief Default destructor.
*
* deletes all Technology objects associated  with this sector.
*
* \author Josh Lurz
*/
Subsector::~Subsector() {
    clear();
}

//! Deallocate the subsector memory.
void Subsector::clear(){
    for( CTechIterator techIter = mTechContainers.begin(); techIter != mTechContainers.end(); ++techIter ) {
        delete *techIter;
    }
    
    delete mDiscreteChoiceModel;
    clearInterpolationRules();
}

/*!
 * \brief A helper method to delete and clear the vector of interpolation rules.
 */
void Subsector::clearInterpolationRules() {
    for( CInterpRuleIterator ruleIter = mShareWeightInterpRules.begin(); ruleIter != mShareWeightInterpRules.end(); ++ruleIter ) {
        delete *ruleIter;
    }
    mShareWeightInterpRules.clear();
}

/*! \brief Returns sector name
*
* \author Sonny Kim
* \return sector name as a string
*/
const string& Subsector::getName() const {
    return mName;
}

//! Initialize Subsector with xml data
void Subsector::XMLParse( const DOMNode* node ) {
    /*! \pre Make sure we were passed a valid node. */
    assert( node );

    // get the name attribute.
    mName = XMLHelper<string>::getAttr( node, "name" );

    // get all child nodes.
    DOMNodeList* nodeList = node->getChildNodes();

    const Modeltime* modeltime = scenario->getModeltime();

    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        else if( nodeName == "share-weight" ){
            XMLHelper<Value>::insertValueIntoVector( curr, mParsedShareWeights, modeltime );
        }
        else if( DiscreteChoiceFactory::isOfType( nodeName ) ) {
            parseSingleNode( curr, mDiscreteChoiceModel, DiscreteChoiceFactory::create( nodeName ).release() );
        }
        else if( nodeName == "fuelprefElasticity" ){
            XMLHelper<double>::insertValueIntoVector( curr, mFuelPrefElasticity, modeltime );  
        }
        else if( nodeName == InterpolationRule::getXMLNameStatic() && XMLHelper<string>::getAttr( curr, "apply-to" ) == "share-weight" ) {
            // if the delete flag is set then for interpolation rules that means to clear
            // out any previously parsed rules
            if( XMLHelper<bool>::getAttr( curr, "delete" ) ) {
                clearInterpolationRules();
            }

            InterpolationRule* tempRule = new InterpolationRule();
            tempRule->XMLParse( curr );
            mShareWeightInterpRules.push_back( tempRule );
        }
        else if( TechnologyContainer::hasTechnologyType( nodeName ) ) {
            parseContainerNode( curr, mTechContainers, new TechnologyContainer );
        }
        else if( nodeName == StubTechnologyContainer::getXMLNameStatic() ) {
            parseContainerNode( curr, mTechContainers, new StubTechnologyContainer );
        }
        // parsed derived classes
        else if( !XMLDerivedClassParse( nodeName, curr ) ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel(ILogger::WARNING);
            mainLog << "Unknown element " << nodeName << " encountered while parsing " << getXMLName() << endl;
        }
    }
}

//! Parses any input variables specific to derived classes
bool Subsector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    // do nothing
    // defining method here even though it does nothing so that we do not
    // create an abstract class.
    return false;
}

/*! \brief Write information useful for debugging to XML output stream
*
* Function writes market and other useful info to XML. Useful for debugging.
*
* \author Josh Lurz
* \param period model period
* \param out reference to the output stream
*/
void Subsector::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
    
    XMLWriteOpeningTag( getXMLName(), out, tabs, mName );
    
    // Write the data for the current period within the vector.
    XMLWriteElement( mShareWeights[ period ], "share-weight", out, tabs );
    mDiscreteChoiceModel->toDebugXML( period, out, tabs );
    XMLWriteElement( mFuelPrefElasticity[ period ], "fuelprefElasticity", out, tabs );
    XMLWriteElement( getEnergyInput( period ), "input", out, tabs );
    XMLWriteElement( getOutput( period ), "output", out, tabs );
    XMLWriteElement( getTotalCalOutputs( period ), "total-cal-outputs", out, tabs );
    XMLWriteElement( containsOnlyFixedOutputTechnologies( period ), "fixed-output-only", out, tabs );

    for( CInterpRuleIterator ruleIt = mShareWeightInterpRules.begin(); ruleIt != mShareWeightInterpRules.end(); ++ruleIt ) {
        (*ruleIt)->toDebugXML( period, out, tabs );
    }
    
    toDebugXMLDerived( period, out, tabs );
    // write out the Technology objects.
    
    for( CTechIterator techIter = mTechContainers.begin(); techIter != mTechContainers.end(); ++techIter ) {
        (*techIter)->toDebugXML( period, out, tabs );
    }    
    
    // finished writing xml for the class members.
    
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& Subsector::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& Subsector::getXMLNameStatic() {
    static const string XML_NAME = "subsector";
    return XML_NAME;
}

/*! \brief Complete the initialization
*
* This routine is only called once per model run
* \param aSectorInfo The parent sector info object.
* \param aLandAllocator Regional land allocator.
* \author Josh Lurz
* \warning markets are not necessarily set when completeInit is called
*/
void Subsector::completeInit( const IInfo* aSectorInfo,
                              ILandAllocator* aLandAllocator )
{
    if( !mDiscreteChoiceModel ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "No Discrete Choice function set in " << mRegionName << ", "
                << mSectorName << ", " << mName << endl;
        abort();
    }

    mSubsectorInfo.reset( InfoFactory::constructInfo( aSectorInfo, mRegionName + "-" + mSectorName + "-" + mName ) );
    
    for ( TechIterator techIter = mTechContainers.begin(); techIter != mTechContainers.end(); ++techIter ) {
        (*techIter)->completeInit( mRegionName, mSectorName, mName, mSubsectorInfo.get(), aLandAllocator );
    }    
    
    const Modeltime* modeltime = scenario->getModeltime();
    // Initialize working share weights with parsed share weights up to and including
    // final calibration period.
    for( int per = 0; per <= modeltime->getFinalCalibrationPeriod(); ++per ) {
       if( mParsedShareWeights[ per ].isInited() ) {
           mShareWeights[ per ] = mParsedShareWeights[ per ];
       }
    }
}

/*!
* \brief Perform any initializations needed for each period.
* \details Perform any initializations or calculations that only need to be done
*          once per period (instead of every iteration) should be placed in this
*          function.
* \warning The ghg part of this routine assumes the existence of technologies in
*          the previous and future periods
* \author Steve Smith, Sonny Kim
* \param aNationalAccount National accounts container.
* \param aDemographics Regional demographics container.
* \param aPeriod Model period
*/
void Subsector::initCalc( NationalAccount* aNationalAccount,
                          const Demographic* aDemographics,
                          const int aPeriod )
{
    mDiscreteChoiceModel->initCalc( mRegionName, mName, false, aPeriod );
    
    // Initialize all technologies.
    for( TechIterator techIter = mTechContainers.begin(); techIter != mTechContainers.end(); ++techIter ) {
        (*techIter)->initCalc( mRegionName, mSectorName, mSubsectorInfo.get(), aDemographics, aPeriod );
    }

    // If calibration is active, reinitialize share weights.
    if( Configuration::getInstance()->getBool( "CalibrationActive" ) ){
        // Check for zero shareweight for subsector with calibration values
        if( getTotalCalOutputs( aPeriod ) > util::getSmallNumber() 
            && mShareWeights[ aPeriod ] == 0 ) 
        {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::NOTICE );
            mainLog << "Resetting zero shareweight for Subsector " << getName()
                << " in sector " << mSectorName << " in region " << mRegionName
                << " since calibration values are present." << endl;
            mShareWeights[ aPeriod ] = 1.0;
        }

        // Reinitialize share weights to 1 for competing subsector with non-zero read-in share weight
        // for calibration periods only, but only if calibration values are read in any of the technologies
        // in this subsector (as there are cases where you want to fix these shares on a pass-through sector).
        else if( mShareWeights[ aPeriod ] != 0 && aPeriod <= scenario->getModeltime()->getFinalCalibrationPeriod()
                && getTotalCalOutputs( aPeriod ) > 0.0 ){
            // Reinitialize to 1 to remove bias, calculate new share weights and
            // normalize in postCalc to anchor to dominant subsector.
            mShareWeights[ aPeriod ] = 1.0;
        }
    }

    // For subsectors with only fixed output technologies or all null technology share weights.
    if( containsOnlyFixedOutputTechnologies( aPeriod ) ){
        // Reset share weight for all periods to 0 to indicate that it does not have an 
        // impact on the results.
        mParsedShareWeights[ aPeriod ].set( 0 );
        mShareWeights[ aPeriod ].set( 0 );
    }

    // Apply share weight interpolation rules and fill in missing share weights.
    interpolateShareWeights( aPeriod );
}

/*! \brief Returns the subsector price.
* \details Calculates and returns share-weighted total price (subsectorprice)
*          and cost of fuel (fuelprice). 
* \author Sonny Kim
* \param aGDP Regional GDP object.
* \param aPeriod Model period
*/
double Subsector::getPrice( const GDP* aGDP, const int aPeriod ) const {
    double subsectorPrice = 0.0; // initialize to 0 for summing
    double sharesum = 0.0;
    const vector<double>& techShares = calcTechShares( aGDP, aPeriod );
    for ( unsigned int i = 0; i < mTechContainers.size(); ++i ) {
        double currCost = mTechContainers[i]->getNewVintageTechnology(aPeriod)->getCost( aPeriod );
        // calculate weighted average price for Subsector.
        /*!
         * \note Negative prices may be produced and are valid.
         */
        subsectorPrice += techShares[ i ] * currCost;
        sharesum += techShares[i];
    }

    if( sharesum < util::getSmallNumber() ) {
        // None of the technologies have a valid share.  Set the price
        // to NaN.  This gets tested in calcShare(), and any subsector
        // with a NaN price gets a share of zero.  Therefore, as long
        // as you use only subsectors with positive shares, you will
        // never see the NaN price.
        return numeric_limits<double>::signaling_NaN();
    }
    else {
        return subsectorPrice;
    }
}


/*! \brief returns Subsector fuel price times share
* \details Returns the share-weighted fuel price, which is later summed to get
*          the sector-weighted fuel price.
* \author Sonny Kim
* \param aGDP GDP container.
* \param aPeriod Model period.
* \return share-weighted fuel price
*/
double Subsector::getAverageFuelPrice( const GDP* aGDP, const int aPeriod ) const {
    // Determine the average fuel price.
    double fuelPrice = 0;

    // The base period is not solved so the current shares can be calculated and
    // used. In future periods the previous period's shares must be used as the
    // current period's are unknown.
    const int sharePeriod = ( aPeriod == 0 ) ? aPeriod : aPeriod - 1;

    const vector<double>& techShares = calcTechShares( aGDP, sharePeriod );
    for ( unsigned int i = 0; i < mTechContainers.size(); ++i) {
        // calculate weighted average price of fuel only
        // Technology shares are based on total cost
        fuelPrice += techShares[ i ] * mTechContainers[i]->getNewVintageTechnology( aPeriod )->getEnergyCost( mRegionName, mSectorName, aPeriod );
    }
    /*! \post Fuel price must be positive. */
    assert( fuelPrice >= 0 );
    return fuelPrice;
}

/*! \brief calculate Technology shares within Subsector
*
* Calls Technology objects to first calculate cost, then their share. Follows this by normalizing shares. 
*
* \author Marshall Wise, Josh Lurz
* \param mRegionName region name
* \param period model period
* \return A vector of technology shares.
*/
const vector<double> Subsector::calcTechShares( const GDP* aGDP, const int aPeriod ) const {
    vector<double> logTechShares ( mTechContainers.size() ); 

    for( unsigned int i = 0; i < mTechContainers.size(); ++i ){
        // determine shares based on Technology costs
        double lts = mTechContainers[ i ]->getNewVintageTechnology( aPeriod )->
            calcShare( mDiscreteChoiceModel, aGDP, aPeriod );

        // Check that Technology shares are valid.
        assert( util::isValidNumber( lts ) || lts == -numeric_limits<double>::infinity() );
        logTechShares[ i ] = lts;
    }
    // Normalize technology shares.  After normalization they will be
    // shares, not log(shares).
    SectorUtils::normalizeLogShares( logTechShares );

    return logTechShares;
}

/*!
* \brief Calculate the cost of the Subsector.
* \details Instructs all technologies to calculate their costs. The subsector
*          can calculate it's costs dynamically once all Technologies have
*          calculated their costs, so the Subsector cost is not stored.
* \param aPeriod Model period.
*/
void Subsector::calcCost( const int aPeriod ){
    // Instruct all technologies up to and including the current period to
    // calculate their costs. Future Technologies cannot have a cost as they do
    // not yet exist.
    for( TechIterator techIter = mTechContainers.begin(); techIter != mTechContainers.end(); ++techIter ) {
        for( ITechnologyContainer::TechRangeIterator vintageIter = (*techIter)->getVintageBegin( aPeriod ); vintageIter != (*techIter)->getVintageEnd( aPeriod ); ++vintageIter ) {
            (*vintageIter).second->calcCost( mRegionName, mSectorName, aPeriod );
        }
    }
}

/*! \brief calculate Subsector unnormalized shares 
 * \details Calculates the unormalized share using the discrete choice function
 *          set in the Sector nested above.
 * \author Sonny Kim, Josh Lurz
 * \param aChoiceFn Discrete choice model for the subsector competition within
 *                  the sector.
 * \param aGDP gdp object
 * \param aPeriod model period
 * \warning There is no difference between demand and supply technologies.
 *          Control behavior with value of parameter mFuelPrefElasticity
 * \return The log of the subsector share.
 * \sa Technology::calcShare()
*/
double Subsector::calcShare( const IDiscreteChoice* aChoiceFn, const GDP* aGDP, const int aPeriod ) const {
    double subsectorPrice = getPrice( aGDP, aPeriod );

    if( std::isnan( subsectorPrice ) ) {
        // Check for a NaN sentinel value.  If we find it, set the
        // subsector's share to zero.
        return -numeric_limits<double>::infinity();
    }

    
    double scaledGdpPerCapita = aGDP->getBestScaledGDPperCap( aPeriod );
    assert( scaledGdpPerCapita > 0.0 );

    double logshare = aChoiceFn->calcUnnormalizedShare( mShareWeights[ aPeriod ], subsectorPrice, aPeriod )
        + mFuelPrefElasticity[ aPeriod ] * log( scaledGdpPerCapita );

    /*! \post logshare is finite or minus-infinity. */
    // Check for invalid shares.
    if( !( util::isValidNumber( logshare ) || logshare == -numeric_limits<double>::infinity() ) ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Invalid share for " << mName << " in " << mRegionName 
                << " log(share) =  " << logshare << endl;
    }
    return logshare;
}


/*! \brief Return the total fixed Technology output for this subsector.
* \details Fixed output may come from vintaged production or exogenously 
*          specified.
* \author Steve Smith
* \param aPeriod model period
* \param aMarginalRevenue The marginal revenue from the sector which may be necessary
*                         for the technology to calculate it's level of fixed output.
*/
double Subsector::getFixedOutput( const int aPeriod, const double aMarginalRevenue ) const {
    double fixedOutput = 0;
    for( CTechIterator techIter = mTechContainers.begin(); techIter != mTechContainers.end(); ++techIter ) {
        ITechnologyContainer::CTechRangeIterator endIter = (*techIter)->getVintageEnd( aPeriod );
        for( ITechnologyContainer::CTechRangeIterator vintageIter = (*techIter)->getVintageBegin( aPeriod ); vintageIter != endIter ; ++vintageIter ) {
            double currFixedOutput = (*vintageIter).second->getFixedOutput( mRegionName, mSectorName, false, "", aMarginalRevenue, aPeriod );
            /*! \invariant Fixed output for each Technology must be -1 or
            *              positive. 
            */
            assert( fixedOutput == -1 || fixedOutput >= 0 );
            if( currFixedOutput > 0 ){
                fixedOutput += currFixedOutput;
            }
        }
    }
    /*! \post Fixed output total must be positive. */
    assert( fixedOutput >= 0 );
    return fixedOutput;
}

/*!
 * \brief Apply share weight interpolation rules for the subsector
 * \details Rules will only apply in the first period after calibration.  It
 *          is an error to have uninitialized share weights after rules have
 *          been applied.
 * \param aPeriod Current model period.
 */
void Subsector::interpolateShareWeights( const int aPeriod ) {
    // Do not apply rules if this is not the first period after calibration.
    const Modeltime* modeltime = scenario->getModeltime();
    if( aPeriod != ( modeltime->getFinalCalibrationPeriod() + 1 ) ) {
        return;
    }

    // Make sure that calibrated values get stored back into the parsed share weights vector so that
    // they get written out.  All other parsed values will initialize the working share weights
    for( int per = 0; per < mParsedShareWeights.size(); ++per ) {
        if( per <= modeltime->getFinalCalibrationPeriod() ) {
            mParsedShareWeights[ per ].set( mShareWeights[ per ] );
        }
        else if( mParsedShareWeights[ per ].isInited() )  {
            mShareWeights[ per ] = mParsedShareWeights[ per ];
        }
    }
    for( CInterpRuleIterator ruleIt = mShareWeightInterpRules.begin(); ruleIt != mShareWeightInterpRules.end(); ++ruleIt ) {
        (*ruleIt)->applyInterpolations( mShareWeights, mParsedShareWeights );
    }

    // Fill in missing period vectors such as from time-step functionality.
    // All missing values are filled, whether intended or not.
    SectorUtils::fillMissingPeriodVectorInterpolated( mShareWeights );

    // All periods must have set a share weight value at this point, not having one is an error.
    for( int per = modeltime->getFinalCalibrationPeriod() + 1; per < mShareWeights.size(); ++per ) {
        if( !mShareWeights[ per ].isInited() ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Found uninitialized share weight in subsector: " << mName << " in period " << per << endl;
            abort();
        }
    }
}

/*! \brief The demand passed to this function is shared out at the Technology
*          level.
* \details Variable demand (could be energy or energy service) is passed to
*          technologies and then shared out at the Technology level.
* \author Sonny Kim, Josh Lurz
* \param aSubsectorVariableDemand Total variable demand for this subsector.
* \param aFixedOutputScaleFactor Scale factor to scale down fixed output
*        technologies.
* \param aPeriod Model period
* \param aGDP Regional GDP container.
*/
void Subsector::setOutput( const double aSubsectorVariableDemand, 
                           const double aFixedOutputScaleFactor,
                           const GDP* aGDP,
                           const int aPeriod )
{
    assert( util::isValidNumber( aSubsectorVariableDemand ) && aSubsectorVariableDemand >= 0 );
    
    // Calculate the technology shares.
    const vector<double>& shares = calcTechShares( aGDP, aPeriod );
    for( TechIterator techIter = mTechContainers.begin(); techIter != mTechContainers.end(); ++techIter ) {
        ITechnologyContainer::TechRangeIterator vintageIter = (*techIter)->getVintageBegin( aPeriod );
        
        // The first year is the current vintage, only pass variable output to current vintage.
        // Make sure that a new vintage technology exists for production.
        if( vintageIter != (*techIter)->getVintageEnd( aPeriod ) ) {
            (*vintageIter).second->production( mRegionName, mSectorName,
                                            aSubsectorVariableDemand * shares[ techIter - mTechContainers.begin() ],
                                            aFixedOutputScaleFactor, aGDP, aPeriod );
            ++vintageIter;
        }
        
        // Loop over old vintages which do not get variable demand.
        for( ; vintageIter != (*techIter)->getVintageEnd( aPeriod ); ++vintageIter ) {
            // calculate Technology output and fuel input for past vintages
            (*vintageIter).second->production( mRegionName, mSectorName, 0,
                                            aFixedOutputScaleFactor, aGDP, aPeriod );
        }
    }
}

/*! \brief Test to see if calibration worked for this subsector
* \author Josh Lurz
* \param aPeriod The model period.
* \param aCalAccuracy Accuracy (fraction) to check if calibrations are within.
* \param aPrintWarnings Whether to print a warning.
* \return Whether calibration was successful.
*/
bool Subsector::isAllCalibrated( const int aPeriod, double aCalAccuracy, const bool aPrintWarnings ) const {
    // Check if each technology is calibrated.
    bool isAllCalibrated = true;
    for( CTechIterator techIter = mTechContainers.begin(); techIter != mTechContainers.end(); ++techIter ) {
        bool isThisTechCalibrated = (*techIter)->getNewVintageTechnology( aPeriod )->isAllCalibrated( aPeriod, aCalAccuracy,
                                               mRegionName, mSectorName, mName, aPrintWarnings );
 
        // if this (or any) technology not calibrated, indicate that subsector is not calibrated
        if (!isThisTechCalibrated) {
                isAllCalibrated = false;
        }
    }

    // if all technologies are calibrated, return true for the subsector, otherwise return false
    return isAllCalibrated;

}

/*! \brief returns the total calibrated output from this sector.
*
* Routine adds up calibrated values from both the sub-sector and (if not
* calibrated at Subsector), Technology levels. This returns only calibrated
* outputs, not values otherwise fixed (as fixed or zero share weights)
*
* \author Steve Smith
* \param period Model period
* \return Total calibrated output for this Subsector
*/
double Subsector::getTotalCalOutputs( const int period ) const {
    double sumCalValues = 0;
    for( CTechIterator techIter = mTechContainers.begin(); techIter != mTechContainers.end(); ++techIter ) {
        double currCalOutput = (*techIter)->getNewVintageTechnology( period )->getCalibrationOutput( false, "", period );
        if( currCalOutput > 0 ){
            sumCalValues += currCalOutput;
        }
    }
    /*! \post Total calibrated output is greater than or equal to zero. */
    assert( sumCalValues >= 0 );

    return sumCalValues;
}

/*! \brief returns true if all output is either fixed or calibrated.
*
* If output is is calibrated, fixed, or share weight is zero for this Subsector or all technologies in this subsector returns true.
*
* \author Steve Smith
* \param period Model period
* \return Total calibrated output for this Subsector
*/
bool Subsector::allOutputFixed( const int period ) const {
    // If there is no shareweight for this subsector than it cannot produce any
    // output, and so the output must be fixed.
    if( util::isEqual( mShareWeights[ period ].get(), 0.0 ) ){
        return true;
    }

    // if not fixed at sub-sector level, then check at the Technology level
    for( CTechIterator techIter = mTechContainers.begin(); techIter != mTechContainers.end(); ++techIter ) {
        if ( !(*techIter)->getNewVintageTechnology( period )->isOutputFixed( false, "", period ) ) {
            return false;
        }
    }
    
    return true;
}

/*!\brief Returns a boolean for whether the subsector contains only fixed output technologies
* or at least one technology that competes on the margin.
*\author Sonny Kim
*\return Boolean for determining whether subsector contains only fixed output technologies.
*/
bool Subsector::containsOnlyFixedOutputTechnologies( const int aPeriod ) const {
    // Returns true if all technologies in the subsector has fixed exogenous outputs
    // for new investments.
    for( CTechIterator techIter = mTechContainers.begin(); techIter != mTechContainers.end(); ++techIter ) {
        // If at least one technology does not have fixed output return false.
        // Do not consider a technology that has a zero share weight
        const ITechnology* newVintageTech = (*techIter)->getNewVintageTechnology( aPeriod );
        if ( newVintageTech->getShareWeight() != 0 
            && !newVintageTech->isFixedOutputTechnology( aPeriod ) ) {
            return false;
        }
    }
    // Otherwise subsector contains only fixed output technologies or all technology share
    // weights are null.
    return true;
}

/*! \brief returns share weight for this Subsector
*
* Needed so that share weights can be scaled by sector
*
* \author Steve Smith
* \param period Model period
* \return share weight
*/
double Subsector::getShareWeight( const int period ) const {
    /*! \post Shareweight is valid and greater than or equal to zero. */
    assert( util::isValidNumber( mShareWeights[ period ] ) && mShareWeights[ period ] >= 0 );
    return mShareWeights[ period ];
}

/*! \brief returns Subsector output
*
* output summed every time to ensure consistency
* this is never called for demand sectors!
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
* \return sector output
*/
double Subsector::getOutput( const int period ) const {
    /*! \pre period is less than max period. */
    assert( period < scenario->getModeltime()->getmaxper() );
    double outputSum = 0;
    for( CTechIterator techIter = mTechContainers.begin(); techIter != mTechContainers.end(); ++techIter ) {
        ITechnologyContainer::CTechRangeIterator endIter = (*techIter)->getVintageEnd( period );
        for( ITechnologyContainer::CTechRangeIterator vintageIter = (*techIter)->getVintageBegin( period ); vintageIter != endIter; ++vintageIter ) {
            outputSum += (*vintageIter).second->getOutput( period );
        }
    }

    /*! \post Total subsector output is positive. */
    assert( outputSum >= 0 );
    return outputSum;
}

/*!
 * \brief Get the energy input for the Subsector.
 * \param aPeriod Period.
 * \return Total energy input.
 */
double Subsector::getEnergyInput( const int aPeriod ) const {
    double totalEnergy = 0;
    for( CTechIterator techIter = mTechContainers.begin(); techIter != mTechContainers.end(); ++techIter ) {
        ITechnologyContainer::CTechRangeIterator endIter = (*techIter)->getVintageEnd( aPeriod );
        for( ITechnologyContainer::CTechRangeIterator vintageIter = (*techIter)->getVintageBegin( aPeriod ); vintageIter != endIter; ++vintageIter ) {
            totalEnergy += (*vintageIter).second->getEnergyInput( aPeriod );
        }
    }
    return totalEnergy;
}

/*! \brief Function to finalize objects after a period is solved.
* \details This function is used to calculate and store variables which are only needed after the current
* period is complete. 
* \param aPeriod The period to finalize.
* \author Josh Lurz, Sonny Kim
*/
void Subsector::postCalc( const int aPeriod ){
    // Finalize all technologies in all periods.
    for( TechIterator techIter = mTechContainers.begin(); techIter != mTechContainers.end(); ++techIter ) {
        (*techIter)->postCalc( mRegionName, aPeriod );
    }
}

void Subsector::accept( IVisitor* aVisitor, const int period ) const {
    aVisitor->startVisitSubsector( this, period );
    const Modeltime* modeltime = scenario->getModeltime();
    for( CTechIterator techIter = mTechContainers.begin(); techIter != mTechContainers.end(); ++techIter ) {
        (*techIter)->accept( aVisitor, period );
    }
            
    aVisitor->endVisitSubsector( this, period );
}
