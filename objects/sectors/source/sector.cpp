/*! 
* \file sector.cpp
* \ingroup Objects
* \brief Sector class source file.
* \author Sonny Kim, Steve Smith, Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <string>
#include <fstream>
#include <cassert>
#include <algorithm>

// xml headers

#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "util/base/include/xml_helper.h"
#include "sectors/include/more_sector_info.h"
#include "sectors/include/sector.h"
#include "sectors/include/subsector.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/configuration.h"
#include "util/base/include/summary.h"
#include "containers/include/world.h"
#include "util/base/include/util.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/info_factory.h"
#include "util/logger/include/logger.h"
#include "containers/include/national_account.h"
#include "util/base/include/ivisitor.h"
#include "reporting/include/sector_report.h"
#include "containers/include/iinfo.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief Default constructor.
*
* Constructor initializes member variables with default values, sets vector
* sizes, and sets value of debug flag.
*
* \author Sonny Kim, Steve Smith, Josh Lurz
*/
Sector::Sector( const string regionNameIn ): regionName( regionNameIn ){
    anyFixedCapacity = false;
    mSectorType = getDefaultSectorType();
    mBaseOutput = 0;
    mBasePrice = 0;

    // resize vectors
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    fixedOutput.resize( maxper );
    summary.resize( maxper );
    capLimitsPresent.resize( maxper, false );
}

/*! \brief Destructor
* \details Deletes all subsector objects associated  with this Sector.
* \author Josh Lurz
*/
Sector::~Sector() {
    clear();
}

//! Clear member variables
void Sector::clear(){
    for( SubsectorIterator subSecIter = subsec.begin(); subSecIter != subsec.end(); subSecIter++ ) {
        delete *subSecIter;
    }
}

/*! \brief Returns Sector name
*
* \author Sonny Kim
* \return Sector name as a string
*/
const string& Sector::getName() const {
    return name;
}

/*! \brief Returns The default sector type.
* \author Steve Smith
* \return Default sector type.
*/
const string& Sector::getDefaultSectorType() {
    const static string DEFAULT_SECTOR_TYPE = "Energy";
    return DEFAULT_SECTOR_TYPE;
}

/*! \brief Return the type of the sector.
* \author Steve Smith
* \return The sector type.
*/
const string& Sector::getSectorType() const {
    return mSectorType;
}

/*! \brief Set data members from XML input
*
* \author Josh Lurz
* \param node pointer to the current node in the XML input tree
* \todo josh to add appropriate detailed comment here
*/
void Sector::XMLParse( const DOMNode* node ){
    /*! \pre make sure we were passed a valid node. */
    assert( node );

    // get the name attribute.
    name = XMLHelper<string>::getAttr( node, "name" );

    // Temporary code to warn about no longer read-in demand sector
    // perCapitaBasedString. TODO: Remove this warning.
    if( XMLHelper<bool>::getAttr( node, "perCapitaBased" ) ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "The perCapitaBased attribute is no longer supported and will not be read."
            << " Convert the attribute to an element." << endl;
    }

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
        else if( nodeName == "price" ){
            // Check if the output year is the base year.
            if( XMLHelper<int>::getAttr( curr, "year" ) == modeltime->getStartYear() ){
                mBasePrice = XMLHelper<double>::getValue( curr );
            }
            else {
                // Warning?
            }
        }
        else if( nodeName == "sectorType" ){
            mSectorType = XMLHelper<string>::getValue( curr );
        }
        else if( nodeName == "output" ) {
            // Check if the output year is the base year.
            if( XMLHelper<int>::getAttr( curr, "year" ) == modeltime->getStartYear() ){
                mBaseOutput = XMLHelper<double>::getValue( curr );
            }
            else {
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::WARNING );
                mainLog << "Output level for years other than " << modeltime->getStartYear()
                        << " are not read in."<< endl;
            }
        }
        else if( nodeName == MoreSectorInfo::getXMLNameStatic() ) {
            parseSingleNode( curr, moreSectorInfo, new MoreSectorInfo );
        }
        else if( nodeName == Subsector::getXMLNameStatic() ){
            parseContainerNode( curr, subsec, subSectorNameMap, new Subsector( regionName, name ) );
        }
        else if( XMLDerivedClassParse( nodeName, curr ) ){
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing " << getXMLName() << "." << endl;
        }
    }
}

/*! \brief Complete the initialization
*
* This routine is only called once per model run
*
* \author Josh Lurz
* \param aRegionInfo Regional information object.
* \param aDepFinder Regional dependency finder.
* \param aLandAllocator Regional land allocator.
* \param aGlobalTechDB Global Technology database.
* \warning markets are not necesarilly set when completeInit is called
*/
void Sector::completeInit( const IInfo* aRegionInfo, DependencyFinder* aDepFinder, 
                           ILandAllocator* aLandAllocator, const GlobalTechnologyDatabase* aGlobalTechDB )
{
    // Allocate the sector info.
    mSectorInfo.reset( InfoFactory::constructInfo( aRegionInfo ) );

    // Complete the subsector initializations. 
    for( vector<Subsector*>::iterator subSecIter = subsec.begin(); subSecIter != subsec.end(); subSecIter++ ) {
        ( *subSecIter )->completeInit( mSectorInfo.get(), aDepFinder, aLandAllocator, aGlobalTechDB );
    }
}

/*! \brief Write object to xml output stream
*
* Method writes the contents of this object to the XML output stream.
*
* \author Josh Lurz
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void Sector::toInputXML( ostream& out, Tabs* tabs ) const {
    const Modeltime* modeltime = scenario->getModeltime();

    XMLWriteOpeningTag ( getXMLName(), out, tabs, name );

    // write the xml for the class members.
    XMLWriteElementCheckDefault( mSectorType, "sectorType", out, tabs, getDefaultSectorType() );
    XMLWriteElementCheckDefault( mBasePrice, "price", out, tabs, 0.0, modeltime->getper_to_yr( 0 ) );
    XMLWriteElementCheckDefault( mBaseOutput, "output", out, tabs, 0.0, modeltime->getper_to_yr( 0 ) );

    // write out variables for derived classes
    toInputXMLDerived( out, tabs );

    if( moreSectorInfo.get() ){
        moreSectorInfo->toInputXML( out, tabs );
    }

    // write out the subsector objects.
    for( CSubsectorIterator k = subsec.begin(); k != subsec.end(); k++ ){
        ( *k )->toInputXML( out, tabs );
    }

    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

/*! \brief Write information useful for debugging to XML output stream
*
* Function writes market and other useful info to XML. Useful for debugging.
*
* \author Josh Lurz
* \param period model period
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs.
*/
void Sector::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    XMLWriteOpeningTag ( getXMLName(), out, tabs, name );

    // write the xml for the class members.
    // Write out the data in the vectors for the current period.
    XMLWriteElement( getPrice( period ), "sectorprice", out, tabs );
    XMLWriteElement( getInput( period ), "input", out, tabs );
    XMLWriteElement( getOutput( period ), "output", out, tabs );

    toDebugXMLDerived (period, out, tabs);

    if( moreSectorInfo.get() ){
        moreSectorInfo->toDebugXML( period, out, tabs );
    }
    // Write out the summary
    // summary[ period ].toDebugXML( period, out );

    // write out the subsector objects.
    for( CSubsectorIterator j = subsec.begin(); j != subsec.end(); j++ ){
        ( *j )->toDebugXML( period, out, tabs );
    }

    // finished writing xml for the class members.

    XMLWriteClosingTag( getXMLName(), out, tabs );
}

/*! \brief Perform any initializations needed for each period.
*
* Any initializations or calcuations that only need to be done once per period
* (instead of every iteration) should be placed in this function.
*
* \author Steve Smith
* \param aNationalAccount national accounts
* \param aDemographics demographics object
* \param aPeriod Model period
*/
void Sector::initCalc( NationalAccount* aNationalAccount,
                      const Demographic* aDemographics,
                      const int aPeriod )
{
    // normalizeShareWeights must be called before subsector initializations
    normalizeShareWeights( aPeriod );

    Marketplace* marketplace = scenario->getMarketplace();

    // do any sub-Sector initializations
    for ( unsigned int i = 0; i < subsec.size(); ++i ){
        subsec[ i ]->initCalc( aNationalAccount, aDemographics, moreSectorInfo.get(), aPeriod );
    }

    // set flag if there are any fixed supplies
    if ( getFixedOutput( aPeriod ) > 0 ) {
        anyFixedCapacity = true;
    }

    // find out if this Sector has any capacity limits
    capLimitsPresent[ aPeriod ] = isCapacityLimitsInSector( aPeriod );
}

/*! \brief Perform any sector level calibration data consistancy checks
*
* \author Steve Smith
* \param period Model period
*/
void Sector::checkSectorCalData( const int period ) {
}

/*! \brief check for fixed demands and set values to counter
*
* Sets up the appropriate market within the marketplace for this Sector. Note
* that the type of market is NORMAL -- signifying that this market is a normal
* market that is solved (if necessary).
*
* \author Steve Smith
* \param period Model period
*/
void Sector::tabulateFixedDemands( const int period, const GDP* gdp ) {
    for( vector<Subsector*>::const_iterator j = subsec.begin(); j != subsec.end(); j++ ){
        ( *j )->tabulateFixedDemands( period, mSectorInfo.get() );
    }
}

/*! \brief Scales sub-sector share weights so that they equal number of subsectors.
*
* This is needed so that 1) share weights can be easily interpreted (> 1 means favored) and so that
* future share weights can be consistently applied relative to calibrated years.
*
* \author Steve Smith
* \param period Model period
* \warning This must be done before subsector inits so that share weights are scaled before they are interpolated
*/
void Sector::normalizeShareWeights( const int period ) {

    // If this sector was completely calibrated, or otherwise fixed, then scale shareweights to equal number of subsectors
    if  ( period > 0 && Configuration::getInstance()->getBool( "CalibrationActive" ) ) {
        if ( inputsAllFixed( period - 1, name ) && ( getCalOutput ( period - 1) > 0 ) ) {

            double shareWeightTotal = 0;
            int numberNonzeroSubSectors = 0;
            for ( unsigned int i = 0; i < subsec.size(); ++i ){
                double subsectShareWeight = subsec[ i ]->getShareWeight( period - 1 );
                shareWeightTotal += subsectShareWeight;
                if ( subsectShareWeight > 0 ) {
                    numberNonzeroSubSectors += 1;
                }
            }

            if ( shareWeightTotal < util::getTinyNumber() ) {
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::ERROR );
                mainLog << "ERROR: in sector " << name << " Shareweights sum to zero." << endl;
            } else {
                for ( int unsigned i= 0; i< subsec.size(); i++ ) {
                    subsec[ i ]->scaleShareWeight( numberNonzeroSubSectors / shareWeightTotal, period - 1 );
                }
                ILogger& calibrationLog = ILogger::getLogger( "calibration_log" );
                calibrationLog.setLevel( ILogger::DEBUG );
                calibrationLog << "Shareweights normalized for sector " << name << " in region " << regionName << endl;
            }
        }
    }
}

/*! \brief Test to see if calibration worked for this sector
*
* Compares the sum of calibrated + fixed values to output of sector.
* Will optionally print warning to the screen (and eventually log file).
* 
* If all outputs are not calibrated then this does not check for consistency.
*
* \author Steve Smith
* \param period Model period
* \calAccuracy Accuracy (fraction) to check if calibrations are within.
* \param printWarnings if true prints a warning
* \return Boolean true if calibration is ok.
*/
bool Sector::isAllCalibrated( const int period, double calAccuracy, const bool printWarnings ) const {  
    bool checkCalResult = true; 
    if ( period > 0 && Configuration::getInstance()->getBool( "CalibrationActive" ) ) {
        double calOutputs = getCalOutput( period );
        double totalFixed = calOutputs + getFixedOutput( period );
        double calDiff =  totalFixed - getOutput( period );

        // Two cases to check for. If outputs are all fixed, then calDiff should be small in either case.
        // Even if outputs are not all fixed, then calDiff shouldn't be > calAccuracy (i.e., totalFixedOutputs > actual output)
        if ( calOutputs > 0 ) {
            double diffFraction = calDiff/calOutputs;
            if ( ( ( calDiff > calAccuracy * totalFixed ) && !outputsAllFixed( period ) ) || ( ( abs(diffFraction) > calAccuracy ) && outputsAllFixed( period ) ) ) {
                checkCalResult = false;
                if ( printWarnings ) {
                    ILogger& mainLog = ILogger::getLogger( "main_log" );
                    mainLog.setLevel( ILogger::WARNING );
                    mainLog << "WARNING: " << name << " in " << regionName << " != cal+fixed vals (";
                    mainLog << totalFixed << " )" << " in yr " <<  scenario->getModeltime()->getper_to_yr( period );
                    mainLog << " by: " << calDiff << " (" << diffFraction*100 << "%) " << endl;
                }
            } // calDiff > calAccuracy branch
        }
    }
    return checkCalResult;
}

/*!
* \brief  Calculate subsector shares, adjusting for capacity limits.

* This routine calls subsector::calcShare for each subsector, which calculated an unnormalized 
* share, and then calls normShare to normalize the shares for each subsector.
* The code below also takes into account sectors with fixed output. The sectors without fixed 
* output are normalized to sum/(1-fixedSum) and the sectors with fixed output are reset to their
* fixed share. Note that the fixed share is an approximation, held over from the last iteration, 
* of the actual share of any technology with a fixed output. 
*
* \param period model period
* \param gdp GDP object used to calculate various types of GDPs.
* \author Sonny Kim, Steve Smith, Josh Lurz
* \todo add warnings to sub-Sector and technology (?) that fixed capacity has to be >0
* \warning model with fixed capacity in sectors where demand is not a solved market may not solve
*/
void Sector::calcShare( const int period, const GDP* gdp ) {
    // Note that this solution for the fixed capacity share problem requires that 
    // simultaneity be turned on. This would seem to be because the fixed share is lagged one period
    // and can cause an oscillation. With the demand for this Sector in the marketplace, however, the
    // fixed capacity converges as the trial value for demand converges. Region::findSimul now checks for this.

    double sum = 0;
    double fixedSum = 0;

    // first loop through all subsectors to get the appropriate sums
    for ( unsigned int i = 0; i < subsec.size(); i++ ) {
        // calculate subsector shares (based on technology shares)
        subsec[ i ]->calcShare( period, gdp );

        // sum fixed capacity separately, but don't bother with the extra code if this Sector has none
        // Calculation re-ordered to eliminate subtraction of fixed share from sum which eliminated 
        // a share <> 1 warning when initial (non-fixed) sum was extremely small (2/04 - sjs)
        double fixedShare = 0;
        if ( anyFixedCapacity ) {
            fixedShare = getFixedShare( i , period );
            fixedSum += fixedShare; // keep track of total fixed shares
        }

        // Sum shares that are not fixed
        if ( fixedShare < util::getTinyNumber() ) {
            /*! \invariant Variable subsector shares must always be valid. */
            assert( util::isValidNumber( subsec[ i ]->getShare( period ) ) );
            sum += subsec[ i ]->getShare( period );
        }

        // initialize cap limit status as false for this sector (will be changed in adjSharesCapLimit if necessary)
        subsec[ i ]->setCapLimitStatus( false , period );
    }

    // Take care of case where fixed share is > 1
    double scaleFixedShare = 1;
    if ( fixedSum > 1 ) {
        scaleFixedShare = 1/fixedSum;
        fixedSum = 1;
    }

    // Now normalize shares
    for ( unsigned int i = 0; i < subsec.size(); i++ ) {
        if ( subsec[ i ]->getFixedOutput( period ) == 0 ) {
            // normalize subsector shares that are not fixed
            if ( fixedSum < 1 ) {
                subsec[ i ]->normShare( sum / ( 1 - fixedSum ) , period );  
            } else {
                subsec[ i ]->normShare( sum / util::getTinyNumber() , period ); // if all fixed supply, eliminate other shares
                // Could this overflow if sum was large?-JPL
            }

            // reset share of sectors with fixed supply to their appropriate value
        } else {
            double fixedShare = getFixedShare( i , period ) * scaleFixedShare;
            double currentShare = subsec[ i ]->getFixedShare( period );

            subsec[ i ]->setShareToFixedValue( period );
            if ( currentShare > 0 ) { 
                subsec[ i ]->scaleFixedOutput( fixedShare/currentShare, period ); 
            }
            subsec[ i ]->setShareToFixedValue( period );
        }
    }

    // Now adjust for capacity limits
    // on 10/22/03 adding this check saves about 1/40 of the model run time.
    if ( capLimitsPresent[ period ] ) {
        adjSharesCapLimit( period );
    }

    const static bool debugChecking = Configuration::getInstance()->getBool( "debugChecking" );
    // Check to make sure shares equal 1
    if ( debugChecking ) {
        checkShareSum( period );
    }
}

/*!
* \brief Determine if any capacity limits are exceeded and adjust for shares if so.
*
* If a capacity limit comes into play the routine shifts the "excess" share (over the capacity limits) to the non-limited sectors. This routine loops several times in case this shift then causes another Sector to exceed a capacity limit.
*
* Sectors that have fixed outputs are not adjusted.
* 
* The logic for the share adjustment is as follows:
*     Sum_notlim(Si) + Sum_limited(Si) + sumSharesOverLimit = 1
*     where:
*         Sum_notlim(Si)  = sum of shares that are not capacity limited
*         Sum_limited(Si) = sum of shares that are capacity limited
*         sumSharesOverLimit = portion of shares that were over capacity limits
*         
*     Need to solve for the amount to increase shares to account for capacity limits
*    if:
*         newSi = a * Si, then a few lines of algebra gives:
*         a = 1 + sumSharesOverLimit/Sum_notlim(Si)
*
* \author Steve Smith
* \warning The routine assumes that shares are already normalized.
* \note This runs in O(n^2) -JPL
* \param period Model period
*/
void Sector::adjSharesCapLimit( const int period ) {
    double totalFixedShares = 0;
    bool capLimited = true; // true if any sub-sectors are over their capacity limit
    //bool capLimited = false; // set to false to turn cap limits off for testing

    // check for capacity limits, repeating to take care of any knock-on effects. 
    // Do this a maximum of times equal to number of subsectors, 
    // which is the maximum number of times could possibly need to do this
    for ( unsigned int n = 0; n < subsec.size() && capLimited; n++ ) {
        double sumSharesOverLimit = 0.0;        // portion of shares over cap limits
        double sumSharesNotLimited = 0.0;   // sum of shares not subject to cap limits
        capLimited = false;

        //  Check for capacity limits and calculate sums, looping through each subsector
        for ( unsigned int i = 0; i < subsec.size(); ++i ){
            double tempSubSectShare = subsec[ i ]->getShare( period ) ;

            // if Sector has been cap limited, then return limit, otherwise transform
            // this is needed because can only do the transform once
            double tempCapacityLimit;
            if ( subsec[ i ]->getCapLimitStatus( period ) ) {
                tempCapacityLimit = subsec[ i ]->getShare( period );
            } 
            else {
                tempCapacityLimit = Subsector::capLimitTransform( subsec[ i ]->getCapacityLimit( period ),
                    tempSubSectShare );
            }

            // if there is a capacity limit and are over then set flag and count excess shares
            if ( tempSubSectShare - tempCapacityLimit > util::getSmallNumber() ) {
                capLimited = true;
                sumSharesOverLimit += tempSubSectShare - tempCapacityLimit;
            }

            // also sum shares under limit (but not those just at their limits)
            if ( tempSubSectShare < tempCapacityLimit ) {
                sumSharesNotLimited += tempSubSectShare;
            }

            // But don't count shares that have fixed outputs. 
            // Sectors with fixed outputs are not adjusted in subsec::limitShares below.
            if ( subsec[ i ]->getFixedShare( period ) > 0 ) {
                sumSharesNotLimited -= tempSubSectShare;
                totalFixedShares += subsec[ i ]->getFixedShare( period );
            }

        } // end of loop over sub-sectors

        // re-normalize subsector shares if capacity limits have been exceeded
        // See comments above for derivation of multiplier
        if ( capLimited ) {
            if ( sumSharesNotLimited > 0 ) {
                for ( unsigned int i = 0; i < subsec.size(); ++i ){
                    double multiplier = 1 + sumSharesOverLimit/sumSharesNotLimited;
                    subsec[ i ]->limitShares( multiplier, period );
                }
            }
            // If there are no sectors without limits and there are still shares to be re-distributed
            else if ( sumSharesOverLimit > 0 ) {
                // if there is no shares left then too much was limited!
                cerr << regionName << ": Insufficient capacity to meet demand in Sector " << name << endl;
            }
        }
    } // end for loop

    // if have exited and still capacity limited, then report error
    if ( capLimited ) {
        cerr << "Capacity limit not resolved in Sector " << name << endl;
    }
}

/*! \brief Check that sum of shares is equal to one.
*
* Routine used for checking. Prints an error if shares do not sum to one. 
* Good to run if debugChecking flag is on.
*
* \author Steve Smith
* \param period Model period
*/
void Sector::checkShareSum( const int period ) const {

    double sumshares = 0;
    for ( unsigned int i = 0; i < subsec.size(); ++i ){
        // Check the validity of shares.
        assert( util::isValidNumber( subsec[ i ]->getShare( period ) ) );
        sumshares += subsec[ i ]->getShare( period ) ;
    }
    if ( fabs(sumshares - 1) > util::getSmallNumber() ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Shares do not sum to 1. Sum = " << sumshares << " in Sector " << name;
        mainLog << ", region: " << regionName << endl;
        mainLog << "Shares: ";
        for ( unsigned int i = 0; i < subsec.size(); ++i ){
            mainLog << subsec[ i ]->getShare( period ) << ", ";
        }
        mainLog << endl;
    }
}

/*! \brief Calculate weighted average price of subsectors.
* \author Sonny Kim, Josh Lurz, James Blackwood
* \param period Model period
* \return Weighted sector price.
*/
double Sector::getPrice( const int aPeriod ) const {
    double price = 0;
    for ( unsigned int i = 0; i < subsec.size(); ++i ){ 
        price += subsec[ i ]->getShare( aPeriod ) * subsec[ i ]->getPrice( aPeriod );
    }
    return price;
}

/*! \brief Returns whether all energy usage is calibrated.
* \details Returns if this is an energy sector with all output fixed.
* If this is not an energy sector then also returns true (since energy input is always zero).
* \param aPeriod Model period.
* \return Whether all energy usage is fixed.
* \warning This (and other functionality) will need to change for multiple inputs.
*/
bool Sector::isEnergyUseFixed( const int aPeriod ) const {
    return getSectorType() != "Energy" || outputsAllFixed( aPeriod );
}

/*! \brief Returns true if all sub-Sector outputs are fixed or calibrated.
*
* Routine loops through all the subsectors in the current Sector. If output is calibrated, 
* assigned a fixed output, or set to zero (because share weight is zero) then true is returned. 
* If all ouptput is not fixed, then the Sector has at least some capacity to respond to a change in prices.
*
* \author Steve Smith
* \param period Model period
* \return Boolean that is true if entire Sector is calibrated or has fixed output
*/
bool Sector::outputsAllFixed( const int period ) const {
    assert( period >= 0 );
    for ( unsigned int i = 0; i < subsec.size(); ++i ){
        if ( !(subsec[ i ]->allOutputFixed( period )) ) {
            return false;
        }
    }
    return true;
}

/*! \brief Returns true if any sub-sectors have capacity limits.
*
* Routine checks to see if any capacity limits are present in the sub-sectors. Is used to avoid calling capacity limit calculation unnecessary. *
* \author Steve Smith
* \param period Model period
* \return Boolean that is true if there are any capacity limits present in any sub-Sector
*/
bool Sector::isCapacityLimitsInSector( const int period ) const {

    if ( period < 0 ) {
        return false;
    } 
    for ( unsigned int i = 0; i < subsec.size(); ++i ) {
        if ( !( subsec[ i ]->getCapacityLimit( period ) == 1 ) ) {
            return true;
        }
    }
    return false;
}

/*! \brief Return subsector fixed Supply.
*
* Returns the total amount of fixed supply from all subsectors and technologies.
*
* \author Steve Smith
* \param period Model period
* \param printValues Toggle to print out each value for debugging (default false)
* \return total fixed supply
*/
double Sector::getFixedOutput( const int period, bool printValues ) const {
    double totalfixedOutput = 0;
    for ( unsigned int i = 0; i < subsec.size(); ++i ){
        totalfixedOutput += subsec[ i ]->getFixedOutput( period );
        if ( printValues ) { cout << "sSubSec["<<i<<"] "<< subsec[ i ]->getFixedOutput( period ) << ", "; } 
    }
    return totalfixedOutput;
}

/*! \brief Returns the share of fixed supply from the given subsector using a particular logic, depending on model setup.
*
* This function returns either the saved sub-Sector share, or the share as derived from the marketplace demand, if available.
*
* \author Steve Smith
* \todo This function should be in subsector, not sector-JPL
* \param subsectorNum Subsector number
* \param period Model period
* \return total fixed supply
* \warning Not sure how well using market demand will work if multiple sectors are adding demands. 
*/
double Sector::getFixedShare( const unsigned int subsectorNum, const int period ) const {
    Marketplace* marketplace = scenario->getMarketplace();

    if ( subsectorNum >= 0 && subsectorNum < subsec.size() ) {
        double fixedShare = subsec[ subsectorNum ]->getFixedShare( period );
        if ( fixedShare > 0) {
            // if demand is available through marketplace then use this instead of lagged value
            double mktDmd = marketplace->getDemand( name, regionName, period );
            if ( mktDmd > 0 ) {
                fixedShare = subsec[ subsectorNum ]->getFixedOutput( period ) / mktDmd;
            }
        }
        return fixedShare;
    } 
    else {
        cerr << "Illegal Subsector number: " << subsectorNum << endl;
        return 0;
    }
}

/*! \brief Return subsector total calibrated outputs.
*
* Returns the total calibrated outputs from all subsectors and technologies. 
* Note that any calibrated input values are converted to outputs and are included.
*
* This returns only calibrated outputs, not values otherwise fixed (as fixed or zero share weights)
*
* \author Steve Smith
* \param period Model period
* \return total calibrated outputs
*/
double Sector::getCalOutput( const int period  ) const {
    double totalCalOutput = 0;
    for ( unsigned int i = 0; i < subsec.size(); ++i ){
        totalCalOutput += subsec[ i ]->getTotalCalOutputs( period );
    }
    return totalCalOutput;
}

/*! \brief Return subsector total calibrated outputs for a specified sector type
*
* Variant form of getCalOutput that checks that to see that this sector is of a specific type
*
* \author James Blackwood
* \param period Model period
* \return total calibrated outputs
*/
double Sector::getCalOutput( const int period, const string aSectorType ) const {
    if ( aSectorType == mSectorType ) {
        return getCalOutput( period );
    }
    return 0;
}

/*! \brief Return subsector total fixed or calibrated inputs.
*
* Returns the total fixed inputs from all subsectors and technologies. 
* Note that any calibrated output values are converted to inputs and are included.
*
* \author Steve Smith
* \param period Model period
* \param goodName market good to return inputs for. If equal to the value "allInputs" then returns all inputs.
* \param bothVals optional parameter. It true (default) both calibration and fixed values are returned, if false only calInputs
* \return total fixed inputs
*/
double Sector::getCalAndFixedInputs( const int period, const std::string& goodName, const bool bothVals ) const {
    double totalFixedInput = 0;
    for ( unsigned int i = 0; i < subsec.size(); ++i ){
        totalFixedInput += subsec[ i ]->getCalAndFixedInputs( period, goodName, bothVals );
    }
    return totalFixedInput;
}

/*! \brief Return subsector total fixed or calibrated inputs.
*
* Returns the total fixed inputs from all subsectors and technologies. 
* Note that any calibrated output values are converted to inputs and are included.
*
* \author Steve Smith
* \param period Model period
* \param goodName market good to return inputs for. If equal to the value "allInputs" then returns all inputs.
* \param bothVals optional parameter. It true (default) both calibration and fixed values are returned, if false only calInputs
* \return total fixed inputs
*/
double Sector::getCalAndFixedOutputs( const int period, const std::string& goodName, const bool bothVals ) const {
    double sumCalOutputValues = 0;
    for ( unsigned int i = 0; i < subsec.size(); ++i ){
        sumCalOutputValues += subsec[ i ]->getCalAndFixedOutputs( period, goodName, bothVals );
    }
    return sumCalOutputValues;
}

/*! \brief Sets the input value needed to produce the required output to the marketplace 
*
* \author Steve Smith
* \param period Model period
* \param goodName market good to determine the inputs for.
* \param requiredOutput Amount of output to produce
*/
void Sector::setImpliedFixedInput( const int period, const std::string& goodName, const double requiredOutput ) {
    bool inputWasChanged = false;
    for ( unsigned int i = 0; i < subsec.size(); ++i ){
        if ( !inputWasChanged ) {
            inputWasChanged = subsec[ i ]->setImpliedFixedInput( period, goodName, requiredOutput );
        } else {
            bool tempChange = subsec[ i ]->setImpliedFixedInput( period, goodName, requiredOutput );
            if ( tempChange ) {
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::NOTICE );
                mainLog << "  WARNING: caldemands for more than one subsector were changed " ; 
                mainLog << " in sector " << name << " in region " << regionName << endl; 
            }
        }
    }
}

/*! \brief Returns true if all subsector inputs for the the specified good are fixed.
*
* Fixed inputs can be by either fixedCapacity, calibration, or zero share. 
*
* \author Steve Smith
* \param period Model period
* \param goodName market good to return inputs for. If equal to the value "allInputs" then returns all inputs.
* \return total calibrated inputs
*/
bool Sector::inputsAllFixed( const int period, const std::string& goodName ) const {
    for ( unsigned int i = 0; i < subsec.size(); ++i ){
        if ( !(subsec[ i ]->inputsAllFixed( period, goodName ) ) ){
            return false;
        }
    }
    return true;
}

/*! \brief Scales calibrated values for the specified good.
*
* \author Steve Smith
* \param period Model period
* \param goodName market good to return inputs for
* \param scaleValue multipliciative scaler for calibrated values 
* \return total calibrated inputs
*/
void Sector::scaleCalibratedValues( const int period, const std::string& goodName, const double scaleValue ) {
    for ( unsigned int i = 0; i < subsec.size(); ++i ){
        subsec[ i ]->scaleCalibratedValues( period, goodName, scaleValue );
    }
}

/*! \brief Calibrate Sector output.
*
* This performs supply Sector technology and sub-Sector output/input calibration. 
Determines total amount of calibrated and fixed output and passes that down to the subsectors.

* Note that this routine only performs subsector and technology-level calibration. Total final energy calibration is done by Region::calibrateTFE and GDP calibration is set up in Region::calibrateRegion.
*
* \author Steve Smith
* \param period Model period
*/
void Sector::calibrateSector( const int period ) {
    const double totalfixedOutput = getFixedOutput( period ); 
    const double mrkdmd = scenario->getMarketplace()->getDemand( name, regionName, period ); // demand for the good produced by this Sector
    const double totalCalOutputs = getCalOutput( period );

    for ( unsigned int i = 0; i < subsec.size(); ++i ){
        if ( subsec[ i ]->getCalibrationStatus( period ) ) {
            subsec[ i ]->adjustForCalibration( mrkdmd, totalfixedOutput, totalCalOutputs, outputsAllFixed( period ), period );
        }
    }
}

/*! \brief Calculate GHG emissions for each Sector from subsectors.
*
* Calculates emissions for subsectors and technologies, then updates emissions maps for emissions by gas and emissions by fuel & gas.
*
* Note that at present (10/03), emissions only occur at technology level.
*
* \author Sonny Kim
* \param period Model period
*/
void Sector::emission( const int period ) {
    summary[ period ].clearemiss(); // clear emissions map
    summary[ period ].clearemfuelmap(); // clear emissions fuel map
    for( unsigned int i = 0; i < subsec.size(); ++i ){
        subsec[ i ]->emission( period );
        summary[ period ].updateemiss( subsec[ i ]->getemission( period )); // by gas
        summary[ period ].updateemfuelmap( subsec[ i ]->getemfuelmap( period )); // by fuel and gas
    }
}

/*! \brief Returns sectoral energy consumption.
*
* Routine sums all input energy consumption and puts that into the input variable.
* Sector input is now summed every time this function is called.
*
* \author Sonny Kim
* \param period Model period
* \return total input
*/
double Sector::getInput( const int period ) const {
    double sumInput = 0;
    for( unsigned int i = 0; i < subsec.size(); ++i ){
        sumInput += subsec[ i ]->getInput( period );
    }
    return sumInput;
}

/*! \brief Returns sectoral energy consumption.
*
* Returns all input for energy sectors.
*
* \author Steve Smith
* \param period Model period
* \return total input
* \todo re-impliment this to be market based, not sector based so that this can work with multiple inputs
*/
double Sector::getEnergyInput( const int period ) const {
    if ( getSectorType() == "Energy" ) {
        return getInput( period );
    }
    
    return 0;
}

//! Write Sector output to database.
void Sector::csvOutputFile() const {
    // function protocol
    void fileoutput3( string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);

    // function arguments are variable name, double array, db name, table name
    // the function writes all years
    // total Sector output
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    for( int per = 0; per < maxper; ++per ){
        temp[ per ] = getOutput( per );
    }
    fileoutput3( regionName, name, " ", " ", "production", "EJ", temp );
    for( int per = 0; per < maxper; ++per ){
        temp[ per ] = getInput( per );
    }
    // total Sector eneryg input
    fileoutput3( regionName, name, " ", " ", "consumption", "EJ", temp );
    // Sector price
    for( int per = 0; per < maxper; ++per ){
        temp[ per ] = getPrice( per );
    }
    fileoutput3( regionName, name, " ", " ", "price", "$/GJ", temp );
    // Sector carbon taxes paid
    for( int per = 0; per < maxper; ++per ){
        temp[ per ] = getTotalCarbonTaxPaid( per );
    }
    fileoutput3( regionName, name, " ", " ", "C tax paid", "Mil90$", temp );
}

//! Write subsector output to database.
void Sector::subsec_outfile( const IndirectEmissionsCalculator* aIndirectEmissCalc ) const {
    // do for all subsectors in the Sector
    for( unsigned int i = 0; i < subsec.size(); ++i ){
        // output or demand for each technology
        subsec[ i ]->csvOutputFile( aIndirectEmissCalc );
    }
}

/*! \brief Returns total carbon tax paid by Sector.
*
* \author Sonny Kim
* \param period Model period
* \return total carbon taxes paid
*/
double Sector::getTotalCarbonTaxPaid( const int period ) const {
    double sum = 0;
    for( vector<Subsector*>::const_iterator currSub = subsec.begin(); currSub != subsec.end(); ++currSub ){
        sum += (*currSub)->getTotalCarbonTaxPaid( period );
    }
    return sum;
}

/*! \brief Return fuel consumption map for this Sector
*
* \author Sonny Kim
* \param period Model period
* \todo Input change name of this and other methods here to proper capitilization
* \return fuel consumption map
*/
map<string, double> Sector::getfuelcons( const int period ) const {
    return summary[ period ].getfuelcons();
}

//!  Get the second fuel consumption map in summary object.
/*! \brief Return fuel consumption for the specifed fuel
*
* \author Sonny Kim
* \param period Model period
* \param fuelName name of fuel
* \return fuel consumption
*/
double Sector::getConsByFuel( const int period, const std::string& fuelName ) const {
    return summary[ period ].get_fmap_second( fuelName );
}

/*! \brief Return the ghg emissions map for this Sector
*
* \author Sonny Kim
* \param period Model period
* \return GHG emissions map
*/
map<string, double> Sector::getemission( const int period ) const {
    return summary[ period ].getemission();
}

/*! \brief Return ghg emissions map in summary object
*
* This map is used to calculate the emissions coefficient for this Sector (and fuel?) in region
*
* \author Sonny Kim
* \param period Model period
* \return GHG emissions map
*/
map<string, double> Sector::getemfuelmap( const int period ) const {
    return summary[ period ].getemfuelmap();
}

/*! \brief update summaries for reporting
*
*  Updates summary information for the Sector and all subsectors.
*
* \author Sonny Kim
* \param period Model period
* \return GHG emissions map
*/
void Sector::updateSummary( const list<string>& aPrimaryFuelList, const int period ) {
    // clears Sector fuel consumption map
    summary[ period ].clearfuelcons();

    for( unsigned int i = 0; i < subsec.size(); ++i ){
        // call update summary for subsector
        subsec[ i ]->updateSummary( period );
        // sum subsector fuel consumption for Sector fuel consumption
        summary[ period ].updatefuelcons( aPrimaryFuelList, subsec[ i ]->getfuelcons( period )); 
    }
}

/*! \brief Initialize the marketplaces in the base year to get initial demands from each technology in subsector
* 
* \author Pralit Patel
* \param period The period is usually the base period
*/
void Sector::updateMarketplace( const int period ) {
    for( unsigned int i = 0; i < subsec.size(); i++ ) {
        subsec[ i ]->updateMarketplace( period );
    }
}

/*! \brief Function to finalize objects after a period is solved.
* \details This function is used to calculate and store variables which are only needed after the current
* period is complete. 
* \param aPeriod The period to finalize.
* \todo Finish this function.
* \author Josh Lurz
*/
void Sector::finalizePeriod( const int aPeriod ){
    // Finalize sectors.
    for( SubsectorIterator subsector = subsec.begin(); subsector != subsec.end(); ++subsector ){
        (*subsector)->finalizePeriod( aPeriod );
    }
}

/*! \brief For outputing SGM data to a flat csv File
* 
* \author Pralit Patel
* \param period The period which we are outputing for
*/
void Sector::csvSGMOutputFile( ostream& aFile, const int period ) const {

    // when csvSGMOutputFile() is called, a new sector report is created, updated and printed
    // this function writes a sector report for each sector
    auto_ptr<IVisitor> sectorReport( new SectorReport( aFile ) );
    accept( sectorReport.get(), period );
    sectorReport->finish();
    for( unsigned int i = 0; i < subsec.size(); i++ ) {
        subsec[ i ]->csvSGMOutputFile( aFile, period );
    }    
}

void Sector::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitSector( this, aPeriod );
    for( unsigned int i = 0; i < subsec.size(); i++ ) {
        subsec[ i ]->accept( aVisitor, aPeriod );
    }
    aVisitor->endVisitSector( this, aPeriod );
}
