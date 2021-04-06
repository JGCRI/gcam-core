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
* \file fractional_secondary_output.cpp
* \ingroup Objects
* \brief FractionalSecondaryOutput class source file.
* \author Pralit Patel
*/
#include "util/base/include/definitions.h"
#include <string>
#include <xercesc/dom/DOMNodeList.hpp>
#include "util/base/include/xml_helper.h"
#include "technologies/include/fractional_secondary_output.h"
#include "containers/include/scenario.h"
#include "containers/include/iinfo.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/market_dependency_finder.h"
#include "functions/include/function_utils.h"
#include "util/curves/include/point_set_curve.h"
#include "util/curves/include/curve.h"
#include "util/curves/include/explicit_point_set.h"
#include "util/curves/include/xy_data_point.h"
#include "sectors/include/sector_utils.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

FractionalSecondaryOutput::FractionalSecondaryOutput()
{
    mCostCurve = 0;
}

FractionalSecondaryOutput::~FractionalSecondaryOutput() {
    delete mCostCurve;
}


FractionalSecondaryOutput* FractionalSecondaryOutput::clone() const
{
    FractionalSecondaryOutput* clone = new FractionalSecondaryOutput();
    clone->copy( *this );
    return clone;
}

void FractionalSecondaryOutput::copy( const FractionalSecondaryOutput& aOther ) {
    mName = aOther.mName;
    mOutputRatio = aOther.mOutputRatio;
    mMarketName = aOther.mMarketName;
    
    delete mCostCurve;
    mCostCurve = aOther.mCostCurve ? aOther.mCostCurve->clone() : 0;
}

const string& FractionalSecondaryOutput::getName() const
{
    // Make sure the name is initialized.
    assert( !mName.empty() );

    return mName;
}

void FractionalSecondaryOutput::setName( const string& aName )
{
    // Make sure the name is initialized.
    assert( !aName.empty() );

    mName = aName;
}

/*! \brief Get the XML name for reporting to XML file.
 *
 * This public function accesses the private constant string, XML_NAME. This way
 * the tag is always consistent for reporting outputs and can be easily
 * changed.
 * \author Sonny Kim
 * \return The constant XML_NAME.
 */
const string& FractionalSecondaryOutput::getXMLReportingName() const{
    return getXMLNameStatic();
}

const string& FractionalSecondaryOutput::getXMLNameStatic()
{
    const static string XML_NAME = "fractional-secondary-output";
    return XML_NAME;
}

bool FractionalSecondaryOutput::isSameType( const string& aType ) const
{
    return aType == getXMLNameStatic();
}

bool FractionalSecondaryOutput::XMLParse( const DOMNode* aNode )
{
    // assume we are passed a valid node.
    assert( aNode );

    // get all the children.
    DOMNodeList* nodeList = aNode->getChildNodes();

    // get the name attribute.
    mName = XMLHelper<string>::getAttr( aNode, "name" );
    
    ExplicitPointSet* currPoints = new ExplicitPointSet();

    for( unsigned int i = 0; i < nodeList->getLength(); ++i ) {
        const DOMNode* curr = nodeList->item( i );
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == XMLHelper<void>::text() ) {
            continue;
        }
        else if( nodeName == "output-ratio" ) {
            mOutputRatio.set( XMLHelper<double>::getValue( curr ) );
        }
        else if( nodeName == "calPrice" ) {
            mCalPrice.set( XMLHelper<double>::getValue( curr ) );
        }
        else if( nodeName == "market-name" ) {
            mMarketName = XMLHelper<string>::getValue( curr );
        }
        else if ( nodeName == "fraction-produced" ){
            double price = XMLHelper<double>::getAttr( curr, "price" );
            double fraction = XMLHelper<double>::getValue( curr );
            XYDataPoint* currPoint = new XYDataPoint( price, fraction );
            currPoints->addPoint( currPoint );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing " << getXMLNameStatic() << "." << endl;
        }
    }
    
    // TODO: should not reset the curve everytime *anything* is parsed
    delete mCostCurve;
    mCostCurve = new PointSetCurve( currPoints );

    // TODO: Improve error handling.
    return true;
}

void FractionalSecondaryOutput::toDebugXML( const int aPeriod,
                                            ostream& aOut,
                                            Tabs* aTabs ) const
{
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, mName );
    XMLWriteElement( mOutputRatio, "output-ratio", aOut, aTabs );
    XMLWriteElement( mCalPrice, "calPrice", aOut, aTabs );
    XMLWriteElement( mPhysicalOutputs[ aPeriod ], "output", aOut, aTabs );
    
    const vector<pair<double,double> > pairs = mCostCurve->getSortedPairs();
    typedef vector<pair<double, double> >::const_iterator PairIterator;
    map<string, double> attrs;
    for( PairIterator currPair = pairs.begin(); currPair != pairs.end(); ++currPair ) {
        attrs[ "price" ] = currPair->first;
        XMLWriteElementWithAttributes( currPair->second, "fraction-produced", aOut, aTabs, attrs );
    }
    
    XMLWriteElement( mMarketName, "market-name", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void FractionalSecondaryOutput::completeInit( const string& aSectorName,
                                              const string& aRegionName,
                                              const IInfo* aTechInfo,
                                              const bool aIsTechOperating )
{
    // FractionalSecondaryOutput adds to supply, so add a dependency.
    if( aIsTechOperating ) {
        scenario->getMarketplace()->getDependencyFinder()->addDependency( aSectorName,
                                                                          aRegionName,
                                                                          getName(),
                                                                          mMarketName.empty() ? aRegionName : mMarketName );
    }

    if( !mCostCurve ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "No fraction-produced read in for " << getXMLNameStatic() << " " << getName() << endl;
        abort();
    }

    if( mCostCurve->getMinY() > 0 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING);
        mainLog << "Minimum fraction-produced greater than zero for " << getXMLNameStatic() << " " << getName() << endl;
    }
}

void FractionalSecondaryOutput::initCalc( const string& aRegionName,
                                          const string& aSectorName,
                                          const int aPeriod )
{
    // If we are in a calibration period and a calibrated value was read in then set the
    // flag on the market that this resource is fully calibrated.
    if( aPeriod <= scenario->getModeltime()->getFinalCalibrationPeriod() && mCalPrice.isInited() ) {
        IInfo* marketInfo = scenario->getMarketplace()->getMarketInfo( mName, aRegionName, aPeriod, true );
        marketInfo->setBoolean( "fully-calibrated", true );
    }
    
    // The fractional supply will not have any additional behavior below the minimum price
    // however there may still be some indirect behavior above the top of the curve as it affects
    // the primary good's economics.
    SectorUtils::setSupplyBehaviorBounds( getName(), mMarketName.empty() ? aRegionName : mMarketName,
            mCostCurve->getMinX(), util::getLargeNumber(), aPeriod );
}


void FractionalSecondaryOutput::postCalc( const string& aRegionName, const int aPeriod ) {
}

void FractionalSecondaryOutput::scaleCoefficient( const double aScaler ) {
}

IOutput::OutputList FractionalSecondaryOutput::calcPhysicalOutput( const double aPrimaryOutput,
                                                                   const string& aRegionName,
                                                                   const ICaptureComponent* aCaptureComponent,
                                                                   const int aPeriod ) const
{
    OutputList outputList;
    outputList.push_back( make_pair( mName, calcPhysicalOutputInternal( aRegionName, aPrimaryOutput, aPeriod ) ) );
    return outputList;
}

void FractionalSecondaryOutput::setPhysicalOutput( const double aPrimaryOutput,
                                                   const string& aRegionName,
                                                   ICaptureComponent* aCaptureComponent,
                                                   const int aPeriod )
{
    // Secondary output is the primary output multiplied by the output ratio.
    mPhysicalOutputs[ aPeriod ] = calcPhysicalOutputInternal( aRegionName, aPrimaryOutput, aPeriod );

    // This class adds to supply of this market as it should be an independently solved market.
    /*!
     * \warning Adding to supply of an intermediate good will not work as intended, in that case a
     *          regular SecondaryOutput should be used which will subtract from demand.
     */
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->addToSupply( mName, mMarketName.empty() ? aRegionName : mMarketName,
            mPhysicalOutputs[ aPeriod ], aPeriod, true );
}

double FractionalSecondaryOutput::getPhysicalOutput( const int aPeriod ) const {
    assert( mPhysicalOutputs[ aPeriod ].isInited() );
    return mPhysicalOutputs[ aPeriod ];
}

double FractionalSecondaryOutput::getValue( const string& aRegionName,
                                            const ICaptureComponent* aCaptureComponent,
                                            const int aPeriod ) const
{
    double secondaryGoodPrice = getMarketPrice( aRegionName, aPeriod );
    // if calibrating then we assume a fractional production of 1
    // do not allow extrapolation
    double productionFraction = aPeriod <= scenario->getModeltime()->getFinalCalibrationPeriod() && mCalPrice.isInited() ?
        1.0 : min( mCostCurve->getMaxY(), mCostCurve->getY( secondaryGoodPrice ) );

    // The value of the secondary output is the market price multiplied by the
    // output ratio adjusted by the fractional production.
    return secondaryGoodPrice * mOutputRatio * productionFraction;
}

string FractionalSecondaryOutput::getOutputUnits( const string& aRegionName ) const {
    return scenario->getMarketplace()->getMarketInfo( getName(), mMarketName.empty() ? aRegionName : mMarketName, 0, true )
        ->getString( "output-unit", false );
}

double FractionalSecondaryOutput::getEmissionsPerOutput( const string& aGHGName,
                                                         const int aPeriod ) const
{
    return 0;
}

void FractionalSecondaryOutput::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitOutput( this, aPeriod );
    aVisitor->endVisitOutput( this, aPeriod );
}

/*!
 * \brief Simply retrieves the price of the secondary output from the marketplace.
 * \param aRegionName The current region.
 * \param aPeriod The current model period.
 * \return The market price.
 */
double FractionalSecondaryOutput::getMarketPrice( const string& aRegionName, const int aPeriod ) const {
    double price = scenario->getMarketplace()->getPrice( mName, mMarketName.empty() ? aRegionName : mMarketName, aPeriod, true );

    // Market price should exist or there is not a sector with this good as the
    // primary output. This can be caused by incorrect input files.
    assert( price != Marketplace::NO_MARKET_PRICE );
    if( price == Marketplace::NO_MARKET_PRICE ) {
        return 0;
    }

    return std::max( price, mCostCurve->getMinX() );
}

/*! 
 * \brief Calculate physical output.
 * \details Physical output of the secondary good is equal to the primary output
 *          multiplied by the coefficient and adjusted by the fractional supply curve.
 * \param aRegionName The current region.
 * \param aPrimaryOutput The total primary output of the technology.
 * \param aPeriod The current model period.
 * \return Physical output.
 */
double FractionalSecondaryOutput::calcPhysicalOutputInternal( const string& aRegionName,
                                                              const double aPrimaryOutput,
                                                              const int aPeriod ) const
{
    double maxSecondaryOutput = aPrimaryOutput * mOutputRatio;
    // If we are calibrating then always assume an output ratio of 1.
    if( aPeriod <= scenario->getModeltime()->getFinalCalibrationPeriod() && mCalPrice.isInited() ) {
        return maxSecondaryOutput;
    }
    double secondaryGoodPrice = getMarketPrice( aRegionName, aPeriod );
    // do not allow extrapolation
    double productionFraction = min( mCostCurve->getMaxY(), mCostCurve->getY( secondaryGoodPrice ) );
    return maxSecondaryOutput * productionFraction;
}

void FractionalSecondaryOutput::doInterpolations( const int aYear, const int aPreviousYear,
                                                  const int aNextYear, const IOutput* aPreviousOutput,
                                                  const IOutput* aNextOutput )
{
    const FractionalSecondaryOutput* prevSecOutput = static_cast<const FractionalSecondaryOutput*>( aPreviousOutput );
    const FractionalSecondaryOutput* nextSecOutput = static_cast<const FractionalSecondaryOutput*>( aNextOutput );
    
    /*!
     * \pre We are given a valid FractionalSecondaryOutput for the previous output.
     */
    assert( prevSecOutput );
    
    /*!
     * \pre We are given a valid FractionalSecondaryOutput for the next output.
     */
    assert( nextSecOutput );
    
    // interpolate the output ratio
    mOutputRatio.set( util::linearInterpolateY( aYear, aPreviousYear, aNextYear,
                                                prevSecOutput->mOutputRatio,
                                                nextSecOutput->mOutputRatio ) );
}
