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
* \file good_value_output.cpp
* \ingroup Objects
* \brief GoodValueOutput class source file.
* \author Pralit Patel
*/
#include "util/base/include/definitions.h"
#include <string>
#include <xercesc/dom/DOMNodeList.hpp>
#include "util/base/include/xml_helper.h"
#include "technologies/include/good_value_output.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "containers/include/iinfo.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/market_dependency_finder.h"
#include "functions/include/function_utils.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief Get the XML name for reporting to XML file.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for reporting outputs and can be easily
* changed.
* \author Sonny Kim
* \return The constant XML_NAME.
*/
const string& GoodValueOutput::getXMLReportingName() const{
    return getXMLNameStatic();
}

const string& GoodValueOutput::getXMLNameStatic()
{
    const static string XML_NAME = "good-value-output";
    return XML_NAME;
}

GoodValueOutput::GoodValueOutput()
    : mPhysicalOutputs( scenario->getModeltime()->getmaxper() ),
      mPriceMult( 0.0 )
{
}

GoodValueOutput* GoodValueOutput::clone() const
{
    return new GoodValueOutput( *this );
}

bool GoodValueOutput::isSameType( const string& aType ) const
{
    return aType == getXMLNameStatic();
}

const string& GoodValueOutput::getName() const
{
    // Make sure the name is initialized.
    assert( !mName.empty() );

    return mName;
}

void GoodValueOutput::setName( const string& aName )
{
    // Make sure the name is initialized.
    assert( !aName.empty() );

    mName = aName;
}

bool GoodValueOutput::XMLParse( const DOMNode* aNode )
{
    // assume we are passed a valid node.
    assert( aNode );

    // get all the children.
    DOMNodeList* nodeList = aNode->getChildNodes();

    // get the name attribute.
    mName = XMLHelper<string>::getAttr( aNode, "name" );

    for( unsigned int i = 0; i < nodeList->getLength(); ++i ) {
        const DOMNode* curr = nodeList->item( i );
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        else if( nodeName == "output-ratio" ) {
            mOutputRatio.set( XMLHelper<double>::getValue( curr ) );
        }
        else if( nodeName == "pMultiplier" ) {
            mPriceMult = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "market-name" ) {
            mMarketName = XMLHelper<string>::getValue( curr );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing " << getXMLNameStatic() << "." << endl;
        }
    }

    // TODO: Improve error handling.
    return true;
}

void GoodValueOutput::toInputXML( ostream& aOut,
                                  Tabs* aTabs ) const
{
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, mName );
    XMLWriteElement( mOutputRatio, "output-ratio", aOut, aTabs );
    XMLWriteElementCheckDefault( mPriceMult, "pMultiplier", aOut, aTabs, 1.0 );
    XMLWriteElementCheckDefault( mMarketName, "market-name", aOut, aTabs, string() );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void GoodValueOutput::toDebugXML( const int aPeriod,
                                  ostream& aOut,
                                  Tabs* aTabs ) const
{
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, mName );
    XMLWriteElement( mOutputRatio, "output-ratio", aOut, aTabs );
    XMLWriteElement( mPriceMult, "pMultiplier", aOut, aTabs );
    XMLWriteElement( mPhysicalOutputs[ aPeriod ], "output", aOut, aTabs );
    XMLWriteElement( mMarketName, "market-name", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void GoodValueOutput::completeInit( const string& aSectorName,
                                    const string& aRegionName,
                                    const IInfo* aTechInfo,
                                    const bool aIsTechOperating )
{
    // GoodValue output is removed from demand, so add a dependency.
    if( aIsTechOperating ) {
        scenario->getMarketplace()->getDependencyFinder()->addDependency( aSectorName,
                                                                          aRegionName,
                                                                          getName(),
                                                                          mMarketName.empty() ? aRegionName : mMarketName );
    }
    mGoodName = aSectorName;
}

void GoodValueOutput::initCalc( const string& aRegionName,
                                const string& aSectorName,
                                const int aPeriod )
{
}


void GoodValueOutput::postCalc( const string& aRegionName,
                          const int aPeriod )
{
}

void GoodValueOutput::scaleCoefficient( const double aScaler ){
}

IOutput::OutputList GoodValueOutput::calcPhysicalOutput( const double aPrimaryOutput,
                                                         const string& aRegionName,
                                                         const ICaptureComponent* aCaptureComponent,
                                                         const int aPeriod ) const
{
    OutputList outputList;
    outputList.push_back( make_pair( mName, calcPhysicalOutputInternal( aPrimaryOutput, aRegionName, aPeriod ) ) );
    return outputList;
}

void GoodValueOutput::setPhysicalOutput( const double aPrimaryOutput,
                                         const string& aRegionName,
                                         ICaptureComponent* aCaptureComponent,
                                         const int aPeriod )
{
    // GoodValue output is the primary output multiplied by the output ratio.
    mPhysicalOutputs[ aPeriod ] = calcPhysicalOutputInternal( aPrimaryOutput, aRegionName, aPeriod );

    Marketplace* marketplace = scenario->getMarketplace();
    mLastCalcValue = marketplace->addToDemand( mName, mMarketName.empty() ? aRegionName : mMarketName, mPhysicalOutputs[ aPeriod ], mLastCalcValue, aPeriod, true );
}

double GoodValueOutput::getPhysicalOutput( const int aPeriod ) const
{
    assert( mPhysicalOutputs[ aPeriod ].isInited() );
    return mPhysicalOutputs[ aPeriod ];
}

double GoodValueOutput::getValue( const string& aRegionName,
                                  const ICaptureComponent* aCaptureComponent,
                                  const int aPeriod ) const
{
    /*
    double price = scenario->getMarketplace()->getPrice( mName, mMarketName.empty() ? aRegionName : mMarketName, aPeriod, true );

    // Market price should exist or there is not a sector with this good as the
    // primary output. This can be caused by incorrect input files.
    assert( price != Marketplace::NO_MARKET_PRICE );
    if( price == Marketplace::NO_MARKET_PRICE ) {
        return 0;
    }

    // The value of the secondary output is the market price multiplied by the
    // output ratio.
    return price * mOutputRatio * mPriceMult;
    */
    return 0.0;
}

string GoodValueOutput::getOutputUnits( const string& aRegionName ) const {
    return scenario->getMarketplace()->getMarketInfo( getName(), mMarketName.empty() ? aRegionName : mMarketName, 0, true )
        ->getString( "output-unit", false );
}

double GoodValueOutput::getEmissionsPerOutput( const string& aGHGName,
                                               const int aPeriod ) const
{
    return 0.0;
}

void GoodValueOutput::accept( IVisitor* aVisitor, const int aPeriod ) const
{
    aVisitor->startVisitOutput( this, aPeriod );
    aVisitor->endVisitOutput( this, aPeriod );
}

/*! 
 * \brief Calculate physical output.
 * \details Physical output of the secondary good is equal to the primary output
 *          multiplied by the coefficient.
 * \return Physical output.
 */
double GoodValueOutput::calcPhysicalOutputInternal( const double aPrimaryOutput, const string& aRegionName, const int aPeriod ) const {
    return aPrimaryOutput * mOutputRatio * scenario->getMarketplace()->getPrice( mGoodName, aRegionName, aPeriod );
}

void GoodValueOutput::doInterpolations( const int aYear, const int aPreviousYear,
                                        const int aNextYear, const IOutput* aPreviousOutput,
                                        const IOutput* aNextOutput )
{
    // TODO: do we really want to do this?
    const GoodValueOutput* prevSecOutput = static_cast<const GoodValueOutput*>( aPreviousOutput );
    const GoodValueOutput* nextSecOutput = static_cast<const GoodValueOutput*>( aNextOutput );
    
    /*!
     * \pre We are given a valid GoodValueOutput for the previous output.
     */
    assert( prevSecOutput );
    
    /*!
     * \pre We are given a valid GoodValueOutput for the next output.
     */
    assert( nextSecOutput );
    
    // interpolate the output ratio
    mOutputRatio.set( util::linearInterpolateY( aYear, aPreviousYear, aNextYear,
                                                prevSecOutput->mOutputRatio,
                                                nextSecOutput->mOutputRatio ) );
}
