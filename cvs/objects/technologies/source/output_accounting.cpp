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
* \file output_accounting.cpp
* \ingroup Objects
* \brief OutputAccounting class source file.
* \author Josh Lurz
*/
#include "util/base/include/definitions.h"
#include <string>
#include "util/base/include/xml_helper.h"
#include "technologies/include/output_accounting.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "containers/include/iinfo.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/market_dependency_finder.h"
#include "functions/include/function_utils.h"

using namespace std;

extern Scenario* scenario;

/*! \brief Get the XML name for reporting to XML file.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for reporting outputs and can be easily
* changed.
* \author Sonny Kim
* \return The constant XML_NAME.
*/
const string& OutputAccounting::getXMLReportingName() const{
    static const string XML_REPORTING_NAME = "output-accounting";
    return XML_REPORTING_NAME;
}

const string& OutputAccounting::getXMLNameStatic()
{
    const static string XML_NAME = "output-accounting";
    return XML_NAME;
}

const string& OutputAccounting::getXMLName() const {
    return getXMLNameStatic();
}

OutputAccounting::OutputAccounting():
    mOutputRatio( 1.0 ),
    mUseBasePrice( false )
{
}

OutputAccounting::~OutputAccounting() {
}

OutputAccounting* OutputAccounting::clone() const
{
    OutputAccounting* clone = new OutputAccounting();
    clone->copy( * this );
    return clone;
}

void OutputAccounting::copy( const OutputAccounting& aOther ) {
    mName = aOther.mName;
    mMarketName = aOther.mMarketName;
    mOutputRatio = aOther.mOutputRatio;
    mUseBasePrice = aOther.mUseBasePrice;
    
    // note results are not copied.
}

bool OutputAccounting::isSameType( const string& aType ) const
{
    return aType == getXMLNameStatic();
}

const string& OutputAccounting::getName() const
{
    // Make sure the name is initialized.
    assert( !mName.empty() );

    return mName;
}

void OutputAccounting::setName( const string& aName )
{
    // Make sure the name is initialized.
    assert( !aName.empty() );

    mName = aName;
}

void OutputAccounting::toDebugXML( const int aPeriod,
                                  ostream& aOut,
                                  Tabs* aTabs ) const
{
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, mName );
    XMLWriteElement( mOutputRatio, "output-ratio", aOut, aTabs );
    XMLWriteElement( mCurrencyOutputs[ aPeriod ], "currency-output", aOut, aTabs );
    XMLWriteElement( mUseBasePrice, "use-base-price", aOut, aTabs );
    XMLWriteElement( mBasePrice, "base-price", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void OutputAccounting::completeInit( const string& aSectorName,
                                     const string& aRegionName,
                                     const IInfo* aTechInfo,
                                     const bool aIsTechOperating )
{
}

void OutputAccounting::initCalc( const string& aRegionName,
                                 const string& aSectorName,
                                 const int aPeriod )
{
    const int finalCalPeriod = scenario->getModeltime()->getFinalCalibrationPeriod();
    if(aPeriod > finalCalPeriod) {
        IInfo* marketInfo = scenario->getMarketplace()->getMarketInfo( aSectorName, aRegionName, finalCalPeriod, false );
        if( marketInfo ) {
            mBasePrice = marketInfo->getDouble( "base-price", mUseBasePrice );
        }
        else if( mUseBasePrice ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Could not find base-price in " << mName << ", region: " << aRegionName << ", sector: " << aSectorName;
        }
    }
}


void OutputAccounting::postCalc( const string& aRegionName,
                          const int aPeriod )
{
}

IOutput::OutputList OutputAccounting::calcPhysicalOutput( const double aPrimaryOutput,
                                                         const string& aRegionName,
                                                         const ICaptureComponent* aCaptureComponent,
                                                         const int aPeriod ) const
{
    OutputList outputList;
    return outputList;
}

void OutputAccounting::setCurrencyOutput( const double aPysicalOutput,
                                          const double aCurrencyConversionPrice,
                                          const string& aRegionName,
                                          const int aPeriod )
{
    // we are expecting a currency conversion price to ultimately land us in 1975 billion dollars:
    // output (EJ), price (75$/GJ), output*price (EJ*75$/GJ=75Bil$)
    // the macro calculations will then be responsible for converting to 1990 million dollars
    double conversionPrice = mUseBasePrice && aPeriod > scenario->getModeltime()->getFinalCalibrationPeriod() ?
                             mBasePrice : aCurrencyConversionPrice;

    mCurrencyOutputs[ aPeriod ] = mOutputRatio * aPysicalOutput * conversionPrice;

    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->addToDemand( mName, mMarketName.empty() ? aRegionName : mMarketName,
                              mCurrencyOutputs[ aPeriod ], aPeriod, false );
}

double OutputAccounting::getCurrencyOutput( const int aPeriod ) const
{
    assert( mCurrencyOutputs[ aPeriod ].isInited() );
    return mCurrencyOutputs[ aPeriod ];
}

string OutputAccounting::getOutputUnits( const string& aRegionName ) const {
    IInfo* marketInfo = scenario->getMarketplace()->getMarketInfo( getName(), aRegionName, 0, false );
    return marketInfo ? marketInfo->getString( "output-unit", false ) : "";
}

void OutputAccounting::accept( IVisitor* aVisitor, const int aPeriod ) const
{
    aVisitor->startVisitOutput( this, aPeriod );
    aVisitor->endVisitOutput( this, aPeriod );
}

void OutputAccounting::doInterpolations( const int aYear, const int aPreviousYear,
                                         const int aNextYear, const IOutput* aPreviousOutput,
                                         const IOutput* aNextOutput )
{
    const OutputAccounting* prevSecOutput = static_cast<const OutputAccounting*>( aPreviousOutput );
    const OutputAccounting* nextSecOutput = static_cast<const OutputAccounting*>( aNextOutput );
    
    /*!
     * \pre We are given a valid OutputAccounting for the previous output.
     */
    assert( prevSecOutput );
    
    /*!
     * \pre We are given a valid OutputAccounting for the next output.
     */
    assert( nextSecOutput );
    
    // interpolate the output ratio
    mOutputRatio.set( util::linearInterpolateY( aYear, aPreviousYear, aNextYear,
                                                prevSecOutput->mOutputRatio,
                                                nextSecOutput->mOutputRatio ) );
}

