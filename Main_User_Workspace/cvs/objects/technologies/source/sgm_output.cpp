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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*! 
* \file sgm_output.cpp
* \ingroup Objects
* \brief SGMOutput class source file.
* \author Josh Lurz
*/
#include "util/base/include/definitions.h"
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "technologies/include/sgm_output.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/iinfo.h"
#include "util/base/include/xml_helper.h"
#include "functions/include/function_utils.h"

extern Scenario* scenario;

using namespace std;
using namespace xercesc;

// static initialize.
const string SGMOutput::XML_REPORTING_NAME = "output-SGM";

SGMOutput::SGMOutput( const string& aSectorName )
    : mName( aSectorName ), mCurrencyOutputs( scenario->getModeltime()->getmaxper() )
{
}

SGMOutput::SGMOutput( const string& aSectorName, const double aConversionFactor, const double aCO2Coef )
    : mName( aSectorName ), mConversionFactor( aConversionFactor ), mCachedCO2Coef( aCO2Coef ),
    mCurrencyOutputs( scenario->getModeltime()->getmaxper() )
{
}

SGMOutput* SGMOutput::clone() const
{
    return new SGMOutput( *this );
}

bool SGMOutput::isSameType( const string& aType ) const
{
    return aType == "sgm-output";
}

const string& SGMOutput::getName() const
{
    // Make sure the name is initialized.
    assert( !mName.empty() );

    return mName;
}

/*! \brief Get the XML name for reporting to XML file.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for reporting outputs and can be easily
* changed.
* \author Sonny Kim
* \return The constant XML_NAME.
*/
const string& SGMOutput::getXMLReportingName() const{
    return XML_REPORTING_NAME;
}

bool SGMOutput::XMLParse( const DOMNode* aNode )
{
    // Primary outputs are created and cannot be parsed.
    assert( false );
    return false;
}

void SGMOutput::toInputXML( ostream& aOut,
                            Tabs* aTabs ) const
{
    // Primary outputs are not parsed and so do not write themselves to XML.
}

void SGMOutput::toDebugXML( const int aPeriod,
                            ostream& aOut,
                            Tabs* aTabs ) const
{
    XMLWriteOpeningTag( "sgm-output", aOut, aTabs, mName );
    XMLWriteElement( mCurrencyOutputs[ aPeriod ], "output", aOut, aTabs );
    XMLWriteElement( mCachedCO2Coef, "cached-co2-coef", aOut, aTabs );
    XMLWriteElement( mConversionFactor, "conversion-factor", aOut, aTabs );
    XMLWriteClosingTag( "sgm-output", aOut, aTabs );
}

void SGMOutput::completeInit( const string& aSectorName,
                              DependencyFinder* aDependencyFinder,
                              const IInfo* aTechInfo,
                              const bool aIsTechOperating )
{
    // SGM outputs do not have any additional dependencies.
}

void SGMOutput::initCalc( const string& aRegionName,
                          const string& aSectorName,
                          const int aPeriod )
{
    // Make sure the primary output has a name.
    assert( !mName.empty() );

    // Initialize the cached CO2 coefficient.
    mCachedCO2Coef.set( FunctionUtils::getCO2Coef( aRegionName, mName, aPeriod ) );
    
    // Initialize the conversion factor
    mConversionFactor.set( FunctionUtils::getMarketConversionFactor( aRegionName, mName ) );
}

void SGMOutput::postCalc( const string& aRegionName,
                          const int aPeriod )
{
}

void SGMOutput::scaleCoefficient( const double aScaler ){
    // SGMOutput outputs do not support scaling.
    assert( false );
}

IOutput::OutputList SGMOutput::calcPhysicalOutput( const double aSGMOutput,
                                                   const string& aRegionName,
                                                   const ICaptureComponent* aCaptureComponent,
                                                   const int aPeriod ) const
{
    assert( false );
    // TODO?
    return OutputList();
}

void SGMOutput::setPhysicalOutput( const double aSGMOutput,
                                   const string& aRegionName,
                                   ICaptureComponent* aCaptureComponent,
                                   const int aPeriod )
{
assert( false );
    // Could make this work.
}

double SGMOutput::getPhysicalOutput( const int aPeriod ) const
{
    assert( mConversionFactor.isInited() );
    
    return getCurrencyOutput( aPeriod ) * mConversionFactor;
}
void SGMOutput::setCurrencyOutput( const std::string& aRegionName,
                                    const double aOutput,
                                    const int aPeriod )
{
    //assert( aOutput >= 0 );
    
    mCurrencyOutputs[ aPeriod ] = aOutput;
    
    // add the output to the marketplace.
    // does this make sense for consumers?  see the note in 
    // PrimaryOutput::setPhysicalDemand
    if( aOutput > util::getSmallNumber() ) {
        Marketplace* marketplace = scenario->getMarketplace();
        marketplace->addToSupply( mName, aRegionName, aOutput, aPeriod, false );
    }
}

double SGMOutput::getCurrencyOutput( const int aPeriod ) const {
    return mCurrencyOutputs[ aPeriod ];
}

double SGMOutput::getValue( const string& aRegionName,
                            const ICaptureComponent* aCaptureComponent,
                            const int aPeriod ) const
{
assert( false );
    // The value of the sgm output is fully accounted by the technology.
    return 0;
}

double SGMOutput::getEmissionsPerOutput( const string& aGHGName,
                                         const int aPeriod ) const
{
    // Currently other GHGs do not use output emissions coefficients.
    assert( aGHGName == "CO2" );
    
    assert( mCachedCO2Coef.isInited() );
    return mCachedCO2Coef;
}

void SGMOutput::accept( IVisitor* aVisitor,
                        const int aPeriod ) const
{
    aVisitor->startVisitOutput( this, aPeriod );
    aVisitor->endVisitOutput( this, aPeriod );
}
