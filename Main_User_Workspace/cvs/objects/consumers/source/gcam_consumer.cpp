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
 * \file gcam_consumer.cpp
 * \ingroup Objects
 * \brief The GCAMConsumer class source file.
 * \author Pralit Patel
 * \author Jiyong Eom
 */
#include "util/base/include/definitions.h"
#include <xercesc/dom/DOMNode.hpp>

#include "consumers/include/gcam_consumer.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "util/base/include/ivisitor.h"
#include "functions/include/node_input.h"
#include "containers/include/iinfo.h"
#include "demographics/include/demographic.h"
#include "sectors/include/sector_utils.h"
#include "technologies/include/generic_output.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//! Default GCAMConsumer
GCAMConsumer::GCAMConsumer()
{
    // GCAM consumers do not vary by year.
    year = 0;
}

//! Destructor
GCAMConsumer::~GCAMConsumer() {
}

void GCAMConsumer::copyParam( const BaseTechnology* aBaseTech, const int aPeriod ) {
    BaseTechnology::copyParam( aBaseTech, aPeriod );
    aBaseTech->copyParamsInto( *this, aPeriod );
}

GCAMConsumer* GCAMConsumer::clone() const {
    return new GCAMConsumer( *this );
}

/*! \brief Get the XML node name for output to XML.
 *
 * This public function accesses the private constant string, XML_NAME.
 * This way the tag is always consistent for both read-in and output and can be easily changed.
 * This function may be virtual to be overriden by derived class pointers.
 * \author Josh Lurz, James Blackwood
 * \return The constant XML_NAME.
 */
const string& GCAMConsumer::getXMLName() const {
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
const string& GCAMConsumer::getXMLNameStatic() {
    const static string XML_NAME = "gcam-consumer";
    return XML_NAME;
}

bool GCAMConsumer::XMLDerivedClassParse( const string& aNodeName, const DOMNode* aCurr ) {
    if ( aNodeName == "subregional-income-share" ) {
        XMLHelper<Value>::insertValueIntoVector( aCurr, mSubregionalIncomeShare, scenario->getModeltime() );
    }
    else if ( aNodeName == "subregional-population-share" ) {
        XMLHelper<Value>::insertValueIntoVector( aCurr, mSubregionalPopulationShare, scenario->getModeltime() );
    }
    else {
        return false;
    }
    return true;
}

void GCAMConsumer::toInputXMLDerived( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteVector( mSubregionalIncomeShare, "subregional-income-share", aOut, aTabs, scenario->getModeltime() );
    XMLWriteVector( mSubregionalPopulationShare, "subregional-population-share", aOut, aTabs, scenario->getModeltime() );
}

void GCAMConsumer::toDebugXMLDerived( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteElement( mSubregionalPopulation[ aPeriod ], "subregional-population", aOut, aTabs );
    XMLWriteElement( mSubregionalIncome[ aPeriod ], "subregional-income", aOut, aTabs );
    XMLWriteElement( mSubregionalPopulationShare[ aPeriod ], "subregional-population-share", aOut, aTabs );
    XMLWriteElement( mSubregionalIncomeShare[ aPeriod ], "subregional-income-share", aOut, aTabs );
}

void GCAMConsumer::completeInit( const string& aRegionName, const string& aSectorName,
                                 const string& aSubsectorName ) {
    BaseTechnology::completeInit( aRegionName, aSectorName, aSubsectorName );
    
    // Basetechnology creates a SGMOutput. This is not needed for GCAM and
    // could cause problems with asserts so replace it with a GenericOutput
    for( unsigned int i = 0; i < mOutputs.size(); ++i ){
        delete mOutputs[ i ];
    }
    mOutputs.clear();
    mOutputs.insert( mOutputs.begin(), new GenericOutput( "generic-output" ) );

    // Interpolate shares for missing periods.
    SectorUtils::fillMissingPeriodVectorInterpolated( mSubregionalPopulationShare );
    SectorUtils::fillMissingPeriodVectorInterpolated( mSubregionalIncomeShare );
}

void GCAMConsumer::initCalc( const MoreSectorInfo* aMoreSectorInfo, const string& aRegionName,
                             const string& aSectorName, NationalAccount& aNationalAccount,
                             const Demographic* aDemographics, const double aCapitalStock,
                             const int aPeriod )
{
    double population = aDemographics->getTotal( aPeriod );
    // aCapitalStock is really GDP
    double gdp = aCapitalStock;

    double subregionalPopulation = mSubregionalPopulationShare[ aPeriod ] * population;
    double subregionalIncome = mSubregionalIncomeShare[ aPeriod ] * gdp / subregionalPopulation;
    
    // store the subregional population and income for reporting
    mSubregionalPopulation[ aPeriod ] = subregionalPopulation;
    mSubregionalIncome[ aPeriod ] = subregionalIncome;

    // Set the subregional income and population into an info object so that nodes in the
    // nesting structure can have access to the values.
    mTechInfo->setDouble( "subregional-population", subregionalPopulation );
    mTechInfo->setDouble( "subregional-income", subregionalIncome );
    Consumer::initCalc( aMoreSectorInfo, aRegionName, aSectorName, aNationalAccount,
                        aDemographics, aCapitalStock, aPeriod );
}

void GCAMConsumer::operate( NationalAccount& aNationalAccount, const Demographic* aDemographics,
                            const MoreSectorInfo* aMoreSectorInfo, const string& aRegionName,
                            const string& aSectorName, const bool aIsNewVintageMode, int aPeriod )
{
    expenditures[ aPeriod ].reset();

    // In calibration periods we will back out coefficients to reproduce the read in base year
    // values in the nested input structure.
    const Modeltime* modeltime = scenario->getModeltime();
    const bool calibrationPeriod = aPeriod > 0 && aPeriod <= modeltime->getFinalCalibrationPeriod();
    if( calibrationPeriod ) {
        mNestedInputRoot->calcCoefficient( aRegionName, aSectorName, aPeriod );
    }

    // Set prices for all leafs and node inputs.
    BaseTechnology::calcPricePaid( aMoreSectorInfo, aRegionName, aSectorName, aPeriod, 
                                   modeltime->gettimestep( aPeriod ) );
    mNestedInputRoot->calcLevelizedCost( aRegionName, aSectorName, aPeriod, 
                                         mNestedInputRoot->getCoefficient( aPeriod ) ); // alpha zero is the root's alpha

    // Drive input demands.
    calcInputDemand( 0, aRegionName, aSectorName, aPeriod );
    
    // Reset speed optimization flags.
    mPricePaidCached = false;
    mNestedInputRoot->resetCalcLevelizedCostFlag();
}

void GCAMConsumer::postCalc( const string& aRegionName, const string& aSectorName, const int aPeriod ) {
    // nothing to do
}

void GCAMConsumer::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitGCAMConsumer( this, aPeriod );

    BaseTechnology::accept( aVisitor, aPeriod );

    aVisitor->endVisitGCAMConsumer( this, aPeriod );
}
