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
 * \file gcam_consumer.cpp
 * \ingroup Objects
 * \brief The GCAMConsumer class source file.
 * \author Pralit Patel
 * \author Jiyong Eom
 */
#include "util/base/include/definitions.h"

#include "consumers/include/gcam_consumer.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "util/base/include/ivisitor.h"
#include "functions/include/node_input.h"
#include "containers/include/iinfo.h"
#include "demographics/include/demographic.h"
#include "containers/include/gdp.h"
#include "sectors/include/sector_utils.h"
#include "technologies/include/generic_output.h"
#include "emissions/include/aghg.h"

#include "util/base/include/initialize_tech_vector_helper.hpp"

using namespace std;

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
    GCAMConsumer* clone = new GCAMConsumer();
    clone->copy( *this );
    return clone;
}

void GCAMConsumer::copy( const GCAMConsumer& aOther ) {
    Consumer::copy( aOther );
    mSubregionalPopulation = aOther.mSubregionalPopulation;
    mSubregionalIncome = aOther.mSubregionalIncome;
    mSubregionalPopulationShare = aOther.mSubregionalPopulationShare;
    mSubregionalIncomeShare = aOther.mSubregionalIncomeShare;
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
    // Basetechnology creates a CO2 object however they are not needed for GCAMConsumers
    // and they may cause trouble with tech vector sizing therefore just delete it.
    for( unsigned int i = 0; i < mGhgs.size(); ++i ){
        delete mGhgs[ i ];
    }
    mGhgs.clear();
    initTechVintageVector();

    // Interpolate shares for missing periods.
    SectorUtils::fillMissingPeriodVectorInterpolated( mSubregionalPopulationShare );
    SectorUtils::fillMissingPeriodVectorInterpolated( mSubregionalIncomeShare );
}

void GCAMConsumer::initCalc( const string& aRegionName,
                             const string& aSectorName, NationalAccount& aNationalAccount,
                             const Demographic* aDemographics, const GDP* aGDP,
                             const double aCapitalStock, const int aPeriod )
{
    double population = aDemographics->getTotal( aPeriod );
    double gdp = aGDP->getGDP( aPeriod );
    double gdpPPP = aGDP->getPPPGDPperCap( aPeriod ) * population;

    double subregionalPopulation = mSubregionalPopulationShare[ aPeriod ] * population;
    double subregionalIncome = mSubregionalIncomeShare[ aPeriod ] * gdp / subregionalPopulation;
    double subregionalIncomePPP = mSubregionalIncomeShare[ aPeriod ] * gdpPPP / subregionalPopulation;
    
    // store the subregional population and income for reporting
    mSubregionalPopulation[ aPeriod ] = subregionalPopulation;
    mSubregionalIncome[ aPeriod ] = subregionalIncome;

    // Set the subregional income and population into an info object so that nodes in the
    // nesting structure can have access to the values.
    mTechInfo->setDouble( "subregional-population", subregionalPopulation );
    mTechInfo->setDouble( "subregional-income", subregionalIncome );
    mTechInfo->setDouble( "subregional-income-ppp", subregionalIncomePPP );
    Consumer::initCalc( aRegionName, aSectorName, aNationalAccount,
                        aDemographics, aCapitalStock, aPeriod );
}

void GCAMConsumer::operate( NationalAccount& aNationalAccount, const Demographic* aDemographics,
                            const string& aRegionName,
                            const string& aSectorName, const bool aIsNewVintageMode, int aPeriod )
{
    // In calibration periods we will back out coefficients to reproduce the read in base year
    // values in the nested input structure.
    const Modeltime* modeltime = scenario->getModeltime();
    const bool calibrationPeriod = aPeriod > 0 && aPeriod <= modeltime->getFinalCalibrationPeriod();
    if( calibrationPeriod ) {
        mNestedInputRoot->calcCoefficient( aRegionName, aSectorName, aPeriod );
    }

    // Set prices for all leafs and node inputs.
    BaseTechnology::calcPricePaid( aRegionName, aSectorName, aPeriod, 
                                   modeltime->gettimestep( aPeriod ) );
    mNestedInputRoot->calcLevelizedCost( aRegionName, aSectorName, aPeriod, 
                                         mNestedInputRoot->getCoefficient( aPeriod ) ); // alpha zero is the root's alpha

    // Drive input demands.
    calcInputDemand( 0, aRegionName, aSectorName, aPeriod );
}

void GCAMConsumer::postCalc( const string& aRegionName, const string& aSectorName, const int aPeriod ) {
    // nothing to do
}

void GCAMConsumer::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitGCAMConsumer( this, aPeriod );

    BaseTechnology::accept( aVisitor, aPeriod );

    aVisitor->endVisitGCAMConsumer( this, aPeriod );
}

/*!
 * \brief Initialize any TechVintageVector in any object that may be contained
 *        in this class.
 * \details GCAMConsumer does not have vintaging so the vectors will be sized for
 *          the entire model time.
 */
void GCAMConsumer::initTechVintageVector() {
    const Modeltime* modeltime = scenario->getModeltime();
    objects::InitializeTechVectorHelper helper( 0, modeltime->getmaxper() );
    helper.initializeTechVintageVector( this );
}
