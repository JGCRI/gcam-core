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
 * \file nonco2_emissions.cpp
 * \ingroup Objects
 * \brief NonCO2Emissions class source file.
 * \author Kate Calvin
 */

#include "util/base/include/definitions.h"

#include <xercesc/dom/DOMNode.hpp>

#include "emissions/include/nonco2_emissions.h"
#include "emissions/include/aemissions_driver.h"
#include "emissions/include/emissions_driver_factory.h"
#include "emissions/include/aemissions_control.h"
#include "emissions/include/emissions_control_factory.h"
#include "containers/include/scenario.h"
#include "util/base/include/xml_helper.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"
#include "technologies/include/ioutput.h"
#include "functions/include/function_utils.h"
#include "marketplace/include/cached_market.h"
#include "technologies/include/icapture_component.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//! Default constructor.
NonCO2Emissions::NonCO2Emissions():
AGHG(),
mShouldCalibrateEmissCoef( false ),
mGDP( 0 )
{
    // default unit for emissions
    mEmissionsUnit = "Tg";
}

//! Default destructor.
NonCO2Emissions::~NonCO2Emissions(){
}

//! Copy constructor.
NonCO2Emissions::NonCO2Emissions( const NonCO2Emissions& aOther )
: AGHG( aOther ){
    copy( aOther );
}

//! Clone operator.
NonCO2Emissions* NonCO2Emissions::clone() const {
    return new NonCO2Emissions( *this );
}

//! Assignment operator.
NonCO2Emissions& NonCO2Emissions::operator=( const NonCO2Emissions& aOther ){
    if( this != &aOther ){
        AGHG::operator=( aOther );
        clear();
        copy( aOther );
    }
    return *this;
}

//! Clear any dynamically allocated memory
void NonCO2Emissions::clear() {
    for ( CControlIterator controlIt = mEmissionsControls.begin(); controlIt != mEmissionsControls.end(); ++controlIt ) {
        delete *controlIt;
    }
    mEmissionsControls.clear();
}

//! Copy helper function.
void NonCO2Emissions::copy( const NonCO2Emissions& aOther ) {
    mEmissionsCoef = aOther.mEmissionsCoef;
    mSavedEmissionsCoef = aOther.mSavedEmissionsCoef;
    mGDP = aOther.mGDP;
    
    // Deep copy the auto_ptr
    if( aOther.mEmissionsDriver.get() ){
        mEmissionsDriver.reset( aOther.mEmissionsDriver->clone() );
    }
    
    /*!
     * \warning This will always copy previous. You can't overwrite it with new MAC curve.
     */
    for ( CControlIterator controlIt = aOther.mEmissionsControls.begin(); controlIt != aOther.mEmissionsControls.end(); ++controlIt ) {
        mEmissionsControls.push_back( (*controlIt)->clone() );
    }
}

void NonCO2Emissions::copyGHGParameters( const AGHG* aPrevGHG ){
    assert( aPrevGHG ); // Make sure valid pointer was passed

    // Ensure that prevGHG can be cast to NonCO2Emissions* otherwise return early
    // TODO: Fix this, could use a double dispatch approach to avoid the cast. See
    // the copyParam/copyParamsInto solution in IInput.
    const NonCO2Emissions* prevComplexGHG = static_cast<const NonCO2Emissions*>( aPrevGHG );
    if( !prevComplexGHG ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Bad dynamic cast occurred in copyGHGParameters." << endl;
        return;
    }

    if( !mEmissionsDriver.get() ) {
        mEmissionsDriver.reset( prevComplexGHG->mEmissionsDriver->clone() );
    }
    else if ( mEmissionsDriver->getXMLName() != prevComplexGHG->mEmissionsDriver->getXMLName() ){
        // Print a warning if driver has changed
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Warning, the driver has been changed from "<< prevComplexGHG->mEmissionsDriver->getXMLName() << " to "
                << mEmissionsDriver->getXMLName() << "." << endl;
    }

    if( !mEmissionsCoef.isInited() ) {
        mEmissionsCoef = prevComplexGHG->mEmissionsCoef;
    }

    mGDP = prevComplexGHG->mGDP;

    // As with other variables, only copy if something is not already present
    if( mEmissionsControls.empty() ) {
        clear();
        for ( CControlIterator controlIt = prevComplexGHG->mEmissionsControls.begin();
              controlIt != prevComplexGHG->mEmissionsControls.end(); ++controlIt )
        {
            mEmissionsControls.push_back( (*controlIt)->clone() );
        }
    }
}

/*!
 * \brief Get the XML node name for output to XML.
 * \details This public function accesses the private constant string, XML_NAME.
 *          This way the tag is always consistent for both read-in and output and can be easily changed.
 *          This function may be virtual to be overridden by derived class pointers.
 * \author Jim Naslund
 * \return The constant XML_NAME.
 */
const string& NonCO2Emissions::getXMLName() const {
    return getXMLNameStatic();
}

const string& NonCO2Emissions::getXMLNameStatic(){
    static const string XML_NAME = "Non-CO2";
    return XML_NAME;
}

bool NonCO2Emissions::XMLDerivedClassParse( const string& aNodeName, const DOMNode* aCurrNode ){
    if( aNodeName == "emiss-coef" ){
        mEmissionsCoef = XMLHelper<Value>::getValue( aCurrNode );
    }
    else if( aNodeName == "input-emissions" ){
        mInputEmissions = XMLHelper<Value>::getValue( aCurrNode );
    }
    else if( EmissionsDriverFactory::isEmissionsDriverNode( aNodeName ) ){
        mEmissionsDriver = EmissionsDriverFactory::create( aNodeName );
    }
    else if( EmissionsControlFactory::isEmissionsControlNode( aNodeName ) ) {
        parseContainerNode( aCurrNode, mEmissionsControls, EmissionsControlFactory::create( aNodeName ).release() );
    }
    else{
        return false;
    }
    return true;
}


void NonCO2Emissions::toInputXMLDerived( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteElement( mEmissionsCoef, "emiss-coef", aOut, aTabs );
    XMLWriteElementCheckDefault( mInputEmissions, "input-emissions", aOut, aTabs, Value() );

    XMLWriteElement( "", mEmissionsDriver->getXMLName(), aOut, aTabs );

    for ( CControlIterator controlIt = mEmissionsControls.begin(); controlIt != mEmissionsControls.end(); ++controlIt ) {
        (*controlIt)->toInputXML( aOut, aTabs );
    }
}

void NonCO2Emissions::toDebugXMLDerived( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteElement( mEmissionsCoef, "emiss-coef", aOut, aTabs );
    XMLWriteElement( mInputEmissions, "input-emissions", aOut, aTabs );
    XMLWriteElement( mSavedEmissionsCoef[ aPeriod ], "saved-ef", aOut, aTabs );

    XMLWriteElement( "", mEmissionsDriver->getXMLName(), aOut, aTabs );

    for ( CControlIterator controlIt = mEmissionsControls.begin(); controlIt != mEmissionsControls.end(); ++controlIt ) {
        (*controlIt)->toDebugXML( aPeriod, aOut, aTabs );
    }
}

/*!
 * \brief Complete the initialization of the ghg object.
 * \note This routine is only called once per model run
 * \param aRegionName Region name.
 * \param aSectorName Sector name, also the name of the product.
 * \param aTechInfo Technology information object.
 * \author Pralit Patel
 * \warning Markets are not necessarily set when completeInit is called
 */
void NonCO2Emissions::completeInit( const string& aRegionName, const string& aSectorName,
                                    const IInfo* aTechInfo )
{
    AGHG::completeInit( aRegionName, aSectorName, aTechInfo );

    const Modeltime* modeltime = scenario->getModeltime();
    mSavedEmissionsCoef.resize( modeltime->getmaxper(), -1.0 );
    
    // If an emissions coefficient was read-in or otherwise set, stash this coefficient
    if ( mEmissionsCoef.isInited() ) {
        // Initialize all years with initial value. Will be updated as calcuation proceeds.
        for( unsigned int aPeriod = 0; aPeriod < mSavedEmissionsCoef.size(); ++aPeriod ) {
            mSavedEmissionsCoef[ aPeriod ] = mEmissionsCoef;
        }
    }
    
    for ( CControlIterator controlIt = mEmissionsControls.begin(); controlIt != mEmissionsControls.end(); ++controlIt ) {
        (*controlIt)->completeInit( aRegionName, aSectorName, aTechInfo );
    }
}

/*!
 * \brief Perform initializations that only need to be done once per period.
 * \param aRegionName Region name.
 * \param aLocalInfo The local information object.
 * \param aPeriod Model period.
 */
void NonCO2Emissions::initCalc( const string& aRegionName, const IInfo* aTechInfo, const int aPeriod ) {
    AGHG::initCalc( aRegionName, aTechInfo, aPeriod );

    // Recalibrate the emissions coefficient if we have input emissions and this is
    // the initial vintage year of the technology.
    mShouldCalibrateEmissCoef = mInputEmissions.isInited() && aTechInfo->getBoolean( "new-vintage-tech", true );
    
     for ( CControlIterator controlIt = mEmissionsControls.begin(); controlIt != mEmissionsControls.end(); ++controlIt ) {
        (*controlIt)->initCalc( aRegionName, aTechInfo, this, aPeriod );
    }
}

double NonCO2Emissions::getGHGValue( const string& aRegionName,
                                     const vector<IInput*>& aInputs,
                                     const vector<IOutput*>& aOutputs,
                                     const ICaptureComponent* aSequestrationDevice,
                                     const int aPeriod ) const 
{   
    // Constants
    const double CVRT90 = 2.212; // 1975 $ to 1990 $
    // Conversion from teragrams (Tg=MT) of X per EJ to metric tons of X per GJ
    const double CVRT_Tg_per_EJ_to_Tonne_per_GJ = 1e-3;
    
    double GHGTax = mCachedMarket->getPrice( getName(), aRegionName, aPeriod, false );
    if( GHGTax == Marketplace::NO_MARKET_PRICE ){
        return 0;
    }

    // Get carbon storage cost from the sequestrion device if there is one.
    double storageCost = aSequestrationDevice ?
        aSequestrationDevice->getStorageCost( aRegionName, getName(), aPeriod ) : 0;

    // Get the remove fraction from the sequestration device. The remove
    // fraction is zero if there is no sequestration device.
    double removeFraction = aSequestrationDevice ? aSequestrationDevice->getRemoveFraction( getName() ) : 0;

    // Compute emissions reductions. These are only applied in future years
    double emissMult = 1.0;
    if ( aPeriod > scenario->getModeltime()->getFinalCalibrationPeriod() ) {
        for ( CControlIterator controlIt = mEmissionsControls.begin(); controlIt != mEmissionsControls.end(); ++controlIt ) {
            emissMult *= 1.0 - (*controlIt)->getEmissionsReduction( aRegionName, aPeriod, mGDP );
        }
    }

    /*!
     * \pre Attampting to recalibrate the emissions coefficient while trying to price the emissions
     *      will lead to inconsistent GHG value calculations because the value must be calculated
     *      before the new coefficient can be recalibrated.
     */
    assert( !mShouldCalibrateEmissCoef );

    // Adjust the GHG tax by taking into account the fraction sequestered, storage costs and adjusting
    // for the emissions intensity as well as reductions.
    double generalizedCost = ( ( 1.0 - removeFraction ) * GHGTax + removeFraction * storageCost ) *
        mEmissionsCoef * emissMult / CVRT90 * CVRT_Tg_per_EJ_to_Tonne_per_GJ;

    // The generalized cost returned by the GHG may be negative if
    // emissions crediting is occurring.
    return generalizedCost;
}

void NonCO2Emissions::calcEmission( const string& aRegionName, 
                                    const vector<IInput*>& aInputs,
                                    const vector<IOutput*>& aOutputs,
                                    const GDP* aGDP,
                                    ICaptureComponent* aSequestrationDevice,
                                    const int aPeriod )
{
    // Stash the GDP object if it has not been already.
    if( !mGDP ) {
        mGDP = aGDP;
    }

    // Ensure the user set an emissions coefficient in the input, either by reading it in, copying it from the previous period
    // or reading in the emissions
    if( !mEmissionsCoef.isInited() && !mShouldCalibrateEmissCoef ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "No emissions coefficient set for " << getName() << " in " << aRegionName << " in period " << aPeriod << endl;
    }
    
    // Primary output is always stored at position zero and used to drive
    // emissions.
    assert( aOutputs.size() > 0 && aOutputs[ 0 ] );
    double primaryOutput = aOutputs[ 0 ]->getPhysicalOutput( aPeriod );

    /*!
     * \warning This is a crude way to determine the input driver. This is problematic
     *          since different input units are not accounted for (although as a driver,
     *          this is less relevant because all inputs currently scale to each other)
     *          but also does not allow an unambiguous definition of an emissions factor.
     * \todo Need to add some way to flag the input objects (or perhaps give the input name
     *       to the GHG) to identify which object is the driver to be used for emissions.
     */
    const double totalInput = FunctionUtils::getPhysicalDemandSum( aInputs, aPeriod );
    const double emissDriver = mEmissionsDriver->calcEmissionsDriver( totalInput, primaryOutput );
    
    // If emissions were read in and this is an appropraite period, compute emissions coefficient
    if( mShouldCalibrateEmissCoef ) {
        mEmissionsCoef = emissDriver > 0 ? mInputEmissions / emissDriver : 0;
        // Since have updated emissions coefficient, updated stashed coefficient
        mSavedEmissionsCoef[ aPeriod ] = mEmissionsCoef;
    }
    
    // Compute emissions reductions. These are only applied in future years
    double emissMult = 1.0;
    if ( aPeriod > scenario->getModeltime()->getFinalCalibrationPeriod() ) {
        for ( CControlIterator controlIt = mEmissionsControls.begin(); controlIt != mEmissionsControls.end(); ++controlIt ) {
            emissMult *= 1.0 - (*controlIt)->getEmissionsReduction( aRegionName, aPeriod, mGDP );
        }
    }

    // Compute emissions, including any reductions.
    double totalEmissions = mEmissionsCoef * emissDriver * emissMult;

    // Compute emissions sequestration and adjust total emissions accordingly.
    if( aSequestrationDevice ) {
        double emissionsSequestered = aSequestrationDevice->calcSequesteredAmount(
           aRegionName, getName(), totalEmissions, aPeriod );
        mEmissionsSequestered[ aPeriod ] = emissionsSequestered;
        totalEmissions -= emissionsSequestered;
    }

    mEmissions[ aPeriod ] = totalEmissions;
    
    // Stash actual emissions coefficient including impact of any controls. Needed by control
    // objects that apply reductions relative to this emission coefficient value
    mSavedEmissionsCoef[ aPeriod ] = emissDriver > 0 ? totalEmissions / emissDriver : 0;
    
    addEmissionsToMarket( aRegionName, aPeriod );
}

void NonCO2Emissions::doInterpolations( const int aYear, const int aPreviousYear,
                                        const int aNextYear, const AGHG* aPreviousGHG,
                                        const AGHG* aNextGHG )
{
    const NonCO2Emissions* prevComplexEmiss = static_cast<const NonCO2Emissions*>( aPreviousGHG );
    const NonCO2Emissions* nextComplexEmiss = static_cast<const NonCO2Emissions*>( aNextGHG );
    
    /*!
     * \pre We are given a valid AComplexEmissions for the previous ghg.
     */
    assert( prevComplexEmiss );
    
    /*!
     * \pre We are given a valid AComplexEmissions for the next ghg.
     */
    assert( nextComplexEmiss );
}
