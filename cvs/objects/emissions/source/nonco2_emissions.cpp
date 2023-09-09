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

#include "emissions/include/nonco2_emissions.h"
#include "emissions/include/iemissions_driver.h"
#include "emissions/include/aemissions_control.h"
#include "containers/include/scenario.h"
#include "util/base/include/xml_helper.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"
#include "marketplace/include/cached_market.h"
#include "technologies/include/icapture_component.h"

using namespace std;

extern Scenario* scenario;

//! Default constructor.
NonCO2Emissions::NonCO2Emissions():
AGHG(),
mEmissionsDriver( 0 )
{
    // default unit for emissions
    mEmissionsUnit = "Tg";
}

//! Default destructor.
NonCO2Emissions::~NonCO2Emissions(){
    clear();
}

//! Clone operator.
NonCO2Emissions* NonCO2Emissions::clone() const {
    NonCO2Emissions* clone = new NonCO2Emissions();
    clone->copy( *this );
    return clone;
}

//! Clear any dynamically allocated memory
void NonCO2Emissions::clear() {
    for ( CControlIterator controlIt = mEmissionsControls.begin(); controlIt != mEmissionsControls.end(); ++controlIt ) {
        delete *controlIt;
    }
    mEmissionsControls.clear();
    delete mEmissionsDriver;
}

//! Copy helper function.
void NonCO2Emissions::copy( const NonCO2Emissions& aOther ) {
    AGHG::copy( aOther );
    
    mEmissionsCoef = aOther.mEmissionsCoef;
    
    // Deep copy the unique_ptr
    if( aOther.mEmissionsDriver ){
        delete mEmissionsDriver;
        mEmissionsDriver = aOther.mEmissionsDriver->clone();
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
        abort();
    }
    
    mEmissionsUnit = prevComplexGHG->mEmissionsUnit;
    
    if( !mEmissionsDriver ) {
        mEmissionsDriver = prevComplexGHG->mEmissionsDriver->clone();
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
        
    // Always copy control objects from previous period except for those read in for this period that
    // have the same name and type as an object from the previous period. In that latter case, use
    // the newer one instead of copying the older one.
    // Loop through all prev control objects.
    // TODO: Also check for match of type of object once this is supported (GCAM Fusion may facilitate this)
    for( CControlIterator prevControlIt = prevComplexGHG->mEmissionsControls.begin();
         prevControlIt != prevComplexGHG->mEmissionsControls.end(); ++prevControlIt )
    {
        // Default to no match, which means will copy forward
        // If there is nothing read in this is what we want to happen
        bool isAMatch = false;
        // Check if any of the new objects match the previous objects
        for( CControlIterator newControlIt = mEmissionsControls.begin();
             !isAMatch && newControlIt != mEmissionsControls.end(); ++newControlIt )
        {
            isAMatch = (*newControlIt)->getName() == (*prevControlIt)->getName();
        }
        // If there was no match, then copy old object forward
        if( !isAMatch ) {
            mEmissionsControls.push_back( (*prevControlIt)->clone() );
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

void NonCO2Emissions::toDebugXMLDerived( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteElement( mEmissionsCoef, "emiss-coef", aOut, aTabs );
    XMLWriteElement( mInputEmissions, "input-emissions", aOut, aTabs );
    
    mEmissionsDriver->toDebugXML( aPeriod, aOut, aTabs );
    
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
    bool shouldCalibrateEmissCoef = mInputEmissions.isInited() && aTechInfo->getBoolean( "new-vintage-tech", true );
    
    for ( CControlIterator controlIt = mEmissionsControls.begin(); controlIt != mEmissionsControls.end(); ++controlIt ) {
        (*controlIt)->initCalc( aRegionName, aTechInfo, this, aPeriod );
    }

    // Ensure the user set an emissions coefficient in the input, either by reading it in, copying it from the previous period
    // or reading in the emissions
    if( !mEmissionsCoef.isInited() && !shouldCalibrateEmissCoef ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "No emissions coefficient set for " << getName() << " in " << aRegionName << " in period " << aPeriod << endl;
        abort();
    }
    
    
    if ( !mEmissionsDriver ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "No emissions driver set for " << getName()
                << " in " << aRegionName << endl;
        abort();
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
            emissMult *= 1.0 - (*controlIt)->getEmissionsReduction( aRegionName, aPeriod );
        }
    }
    
    /*!
     * \warning Attempting to recalibrate the emissions coefficient while trying to price the emissions
     *      will lead to inconsistent GHG value calculations because the value must be calculated
     *      before the new coefficient can be recalibrated.
     */
    
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
                                    ICaptureComponent* aSequestrationDevice,
                                    const int aPeriod )
{
    // calculate the emissions driver
    const double emissDriver = mEmissionsDriver->calcEmissionsDriver( aInputs, aOutputs, aPeriod );
    
    // Compute emissions reductions.
    double emissMult = 1.0;
    for ( CControlIterator controlIt = mEmissionsControls.begin(); controlIt != mEmissionsControls.end(); ++controlIt ) {
            emissMult *= 1.0 - (*controlIt)->getEmissionsReduction( aRegionName, aPeriod );
    }
    
    // Compute emissions, including any reductions.
    double totalEmissions = mEmissionsCoef * emissDriver * emissMult;
    
    // Compute emissions sequestration and adjust total emissions accordingly.
    if( aSequestrationDevice ) {
        double emissionsSequestered = aSequestrationDevice->calcSequesteredAmount(
           aRegionName, getName(), totalEmissions, aPeriod );
        totalEmissions -= emissionsSequestered;
    }
    
    mEmissions[ aPeriod ] = totalEmissions;
    addEmissionsToMarket( aRegionName, aPeriod );
}

void NonCO2Emissions::postCalc( const string& aRegionName,
                                const bool aIsInitialTechYear,
                                const vector<IInput*>& aInputs,
                                const vector<IOutput*>& aOutputs,
                                ICaptureComponent* aSequestrationDevice,
                                const int aPeriod )
{
    const double emissDriver = mEmissionsDriver->calcEmissionsDriver( aInputs, aOutputs, aPeriod );
    bool shouldCalibrateEmissCoef = aIsInitialTechYear && mInputEmissions.isInited();
    if(shouldCalibrateEmissCoef) {
        // If emissions were read in and this is an appropriate period, compute emissions coefficient
        mEmissionsCoef = emissDriver > 0 ? mInputEmissions / emissDriver : 0;
        // We need to recalculate the emissions too now that we actually have the coefficient
        // NOTE: It is an error to have model solution dependent on these emissions (i.e. have a policy
        // on this GHG) while trying to calibrate input emissions as that creates a simultaneous equation,
        // therefore postponing this to postCalc is safe to do.
        // We explicitly check to make sure this wasn't attempted here
        calcEmission(aRegionName, aInputs, aOutputs, aSequestrationDevice, aPeriod);
        if(getGHGValue(aRegionName, aInputs, aOutputs, aSequestrationDevice, aPeriod) != 0.0) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Attempting to calibrate an emissions coef while a policy is active is an error: " << getName()
                    << " in " << aRegionName << endl;
            abort();
        }
    }
    
    // back calculate the final adjusted emissions coef in case some control
    // object needs to stash it
    double adjustedEmissCoef = emissDriver > 0.0 ? mEmissions[aPeriod] / emissDriver : 0.0;
    for(auto emissControl : mEmissionsControls) {
        emissControl->setAdjustedEmissCoef(adjustedEmissCoef, aPeriod);
    }
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
