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
* \file unmanaged_land_technology.cpp
* \ingroup Objects
* \brief UnmanagedLandTechnology class source file.
* \author Steve Smith, Kate Calvin
*/

#include "util/base/include/definitions.h"
#include "technologies/include/unmanaged_land_technology.h"
#include "land_allocator/include/iland_allocator.h"
#include "land_allocator/include/aland_allocator_item.h"
#include "emissions/include/aghg.h"
#include "containers/include/scenario.h"
#include "containers/include/iinfo.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "technologies/include/ical_data.h"
#include "technologies/include/iproduction_state.h"
#include "technologies/include/ioutput.h"
#include "technologies/include/generic_output.h"
#include "functions/include/renewable_input.h"
#include "containers/include/market_dependency_finder.h"

using namespace std;

extern Scenario* scenario;

/*! 
 * \brief Constructor.
 * \param aName Technology name.
 * \param aYear Technology year.
 */
UnmanagedLandTechnology::UnmanagedLandTechnology( const string& aName, const int aYear )
:AgProductionTechnology( aName, aYear )
{
    mResourceInput = mInputs.end();
}

UnmanagedLandTechnology::UnmanagedLandTechnology() {
    mResourceInput = mInputs.end();
}

// ! Destructor
UnmanagedLandTechnology::~UnmanagedLandTechnology() {
}

//! Clone Function. Returns a deep copy of the current technology.
UnmanagedLandTechnology* UnmanagedLandTechnology::clone() const {
    UnmanagedLandTechnology* clone = new UnmanagedLandTechnology( mName, mYear );
    clone->copy( *this );
    return clone;
}

void UnmanagedLandTechnology::copy( const UnmanagedLandTechnology& aOther ) {
    AgProductionTechnology::copy( aOther );
    
    mLandItemName = aOther.mLandItemName;
    // note mResourceInput is left unitialized
}

//! write object to xml output stream
void UnmanagedLandTechnology::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    AgProductionTechnology::toDebugXMLDerived( period, out, tabs);
    XMLWriteElement( mLandItemName, "itemName", out, tabs );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& UnmanagedLandTechnology::getXMLName() const {
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
const string& UnmanagedLandTechnology::getXMLNameStatic() {
    const static string XML_NAME = "UnmanagedLandTechnology";
    return XML_NAME;
}

/*! \brief Return name to be used for input object containing land amount.
*
* NOTE -- this is here only to faciliate production of non-CO2 GHG emissions.
* A full implementation of land as a multiple input is yet to be done.
* \author Steve Smith
* \return The constant XML_NAME as a static.
*/
const string& UnmanagedLandTechnology::getLandInputName( ) const {
    const static string LAND_INPUT_NAME = "land-input";
    return LAND_INPUT_NAME;
}

/*! 
* \brief Perform initializations that only need to be done once per period.
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aSubsectorInfo Parent information container.
* \param aDemographics Regional demographics container.
* \param aPeriod Model period.
* \author Steve Smith
*/
void UnmanagedLandTechnology::initCalc( const string& aRegionName,
                                           const string& aSectorName,
                                           const IInfo* aSubsectorInfo,
                                           const Demographic* aDemographics,
                                           PreviousPeriodInfo& aPrevPeriodInfo,
                                           const int aPeriod )
{
    // need to initialize the unused share-weight
    // see FoodProductionTechnology::initCalc
    mShareWeight.set( 1.0 );
    Technology::initCalc( aRegionName, aSectorName, aSubsectorInfo, aDemographics, aPrevPeriodInfo, aPeriod );

    initializeInputLocations( aRegionName,aSectorName, aPeriod );
}

/*!
* \brief Complete the initialization of the technology.
* \note This routine is only called once per model run
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aDepDefinder Regional dependency finder.
* \param aSubsectorInfo Subsector information object.
* \param aLandAllocator Regional land allocator.
* \author Steve Smith
*/
void UnmanagedLandTechnology::completeInit( const string& aRegionName,
                                               const string& aSectorName,
                                               const std::string& aSubsectorName,
                                               const IInfo* aSubsectorInfo,
                                               ILandAllocator* aLandAllocator )
{
    // Initialize a renewable-energy input to hold amount of land in unmanaged land leaf
    // This is a renewable energy input (so nothing goes to the marketplace) and is here purely to 
    // allow non-CO2 GHG emissions that are proportional to unmanaged land area

    // This needs be be done before calling other methods so that 1) input vector size is fixed 
    // (so references won't change) and 2) so that initCalc() methods for new objects can be called.
   
    if( util::searchForValue( mInputs, getLandInputName() ) == mInputs.end() ){
        mInputs.push_back( new RenewableInput( getLandInputName() ) );
    }
    
    // Create the generic output for this technology. Insert the generic
    // output at position 0, so it can be used for emissions calculation.
    // Note we use a GenericOutput instead the typical PrimaryOutput becasuse
    // we want to avoid adding any values to the marketplace and instead are
    // only keeping track of the output for the sake of calculating Non-CO2s.
    mOutputs.push_back( new GenericOutput( aSectorName ) );

    Technology::completeInit( aRegionName, aSectorName, aSubsectorName, aSubsectorInfo,
                              aLandAllocator );
    
    mOutputs.erase( mOutputs.begin() );
	

    // Store away the corresponding leaf in the land allocator.
    mProductLeaf = aLandAllocator->findProductLeaf( mLandItemName );
    
    // Unmanaged land sector is dependent on the land allocator
    scenario->getMarketplace()->getDependencyFinder()->addDependency( "land-allocator",
                                                                      aRegionName,
                                                                      aSectorName,
                                                                      aRegionName );

}


void UnmanagedLandTechnology::calcCost( const string& aRegionName,
                                           const string& aSectorName,
                                           const int aPeriod )
{
    // Override costs to a non-zero value as the cost for a ag production
    // technology is not used for the shares.
    mCosts[ aPeriod ] = 1;
}

/*! \brief Calculates the output of the technology.
* \details For unmanaged land technologies primary output is always zero, but 
*          secondary output due to land-use change, proportional to amount of  
*          land-use change, is possible. Most common use of this would be for
*          products produced as a result of deforestation.
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aVariableDemand Subsector demand for output.
* \param aFixedOutputScaleFactor Fixed output scale factor.
* \param aGDP Regional GDP container.
* \param aPeriod Model period.
* \author Steve Smith, Kate Calvin
*/
void UnmanagedLandTechnology::production( const string& aRegionName,
                                             const string& aSectorName,
                                             const double aVariableDemand,
                                             const double aFixedOutputScaleFactor,
                                             const GDP* aGDP,
                                             const int aPeriod )
{
    // If the product does not exist then no calculations need to occur.
    if( !mProductLeaf ) {
        return;
    }
    
    // If this technology is not operating this period then return without calculating emissions
    if( !mProductionState[ aPeriod ]->isOperating() ){
        return;
    }

    // Set the input to be the land used. 
    double landInput = mProductLeaf->getLandAllocation( mLandItemName, aPeriod );

    double previousLandAllocation = 0;
    if ( aPeriod > 0 ) {
      previousLandAllocation = mProductLeaf->getLandAllocation( mLandItemName, aPeriod - 1 );
    }

    // Land use decrease will be temporarily set as the primary output and is only
    // used when calculating deforestation emissions.  Divide by time-step to get
    // the average annual decrease.
    int timestep = scenario->getModeltime()->gettimestep( aPeriod );
    double annualLandUseDecrease = ( previousLandAllocation - landInput ) / timestep;
    if ( annualLandUseDecrease < 0 ) {
      annualLandUseDecrease = 0;
    }

    calcEmissionsAndOutputs( aRegionName, annualLandUseDecrease, aGDP, aPeriod );
}


/*!
 * \brief Calculate the non-CO2 emissions for the unmanaged land types
 * \details Uses the GHG object to calculate non-CO2 emissions. It sets the "primary output"
 *          equal to the average annual change in land cover. This way any GHG object with 
 *          an output driver will use land use change as a driver.  Input drivers will
 *          use land area as a driver.  After emissions are calculated, the primary output
 *          is set to zero since unmanaged land technologies do not produce anything.
 * \param aRegionName Region name.
 * \param aPrimaryOutput Average annual land use change
 * \param aGDP Regional GDP container.
 * \param aPeriod Period.
* \author Steve Smith
 */
void UnmanagedLandTechnology::calcEmissionsAndOutputs( const string& aRegionName,
                                          const double aPrimaryOutput,
                                          const GDP* aGDP,
                                          const int aPeriod )
{
    // Sets the "output" equal to the average annual land use change
    // This is used as an emissions driver
    
    mOutputs[0]->setPhysicalOutput( aPrimaryOutput, aRegionName, 0, aPeriod );

    // Set land amount to land input for purposes of emissions calculation in any input objects
    double landArea = mProductLeaf->getLandAllocation( mLandItemName, aPeriod );
    ( *mResourceInput )->setPhysicalDemand( landArea, aRegionName, aPeriod );

    // calculate emissions for each gas
    for ( unsigned int i = 0; i < mGHG.size(); ++i ) {
        mGHG[ i ]->calcEmission( aRegionName, mInputs , mOutputs, aGDP, mCaptureComponent, aPeriod );
    }
}

/*! 
 * \brief Initialize the cached locations of the internal land input.
 * \details Determines and caches the locations of the internal input used to store
 *          amount of land in the associated land leaf.
 * \param aRegionName Name of the containing region.
 * \param aSectorName Name of the containing sector.
 * \param aPeriod Period.
 * \warning If the input vector changes size, these positions will not be valid.
 */
void UnmanagedLandTechnology::initializeInputLocations( const string& aRegionName,
                                                       const string& aSectorName,
                                                       const int aPeriod )
{
    // Set the inputs to the error value.
    mResourceInput = mInputs.end();

    for( InputIterator i = mInputs.begin(); i != mInputs.end(); ++i ){
        // Parse location for energy inputs.
        if( ( *i )->hasTypeFlag( IInput::ENERGY ) ){
            if ( ( *i )->getName() == getLandInputName() && ( getLandInputName() != "" ) ) {
               mResourceInput = i;
               continue;
            } 
        }
    }
}



