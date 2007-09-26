/*!
* \file unmanaged_land_technology.cpp
* \ingroup Objects
* \brief UnmanagedLandTechnology class source file.
* \author Steve Smith
*/

#include "util/base/include/definitions.h"
#include "technologies/include/unmanaged_land_technology.h"
#include "land_allocator/include/iland_allocator.h"
#include "emissions/include/aghg.h"
#include "containers/include/scenario.h"
#include "containers/include/iinfo.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "technologies/include/ical_data.h"
#include "technologies/include/iproduction_state.h"
#include "technologies/include/ioutput.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! 
 * \brief Constructor.
 * \param aName Technology name.
 * \param aYear Technology year.
 */
UnmanagedLandTechnology::UnmanagedLandTechnology( const string& aName, const int aYear )
:FoodProductionTechnology( aName, aYear ){
}

// ! Destructor
UnmanagedLandTechnology::~UnmanagedLandTechnology() {
}

//! Parses any input variables specific to derived classes
bool UnmanagedLandTechnology::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    if( nodeName == "leafName" ) {
        mLeafName = XMLHelper<string>::getValue( curr );
    }
    else if( nodeName == "carbon-to-energy-conversion" ) {
        mCarbonToEnergy = XMLHelper<double>::getValue( curr );
    }
    else if( !FoodProductionTechnology::XMLDerivedClassParse(nodeName, curr)) {
        return false;
    }
    return true;
}

//! write object to xml output stream
void UnmanagedLandTechnology::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    FoodProductionTechnology::toInputXMLDerived( out, tabs);

   XMLWriteElement( mLeafName, "leafName", out, tabs );
   XMLWriteElement( mCarbonToEnergy, "carbon-to-energy-conversion", out, tabs );
}

//! write object to xml output stream
void UnmanagedLandTechnology::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    FoodProductionTechnology::toDebugXMLDerived( period, out, tabs);
   XMLWriteElement( mLeafName, "leafName", out, tabs );
   XMLWriteElement( mCarbonToEnergy, "carbon-to-energy-conversion", out, tabs );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& UnmanagedLandTechnology::getXMLName1D() const {
    return getXMLNameStatic1D();
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
const string& UnmanagedLandTechnology::getXMLNameStatic1D() {
    const static string XML_NAME = "UnmanagedLandTechnology";
    return XML_NAME;
}

//! Clone Function. Returns a deep copy of the current technology.
UnmanagedLandTechnology* UnmanagedLandTechnology::clone() const {
    return new UnmanagedLandTechnology( *this );
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
                                           const int aPeriod )
{
    // This is skiping parent class. This is bad form and can be corrected once ag classes need to be re-factored
    // TODO re-factor Ag production classes

    Technology::initCalc( aRegionName, aSectorName, aSubsectorInfo, aDemographics, aPeriod );

}

/*!
* \brief Complete the initialization of the technology.
* \note This routine is only called once per model run
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aDepDefinder Regional dependency finder.
* \param aSubsectorInfo Subsector information object.
* \param aLandAllocator Regional land allocator.
* \param aGlobalTechDB Global Technology database.
* \author Steve Smith
*/
void UnmanagedLandTechnology::completeInit( const string& aRegionName,
                                               const string& aSectorName,
                                               DependencyFinder* aDepFinder,
                                               const IInfo* aSubsectorInfo,
                                               ILandAllocator* aLandAllocator,
                                               const GlobalTechnologyDatabase* aGlobalTechDB )
{
    // Setup the land allocators for the secondary outputs
    if ( mOutputs.size() ) {
        // Technology::completeInit() will add the primary output.
        // At this point, all are secondary outputs
        
        // Note that instead of techname mLeafName is passed
        for ( vector<IOutput*>::iterator outputIter = mOutputs.begin(); outputIter != mOutputs.end(); ++outputIter ) {
           ( *outputIter )->setLandAllocator( aLandAllocator, mLeafName, landType );
        }
    }

    // TODO: Change to be able to call the parent function.
    // Right now doesn't work since two classes aren't derived from common parent.
    // To do this, likely need a     ILandAllocator::LandUsageType getLandType() function so as to
    // create the proper land leaf type.
    
    Technology::completeInit( aRegionName, aSectorName, aDepFinder, aSubsectorInfo,
                              aLandAllocator, aGlobalTechDB );

    // Store away the land allocator.
    mLandAllocator = aLandAllocator;

}

/*!
* \brief Calculate unnormalized technology unnormalized shares.
* \details No actual share for this technology as it is driven by amount of land
*          which is determined through other means.
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aGDP Regional GDP container.
* \param aPeriod Model period.
* \return Technology share, always 1 for UnmanagedLandTechnologies.
* \author Steve Smith
*/
double UnmanagedLandTechnology::calcShare( const string& aRegionName,
                                              const string& aSectorName,
                                              const GDP* aGDP,
                                              const int aPeriod ) const
{
    assert( mProductionState[ aPeriod ]->isNewInvestment() );

    return 1;
}

void UnmanagedLandTechnology::calcCost( const string& aRegionName,
                                           const string& aSectorName,
                                           const int aPeriod )
{
    if( !mProductionState[ aPeriod ]->isOperating() ){
        return;
    }

    // Override costs to a non-zero value as the cost for a food production
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
* \author Steve Smith
*/
void UnmanagedLandTechnology::production( const string& aRegionName,
                                             const string& aSectorName,
                                             const double aVariableDemand,
                                             const double aFixedOutputScaleFactor,
                                             const GDP* aGDP,
                                             const int aPeriod )
{
    // now calculate the amount to be consumed this period (ie. planted steps
    // periods ago).
    double primaryOutput = 0;

    // Set the input to be the land used. 
    mInput[ aPeriod ] = mLandAllocator->getLandAllocation( landType, mLeafName, aPeriod );

    //TODO replace with annual veg carbon change once that is available
    double previousLandAllocation = 0;
    if ( aPeriod > 0 ) {
      previousLandAllocation = mLandAllocator->getLandAllocation( landType, mLeafName, aPeriod - 1 );
    }

    double landUseDecrease = previousLandAllocation - mInput[ aPeriod ] ;
    if ( landUseDecrease < 0 ) {
      landUseDecrease = 0;
   }

    calcEmissionsAndOutputs( aRegionName, mInput[ aPeriod ], landUseDecrease, aGDP, aPeriod );
}


/*!
 * \brief Calculate the emissions, primary and secondary outputs for the
 *        Technology.
 * \details Sets primary output to zero.
            Determines the secondary output levels and emissions for the Technology once
 *          the primary output and input quantities are known. Secondary output is
 *          proportional to land-use change. Emissions driven by output are driven with
 *          a value that subtracts the amount produced by secondary production.
 *          Emissions and outputs are added to the marketplace by the Output and GHG objects.
 * \param aRegionName Region name.
 * \param aInput Input quantity.
 * \param aPrimaryOutput Primary output quantity.
 * \param aGDP Regional GDP container.
 * \param aPeriod Period.
* \author Steve Smith
 */
void UnmanagedLandTechnology::calcEmissionsAndOutputs( const string& aRegionName,
                                          const double aInput,
                                          const double aPrimaryOutput,
                                          const GDP* aGDP,
                                          const int aPeriod )
{

    // Skip first slot for primary output
    for( unsigned int i = 1; i < mOutputs.size(); ++i ){
        mOutputs[ i ]->setPhysicalOutput( aPrimaryOutput, aRegionName,
                                          mCaptureComponent.get(), aPeriod );
    }

    // TODO -- much of the code in the rest of this section could likely be 
    // eliminated once units are in the model as we could then use the
    // input - output form of complex GHG driver. Can't use that form now
    // since input and output are in different units.
    
    // Subtract secondary output amount from primary output
    // The remainder drives deforestation emissions
    double secondaryOutputCarbon = 0;
     for( unsigned int i = 1; i < mOutputs.size(); ++i ){
       secondaryOutputCarbon += mOutputs[ i ]->getPhysicalOutput( aPeriod ) / mCarbonToEnergy;
    }
   
    double remainingOutput = aPrimaryOutput - secondaryOutputCarbon;
    remainingOutput = remainingOutput > 0 ? remainingOutput : 0;
   
   // Set primary output to remaining output for purposes of emissions calculation
    mOutputs[ 0 ]->setPhysicalOutput( remainingOutput, aRegionName, mCaptureComponent.get(), aPeriod );

    // calculate emissions for each gas
    for ( unsigned int i = 0; i < ghg.size(); ++i ) {
        ghg[ i ]->calcEmission( aRegionName, mTechData->getFuelName(), mInput[ aPeriod ] , mOutputs,
                                aGDP, mCaptureComponent.get(), aPeriod );
    }

    // Cancel previous set output to the market by subtracting from the marketplace
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->addToSupply( mOutputs[ 0 ]->getName(), aRegionName, -remainingOutput, aPeriod, false );

    // Set primary output to zero -- there is no primary output from unmanaged land
    mOutputs[ 0 ]->setPhysicalOutput( 0, aRegionName, mCaptureComponent.get(), aPeriod );
}

