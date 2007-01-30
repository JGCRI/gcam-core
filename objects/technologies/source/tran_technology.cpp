/*! 
* \file tran_technology.cpp
* \ingroup Objects
* \brief transporation technology class source file.
* \author Sonny Kim, Josh Lurz, Steve Smith
*/

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <cmath>
#include "technologies/include/tran_technology.h"
#include "emissions/include/aghg.h"
#include "containers/include/scenario.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/gdp.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/iinfo.h"
#include "technologies/include/ical_data.h"
#include "technologies/include/ioutput.h"
#include "technologies/include/iproduction_state.h"
#include "technologies/include/marginal_profit_calculator.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

const string TranTechnology::XML_NAME = "tranTechnology";

/*! 
 * \brief Constructor.
 * \param aName Technology name.
 * \param aYear Technology year.
 */
TranTechnology::TranTechnology( const string& aName, const int aYear )
:Technology( aName, aYear ){
    mIntensity = 1;
    mLoadFactor = 1;
    mTechnicalChange = 0;
    mServiceOutput = 0;
}

//! Clone function. Returns a deep copy of the current TranTechnology.
TranTechnology* TranTechnology::clone() const {
    return new TranTechnology( *this );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& TranTechnology::getXMLName1D() const {
    return XML_NAME;
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
const std::string& TranTechnology::getXMLNameStatic1D() {
    return XML_NAME;
}

/*! \brief Function Parses any input variables specific to derived classes.
* \param nodeName The name of the XML node.
* \param curr A pointer to the XML DOM node.
* \author Josh Lurz, Sonny Kim
* \return Boolean for node match.
*/
bool TranTechnology::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    if( nodeName == "intensity" ){
        mIntensity = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "loadFactor" ){
        mLoadFactor = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "techchange" ){
        mTechnicalChange = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "serviceOutput" ){
        mServiceOutput = XMLHelper<double>::getValue( curr );
    }
    else {
        return false;
    }
    return true;
}

/*! \brief XML output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML
*
* \author Josh Lurz
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void TranTechnology::toInputXMLDerived( ostream& out, Tabs* tabs ) const {  
    XMLWriteElementCheckDefault( mIntensity, "intensity", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( mLoadFactor, "loadFactor", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( mTechnicalChange, "techchange", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( mServiceOutput, "serviceOutput", out, tabs, 0.0 );
}

/*! \brief XML output for debugging.
* Function writes output to debugging XML
* \author Josh Lurz, Sonny Kim
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void TranTechnology::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const { 
    XMLWriteElement( mIntensity, "intensity", out, tabs );
    XMLWriteElement( mLoadFactor, "loadFactor", out, tabs );
    XMLWriteElement( mServiceOutput, "serviceOutput", out, tabs );
    XMLWriteElement( getCumulativeTechnicalChange( period ), "techChangeCumm", out, tabs );
    XMLWriteElement( getOutput( period ) / mLoadFactor, "vehicleOutput", out, tabs );
    XMLWriteElement( getOutput( period ), "serviceOutput", out, tabs );
    XMLWriteElement( mTechnicalChange, "techchange", out, tabs );
}   

/*!
* \brief Complete the initialization of the technology.
* \note This routine is only called once per model run.
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aDepDefinder Regional dependency finder.
* \param aSubsectorInfo Subsector information object.
* \param aLandAllocator Regional land allocator.
* \author Josh Lurz
* \warning Markets are not necessarily set when completeInit is called
*/
void TranTechnology::completeInit( const string& aRegionName,
                                   const string& aSectorName,
                                   DependencyFinder* aDepFinder,
                                   const IInfo* aSubsectorInfo,
                                   ILandAllocator* aLandAllocator,
                                   const GlobalTechnologyDatabase* aGlobalTechDB )
{
    Technology::completeInit( aRegionName, aSectorName, aDepFinder,
                              aSubsectorInfo, aLandAllocator, aGlobalTechDB );
}

/*! \brief Perform initializations that only need to be done once per period.
* \details Check to see if illegal values have been read in. This avoids serious
*          errors that can be hard to trace.
* \param aSubsectorInfo Subsector information object.
* \param aRegionName Name of the containing region.
* \param aSectorName Name of the containing sector.
* \param aPeriod Model period.
* \author Sonny Kim, Josh Lurz
*/
void TranTechnology::initCalc( const string& aRegionName, const string& aSectorName, 
							  const IInfo* aSubsectorInfo, const Demographic* aDemographics,
							  const int aPeriod )   
{
	// initialize mOutput to read-in service output
    if( aPeriod <= 1 ) {
        mOutputs[ 0 ]->setPhysicalOutput( mServiceOutput, aRegionName,
                                          mCaptureComponent.get(), aPeriod );
    }

    Technology::initCalc( aRegionName, aSectorName, aSubsectorInfo, aDemographics, aPeriod );

    // Check if illegal values have been read in
    if ( mLoadFactor == 0 ) {
        mLoadFactor = 1;
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "LoadFactor was zero in technology: " << mName << ". Reset to 1." << endl;
    }
    
    if ( util::isEqual( mIntensity, 0.0 ) ) {
        mIntensity = 1;
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Intensity was zero in technology: " << mName << ". Reset to 1." << endl;
    }
}

/*! \brief Perform calculations that need to be done after solution is found.
* \param aRegionName Region name.
* \param aPeriod Model period.
* \author Sonny Kim
*/
void TranTechnology::postCalc( const string& aRegionName, const int aPeriod ) {
    Technology::postCalc( aRegionName, aPeriod );
}

/*! \brief Return transportation technology fuel cost.
* \param aRegionName The region containing the technology.
* \param aSectorName The sector containing the technology.
* \param aPeriod Period in which to calculate the fuel cost.
* \return A calculate fuel cost for the technology.
* \author Sonny Kim, Josh Lurz, Steve Smith
*/
double TranTechnology::getFuelCost( const string& aRegionName, const string& aSectorName, const int aPeriod ) const {
    const double CVRT90 = 2.212; // 1975 $ to 1990 $
    const double JPERBTU = 1055; // 1055 Joules per BTU
	const double GIGA = 1.0E9; // for getting price per GJ

    Marketplace* marketplace = scenario->getMarketplace();
    double fuelprice = marketplace->getPrice( mTechData->getFuelName(), aRegionName, aPeriod );
    /*! \invariant The market price of the fuel must be valid. */
    assert( fuelprice != Marketplace::NO_MARKET_PRICE );
    double secondaryValue = calcSecondaryValue( aRegionName, aPeriod );
	return ( ( fuelprice * mTechData->getFMultiplier() ) - secondaryValue  )
		       * mIntensity / getCumulativeTechnicalChange( aPeriod ) * JPERBTU / GIGA * CVRT90;
}

void TranTechnology::calcCost( const string& aRegionName, const string& aSectorName, const int aPeriod ) {
	// techcost in cost per vehicle
    double techCost = ( getFuelCost( aRegionName, aSectorName, aPeriod ) + getNonEnergyCost( aPeriod ) ) 
		                * pMultiplier;
    // Convert cost per vehicle to cost per service.
    // For example,  convert $/vehicle-mi into $/pass-mi or $/ton-mi 
    mCosts[ aPeriod ] = techCost / mLoadFactor;
}

//! Calculates fuel input and TranTechnology output.
/*! Adds demands for fuels and ghg emissions to markets in the marketplace
* NOTE, the demand passed in is the service demand, 
* which is then converted to a per vehicle demand through the loadfactor
* \param aRegionName name of the region
* \param aSectorName name of the product for this sector
* \param aVariableDemand Demand for the Technology's product.
* \param aFixedOutputScaleFactor Scale factor by which to reduce production when
*        fixed output is greater than demand.
* \param aGDP Regional GDP container.
* \param aPeriod Model period.
* \author Sonny Kim, Josh Lurz, Steve Smith
*/
void TranTechnology::production( const string& aRegionName, const string& aSectorName,
                                 double aVariableDemand, double aFixedOutputScaleFactor,
                                 const GDP* aGDP, const int aPeriod )
{
    assert( util::isValidNumber( aVariableDemand ) && aVariableDemand >= 0 );

    // Construct a marginal profit calculator. This allows the calculation of 
    // marginal profits to be lazy.
    MarginalProfitCalculator marginalProfitCalc( this );

    // Use the production state to determine output. This ensures the correct action
    // is taken when the technology is retired.
    double primaryOutput =
        mProductionState[ aPeriod ]->calcProduction( aRegionName,
                                                     aSectorName,
                                                     aVariableDemand,
                                                     &marginalProfitCalc,
                                                     aFixedOutputScaleFactor,
                                                     mShutdownDeciders,
                                                     aPeriod );

    // Convert from service demand (pass-km) to vehicle demand (vehicle-km)
    double vehicleOutput = primaryOutput / mLoadFactor;
        
    // for transportation technology use intensity instead of efficiency
    // convert from million Btu to EJ, (mInput in EJ)
    const double ECONV = 1.055e-9;

    mInput[ aPeriod ] = vehicleOutput * mIntensity * ECONV / getCumulativeTechnicalChange( aPeriod );
    
    Marketplace* marketplace = scenario->getMarketplace();    
    // set demand for fuel in marketplace
    marketplace->addToDemand( mTechData->getFuelName(), aRegionName, mInput[ aPeriod ], aPeriod );

    calcEmissionsAndOutputs( aRegionName, mInput[ aPeriod ], primaryOutput, aGDP, aPeriod );  
}

/*! \brief Calculate unnormalized shares.
* \param aRegionName Region name.
* \param aSectorName SecotrName.
* \param aGDP Regional GDP container.
* \param aPeriod Model period.
* \return The unnormalized share.
* \author Sonny Kim, Josh Lurz, Steve Smith
*/
double TranTechnology::calcShare( const string& aRegionName, const string& aSectorName,
                                  const GDP* aGDP, const int aPeriod ) const
{
    return Technology::calcShare( aRegionName, aSectorName, aGDP, aPeriod );
}

/*! \brief Calculate and get calibrated output based on calibrated input.
* \param aPeriod Model period.
* \return Calibrated output value.
* \author Sonny Kim, Josh Lurz, Steve Smith
*/
double TranTechnology::getCalibrationOutput( const int aPeriod ) const {
    if( mCalValue.get() && mProductionState[ aPeriod ]->isNewInvestment() ){
		// assumes intensity is in BTU/veh and service in millions
		// use if calibration input is in EJ
		//const double ECONV = 1.055e-9;
		// use if calibration input is in same energy type as intensity
		const double ECONV = 1.0e-9;
		return mCalValue->getCalInput( getEfficiency( aPeriod )) 
			* getCumulativeTechnicalChange( aPeriod ) * mLoadFactor 
			/ ( mIntensity * ECONV );
		// use for calibration based on read-in service
		//return mServiceOutput;
	}
    return -1;
}

/*! \brief Get technology intensity with technical change applied.
* \param aPeriod Model period.
* \return Technology intensity with technical change applied.
* \author Sonny Kim, Josh Lurz, Steve Smith
*/
double TranTechnology::getIntensity( const int aPeriod ) const {
    return mIntensity / getCumulativeTechnicalChange( aPeriod );
}

/*! \brief Get the efficiency with technical change applied.
* \param aPeriod Model period.
* \return The technology efficiency with technical change applied.
* \author Sonny Kim, Josh Lurz, Steve Smith
*/
double TranTechnology::getEfficiency( const int aPeriod ) const {
    return 1 / getIntensity( aPeriod );
}

/*! \brief Get the non-energy cost.
* \param aPeriod Model period.
* \return The non-energy cost.
* \author Sonny Kim, Josh Lurz, Steve Smith
*/
double TranTechnology::getNonEnergyCost( const int aPeriod ) const {
    return Technology::getNonEnergyCost( aPeriod );
}

/*! \brief Calculate the cumulative technical change for a period.
* \param aPeriod Period for which to calculate the cumulative technical change.
* \return Cumulative technical change.
* \author Sonny Kim, Josh Lurz, Steve Smith
*/
double TranTechnology::getCumulativeTechnicalChange( const int aPeriod ) const {
    /*! \pre The period is valid. */
    assert( aPeriod >= 0 && aPeriod < scenario->getModeltime()->getmaxper() );

    // Default to 1 for period 0 and 1.
    double cumulativeTechChange = 1;
    if( aPeriod > 1 ){
        const Modeltime* modeltime = scenario->getModeltime();
        const int timestep = modeltime->gettimestep( aPeriod );

        // Calculate cumulative technical change from period 1.
        // TODO: Correct this for variable timesteps.
        cumulativeTechChange = pow( 1 + mTechnicalChange, timestep * ( aPeriod - 1 ) );
    }

    /*! \post Cumulative technical change is greater than or equal to one.*/
    assert( cumulativeTechChange >= 1 );
    return cumulativeTechChange;
}
