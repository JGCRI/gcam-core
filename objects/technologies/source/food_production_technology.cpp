/*!
* \file food_production_technology.cpp
* \ingroup Objects
* \brief FoodProductionTechnology class source file.
* \author James Blackwood
*/

#include "technologies/include/food_production_technology.h"
#include "land_allocator/include/iland_allocator.h"
#include "emissions/include/ghg.h"
#include "containers/include/scenario.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

// Technology class method definition

//! Default constructor.
FoodProductionTechnology::FoodProductionTechnology(){
    mLandAllocator = 0;
    variableCost = 2; // Need a better default value for this (0 is probably ok, with a warning to logfile).
    fuelname = "none";
    calLandUsed = -1;
    calProduction = -1;
    calYield = -1;
    calObservedYield = -1;
    agProdChange = 0;
    mAboveGroundCarbon = 0;
    mBelowGroundCarbon = 0;
}

// ! Destructor
FoodProductionTechnology::~FoodProductionTechnology() {
}

//! Parses any input variables specific to derived classes
bool FoodProductionTechnology::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    const Modeltime* modeltime = scenario->getModeltime();

    if ( nodeName == "variableCost" ) {
        variableCost = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "landType" ) {
        landType = XMLHelper<string>::getValue( curr );
    }
    else if( nodeName == "calLandUsed" ) {
        calLandUsed = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "calYield" ) {
        calYield = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "calProduction" ) {
        calProduction = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "agProdChange" ) {
        agProdChange = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "above-ground-carbon" ){
        mAboveGroundCarbon = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "below-ground-carbon" ){
        mBelowGroundCarbon = XMLHelper<double>::getValue( curr );
    }
    else {
        return false;
    }
    return true;
}

//! write object to xml output stream
void FoodProductionTechnology::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    XMLWriteElement( landType, "landType", out, tabs );
    XMLWriteElement( variableCost, "variableCost", out, tabs );
    XMLWriteElementCheckDefault( calYield, "calYield", out, tabs, -1.0 );
    XMLWriteElementCheckDefault( calLandUsed, "calLandUsed", out, tabs, -1.0 );
    XMLWriteElementCheckDefault( calProduction, "calProduction", out, tabs, -1.0 );
    XMLWriteElementCheckDefault( agProdChange, "agProdChange", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( mAboveGroundCarbon, "above-ground-carbon", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( mBelowGroundCarbon, "below-ground-carbon", out, tabs, 0.0 );
}

//! write object to xml output stream
void FoodProductionTechnology::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    XMLWriteElement( landType, "landType", out, tabs );
    XMLWriteElement( variableCost, "variableCost", out, tabs );
    XMLWriteElement( calYield, "calYield", out, tabs );
    XMLWriteElement( calLandUsed, "calLandUsed", out, tabs );
    XMLWriteElement( calProduction, "calProduction", out, tabs );
    XMLWriteElement( agProdChange, "agProdChange", out, tabs );
    XMLWriteElement( mAboveGroundCarbon, "above-ground-carbon", out, tabs );
    XMLWriteElement( mBelowGroundCarbon, "below-ground-carbon", out, tabs );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& FoodProductionTechnology::getXMLName1D() const {
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
const string& FoodProductionTechnology::getXMLNameStatic1D() {
    const static string XML_NAME1D = "FoodProductionTechnology";
    return XML_NAME1D;
}

//! Clone Function. Returns a deep copy of the current technology.
FoodProductionTechnology* FoodProductionTechnology::clone() const {
    return new FoodProductionTechnology( *this );
}


bool FoodProductionTechnology::outputFixed( ) const {
    return ( calProduction != -1 ); // this sector has fixed output
}

//! return technology calibration value
double FoodProductionTechnology::getCalibrationOutput( ) const {
    return ( calProduction != -1 ) ? calProduction : 0;
}

/*! \brief Returns calibration status for this technoloy
*
* This is true if a calibration value has been read in for this technology.
* 
* \author James Blackwood
* \return Boolean that is true if technoloy is calibrated
*/
bool FoodProductionTechnology::getCalibrationStatus( ) const {
    return ( calProduction != -1 );
}

/*! 
* \brief Perform initializations that only need to be done once per period.
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aSubsectorInfo Parent information container.
* \param aLandAllocator Regional land allocator.
* \param aPeriod Model period.
*/
void FoodProductionTechnology::initCalc( const string& aRegionName,
                                         const string& aSectorName,
                                         const IInfo* aSubsectorInfo,
                                         const Demographic* aDemographics,
                                         const int aPeriod )
{
    technology::initCalc( aRegionName, aSectorName, aSubsectorInfo, aDemographics, aPeriod );
    const Modeltime* modeltime = scenario->getModeltime();

    // Only setup calibration information if this is the initial year of the
    // technology.
    if( aPeriod != modeltime->getyr_to_per( year ) ){
        return;
    }

    // Only apply technical change if the year is past the base year.
    if( year > modeltime->getper_to_yr( 1 ) ){
        mLandAllocator->applyAgProdChange( landType, name, agProdChange, aPeriod );
    }

    // TODO: Use a better method of passing forward calibration information.
    // Since the market may be global, create a unique regional string for the
    // calibrated variable cost.
    const string calVarCostName = "calVarCost" + aRegionName;
    double calVarCost;

    // Get the information object for this market.
    Marketplace* marketplace = scenario->getMarketplace();
    IInfo* marketInfo = marketplace->getMarketInfo( aSectorName, aRegionName, aPeriod, true );
    assert( marketInfo );
    if( calObservedYield != -1 ) {
        double calPrice = marketInfo->getDouble( "calPrice", true );

        // Calculate the calibrated variable cost.
        // TODO: This is the only access of this variable from outside the AgLU. Change this function
        // to getCalNumeraireAveObservedRate.
        calVarCost = calPrice - mLandAllocator->getCalAveObservedRate( "UnmanagedLand", aPeriod )
                                / calcDiscountFactor()
                                / calObservedYield;
        
        // Set the variable cost for the technology to the calibrated variable cost.
        if ( calVarCost > util::getSmallNumber() ) {
            // TODO: Add warning if there was a read-in variable cost.
            variableCost = calVarCost;
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::DEBUG );
            mainLog << "Read in value for calPrice in " << aRegionName << " "
                    << name << " is too low by " << fabs( calVarCost ) << endl;
        }
    }
    else {
        // Get the calibrated variable cost from the market info.
        calVarCost = marketInfo->getDouble( calVarCostName, true );
        if ( calVarCost > util::getSmallNumber() ) {
            // TODO: Add warning if there was a read-in variable cost.
            variableCost = calVarCost;
        }
    }

    // If this is not the end period of the model, set the market info
    // variable for the next period.
    if( aPeriod + 1 < modeltime->getmaxper() ){
        IInfo* nextPerMarketInfo = marketplace->getMarketInfo( aSectorName, aRegionName, aPeriod + 1, true );
        assert( nextPerMarketInfo );
        nextPerMarketInfo->setDouble( calVarCostName, calVarCost );
    }

    // Set the above and below ground carbon for this technology.
    // TODO: This may need to be moved if the carbon content is calculated dynamically.
    mLandAllocator->setCarbonContent( landType, name, mAboveGroundCarbon, mBelowGroundCarbon, aPeriod );
}

/*!
* \brief Complete the initialization of the technology.
* \note This routine is only called once per model run
* \param aSectorName Sector name, also the name of the product.
* \param aDepDefinder Regional dependency finder.
* \param aSubsectorInfo Subsector information object.
* \param aLandAllocator Regional land allocator.
* \author James Blackwood
* \warning Markets are not necesarilly set when completeInit is called
*/
void FoodProductionTechnology::completeInit( const string& aSectorName,
                                             DependencyFinder* aDepFinder,
                                             const IInfo* aSubsectorInfo,
                                             ILandAllocator* aLandAllocator )
{
    // Store away the land allocator.
    mLandAllocator = aLandAllocator;

    // Setup the land usage for this production. Only add land usage once for
    // all technologies, of a given type. TODO: This is error prone if
    // technologies don't all have the same land type.
    if( year == scenario->getModeltime()->getStartYear() ){
        mLandAllocator->addLandUsage( landType, name, ILandAllocator::eCrop );
    }

    const int period = scenario->getModeltime()->getyr_to_per( year );
    if ( ( calProduction != -1 ) && ( calLandUsed != -1 ) ) {
        calObservedYield = calProduction / calLandUsed;

        // Warn the user that the calibrated yield will not be used since an
        // observed yield can be calculated.
        if( calYield != -1 ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::NOTICE );
            mainLog << "Calibrated yield will be overridden by the observied yield." << endl;
        }

        // Want to pass in yied in units of GCal/kHa
        mLandAllocator->setCalLandAllocation( landType, name, calLandUsed, period, period );
        mLandAllocator->setCalObservedYield( landType, name, calObservedYield, period );
    } 
    else if ( calYield != -1 ) {
        mLandAllocator->setCalObservedYield( landType, name, calYield, period );
    }

    technology::completeInit( aSectorName, aDepFinder, aSubsectorInfo,
                              aLandAllocator );
}

/*!
* \brief Calculate unnormalized technology unnormalized shares.
* \details Since food and forestry technolgies are profit based, they do not
*          directly calculate a share. Instead, their share of total supply is
*          determined by the sharing which occurs in the land allocator. To
*          facilitate this the technology sets the intrinisic rate for the land
*          use into the land allocator. The technology share itself is set to 1.
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aGDP Regional GDP container.
* \param aPeriod Model period.
* \author James Blackwood, Steve Smith
*/
void FoodProductionTechnology::calcShare( const string& aRegionName,
                                          const string& aSectorName,
                                          const GDP* aGDP,
                                          const int aPeriod )
{
    // If yield is GCal/Ha and prices are $/GCal, then rental rate is $/Ha
    // Passing in rate as $/GCal and setIntrinsicRate will set it to  $/Ha.
    double profitRate = calcProfitRate( aRegionName, aSectorName, aPeriod );

    mLandAllocator->setIntrinsicRate( aRegionName, landType, name, profitRate, aPeriod );
    
    // Food production technologies are profit based, so the amount of output
    // they produce is independent of the share.
    share = 1;
}

/*! \brief Calculate technology fuel cost and total cost.
*
* This caculates the cost (per unit output) of this specific technology. 
* The cost includes fuel cost, carbon value, and non-fuel costs.
* Conversion efficiency, and optional fuelcost and total price multipliers are used if specified.

* \author James Blackwood
* \todo The GHG cost is not feeding into the land use decision or technology cost right now. 
*/
void FoodProductionTechnology::calcCost( const string& regionName, const string& sectorName, const int per ) {
    // Set the techcost to 1 to avoid zero shares.
    // TODO: Fix share calculation to avoid this.
    techcost = 1;
}

/*! \brief Calculates the output of the technology.
* \details Calculates the amount of current forestry output based on the amount
*          of planted forestry land and it's yield. Forestry production
*          technologies are profit based and determine their supply
*          independently of the passed in subsector demand. However, since this
*          is a solved market, in equalibrium the sum of the production of
*          technologies within a sector will equal the demand for the sector.
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aDemand Subsector demand for output.
* \param aGDP Regional GDP container.
* \param aPeriod Model period.
* \todo Need better way to deal with biomass units than the hard coded
*       conversion below. 
*/
void FoodProductionTechnology::production( const string& aRegionName,
                                           const string& aSectorName,
                                           const double aDemand,
                                           const GDP* aGDP,
                                           const int aPeriod )
{
    // Calculate the profit rate.
    double profitRate = calcProfitRate( aRegionName, aSectorName, aPeriod );

    // Calculate the yield.
    mLandAllocator->calcYield( landType, name, aRegionName, 
                               profitRate, aPeriod, aPeriod );

    // Calculate the output of the technology.
    double primaryOutput = calcSupply( aRegionName, aSectorName, aPeriod );

    // This output needs to be in EJ instead of GJ.
    // TODO: Fix this once units framework is complete.
    if( name == "biomass" ) {
        primaryOutput /= 1e9;
    }

    // Set the input to be the land used. TODO: Determine a way to improve this.
    // This would be wrong if the fuelname had an emissions coefficient, or if
    // there were a fuel or other input. When multiple inputs are complete there
    // should be a specific land input.
    input = mLandAllocator->getLandAllocation( name, aPeriod );
    calcEmissionsAndOutputs( aRegionName, input, primaryOutput, aGDP, aPeriod );
}

/*! \brief Calculate the profit rate for the technology.
* \details Calculates the profit rate which is equal to the market price minus
*          the variable cost.
*          Profit rate can be negative.
* \param aRegionName Region name.
* \param aProductName Name of the product for which to calculate the profit
*        rate. Must be an output of the technology.
* \return The profit rate.
*/
double FoodProductionTechnology::calcProfitRate( const string& aRegionName,
                                                 const string& aProductName,
                                                 const int aPeriod ) const
{
    // Conversion from 1990 to 1975 dollars
    const double CVRT_75_TO_90 = 2.212;
    
    // Calculate profit rate.
    const Marketplace* marketplace = scenario->getMarketplace();

    // TODO: Units here will be wrong for anything other than biomass because prices will be in $/Gcal
    // as GHG costs/profits are always in $/GJ.
    double secondaryValue = calcSecondaryValue( aRegionName, aPeriod );
    double profitRate = ( marketplace->getPrice( aProductName, aRegionName, aPeriod ) + secondaryValue ) 
                         * CVRT_75_TO_90 - variableCost;

    return profitRate;
}

/*! \brief Calculate the factor to discount between the present period and the
*          harvest period.
* \return The discount factor.
*/
double FoodProductionTechnology::calcDiscountFactor() const {
    // Food products are produced in a single year, so they do not have a
    // discount factor.
    return 1;
}

/*! \brief Calculate the supply for the technology.
* \details Calculates the food produced which is equal to the yield multiplied
*          by the land allocated.
* \param aProductName Product name.
* \param aPeriod Period.
* \return The supply produced by the technology.
*/
double FoodProductionTechnology::calcSupply( const string& aRegionName,
                                             const string& aProductName,
                                             const int aPeriod ) const
{
    double yield = mLandAllocator->getYield( landType, name, aPeriod ); 
    double landAllocation = mLandAllocator->getLandAllocation( name, aPeriod );
    // Check that if yield is zero the land allocation is zero.
    // TODO: Determine why a small number is too large.
    if( yield < util::getSmallNumber() && landAllocation > 0.1 ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Zero production of " << aProductName << " by technology " << name
                << " in region " << aRegionName << " with a positive land allocation of "
                << landAllocation << "." << endl;
    }

    // Set output to yield times amount of land.
    return yield * landAllocation;
}

double FoodProductionTechnology::getFuelcost() const {
    return variableCost;
}
