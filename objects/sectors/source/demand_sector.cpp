/*! 
* \file demand_sector.cpp
* \ingroup Objects
* \brief DemandSector class source file.
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cmath>
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/xml_helper.h"
#include <algorithm>

#include "sectors/include/demand_sector.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/summary.h"
#include "sectors/include/subsector.h"
#include "containers/include/scenario.h"
#include "containers/include/gdp.h"
#include "demographics/include/demographic.h"
#include "util/base/include/ivisitor.h"
#include "reporting/include/indirect_emissions_calculator.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief Default constructor.
*
* Constructor initializes member variables with default values, sets vector sizes, and sets value of debug flag.
*
* \author Sonny Kim, Steve Smith, Josh Lurz
*/
DemandSector::DemandSector( const string& aRegionName ): Sector( aRegionName ){
    mIsPerCapitaBased = false;
    mIsPPP = false;

    // resize vectors
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    mService.resize( maxper );
    mIncomeElasticity.resize( maxper );
    mPriceElasticity.resize( maxper );
    mAEEI.resize( maxper );
    mCalFinalEnergy.resize( maxper, -1 );
    mBaseScaler.resize( maxper, 1.0 );
}

//! Default destructor
DemandSector::~DemandSector() {
}

/*! \brief Override the calculation of the final supply price to do nothing currently.
* \details Does not do anything.
* \param aGDP The regional GDP container.
* \param aPeriod The period in which to calculate the final supply price.
*/
void DemandSector::calcFinalSupplyPrice( const GDP* aGDP, const int aPeriod ){
}

/*! \brief Override the setting of supply to not produce any output.
* \details Demand sectors to do not produce any output.
* \author Josh Lurz
* \param aGDP GDP object uses to calculate various types of GDPs.
* \param aPeriod Model period.
*/
void DemandSector::supply( const GDP* aGDP, const int aPeriod ) {
}

/*! \brief Parses any attributes specific to derived classes
*
* Method parses any input data attributes (not child nodes, see XMLDerivedClassParse) that are specific to any classes derived from this class.
*
* \author Josh Lurz, Steve Smith
* \param nodeName The name of the curr node. 
* \param curr pointer to the current node in the XML input tree
*/
bool DemandSector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {

    const Modeltime* modeltime = scenario->getModeltime();

    if( nodeName == "priceelasticity" ) {
        XMLHelper<double>::insertValueIntoVector( curr, mPriceElasticity, modeltime );
    }
    else if( nodeName == "incomeelasticity" ){
        XMLHelper<double>::insertValueIntoVector( curr, mIncomeElasticity, modeltime );
    }
    else if( nodeName == "aeei" ) {
        XMLHelper<double>::insertValueIntoVector( curr, mAEEI, modeltime );
    }
    else if( nodeName == "perCapitaBased" ) {
        mIsPerCapitaBased = XMLHelper<bool>::getValue( curr );
    }
    else if( nodeName == "isPPP" ){
        mIsPPP = XMLHelper<bool>::getValue( curr );
    }
    else if( nodeName == "cal-final-energy" ){
        XMLHelper<double>::insertValueIntoVector( curr, mCalFinalEnergy, modeltime );
    }
    else if( nodeName == "base-scaler" ){
        XMLHelper<double>::insertValueIntoVector( curr, mBaseScaler, modeltime );
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
* \author Steve Smith, Josh Lurz
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void DemandSector::toInputXMLDerived( ostream& out, Tabs* tabs ) const {  

    const Modeltime* modeltime = scenario->getModeltime();

    // write the xml for the class members.
    XMLWriteElementCheckDefault( mOutputUnit, "outputUnit", out, tabs, string("SerUnit") );
    XMLWriteElementCheckDefault( mInputUnit, "inputUnit", out, tabs, string("EJ") );
    XMLWriteElementCheckDefault( mPriceUnit, "priceUnit", out, tabs, string("75$/Ser") );
    XMLWriteElementCheckDefault( mIsPerCapitaBased, "perCapitaBased", out, tabs, false );

    XMLWriteVector( mPriceElasticity, "priceelasticity", out, tabs, modeltime, 0.0 );
    XMLWriteVector( mIncomeElasticity, "incomeelasticity", out, tabs, modeltime, 0.0 );
    XMLWriteVector( mAEEI, "aeei", out, tabs, modeltime, 0.0 );
    XMLWriteVector( mBaseScaler, "base-scaler", out, tabs, modeltime, 1.0 );

    XMLWriteElementCheckDefault( mIsPPP, "isPPP", out, tabs, false );

    /* Don't write this out until we have a way of deactivating this for policy runs.
    for( unsigned int i = 0; i < mCalFinalEnergy.size(); i++ ){
        XMLWriteElementCheckDefault( mCalFinalEnergy[ i ], "mCalFinalEnergy", out, tabs, -1.0, modeltime->getper_to_yr( i ) );
    }
    */
}

//! Write object to debugging xml output stream.
void DemandSector::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {

    // write the xml for the class members.
    // write out the market string.
    XMLWriteElement( mOutputUnit, "outputUnit", out, tabs );
    XMLWriteElement( mInputUnit, "inputUnit", out, tabs );
    XMLWriteElement( mPriceUnit, "priceUnit", out, tabs );
    XMLWriteElement( mIsPerCapitaBased, "perCapitaBased", out, tabs );
    XMLWriteElement( getTechnicalChange( period ), "tech-change", out, tabs );

    // Now write out own members.
    XMLWriteElement( getEnergyInput( period ), "TFE", out, tabs );
    XMLWriteElement( outputsAllFixed( period ), "OutputAllFixed", out, tabs );
    XMLWriteElement( mService[ period ] / getTechnicalChange( period ),
                     "service", out, tabs );
    XMLWriteElement( getCalOutput( period ), "TotalCalOutput", out, tabs );
    XMLWriteElement( mService[ period ], "service-pre-tech-change", out, tabs );
    XMLWriteElement( mIncomeElasticity[ period ], "iElasticity", out, tabs );
    XMLWriteElement( mPriceElasticity[ period ], "pElasticity", out, tabs );
    XMLWriteElement( mAEEI[ period ], "aeei", out, tabs );
    XMLWriteElement( mCalFinalEnergy[ period ], "cal-final-energy", out, tabs );
    XMLWriteElement( getEnergyInput( period ), "actual-final-energy", out, tabs );
    XMLWriteElement( mBaseScaler[ period ], "base-scaler", out, tabs );

    // Write out the summary
    // summary[ period ].toDebugXML( period, out );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& DemandSector::getXMLName() const {
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
const string& DemandSector::getXMLNameStatic() {
    const static string XML_NAME = "demandsector";
    return XML_NAME;
}

/*! \brief Create new market for this Sector
*
* Normal demand sectors do not set any markets so this is blank.
*
* \author Steve Smith
*/
void DemandSector::setMarket() {    
}

/*! \brief Calibrate sector output
* \details This performs supply sector technology and sub-sector output/input
*          calibration. Determines total amount of calibrated and fixed output
*          and passes that down to the subsectors. The first part of this code
*          is similar to that for the supply sectors. The second portion is
*          specific to demand sectors. For demand sectors, the aggregate output
*          is assured to be the same as the summed calibrated output from the
*          technologies. If all subsector demands are calibrated (or zero) then
*          also adjusts AEEI in order to be consistent with calibrated values.
* \author Steve Smith
* \param regionName region name
* \param period Model period
*/
void DemandSector::calibrateSector( const GDP* aGDP, const int period ) {
    double totalCalOutputs = getCalOutput( period );
    double marketDemand;

    // TODO: Is this logic right? Shouldn't the cal output still be taken into
    // account if there is fixed output?
    if ( outputsAllFixed( period ) && util::isEqual( getFixedOutput( period ), 0.0 ) ) {
        marketDemand = totalCalOutputs; // If all outputs are calibrated, then make demand equal to calibrated outputs for consistancy
    }
    else {
        marketDemand = max( getOutput( period ) - getFixedOutput( period ), 0.0 );
    }

    const vector<double> subsecShares = calcSubsectorShares( aGDP, period );
    for ( unsigned int i = 0; i < subsec.size(); ++i ) {
        subsec[i]->adjustForCalibration( subsecShares[ i ] * marketDemand, aGDP, period );
    }

    // If outputs are all fixed then scale AEEI for aggregate demand function to
    // match this output so that all parameters are consistent. This can happen
    // many times during the calculation.
    if ( outputsAllFixed( period ) ) {
        if ( util::isEqual( getOutput( period ), 0.0 ) ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Service less than or equal to zero in demand sector " << name
                << " in region " << regionName << endl;
        } 
        else {
            double scaleFactor = totalCalOutputs / getOutput( period );
            scaleOutput( period, scaleFactor );
            if ( abs( scaleFactor - 1 ) > util::getSmallNumber() ) {
                ILogger& calibrationLog = ILogger::getLogger( "calibration_log" );
                calibrationLog.setLevel( ILogger::DEBUG );
                calibrationLog << "Scaled demand sector output for " << regionName
                    << ":" << name << " by " << scaleFactor << endl;
            }
        }
    }
}

/*! \brief Returns the weighted energy price.
* \param aGDP Regional GDP container.
* \param aPeriod Model period.
* \details Returns the price of the sector weighted by the output in the base
*          period.
* \return The weighted energy price.
*/
double DemandSector::getWeightedEnergyPrice ( const GDP* aGDP, const int aPeriod ) const {
    return getOutput( 0 ) * getPrice( aGDP, aPeriod );
}

/*! \brief Set output for the demand sector.
* \details Demand from the "dmd" parameter (could be energy or energy service)
*          is passed to subsectors. This is then shared out at the technology
*          level. In the case of demand, what is passed here is the energy
*          service demand. The technologies convert this to an energy demand.
*          The demand is then summed at the subsector level then later at the
*          Sector level to equal the total Sector output.
* \author Sonny Kim, Josh Lurz, Steve Smith
* \param aDemand Demand to be passed to the subsectors.
* \param aPeriod Model period
* \param aGDP GDP object uses to calculate various types of GDPs.
*/
void DemandSector::setOutput( const double aDemand, const GDP* aGDP, const int aPeriod ) {
    assert( util::isValidNumber( aDemand ) && aDemand >= 0 );
    // Determine if fixed output must be scaled because fixed supply
    // exceeded demand.
    double fixedOutput = getFixedOutput( aPeriod );
    double scaleFactor = 1;
    if( fixedOutput > aDemand ){
        scaleFactor = aDemand / fixedOutput;
    }

    // Calculate the demand for new investment.
    double newInvestment = max( aDemand - fixedOutput, 0.0 );
    const vector<double> subsecShares = calcSubsectorShares( aGDP, aPeriod );

    for ( unsigned int i = 0; i < subsec.size(); ++i ){
        // set subsector output from Sector demand
        subsec[ i ]->setOutput( subsecShares[ i ] * newInvestment, scaleFactor, aGDP, aPeriod );
    }

    // Set the final energy for the calibration market.
    const Modeltime* modeltime = scenario->getModeltime();
    if( aPeriod > modeltime->getFinalCalibrationPeriod() && mCalFinalEnergy[ aPeriod ] != -1 ){
        const string TFEMarketName = name + "-tfe";
        scenario->getMarketplace()->addToDemand( TFEMarketName, regionName, getEnergyInput( aPeriod ), aPeriod, true );
    }
}

void DemandSector::scaleOutput( int period, double scaleFactor ) {

    mBaseScaler[ period ] *= scaleFactor;

    ILogger& calibrationLog = ILogger::getLogger( "calibration_log" );
    calibrationLog.setLevel( ILogger::DEBUG );
    calibrationLog << "Scaled demand sector output for " << regionName << " " << name
        << " by " << scaleFactor << " using period 0 base output." << endl;
}

/*! \brief Perform any initializations needed for each period.
*
* Any initializations or calcuations that only need to be done once per period
* (instead of every iteration) should be placed in this function.
*
* \author James Blackwood
* \param aNationalAccount National accounts container.
* \param aDemographics Regional demographics object.
* \param aPeriod Period for which to initialize the DemandSector.
*/
void DemandSector::initCalc( NationalAccount* nationalAccount, const Demographic* aDemographics, const int aPeriod )
{
    Sector::initCalc( nationalAccount, aDemographics, aPeriod );
}

/*! \brief Complete the initialization of a demand sector.
* \param aRegionInfo Regional information object.
* \param aDependencyFinder The region's dependency finder.
* \param aLandAllocator Regional land allocator.
* \param aGlobalTechDB Global technology database.
*/
void DemandSector::completeInit( const IInfo* aRegionInfo,
                                 DependencyFinder* aDependencyFinder,
                                 ILandAllocator* aLandAllocator,
                                 const GlobalTechnologyDatabase* aGlobalTechDB )
{
    // default unit to Service Unit
    if ( mOutputUnit.empty() ) {
        mOutputUnit = "SerUnit"; 
    }
    // default unit to EJ
    if ( mInputUnit.empty() ) {
        mInputUnit = "EJ"; 
    }
    // default unit to $/Service
    if ( mPriceUnit.empty() ) {
        mPriceUnit = "75$/Ser"; 
    }
    
    // All demand sectors must have a non-zero base output.
    if( mBaseOutput < util::getSmallNumber() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "No base output read for demand sector " << name << ". Resetting to 1." << endl;
        mBaseOutput = 1;
    }

    // Check that price elasticities were not read-in for base years.
    const Modeltime* modeltime = scenario->getModeltime();
    for( int i = 0; i <= modeltime->getFinalCalibrationPeriod(); ++i ){
        if( !util::isEqual( mPriceElasticity[ i ], 0.0 ) ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Price elasticity for demand sector " << name
                    << " in region " << regionName << " for calibration year "
                    << modeltime->getper_to_yr( i ) << endl;
            mPriceElasticity[ i ] = 0;
        }
    }

    // units must be set before calling base sector complete init
    Sector::completeInit( aRegionInfo, aDependencyFinder, aLandAllocator, aGlobalTechDB );

    // Set up demand sector calibration market.
    if( count( mCalFinalEnergy.begin(), mCalFinalEnergy.end(), -1 ) != 0 ){
        Marketplace* marketplace = scenario->getMarketplace();
        const string TFEMarketName = name + "-tfe";
        marketplace->createMarket( regionName, regionName, TFEMarketName, IMarketType::INVERSE_CALIBRATION );
        for( unsigned int i = modeltime->getFinalCalibrationPeriod() + 1; i < mCalFinalEnergy.size(); ++i ){
            if( mCalFinalEnergy[ i ] != -1 ){
                // Solve all initialized periods.
                marketplace->setMarketToSolve( TFEMarketName, regionName, i );

                // Setup the constraint.
                marketplace->addToSupply( TFEMarketName, regionName, mCalFinalEnergy[ i ], i, true );

                // Set the initial price.
                double totalAEEI = pow( 1 + mAEEI[ i ], modeltime->gettimestep( i ) );
                marketplace->setPrice( TFEMarketName, regionName, totalAEEI, i );
            }
        }
    }

    // Check to see if demand sector has the same name as a market.
    // TODO: This check relies on SupplySector completeInit occurring first.
    Marketplace* marketplace = scenario->getMarketplace();
    if( marketplace->getPrice(name, regionName, modeltime->getBasePeriod(), false ) != Marketplace::NO_MARKET_PRICE ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Demand sector " << name << " in region " << regionName 
            << " appears to have the same name as an exiting market. This may cause an error. " << endl;
    }
}

/*! \brief Calculate end-use service price elasticity
*
*
* \author Sonny Kim
* \param period Model period
* \todo Sonny to add more to this description
*/
double DemandSector::getAdjustedPriceElasticity( const GDP* aGDP, int aPeriod ) const {
    // In the base period return the read-in price elasticity.
    // TODO: This should be done dynamically.
    const bool USE_READ_IN = true;
    if( aPeriod == 0 || USE_READ_IN ){
        return mPriceElasticity[ aPeriod ];
    }

    // Adjust the elasticity for the ratio.
    return mPriceElasticity[ 0 ] * getFuelPriceRatio( aGDP, aPeriod );
}

/*! \brief Get the ratio of service price to fuel price.
* \param aGDP Regional GDP container.
* \param aPeriod Model period.
* \return Service to fuel price ratio.
* \bug This avoids using current period shares for calculating the fuel price,
*      but then uses them for calculating the service price.
*/
double DemandSector::getFuelPriceRatio( const GDP* aGDP, const int aPeriod ) const {
    // Cannot calculate the ratio in the base period as fuel shares would be unknown.
    if( aPeriod == 0 ){
        return 1;
    }

    // Lag the shares one period.
    const vector<double> shares = calcSubsectorShares( aGDP, aPeriod - 1 );

    // Calculate a weighted average fuel cost.
    double sectorFuelCost = 0;
    for ( unsigned int i = 0; i < subsec.size(); ++i ) {
        sectorFuelCost += shares[ i ] * subsec[ i ]->getAverageFuelPrice( aGDP, aPeriod );
    }

    assert( sectorFuelCost > 0 );
    return getPrice( aGDP, aPeriod ) / sectorFuelCost;
}

/*! \brief Aggregate sector energy service demand function.
*
* Function calculates the aggregate demand for energy services and passes that
* down to the sub-sectors. Demand is proportional to either GDP (to a power) or
* GDP per capita (to a power) times population.
*
* \author Sonny Kim
* \param aGDP GDP object for calculating various types of gdps.
* \param aDemographics Demographics container.
* \param period Model period
* \todo Sonny to add more to this description if necessary
*/
void DemandSector::calcAggregateDemand( const GDP* aGDP,
                                       const Demographic* aDemographics,
                                       const int aPeriod )
{
    // Demand for service.
    if ( aPeriod == 0  ) {
        // Set the base output to the read-in level.
        mService[ aPeriod ] = getOutput( aPeriod );
    }
    else {
        double priceElasticity = getAdjustedPriceElasticity( aGDP, aPeriod );
        double priceRatio = getPrice( aGDP, aPeriod ) /
                            getPrice( aGDP, aPeriod - 1 );

        // If mIsPerCapitaBased, service_demand = B * P^r * GDPperCap^r * Population.
        if ( mIsPerCapitaBased ) { // demand based on per capita GDP.
            double perCapGDPRatio;
            if( mIsPPP ){
                perCapGDPRatio = aGDP->getPPPGDPperCap( aPeriod ) /
                                 aGDP->getPPPGDPperCap( aPeriod - 1 );
            }
            else {   
                perCapGDPRatio = aGDP->getGDPperCap( aPeriod ) /
                                 aGDP->getGDPperCap( aPeriod - 1 );
            }
            double populationRatio = aDemographics->getTotal( aPeriod )
                                     / aDemographics->getTotal( aPeriod - 1 );

            mService[ aPeriod ] = mBaseScaler[ aPeriod ] *
                                  mService[ aPeriod - 1 ] *
                                  pow( priceRatio, priceElasticity ) *
                                  pow( perCapGDPRatio,
                                    mIncomeElasticity[ aPeriod ] ) *
                                  populationRatio;
        }
        // If not mIsPerCapitaBased, service_demand = B * P^r * GDP^r
        else { // demand based on scale of GDP.
            double gdpRatio;
            if( mIsPPP ){
                gdpRatio = ( aGDP->getPPPGDPperCap( aPeriod ) *
                    aDemographics->getTotal( aPeriod ) )
                / ( aGDP->getPPPGDPperCap( aPeriod - 1 ) *
                aDemographics->getTotal( aPeriod - 1 ) );
            }
            else {
                gdpRatio = aGDP->getGDP( aPeriod ) /
                    aGDP->getGDP( aPeriod - 1 );
            }
            mService[ aPeriod ] = mBaseScaler[ aPeriod ] * 
                                  mService[ aPeriod - 1 ] *
                                  pow( priceRatio, priceElasticity ) *
                                    pow( gdpRatio,
                                         mIncomeElasticity[ aPeriod ] );
        }

    }
    
    // Get the technical change parameter from the calibration market.
    const Modeltime* modeltime = scenario->getModeltime();
    if( aPeriod > modeltime->getFinalCalibrationPeriod() &&
        mCalFinalEnergy[ aPeriod ] != -1 )
    {
        double totalAEEI =
            scenario->getMarketplace()->getPrice( name + "-tfe", regionName,
                                                  aPeriod, true );
        if( totalAEEI > 0 ){
            mAEEI[ aPeriod ] = pow( totalAEEI, double( 1 ) /
                double( modeltime->gettimestep( aPeriod ) ) ) - 1;
        }
    }
    assert( util::isValidNumber( mService[ aPeriod ] ) &&
            mService[ aPeriod ] >= 0 );

    if( mService[ aPeriod ] < util::getSmallNumber() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Service of " << mService[ aPeriod ]
                << " is less than or equal to zero for demand sector "
                << name << ". " << "Current price is "
                << getPrice( aGDP, aPeriod ) << "." << endl;
    }

    // Sets subsector outputs, technology outputs, and market demands. Adjust
    // demand using technical change.
    setOutput( mService[ aPeriod ] / getTechnicalChange( aPeriod ), aGDP,
               aPeriod );
}

/*! \brief Get the technical change for the period.
* \details Calculate the technical change using AEEI, autonomous end-use
*          energy intensity.
* \param aPeriod Model period.
* \return Technical change which has occurred in the given period.
*/
double DemandSector::getTechnicalChange( const int aPeriod ) const {
    // No technical change in the base period.
    if( aPeriod == 0 ){
        return 1;
    }

    // Calculate technical change which occurred in the current period.
    const Modeltime* modeltime = scenario->getModeltime();
    double techChange = getTechnicalChange( aPeriod - 1 ) 
        * pow( 1 + mAEEI[ aPeriod ], modeltime->gettimestep( aPeriod ) );

    /*! \post Technical change is positive. */
    assert( techChange > 0 );
    return techChange;
}

//! Write sector output to database.
void DemandSector::csvOutputFile( const GDP* aGDP,
                                  const IndirectEmissionsCalculator* aIndirectEmissCalc ) const {
    // function protocol
    void fileoutput3( string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);

    // function arguments are variable name, double array, db name, table name
    // the function writes all years
    // total Sector output
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    fileoutput3( regionName, getName(), " ", " ", "production", mOutputUnit, mService );
    // total Sector eneryg input
    for( int per = 0; per < maxper; ++per ){
        temp[ per ] = getInput( per );
    }
    fileoutput3( regionName, getName(), " ", " ", "consumption", mInputUnit, temp );
    // Sector price
    for( int per = 0; per < maxper; ++per ){
        temp[ per ] = getPrice( aGDP, per );
    }
    fileoutput3( regionName, getName(), " ", " ", "price", mPriceUnit, temp );

    // do for all subsectors in the Sector
    for( unsigned int i = 0; i < subsec.size(); ++i ){
        // output or demand for each technology
        subsec[ i ]->csvOutputFile( aGDP, aIndirectEmissCalc );
    }
}

//! Write MiniCAM style demand sector output to database.
void DemandSector::dbOutput( const GDP* aGDP,
                             const IndirectEmissionsCalculator* aIndEmissCalc ) const {

    const Modeltime* modeltime = scenario->getModeltime();

    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);

    // function protocol
    void dboutput4(string var1name,string var2name, string var3name,string var4name,
        string uname,vector<double> dout);

    const string secname = Sector::getName();

    // total sector output
    for ( int i = 0; i < maxper; i++ ) {
        temp[ i ] = getOutput( i );
    }
    dboutput4( regionName, "End-Use Service", "by Sector", secname, mOutputUnit, temp );
    for ( int i = 0; i < maxper; i++ ) {
        temp[ i ] = getService( i );
    }
    dboutput4( regionName, "End-Use Service", "by Sector w/o TC", secname, mOutputUnit, temp );

    // End-use technical change elasticity
    dboutput4(regionName,"End-Use Service","AEEI",secname," ", mAEEI );

    // End-use service price elasticity
    for ( int i = 0; i < maxper; i++ ) {
        temp[ i ] = getAdjustedPriceElasticity( aGDP, i );
    }
    dboutput4(regionName,"End-Use Service","Elasticity",secname + "_price"," ", temp );

    // Cumulative technical change
    for ( int i = 0; i < maxper; i++ ) {
        temp[ i ] = getTechnicalChange( i );
    }

    dboutput4( regionName,"End-Use Service","Cumulative-AEEI",secname," ", temp );

    // End-use service income elasticity
    dboutput4( regionName,"End-Use Service","Elasticity",secname + "_income"," ", mIncomeElasticity );

    // TFE for this demand sector
    for (int m=0;m<maxper;m++) {
        temp[m] = getEnergyInput( m );
    }
    dboutput4(regionName,"Final Energy Cons","by Sector", name, mInputUnit, temp);

    // sector fuel consumption by fuel type
    typedef map<string,double>:: const_iterator CI;
    map<string,double> tfuelmap = Sector::getfuelcons(m=0);
    // Write out total (zTotal) fuel consumption for each sector only.
    if( !tfuelmap.empty() ){
        CI fmap = --tfuelmap.end();
        for (m=0;m<maxper;m++) {
            temp[m] = Sector::getConsByFuel(m,fmap->first);
        }
        dboutput4( regionName, "Fuel Consumption", secname,fmap->first, mInputUnit, temp );
        dboutput4( regionName, "Fuel Consumption", "by End-Use Sector", secname, mInputUnit, temp );
        // output for zTotal gets written for each demand sector and dataviewer sums it up
        dboutput4( regionName, "Fuel Consumption", "by End-Use Sector", "zTotal", mInputUnit, temp );
    }

    // Sector fuel consumption by fuel type
    //map<string,double> tfuelmap = summary[0].getfuelcons();
    for (CI fmap=tfuelmap.begin(); fmap!=tfuelmap.end(); ++fmap) {
        for (int m=0; m<maxper; m++) {
            temp[m] = summary[m].get_fmap_second(fmap->first);
        }
        if( fmap->first == "" ){
            dboutput4( regionName, "Fuel Consumption", name + " by Fuel", "No Fuelname", mInputUnit, temp );
        }
        else {
            dboutput4( regionName, "Fuel Consumption", name + " by Fuel", fmap->first, mInputUnit, temp );
        }
    }

    // sector emissions for all greenhouse gases
    map<string,double> temissmap = summary[0].getemission(); // get gases for period 0
    for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
        for (int m=0;m<maxper;m++) {
            temp[m] = summary[m].get_emissmap_second(gmap->first);
        }
        dboutput4(regionName,"Emissions","Sec-"+secname,gmap->first,"MTC",temp);
    }

    // CO2 emissions by sector
    for (int m=0;m<maxper;m++) {
        temp[m] = summary[m].get_emissmap_second("CO2");
    }
    dboutput4(regionName,"CO2 Emiss","by Sector",secname,"MTC",temp);
    dboutput4(regionName,"CO2 Emiss",secname,"zTotal","MTC",temp);

    // CO2 indirect emissions by sector
    for (m= 0;m<maxper;m++) {
        temp[m] =  aIndEmissCalc->getIndirectEmissions( name, m );
    }
    dboutput4(regionName,"CO2 Emiss(ind)",secname,"zTotal","MTC",temp);

    // sector price (not normalized)
    for (int m=0;m<maxper;m++) {
        temp[m] = getPrice( aGDP, m );
    }
    dboutput4( regionName, "Price", secname, "zSectorAvg", mPriceUnit, temp);

    // sector price normalized to base price
    for (int m=0;m<maxper;m++) {
        temp[m] = getPrice( aGDP, m ) / getPrice( aGDP, 0 );
    }
    dboutput4(regionName,"Price","by End-Use Sector",secname,"Norm75",temp);

    // do for all subsectors in the sector
    MCoutput_subsec( aGDP, aIndEmissCalc );
}

//! Write out subsector results from demand Sector.
void DemandSector::MCoutput_subsec( const GDP* aGDP,
                                    const IndirectEmissionsCalculator* aIndirectEmissCalc) const {
    // Store sector output to pass it to subsector output routines which
    // need it to calculate the share.
    const int maxper = scenario->getModeltime()->getmaxper();
    vector<double> temp( maxper );
    for( int per = 0; per < maxper; ++per ){
        temp[ per ] = getOutput( per );
    }

    // do for all subsectors in the Sector
    for ( unsigned int i = 0; i < subsec.size(); ++i ){
        // output or demand for each technology
        subsec[ i ]->MCoutputDemandSector( aGDP );
        subsec[ i ]->MCoutputAllSectors( aGDP, aIndirectEmissCalc, temp );
    }
}

/*! \brief returns the demand sector service supplied.
*
*
* \author Sonny Kim
* \param period Model period
* \return amount of energy service supplied by this sector
*/
double DemandSector::getService( const int period ) const {
    return mService[period];
}

/*! \brief Returns sectoral energy consumption.
*
* Returns all input for energy sectors.
*
* \author Steve Smith
* \param period Model period
* \return total input
* \todo re-impliment this to be market based, not sector based so that this can work with multiple inputs
*/
double DemandSector::getEnergyInput( const int period ) const {
    if ( getSectorType() == "Energy" ) {
        return getInput( period );
    }

    // TODO: Remove this once this version is merged to the mainline which already handles
    // this problem through the sector type.
    if( name == "Cement" ){
        return 0;
    }
    return 0;
}

/*! \brief Get the output for the demand sector, which is equivalent to the demand.
* \param aPeriod Period to get the output for.
* \return Output for the period.
* \author Josh Lurz
*/
double DemandSector::getOutput( int aPeriod ) const {
    // In the base period return a read in output.
    if( aPeriod == 0 ){
        return mBaseOutput;
    }

    double output = 0;
    for ( unsigned int i = 0; i < subsec.size(); ++i ) {
        double subsecOutput = subsec[ i ]->getOutput( aPeriod );
        // error check.
        if ( !util::isValidNumber( subsecOutput ) ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Output for subsector " << subsec[ i ]->getName() << " in Sector " << name 
                << " in region " << regionName <<" is not valid." << endl;
            continue;
        }
        output += subsecOutput;
    }

    return output;
}

/*! \brief Return the price of the DemandSector.
* \details The price of a DemandSector is the weighted average subsector price.
* \param aPeriod Model period.
* \return Price.
* \todo Remove this calculation once DemandSectors are rewritten.
*/
double DemandSector::getPrice( const GDP* aGDP, const int aPeriod ) const {
    return Sector::getPrice( aGDP, aPeriod );
}

void DemandSector::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitDemandSector( this, aPeriod );
    Sector::accept( aVisitor, aPeriod );
    aVisitor->endVisitDemandSector( this, aPeriod );
}
