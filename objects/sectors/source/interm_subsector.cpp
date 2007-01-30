/*!
* \file interm_subsector.cpp
* \ingroup Objects
* \brief IntermittentSubsector class source file.
* \author Marshall Wise
*/

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <vector>
#include <xercesc/dom/DOMNode.hpp>

#include "technologies/include/technology.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "sectors/include/interm_subsector.h"
#include "sectors/include/ibackup_calculator.h"
#include "sectors/include/backup_calculator_factory.h"
#include "containers/include/iinfo.h"
#include "sectors/include/sector_utils.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief Default constructor.
* \details Constructor passes up to Subsector constructor.
* \author Marshall Wise
*/

IntermittentSubsector::IntermittentSubsector( const string& aRegionName,
                                              const string& aSectorName ) 
:Subsector( aRegionName, aSectorName ),
resourceTechNumber( 0 ),
backupTechNumber( 1 ),
electricSectorName( "electricity" )
{}

/*! \brief Complete the initialization
*
* This routine is only called once per model run
* \param aSectorInfo Sector information object.
* \param aDependencyFinder Regional dependency finder.
* \param aLandAllocator Regional land allocator.
* \param aGlobalTechDB Global technology database.
* \author Marshall Wise
* \warning markets are not necessarily set when completeInit is called. For the
*          intermittent subsector, use this to make sure there is a trial market
*          set for electricity as there might not be one set if there are no
*          simultaneities
*/
void IntermittentSubsector::completeInit( const IInfo* aSectorInfo,
                                          DependencyFinder* aDependencyFinder,
                                          ILandAllocator* aLandAllocator,
                                          const GlobalTechnologyDatabase* aGlobalTechDB )
{
    // first call parent method
    Subsector::completeInit( aSectorInfo, aDependencyFinder, aLandAllocator, aGlobalTechDB );
    // Warn if a backup calculator was not read-in.
    if( !mBackupCalculator.get() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Intermittent subsector " << name << " in sector " << sectorName << " in region " << regionName
                << " does not have a backup calculator. Backup costs will default to zero. " << endl;
    }
}

//! Parses any input variables specific to derived classes
bool IntermittentSubsector::XMLDerivedClassParse( const string& aNodeName,
                                                  const DOMNode* aCurr )
{
    if( aNodeName == "backupTechNumber" ){
        backupTechNumber = XMLHelper<int>::getValue( aCurr );
    }
    else if( aNodeName == "resourceTechNumber" ){
        resourceTechNumber = XMLHelper<int>::getValue( aCurr );
    }
    else if( aNodeName == "electricSectorName" ){
        electricSectorName = XMLHelper<string>::getValue( aCurr );
    }
    else if( BackupCalculatorFactory::isOfType( aNodeName ) ){
        // Check if a new backup calculator needs to be created because
        // there is not currently one or the current type does not match the
        // new type.
        if( !mBackupCalculator.get() || !mBackupCalculator->isSameType( aNodeName ) ){
            mBackupCalculator = BackupCalculatorFactory::create( aNodeName );
        }
        mBackupCalculator->XMLParse( aCurr );
    }
    else {
        return false;
    }
    return true;
}

/*! \brief XML output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML
* \author Josh Lurz
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void IntermittentSubsector::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    // Write out parent class information.
    XMLWriteElement( backupTechNumber, "backupTechNumber", out, tabs);
    XMLWriteElement( resourceTechNumber, "resourceTechNumber", out, tabs);
    XMLWriteElement( electricSectorName, "electricSectorName", out, tabs);
    if( mBackupCalculator.get() ){
        mBackupCalculator->toInputXML( out, tabs );
    }
}   

//! Write object to debugging xml output stream.
void IntermittentSubsector::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    // Write out parent class information.
    XMLWriteElement( backupTechNumber, "backupTechNumber", out, tabs );
    XMLWriteElement( resourceTechNumber, "resourceTechNumber", out, tabs );
    XMLWriteElement( electricSectorName, "electricSectorName", out, tabs );
    XMLWriteElement( getAverageBackupCapacity( period ), "avg-backup-capacity", out, tabs );
    XMLWriteElement( getMarginalBackupCapacity( period ), "marginal-backup-capacity", out, tabs );

    XMLWriteElement( getAverageBackupCapacity( period ) * calcEnergyFromBackup(),
                     "avg-backup-energy", out, tabs );
    
    XMLWriteElement( getMarginalBackupCapacity( period ) * calcEnergyFromBackup(),
                     "marginal-backup-energy", out, tabs );

    XMLWriteElement( calcEnergyFromBackup(), "energy-to-backup", out, tabs );

    if( mBackupCalculator.get() ){
        mBackupCalculator->toDebugXML( period, out, tabs );
    }
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. This function may be virtual to be overridden by derived class
* pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& IntermittentSubsector::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. The "==" operator that is used when parsing, required this second
* function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& IntermittentSubsector::getXMLNameStatic() {
    const static string XML_NAME = "intermittent-subsector";
    return XML_NAME;
}

/*! \brief initilaization before each period
*
*
* \author Marshall Wise
* \param period Model period
*/
void IntermittentSubsector::initCalc( NationalAccount* aNationalAccount,
                                      const Demographic* aDemographics,
                                      const MoreSectorInfo* aMoreSectorInfo,
                                      const int aPeriod )
{
    // Call parent method
    Subsector::initCalc( aNationalAccount, aDemographics,
                         aMoreSectorInfo, aPeriod );
    
    // Ask the electric sector to create a trial market for supply. Given that
    // sector ordering has already occurred and the intermittent sector must
    // produce electricity, this should be before initCalc of the electric
    // sector. This is done here to ensure that the electricity market already
    // exists.
    if( aPeriod == 0 ){
        SectorUtils::askToCreateTrialSupply( regionName, electricSectorName );
    }
}

/*! \brief Set tech shares based on backup energy needs for an intermittent
*          resource.
* \author Marshall Wise
* \param aGDP Regional GDP container.
* \param APeriod Model period.
*/
const vector<double> IntermittentSubsector::calcTechShares( const GDP* aGDP,
                                                            const int aPeriod ) const
{
    // Note: these shares are only valid for price, not output.

    // Convert backup capacity per unit of resource energy to energy required
    // (in EJ) per unit of resource energy (in EJ) using backup capacity factor.
    double backupEnergyFraction = getMarginalBackupCapacity( aPeriod )
                                  * calcEnergyFromBackup();

    /*! \invariant Backup energy fraction must be positive. */
    assert( util::isValidNumber( backupEnergyFraction ) &&
              backupEnergyFraction >= 0 );

    // Check that the read-in resourceTechNumber and backupTechNumber are within
    // the size of the tech vector.
    assert( resourceTechNumber < techs.size() );
    assert( backupTechNumber < techs.size() );

    // Normalize shares.
    vector<double> techShares( 2 );
    techShares[ resourceTechNumber ] = 1.0 / ( 1.0 + backupEnergyFraction );
    techShares[ backupTechNumber ] = backupEnergyFraction / ( 1.0 + backupEnergyFraction );
    return techShares;
}

/*! \brief Computes weighted cost of all technologies in Subsector plus backup
*          costs.
* \details Computes a total cost of the subsector by adding the weighted
*          technology costs and adding the additional backup cost based on the
*          backup capacity required.
* \author Marshall Wise
* \param aGDP Regional GDP container.
* \param aPeriod Model period.
* \return Total intermittent subsector cost.
*/
double IntermittentSubsector::getPrice( const GDP* aGDP,
                                        const int aPeriod ) const
{
    // Add per unit cost of backup capacity to subsector price backup capacity
    // is in GW/EJ, so have to convert to kW/GJ (multiply numerator by 1E6 and
    // denominator by 1E9 to get * 1/1000) to make consistent with market price
    // which is in $/GJ. BackupCost is in $/kw/yr.
    // Total cost[$/GJ] = Subsector price[$/GJ] + Marginal backup[GW/EJ] * ? * Backup cost[ $/kw/yr]
    return Subsector::getPrice( aGDP, aPeriod )
           + getMarginalBackupCapacity( aPeriod ) / 1000
           * mSubsectorInfo->getDouble( "backupCost", true );
}

void IntermittentSubsector::setOutput( const double aSubsectorVariableDemand, 
                                       const double aFixedOutputScaleFactor,
		                               const GDP* aGDP,
                                       const int aPeriod )
{
    // There is currently no vintaging in IntermittentSubsectors. Set production
    // for past technologies explicitly to zero.
    for( unsigned int i = 0; i < techs.size(); ++i ){
        for( int j = 0; j < aPeriod; ++j ){
            techs[ i ][ j ]->production( regionName, sectorName, 0, 0, aGDP,
                                         aPeriod );
        }
    }

    // Calculate the energy produced from the backup. Total backup capacity is
    // calculated as the total subsector output multiplied by the backup
    // capacity per unit output. The backup capacity is then adjusted for the
    // backup capacity factor and converted to energy.
    double backupEnergy = getAverageBackupCapacity( aPeriod ) *
                          aSubsectorVariableDemand *
                          calcEnergyFromBackup();

    // Don't allow backup energy production to exceed requested production.
    backupEnergy = min( backupEnergy, aSubsectorVariableDemand );

    // Set production from the backup technology.
    techs[ backupTechNumber ][ aPeriod ]->production( regionName, sectorName,
                                                      backupEnergy,
                                                      aFixedOutputScaleFactor,
                                                      aGDP, aPeriod );

    // Remaining output is variable output.
    techs[ resourceTechNumber ][ aPeriod ]->production( regionName, sectorName,
                                                        aSubsectorVariableDemand - backupEnergy,
                                                        aFixedOutputScaleFactor, aGDP, aPeriod );
}

/*!
 * \brief Get the marginal backup capacity required per unit of energy output.
 * \details Uses the internal backup calculator to determine the marginal backup
 *          capacity per unit output. If a backup calculator was not read-in,
 *          this is assumed to be zero.
 * \param aPeriod Model period.
 * \return Marginal ackup capacity per unit of energy output.
 */
double IntermittentSubsector::getMarginalBackupCapacity( const int aPeriod ) const {
    double backupCapacity = 0;
    if( mBackupCalculator.get() ){
        const string& resourceName =
            techs[ resourceTechNumber ][ aPeriod ]->getFuelName();

        backupCapacity =
            mBackupCalculator->getMarginalBackupCapacity( sectorName,
                                                          electricSectorName,
                                                          resourceName,
                                                          regionName,
                                                          mSubsectorInfo->getDouble( "elecReserveMargin", true ),
                                                          mSubsectorInfo->getDouble( "aveGridCapacityFactor", true ),
                                                          aPeriod );
    }

    /*! \post Backup capacity is a valid number and positive. */
    assert( backupCapacity >= 0 && util::isValidNumber( backupCapacity ) );
    return backupCapacity;
}

/*!
 * \brief Get the average backup capacity per unit output for the intermittent
 *        subsector.
 * \details Uses the internal backup calculator to determine the average backup
 *          capacity per unit output. If a backup calculator was not read-in,
 *          this is assumed to be zero.
 * \param aPeriod Model period.
 * \return Average backup capacity per unit output.
 */
double IntermittentSubsector::getAverageBackupCapacity( const int aPeriod ) const {
    double backupCapacity = 0;
    if( mBackupCalculator.get() ){
        const string& resourceName =
            techs[ resourceTechNumber ][ aPeriod ]->getFuelName();
        
        backupCapacity =
           mBackupCalculator->getAverageBackupCapacity( sectorName,
                                                        electricSectorName,
                                                        resourceName,
                                                        regionName,
                                                        mSubsectorInfo->getDouble( "elecReserveMargin", true ),
                                                        mSubsectorInfo->getDouble( "aveGridCapacityFactor", true ),
                                                        aPeriod );
    }

    /*! \post Backup capacity is a valid number and positive. */
    assert( backupCapacity >= 0 && util::isValidNumber( backupCapacity ) );
    return backupCapacity;
}

/*!
 * \brief Determine the amount of energy produced by the backup per unit of
 *        capacity.
 * \details Determines the amount of energy produced per unit of backup capacity
 *          by adjusting for the backup capacity factor and converting the
 *          resulting operating backup into energy.
 * \return Capacity to energy production conversion factor.
 */
double IntermittentSubsector::calcEnergyFromBackup() const {
    // Conversion: 1 gigaWattHour of electricity = 3.6E-6 ExaJoules
    const double EJ_PER_GWH = 0.0000036;

    // Number of hours in a year.
    const int HOURS_PER_YEAR = 8760;

    return HOURS_PER_YEAR * mSubsectorInfo->getDouble( "backupCapacityFactor", true )
           * EJ_PER_GWH;
}

/*!
 * \brief Write resource and backup capacity results to database
 * \details Writes outputs with titles and units appropriate to supply sectors.
 * \param aGDP Regional GDP container.
 * \author Marshall Wise
 */
void IntermittentSubsector::MCoutputSupplySector( const GDP* aGDP ) const {

    // First call parent subsector class method
    Subsector::MCoutputSupplySector( aGDP );

    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);

    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    const double EJ_PER_GWH = 0.0000036;  // conversion: 1 gigaWattHour of electricity = 3.6E-6 ExaJoules
    const int HOURS_PER_YEAR = 8760;   //number of hours in a year
    const double CVRT90 = 2.212; //  convert '75 price to '90 price c/kwh
    vector<double> capacityGW(maxper);
    vector<double> backupCapacityGW(maxper);
    vector<double> backupCostCentsPerkWh(maxper);
    vector<double> resourceCostCentsPerkWh(maxper);

    Marketplace* marketplace = scenario->getMarketplace();
 
    // Intermittent Resource Subsector and Backup Capacity in GW
    for (int m= 0;m<maxper;m++) {
        // Get resource name from fuel name of technology
        string resourceName = techs[resourceTechNumber][m]->getFuelName();
        // get resource capacity factor from market info
        double resourceCapacityFactor = marketplace->getMarketInfo(resourceName,regionName,m, true )
                                                   ->getDouble( "resourceCapacityFactor", false );
        if (resourceCapacityFactor > util::getSmallNumber()) {
            // TODO: This is wrong because output is serviced by renewable and
            // backup with different capacity factors.
            capacityGW[m] = getOutput(m) / (EJ_PER_GWH * HOURS_PER_YEAR * resourceCapacityFactor);
            backupCapacityGW[m] = getAverageBackupCapacity( m ) * getOutput( m );
            backupCostCentsPerkWh[m] = getMarginalBackupCapacity( m ) / 1000
                                       * mSubsectorInfo->getDouble( "backupCost", true ) * CVRT90 * 0.36;
            resourceCostCentsPerkWh[m] = getPrice( aGDP, m ) * CVRT90 * 0.36 - backupCostCentsPerkWh[m];
        }
    }

    dboutput4(regionName,"Capacity",sectorName,name,"GW",capacityGW);
    dboutput4(regionName,"Capacity",sectorName,name + "Backup","GW",backupCapacityGW);
    dboutput4(regionName,"Capacity",sectorName,name + "BackupCost","90c/kWh",backupCostCentsPerkWh);
    dboutput4(regionName,"Capacity",sectorName,name + "ResourceCost","90c/kWh",resourceCostCentsPerkWh);
}
