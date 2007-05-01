/*!
 * csp_technology.cpp
 * Created: 03/06/2007
 * Version: 04/24/2007
 *
 * This software, which is provided in confidence, was prepared by employees
 * of Pacific Northwest National Laboratory operated by Battelle Memorial
 * Institute. Battelle has certain unperfected rights in the software
 * which should not be copied or otherwise disseminated outside your
 * organization without the express written authorization from Battelle.
 * All rights to the software are reserved by Battelle.   Battelle makes no
 * warranty, express or implied, and assumes no liability or responsibility
 * for the use of this software.
 */

// include files ***********************************************************

#include "util/base/include/definitions.h"
#include "technologies/include/csp_technology.h"
#include "technologies/include/marginal_profit_calculator.h"
#include "technologies/include/iproduction_state.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"
#include "sectors/include/sector_utils.h"
#include "util/base/include/TValidatorInfo.h"
#include "util/base/include/util.h"
#include "util/base/include/xml_helper.h"

#include <algorithm>

// namespaces **************************************************************

// constants ***************************************************************

const double CSPTechnology::kWhrtoGJ = 0.0036;
const std::string CSPTechnology::ELECTRIC_SECTOR_NAME_KEY = "electricSectorName";
const std::string CSPTechnology::NO_SUN_DAYS_KEY = "no-sun-days";
const std::string CSPTechnology::TOTAL_ANNUAL_IRRADIANCE_KEY = "total-annual-irradiance";
const double DEFAULT_TOTAL_ANNUAL_IRRADIANCE = 1000.0;

// Constructors: CSPTechnology *********************************************

/*! Default constructor
 *  \param aName the name of the technology
 *  \param aYear the year
 */
CSPTechnology::CSPTechnology(
   const std::string& aName,
   const int          aYear )
   : parent( aName, aYear ),
     mCapitalCost( 3486 ),
     mCSPCapacityFactor( 1 ),
     mElectricSectorName(),
     mFCR( 0.0856 ),
     mGridConnectionCost( 1500 ),
     mOM( 47.87 ),
     mRegionName(),
     mSectorName(),
     mSolarFieldFraction( 0.3 ),
     mSolarFieldArea( 6.9 )
{
}

/*! Copy constructor
 *  \param other the instance to copy
 */
CSPTechnology::CSPTechnology( const CSPTechnology& other )
   : parent( other ),
     mCapitalCost( other.mCapitalCost ),
     mCSPCapacityFactor( other.mCSPCapacityFactor ),
     mElectricSectorName( other.mElectricSectorName ),
     mFCR( other.mFCR ),
     mGridConnectionCost( other.mGridConnectionCost ),
     mOM( other.mOM ),
     mRegionName( other.mRegionName ),
     mSectorName( other.mSectorName ),
     mSolarFieldFraction( other.mSolarFieldFraction ),
     mSolarFieldArea( other.mSolarFieldArea )
{
}

// Destructor: CSPTechnology ***********************************************

CSPTechnology::~CSPTechnology(void)
{
}

// CSPTechnology::operator = ***********************************************

/*! Assignment operator
 *  \param other the instance to copy
 *  \return *this
 */
CSPTechnology& CSPTechnology::operator = ( const CSPTechnology& other )
{
   if ( &other != this )
   {
      parent::operator = ( other );
      mCapitalCost        = other.mCapitalCost;
      mCSPCapacityFactor  = other.mCSPCapacityFactor;
      mElectricSectorName = other.mElectricSectorName;
      mFCR                = other.mFCR;
      mGridConnectionCost = other.mGridConnectionCost;
      mOM                 = other.mOM;
      mRegionName         = other.mRegionName;
      mSectorName         = other.mSectorName;
      mSolarFieldFraction = other.mSolarFieldFraction;
      mSolarFieldArea     = other.mSolarFieldArea;
   }

   return *this;
}

// CSPTechnology::calcCost *************************************************

// Documentation is inherited
void CSPTechnology::calcCost(
   const std::string& aRegionName,
   const std::string& aSectorName,
   const int          aPeriod )
{
   static const double denom = 24 * 365;

   // If it is an existing stock or is a fixed output technology it has no
   // marginal cost.
   if( !mProductionState[ aPeriod ]->isNewInvestment() ||
       mFixedOutput != IProductionState::fixedOutputDefault() )
   {
      mCosts[ aPeriod ] = 0;
   }

   // Get marketplace and calculate costs
   Marketplace*       pMarketplace = scenario->getMarketplace();
   const std::string& fuelName     = mTechData->getFuelName();
   const IInfo*       pInfo        = pMarketplace->getMarketInfo( fuelName, aRegionName, aPeriod, true );

   double totalAnnualIrradiance = pInfo->hasValue( TOTAL_ANNUAL_IRRADIANCE_KEY ) ? pInfo->getDouble( TOTAL_ANNUAL_IRRADIANCE_KEY, true ) : DEFAULT_TOTAL_ANNUAL_IRRADIANCE;
   double dConnect              = pMarketplace->getPrice( fuelName, aRegionName, aPeriod );
   double CSPEfficiency         = getEfficiency( aPeriod );

   // Equation 1.
   mCGeneration = ( mFCR * mCapitalCost + mOM ) / ( totalAnnualIrradiance * mSolarFieldArea * CSPEfficiency * kWhrtoGJ );
   // Equation 3.
   mCSPCapacityFactor = ( totalAnnualIrradiance * mSolarFieldArea * CSPEfficiency ) / denom;
   // Equation 2.
   mCConnect = mFCR * dConnect * mGridConnectionCost / ( mCSPCapacityFactor * 1000 * kWhrtoGJ );

   /* Save to array
   * Total cost is the sum of the generation and connection cost
   */
   mCosts[ aPeriod ] = std::max( mCGeneration + mCConnect, util::getSmallNumber() );
}

// CSPTechnology::calcResourceArea *****************************************

/*! Calculate the resource area in km^2
 *  \param aRegionName the region name
 *  \param aSectorName the sector name
 *  \param aVariableDemand the variable demand
 *  \param aPeriod the period
 *  \return the resource area
 */
double CSPTechnology::calcResourceArea(
   const std::string& aRegionName,
   const std::string& aSectorName,
   double             aVariableDemand,
   const int          aPeriod )
{
/*
 * Equation 4 from "First Phase Implementation of CSP" is as follows:
 *
 * CSPGeneration / (kWhrtoGJ • 10^-9) = totalAnnualIrradiance • (ResourceArea •1000^2)• solarFieldFraction • CSPEfficiency
 *
 * Solving for CSPGeneration, we get:
 *
 * CSPGeneration  = totalAnnualIrradiance • ResourceArea • solarFieldFraction • CSPEfficiency • kWhrtoGJ • 10^-9 •1000^2
 *
 * And finally, solving for ResourceArea, we get:
 *
 * ResourceArea = CSPGeneration / ( totalAnnualIrradiance  • solarFieldFraction • CSPEfficiency • kWhrtoGJ • 10^-9 •1000^2 )
 */
   static const double conversionFact = kWhrtoGJ * 1e-3;

   // Set market demand for km^2
   Marketplace*       pMarketplace = scenario->getMarketplace();
   const std::string& fuelName     = mTechData->getFuelName();
   const IInfo*       pInfo        = pMarketplace->getMarketInfo( fuelName, aRegionName, aPeriod, true );

   double totalAnnualIrradiance = pInfo->hasValue( TOTAL_ANNUAL_IRRADIANCE_KEY ) ? pInfo->getDouble( TOTAL_ANNUAL_IRRADIANCE_KEY, true ) : DEFAULT_TOTAL_ANNUAL_IRRADIANCE;
   double CSPGeneration         = aVariableDemand;
   double CSPEfficiency         = getEfficiency( aPeriod );
   // Equation 4.
   double resourceArea          = CSPGeneration / ( totalAnnualIrradiance * mSolarFieldFraction * CSPEfficiency * conversionFact );

   return resourceArea;
}

// CSPTechnology::calcShare ************************************************

// Documentation is inherited
double CSPTechnology::calcShare(
   const std::string& aRegionName,
   const std::string& aSectorName,
   const GDP*         aGDP,
   const int          aPeriod ) const
{
   return parent::calcShare( aRegionName, aSectorName, aGDP, aPeriod );
}

// CSPTechnology::clone ****************************************************

// Documentation is inherited
CSPTechnology* CSPTechnology::clone( void ) const
{
   return new CSPTechnology( *this );
}

// CSPTechnology::completeInit *********************************************

// Documentation is inherited
void CSPTechnology::completeInit(
   const std::string&              aRegionName,
   const std::string&              aSectorName,
   DependencyFinder*               aDepFinder,
   const IInfo*                    aSubsectorIInfo,
   ILandAllocator*                 aLandAllocator,
   const GlobalTechnologyDatabase* aGlobalTechDB )
{
#if !defined( _MSC_VER )
   using std::exit;
#endif

   parent::completeInit( aRegionName, aSectorName, aDepFinder, aSubsectorIInfo, aLandAllocator, aGlobalTechDB );

   // Validate input parameters
   typedef ObjECTS::TValidatorInfo<> validator_type;
   validator_type   validator[] =
   {
      validator_type( mCapitalCost, "capital-cost", mCapitalCost > 0 ),
      validator_type( mCSPCapacityFactor, "csp-capacity-factor", mCSPCapacityFactor > 0 ),
      validator_type( mFCR, "fcr", mFCR > 0 ),
      validator_type( mSolarFieldFraction, "solar-field-fraction", mSolarFieldFraction > 0 ),
      validator_type( mSolarFieldArea, "solar-field-area", mSolarFieldArea > 0 )
   };

   unsigned short numParams = sizeof( validator ) / sizeof( validator[0] );
   std::string    msg       = ObjECTS::getInvalidNames(
      &validator[0],
      &validator[numParams] );

   if ( msg.length() )
   // Invalid input parameter
   {
      ILogger& mainLog = ILogger::getLogger( "main_log" );
      mainLog.setLevel( ILogger::ERROR );
      mainLog << "Invalid input parameter(s) to "
         << getXMLNameStatic1D()
         << " in sector " << aSectorName
         << ": " << msg << std::endl;
      exit( -1 );
   }
}

// CSPTechnology::getEfficiency ********************************************

// Documentation is inherited
double CSPTechnology::getEfficiency( const int aPeriod ) const
{
   const double maxLoss        = 0.55;  // 55%
   const double b              = 3.0;
   const double IPFraction     = 0.25;  // 25%
   const double elecCapacity   = 1;
   const double sectorCapacity = 1;

   // Compute the CSP penetration level
   double CSPPenetration = 0;
   if ( aPeriod > 0 )
   {
      double elecSupply   = SectorUtils::getTrialSupply( mRegionName, mElectricSectorName, aPeriod );
      if ( elecSupply > 0 )
      {
         double sectorSupply = SectorUtils::getTrialSupply( mRegionName, mSectorName, aPeriod );
         CSPPenetration = ( sectorSupply / ( elecSupply * IPFraction ) ) * ( elecCapacity / sectorCapacity );
      }
   }

   // Compute the CSP loss
   double CSPLoss = std::min( maxLoss * std::pow( CSPPenetration, b ), 0.99 );

   // Compute the efficiency
   double result = parent::getEfficiency( aPeriod ) * ( 1.0 - CSPLoss );

   return result;
}

// CSPTechnology::getFuelCost **********************************************

// Documentation is inherited
double CSPTechnology::getFuelCost(
   const std::string& aRegionName,
   const std::string& aSectorName,
   const int          aPeriod ) const
{
   return 0;
}

// CSPTechnology::getNonEnergyCost *****************************************

// Documentation is inherited
double CSPTechnology::getNonEnergyCost( const int aPeriod ) const
{
   return mCosts[ aPeriod ];
}

// CSPTechnology::getXMLName1D *********************************************

// Documentation is inherited
const std::string& CSPTechnology::getXMLName1D( void ) const
{
   return getXMLNameStatic1D();
}

// CSPTechnology::getXMLNameStatic1D ***************************************

const std::string& CSPTechnology::getXMLNameStatic1D( void )
{
   static const std::string XML_NAME1D = "csp-technology";
   return XML_NAME1D;
}

// CSPTechnology::initCalc *************************************************

// Documentation is inherited
void CSPTechnology::initCalc(
   const std::string& aRegionName,
   const std::string& aSectorName,
   const IInfo*       aSubsectorIInfo,
   const Demographic* aDemographics,
   const int          aPeriod )
{
   parent::initCalc( aRegionName, aSectorName, aSubsectorIInfo, aDemographics, aPeriod );

   // Cache the region name
   mRegionName = aRegionName;

   // Cache the sector name
   mSectorName = aSectorName;

   // Cache the electric sector name
   if ( aSubsectorIInfo->hasValue( ELECTRIC_SECTOR_NAME_KEY ) )
   {
      mElectricSectorName = aSubsectorIInfo->getString( ELECTRIC_SECTOR_NAME_KEY, true );
   }
   else
   {
      mElectricSectorName = "electricity";
   }

   // Get marketplace and make sure we have the total annual irradiance
   Marketplace*       pMarketplace = scenario->getMarketplace();
   const std::string& fuelName     = mTechData->getFuelName();
   const IInfo*       pInfo        = pMarketplace->getMarketInfo( fuelName, aRegionName, aPeriod, true );

   if ( !pInfo || !pInfo->hasValue( TOTAL_ANNUAL_IRRADIANCE_KEY ) )
   // Invalid input parameter
   {
      ILogger& mainLog = ILogger::getLogger( "main_log" );
      mainLog.setLevel( ILogger::ERROR );
      mainLog << "Invalid input parameter(s) to "
         << getXMLNameStatic1D()
         << " in sector " << aSectorName
         << ": " << TOTAL_ANNUAL_IRRADIANCE_KEY << std::endl;
   }
}

// CSPTechnology::postCalc *************************************************

// Documentation is inherited
void CSPTechnology::postCalc(
   const std::string& aRegionName,
   const int          aPeriod )
{
   return parent::postCalc( aRegionName, aPeriod );
}

// CSPTechnology::production ***********************************************

// Documentation is inherited
void CSPTechnology::production(
   const std::string& aRegionName,
   const std::string& aSectorName,
   double             aVariableDemand,
   double             aFixedOutputScaleFactor,
   const GDP*         aGDP,
   const int          aPeriod )
{
   // Can't have a scale factor and positive demand.
   assert( aFixedOutputScaleFactor == 1 || aVariableDemand == 0 );

   // Can't have negative variable demand.
   assert( aVariableDemand >= 0 && util::isValidNumber( aVariableDemand ) );

   // Check for positive variable demand and positive fixed output.
   assert( mFixedOutput == IProductionState::fixedOutputDefault() || util::isEqual( aVariableDemand, 0.0 ) );

   // Check that a state has been created for the period.
   assert( mProductionState[ aPeriod ] );

   // Construct a marginal profit calculator. This allows the calculation of
   // marginal profits to be lazy.
   MarginalProfitCalculator marginalProfitCalc( this );

   // Use the production state to determine output.
   double primaryOutput = mProductionState[ aPeriod ]->calcProduction( aRegionName, aSectorName, aVariableDemand, &marginalProfitCalc, aFixedOutputScaleFactor, mShutdownDeciders, aPeriod );

   // Calculate input demand.
   // Efficiency should be positive because invalid efficiencies were
   // already corrected.
   assert( getEfficiency( aPeriod ) > 0 );
   mInput[ aPeriod ] = calcResourceArea( aRegionName, aSectorName, aVariableDemand, aPeriod );

   // set demand for fuel in marketplace
   Marketplace*       marketplace = scenario->getMarketplace();
   const std::string& fuelName    = mTechData->getFuelName();
   if( ( fuelName != "renewable" ) &&
       ( fuelName != "none" ) &&
       mInput[ aPeriod ] > util::getSmallNumber() )
   {
      marketplace->addToDemand( fuelName, aRegionName, mInput[ aPeriod ], aPeriod );
   }

   // Set the supply of the good to the marketplace.
   calcEmissionsAndOutputs( aRegionName, mInput[ aPeriod ], primaryOutput, aGDP, aPeriod );
}

// CSPTechnology::toDebugXMLDerived ****************************************

// Documentation is inherited
void CSPTechnology::toDebugXMLDerived(
   const int     period,
   std::ostream& out,
   Tabs*         tabs ) const
{
   XMLWriteElement( mCapitalCost, "capital-cost", out, tabs );
   XMLWriteElement( mCConnect, "c-connect", out, tabs );
   XMLWriteElement( mCGeneration, "c-generation", out, tabs );
   XMLWriteElement( mCSPCapacityFactor, "csp-capacity-factor", out, tabs );
   XMLWriteElement( mFCR, "fcr", out, tabs );
   XMLWriteElement( mGridConnectionCost, "grid-connection-cost", out, tabs );
   XMLWriteElement( mOM, "om", out, tabs );
   XMLWriteElement( mSolarFieldFraction, "solar-field-fraction", out, tabs );
   XMLWriteElement( mSolarFieldArea, "solar-field-area", out, tabs );
}

// CSPTechnology::toInputXMLDerived ****************************************

// Documentation is inherited
void CSPTechnology::toInputXMLDerived(
   std::ostream& out,
   Tabs*         tabs ) const
{
   XMLWriteElementCheckDefault( mCapitalCost, "capital-cost", out, tabs, double( 3486 ) );
   XMLWriteElementCheckDefault( mCSPCapacityFactor, "csp-capacity-factor", out, tabs, double( 1 ) );
   XMLWriteElementCheckDefault( mFCR, "fcr", out, tabs, double( 0.0856 ) );
   XMLWriteElementCheckDefault( mGridConnectionCost, "grid-connection-cost", out, tabs, double( 1500 ) );
   XMLWriteElementCheckDefault( mOM, "om", out, tabs, double( 47.87 ) );
   XMLWriteElementCheckDefault( mSolarFieldFraction, "solar-field-fraction", out, tabs, double( 0.3 ) );
   XMLWriteElementCheckDefault( mSolarFieldArea, "solar-field-area", out, tabs, double( 6.9 ) );
}

// CSPTechnology::XMLDerivedClassParse *************************************

// Documentation is inherited
bool CSPTechnology::XMLDerivedClassParse(
   const std::string&      nodeName,
   const xercesc::DOMNode* curr )
{
   if ( nodeName == "capital-cost" )
   {
      mCapitalCost = XMLHelper<double>::getValue( curr );
   }
   else if ( nodeName == "csp-capacity-factor" )
   {
      mCSPCapacityFactor = XMLHelper<double>::getValue( curr );
   }
   else if ( nodeName == "fcr" )
   {
      mFCR = XMLHelper<double>::getValue( curr );
   }
   else if ( nodeName == "grid-connection-cost" )
   {
      mGridConnectionCost = XMLHelper<double>::getValue( curr );
   }
   else if ( nodeName == "om" )
   {
      mOM = XMLHelper<double>::getValue( curr );
   }
   else if ( nodeName == "solar-field-fraction" )
   {
      mSolarFieldFraction = XMLHelper<double>::getValue( curr );
   }
   else if ( nodeName == "solar-field-area" )
   {
      mSolarFieldArea = XMLHelper<double>::getValue( curr );
   }
   else
   {
      return false;
   }

   return true;
}

// end of csp_technology.cpp ***********************************************





