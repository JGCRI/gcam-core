/*
 * residue_biomass_production.cpp
 * Created: 01/24/2007
 * Version: 02/21/2007
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

#include "containers/include/dependency_finder.h"
#include "containers/include/iinfo.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "technologies/include/residue_biomass_production.h"
#include "util/base/include/ivisitor.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/TValidatorInfo.h"

#include <xercesc/dom/DOMNodeList.hpp>

#include <cstdio>

// externs *****************************************************************

extern Scenario* scenario;

// namespaces **************************************************************

// ResidueBiomassProduction::accept ****************************************

// Documentation is inherited.
void ResidueBiomassProduction::accept(
   IVisitor* aVisitor,
   const int aPeriod ) const
{
   if ( aVisitor )
   {
      aVisitor->startVisitOutput( this, aPeriod );
      aVisitor->endVisitOutput( this, aPeriod );
   }
}

// ResidueBiomassProduction::calcPhysicalOutput ****************************

/*!
 * \return the secondary output for the specified region and period
 * \param aPrimaryOutput
 * \param aRegionName
 * \param aCaptureComponent
 * \param aPeriod
 */
double ResidueBiomassProduction::calcPhysicalOutput(
   const double             aPrimaryOutput,
   const std::string&       aRegionName,
   const ICaptureComponent* aCaptureComponent,
   const int                aPeriod ) const
{
   if ( aPrimaryOutput <= 0 )
   {
      return 0;
   }

   // Initialize debugging vars to -1 so that know that a valid set of values are returned
   mYield = mResMass = mCropMass = mResAvail = mMeanErosCtrl = mMaxBioEnergySupply = mFPrice = -1;
   
   assert( scenario != 0 );
   const Marketplace*   pMarketplace = scenario->getMarketplace();
   assert( pMarketplace != 0 );
   double               price        = pMarketplace->getPrice(
      getName(),
      aRegionName,
      aPeriod,
      true );

   // Market price should exist or there is not a sector with this good
   // as the primary output. This can be caused by incorrect input files.
   assert( price != Marketplace::NO_MARKET_PRICE );

   if( price == Marketplace::NO_MARKET_PRICE )
   {
      return 0;
   }

   // Get the yield
   mYield = mLandAllocator->getYield(
      mLandType,
      mTechnologyName,
      aPeriod );
   if ( mYield <= 0 )
   {
      return 0;
   }

   // Get the area
   double   area = mLandAllocator->getLandAllocation(
      mLandType,
      mTechnologyName,
      aPeriod );
   if ( area <= 0 )
   {
      return 0;
   }
   
   // Compute the amount of crop produced (Equation 1)
   mCropMass = area * mYield * mMassConversion;

   // Equation 2 is mHarvestIndex

   // Compute the above-ground biomass (Equation 3)
   mResMass = mCropMass *
      ( std::pow( mHarvestIndex, double( -1 ) ) - double( 1 ) );

   // Compute the below-ground biomass(Equation 4)
   //double   rootMass = mRootToShoot  * ( mCropMass + mResMass );

   // Equation 5 is the mErosCtrl variable

   // Compute the mass of residue that are required to sustain
   // agriculture in terms of soil nutrients and erosion (Equation 6)
   mMeanErosCtrl = area * mErosCtrl;

   // Compute the amount of residue biomass available to the energy sector
   // (Equation 7)
   mResAvail = mResMass - mMeanErosCtrl /*- resFeed*/;
   if ( mResAvail <= 0 )
   {
      return 0;
   }

   // Compute energy content of the available biomass (Equation 8)
   mMaxBioEnergySupply = mResAvail * mMassToEnergy;

   // Compute the fraction of the total possible supply that is
   // available at a given price (Equation 10)
   mFPrice = mCostCurve( price );

   //! This is the secondary output
   // Compute the quantity of a crop residue biomass (Equation 9)
   double   resEnergy = mMaxBioEnergySupply * mFPrice;

   return resEnergy;
}

// ResidueBiomassProduction::completeInit **********************************

// Documentation is inherited.
void ResidueBiomassProduction::completeInit(
   const std::string& aSectorName,
   DependencyFinder*  aDependencyFinder,
   const bool         aIsTechOperating )
{
#if !defined( _MSC_VER )
   using std::exit;
#endif

   // Validate land allocator
   // Note: The land allocator should be assigned at this point.
   //       However, it has not been initialized
   if ( !mLandAllocator ||
        !mTechnologyName.length() ||
        !mLandType.length() )
   {
      ILogger& mainLog = ILogger::getLogger( "main_log" );
      mainLog.setLevel( ILogger::ERROR );
      mainLog << "Invalid land allocator for "
         << getXMLNameStatic()
         << " in sector " << aSectorName << std::endl;
      exit( -1 );

   }

   // Validate input parameters
   typedef ObjECTS::TValidatorInfo<> validator_type;
   validator_type   validator[] =
   {
      validator_type(
         mCostCurve.getCurveExponent(),
         "curve-exponent",
         mCostCurve.getCurveExponent() > 0 ),
      validator_type(
         mErosCtrl,
         "eros-ctrl",
         mErosCtrl > 0 ),
      validator_type(
         mHarvestIndex,
         "harvest-index",
         mHarvestIndex > 0 ),
      validator_type(
         mMassConversion,
         "mass-conversion",
         mMassConversion > 0 ),
      validator_type(
         mMassToEnergy,
         "mass-to-energy",
         mMassToEnergy > 0 ),
      validator_type(
         mCostCurve.getMidprice(),
         "mid-price",
         mCostCurve.getMidprice() > 0 )
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
         << getXMLNameStatic()
         << " in sector " << aSectorName
         << ": " << msg << std::endl;
      exit( -1 );
   }

   // Residue biomass is removed from demand, so add a dependency.
   if ( aIsTechOperating )
   {
      aDependencyFinder->addDependency( aSectorName, getName() );
   }
}

// ResidueBiomassProduction::getEmissionsPerOutput *************************

// Documentation is inherited.
double ResidueBiomassProduction::getEmissionsPerOutput(
   const std::string& aGHGName,
   const int          aPeriod ) const
{
   // Currently other GHGs do not use output emissions coefficients.
   assert( aGHGName == "CO2" );
   assert( mCachedCO2Coef.isInited() );
   return mCachedCO2Coef;
}

// ResidueBiomassProduction::getPhysicalOutput *****************************

// Documentation is inherited.
double ResidueBiomassProduction::getPhysicalOutput( const int aPeriod ) const
{
   if ( !mPhysicalOutputs.size() )
   // initialise
   {
      assert( scenario != 0 );
      const Modeltime*   pModeltime = scenario->getModeltime();
      assert( pModeltime != 0 );
      mPhysicalOutputs.resize( pModeltime->getmaxper() );
   }

   assert( mPhysicalOutputs[ aPeriod ].isInited() );
   return mPhysicalOutputs[ aPeriod ];
}

// ResidueBiomassProduction::getValue **************************************

// Documentation is inherited.
double ResidueBiomassProduction::getValue(
   const std::string&       aRegionName,
   const ICaptureComponent* aCaptureComponent,
   const int                aPeriod ) const
{
   // TODO
   return 0;
}

// ResidueBiomassProduction::getXMLNameStatic ******************************

// Documentation is inherited.
const std::string& ResidueBiomassProduction::getXMLNameStatic( void )
{
   static const std::string XMLName = "residue-biomass-production";
   return XMLName;
}

// ResidueBiomassProduction::initCalc **************************************

// Documentation is inherited.
void ResidueBiomassProduction::initCalc(
   const std::string& aRegionName,
   const int          aPeriod )
{
   assert( scenario != 0 );
   const Marketplace*   pMarketplace = scenario->getMarketplace();
   assert( pMarketplace != 0 );
   const IInfo*            pProductInfo = pMarketplace->getMarketInfo(
      getName(), aRegionName, aPeriod, false );

   mCachedCO2Coef.set(
      pProductInfo ? pProductInfo->getDouble( "CO2Coef", false ) : 0 );
}

// ResidueBiomassProduction::postCalc **************************************

// Documentation is inherited.
void ResidueBiomassProduction::postCalc(
   const std::string& aRegionName,
   const int          aPeriod )
{
   // Not used
}

// ResidueBiomassProduction::scaleCoefficient ******************************

// Documentation is inherited.
void ResidueBiomassProduction::scaleCoefficient( const double aScaler )
{
   // Not used
}

// ResidueBiomassProduction::setLandAllocator ******************************

// Documentation is inherited.
void ResidueBiomassProduction::setLandAllocator(
   ILandAllocator*    aLandAllocator,
   const std::string& aName,
   const std::string& aLandType )
{
   mLandAllocator  = aLandAllocator;
   mTechnologyName = aName;
   mLandType       = aLandType;
}

// ResidueBiomassProduction::setPhysicalOutput *****************************

// Documentation is inherited.
void ResidueBiomassProduction::setPhysicalOutput(
   const double       aPrimaryOutput,
   const std::string& aRegionName,
   ICaptureComponent* aCaptureComponent,
   const int          aPeriod )
{
   assert( scenario != 0 );

   if ( !mPhysicalOutputs.size() )
   // initialise
   {
      const Modeltime*   pModeltime = scenario->getModeltime();
      assert( pModeltime != 0 );
      mPhysicalOutputs.resize( pModeltime->getmaxper() );
   }

   // Set the physical output for the specified period
   mPhysicalOutputs[ aPeriod ].set(
      calcPhysicalOutput(
         aPrimaryOutput,
         aRegionName,
         aCaptureComponent,
         aPeriod ) );

   // Remove the secondary output from demand instead of adding to supply
   // because the sector which has this output as a primary will attempt to
   // fill all of demand. If this technology also added to supply, supply
   // would not equal demand.
   Marketplace*   pMarketplace = scenario->getMarketplace();
   assert( pMarketplace != 0 );
   pMarketplace->addToDemand(
      getName(),
      aRegionName,
      double( -1 ) * mPhysicalOutputs[ aPeriod ],
      aPeriod,
      true );
}

// ResidueBiomassProduction::toDebugXML ************************************

// Documentation is inherited.
void ResidueBiomassProduction::toDebugXML(
   const int     aPeriod,
   std::ostream& aOut,
   Tabs*         aTabs ) const
{
   XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, getName() );
   if ( value_vector_type::size_type( aPeriod ) < mPhysicalOutputs.size() )
   {
      XMLWriteElement( mPhysicalOutputs[ aPeriod ], "output", aOut, aTabs );
   }
   XMLWriteElement(
      mCostCurve.getCurveExponent(), "curve-exponent", aOut, aTabs );
   XMLWriteElement( mErosCtrl, "eros-ctrl", aOut, aTabs );
   XMLWriteElement( mHarvestIndex, "harvest-index", aOut, aTabs );
   //XMLWriteElement( mRootToShoot, "root-to-shoot", aOut, aTabs );
   XMLWriteElement( mMassConversion, "mass-conversion", aOut, aTabs );
   XMLWriteElement( mMassToEnergy, "mass-to-energy", aOut, aTabs );
   XMLWriteElement( mCostCurve.getMidprice(), "mid-price", aOut, aTabs );
   XMLWriteElement( mResMass, "Residue-Mass", aOut, aTabs );
   XMLWriteElement( mCropMass, "Crop-Mass", aOut, aTabs );
   XMLWriteElement( mYield, "yield", aOut, aTabs );
   XMLWriteElement( mResAvail, "resAvail", aOut, aTabs );
   XMLWriteElement( mMeanErosCtrl, "meanErosCtrl", aOut, aTabs );
   XMLWriteElement( mMaxBioEnergySupply, "maxBioEnergySupply", aOut, aTabs );
   XMLWriteElement( mFPrice, "fPrice", aOut, aTabs );
   XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

// ResidueBiomassProduction::toInputXML ************************************

// Documentation is inherited.
void ResidueBiomassProduction::toInputXML(
   std::ostream& aOut,
   Tabs*         aTabs ) const
{
   XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, getName() );
   XMLWriteElement(
      mCostCurve.getCurveExponent(), "curve-exponent", aOut, aTabs );
   XMLWriteElement( mErosCtrl, "eros-ctrl", aOut, aTabs );
   XMLWriteElement( mHarvestIndex, "harvest-index", aOut, aTabs );
   //XMLWriteElement( mRootToShoot, "root-to-shoot", aOut, aTabs );
   XMLWriteElement( mMassConversion, "mass-conversion", aOut, aTabs );
   XMLWriteElement( mMassToEnergy, "mass-to-energy", aOut, aTabs );
   XMLWriteElement( mCostCurve.getMidprice(), "mid-price", aOut, aTabs );
   XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

// ResidueBiomassProduction::XMLParse **************************************

// Documentation is inherited.
bool ResidueBiomassProduction::XMLParse( const xercesc::DOMNode* aNode )
{
   // assume we are passed a valid node.
   if ( !aNode )
   {
      return false;
   }

   // get all the children.
   xercesc::DOMNodeList* pNodeList = aNode->getChildNodes();
   if ( !pNodeList )
   {
      return false;
   }

   // get the sector name attribute.
   std::string sectorName =
      XMLHelper<std::string>::getAttr( aNode, "name" );
   if ( !sectorName.length() )
   {
      return false;
   }
   setName( sectorName );

   XMLSize_t   n = pNodeList->getLength();
   for ( XMLSize_t i = 0; i != n; ++i )
   {
      const xercesc::DOMNode* pCurr = pNodeList->item( i );
      if ( !pCurr )
      {
         return false;
      }

      const std::string nodeName =
         XMLHelper<std::string>::safeTranscode( pCurr->getNodeName() );

      if( nodeName == "#text" )
      {
         continue;
      }
      else if( nodeName == "curve-exponent" )
      {
         mCostCurve.setCurveExponent(
            XMLHelper<double>::getValue( pCurr ) );
      }
      else if( nodeName == "eros-ctrl" )
      {
         mErosCtrl = XMLHelper<double>::getValue( pCurr );
      }
      else if( nodeName == "harvest-index" )
      {
         mHarvestIndex = XMLHelper<double>::getValue( pCurr );
      }
//       else if( nodeName == "root-to-shoot" )
//       {
//          mRootToShoot = XMLHelper<double>::getValue( pCurr );
//       }
      else if( nodeName == "mass-conversion" )
      {
         mMassConversion = XMLHelper<double>::getValue( pCurr );
      }
      else if( nodeName == "mass-to-energy" )
      {
         mMassToEnergy = XMLHelper<double>::getValue( pCurr );
      }
      else if( nodeName == "mid-price" )
      {
         mCostCurve.setMidprice(
            XMLHelper<double>::getValue( pCurr ) );
      }
      else
      {
         ILogger& mainLog = ILogger::getLogger( "main_log" );
         mainLog.setLevel( ILogger::WARNING );
         mainLog << "Unrecognized text string: " << nodeName
            << " found while parsing "
            << getXMLNameStatic() << "." << std::endl;
         return false;
      }
   }

   return true;
}

// end of residue_biomass_production.cpp ***********************************


