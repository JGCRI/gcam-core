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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*
 * residue_biomass_output.cpp
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
#include "technologies/include/residue_biomass_output.h"
#include "util/base/include/ivisitor.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/TValidatorInfo.h"

#include <xercesc/dom/DOMNodeList.hpp>

#include <cstdio>

// namespaces **************************************************************

using namespace std;
using namespace xercesc;

// externs *****************************************************************

extern Scenario* scenario;

// static initialize.
const string ResidueBiomassOutput::XML_REPORTING_NAME = "output-residue-biomass";

/*! \brief Get the XML name for reporting to XML file.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for reporting outputs and can be easily
* changed.
* \author Sonny Kim
* \return The constant XML_NAME.
*/
const string& ResidueBiomassOutput::getXMLReportingName() const{
    return XML_REPORTING_NAME;
}

// ResidueBiomassOutput::accept ****************************************

// Documentation is inherited.
void ResidueBiomassOutput::accept(
   IVisitor* aVisitor,
   const int aPeriod ) const
{
   if ( aVisitor )
   {
      aVisitor->startVisitOutput( this, aPeriod );
      aVisitor->endVisitOutput( this, aPeriod );
   }
}

// ResidueBiomassOutput::calcPhysicalOutput ****************************

/*!
 * \return the secondary output for the specified region and period
 * \param aPrimaryOutput
 * \param aRegionName
 * \param aCaptureComponent
 * \param aPeriod
 */
IOutput::OutputList ResidueBiomassOutput::calcPhysicalOutput(
   const double             aPrimaryOutput,
   const std::string&       aRegionName,
   const ICaptureComponent* aCaptureComponent,
   const int                aPeriod ) const
{
    // create the list to return and default it to zero
    OutputList outputList;
    outputList.push_back( make_pair( mName, 0 ) );

   if ( aPrimaryOutput <= 0 )
   {
      return outputList;
   }

   // Initialize debugging vars to -1 so that know that a valid set of values are returned
   mResMass = mCropMass = mResAvail = mMeanErosCtrl = mMaxBioEnergySupply = mFPrice = -1;
   
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
      return outputList;
   }

   // Get the area if relevant
   double landArea = 0;
   if ( mErosCtrl > 0 ) {
       landArea = mLandAllocator->getLandAllocation(
          mLandType,
          mTechnologyName,
          aPeriod );
       if ( landArea <= 0 )
       {
          return outputList;
       }
   }
   
   // Compute the amount of crop produced (Equation 1)
   mCropMass = aPrimaryOutput * mMassConversion;

   // Equation 2 is mHarvestIndex

   // Compute the above-ground biomass (Equation 3)
   mResMass = mCropMass *
      ( std::pow( mHarvestIndex, double( -1 ) ) - double( 1 ) );

   // Compute the below-ground biomass(Equation 4)
   //double   rootMass = mRootToShoot  * ( mCropMass + mResMass );

   // Equation 5 is the mErosCtrl variable

   // Compute the mass of residue that are required to sustain
   // agriculture in terms of soil nutrients and erosion (Equation 6)
   mMeanErosCtrl = landArea * mErosCtrl;

   // Compute the amount of residue biomass available to the energy sector
   // (Equation 7)
   mResAvail = mResMass - mMeanErosCtrl /*- resFeed*/;
   if ( mResAvail <= 0 )
   {
      return outputList;
   }

   // Compute energy content of the available biomass (Equation 8)
   mMaxBioEnergySupply = mResAvail * mMassToEnergy;

   // Compute the fraction of the total possible supply that is
   // available at a given price (Equation 10)
   mFPrice = mCostCurve( price );

   //! This is the secondary output
   // Compute the quantity of a crop residue biomass (Equation 9)
   double   resEnergy = mMaxBioEnergySupply * mFPrice;

   outputList.front().second = resEnergy;
   return outputList;
}

// ResidueBiomassOutput::completeInit **********************************

// Documentation is inherited.
void ResidueBiomassOutput::completeInit(
   const std::string& aSectorName,
   DependencyFinder*  aDependencyFinder,
   const IInfo* aTechInfo,
   const bool         aIsTechOperating )
{
#if !defined( _MSC_VER )
   using std::exit;
#endif

   // Validate land allocator
   // Note: The land allocator should be assigned at this point.
   //       However, it has not been initialized
   if ( ( !mLandAllocator || !mTechnologyName.length() || !mLandType.length() ) &&
        mErosCtrl > 0 )
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
         mErosCtrl >= 0 ),
      validator_type(
         mHarvestIndex,
         "harvest-index",
         mHarvestIndex >= 0 ),
      validator_type(
         mMassConversion,
         "mass-conversion",
         mMassConversion >= 0 ),
      validator_type(
         mMassToEnergy,
         "mass-to-energy",
         mMassToEnergy >= 0 ),
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
   // aDependencyFinder check needed in case this is in demand technology. Can remove for multi-inputs vers.
   if ( aIsTechOperating && aDependencyFinder ) 
   {
      aDependencyFinder->addDependency( aSectorName, getName() );
   }
}

// ResidueBiomassOutput::getEmissionsPerOutput *************************

// Documentation is inherited.
double ResidueBiomassOutput::getEmissionsPerOutput(
   const std::string& aGHGName,
   const int          aPeriod ) const
{
   // Currently other GHGs do not use output emissions coefficients.
   assert( aGHGName == "CO2" );
   assert( mCachedCO2Coef.isInited() );
   return mCachedCO2Coef;
}

// ResidueBiomassOutput::getPhysicalOutput *****************************

// Documentation is inherited.
double ResidueBiomassOutput::getPhysicalOutput( const int aPeriod ) const
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

// ResidueBiomassOutput::getValue **************************************

// Documentation is inherited.
double ResidueBiomassOutput::getValue(
   const std::string&       aRegionName,
   const ICaptureComponent* aCaptureComponent,
   const int                aPeriod ) const
{
   // TODO
   return 0;
}

// ResidueBiomassOutput::getXMLNameStatic ******************************

// Documentation is inherited.
const std::string& ResidueBiomassOutput::getXMLNameStatic( void )
{
   static const std::string XMLName = "residue-biomass-production";
   return XMLName;
}

// ResidueBiomassOutput::initCalc **************************************

// Documentation is inherited.
void ResidueBiomassOutput::initCalc(
   const std::string& aRegionName,
   const std::string& aSectorName,
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

// ResidueBiomassOutput::postCalc **************************************

// Documentation is inherited.
void ResidueBiomassOutput::postCalc(
   const std::string& aRegionName,
   const int          aPeriod )
{
   // Not used
}

// ResidueBiomassOutput::scaleCoefficient ******************************

// Documentation is inherited.
void ResidueBiomassOutput::scaleCoefficient( const double aScaler )
{
   // Not used
}

// ResidueBiomassOutput::setLandAllocator ******************************

// Documentation is inherited.
void ResidueBiomassOutput::setLandAllocator(
   const ILandAllocator*    aLandAllocator,
   const std::string& aName,
   const std::string& aLandType )
{
   mLandAllocator  = aLandAllocator;
   mTechnologyName = aName;
   mLandType       = aLandType;
}

// ResidueBiomassOutput::setPhysicalOutput *****************************

// Documentation is inherited.
void ResidueBiomassOutput::setPhysicalOutput(
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
    // calcPhysicalOutput only returns a single value.
    OutputList outputList = calcPhysicalOutput( aPrimaryOutput, aRegionName,
        aCaptureComponent, aPeriod );
    mPhysicalOutputs[ aPeriod ].set( outputList.front().second );

    // Since biomass is a solved market, we can add the secondary
    // output to the supply.
    Marketplace*   pMarketplace = scenario->getMarketplace();
    assert( pMarketplace != 0 );
    pMarketplace->addToSupply(
        getName(),
        aRegionName,
        mPhysicalOutputs[ aPeriod ],
        aPeriod,
        true );
}

// ResidueBiomassOutput::toDebugXML ************************************

// Documentation is inherited.
void ResidueBiomassOutput::toDebugXML(
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
   XMLWriteElement( mResAvail, "resAvail", aOut, aTabs );
   XMLWriteElement( mMeanErosCtrl, "meanErosCtrl", aOut, aTabs );
   XMLWriteElement( mMaxBioEnergySupply, "maxBioEnergySupply", aOut, aTabs );
   XMLWriteElement( mFPrice, "fPrice", aOut, aTabs );
   XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

// ResidueBiomassOutput::toInputXML ************************************

// Documentation is inherited.
void ResidueBiomassOutput::toInputXML(
   std::ostream& aOut,
   Tabs*         aTabs ) const
{
   XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, getName() );
   XMLWriteElement(
      mCostCurve.getCurveExponent(), "curve-exponent", aOut, aTabs );
   XMLWriteElementCheckDefault( mErosCtrl, "eros-ctrl", aOut, aTabs, 0.0 );
   XMLWriteElement( mHarvestIndex, "harvest-index", aOut, aTabs );
   //XMLWriteElement( mRootToShoot, "root-to-shoot", aOut, aTabs );
   XMLWriteElement( mMassConversion, "mass-conversion", aOut, aTabs );
   XMLWriteElement( mMassToEnergy, "mass-to-energy", aOut, aTabs );
   XMLWriteElement( mCostCurve.getMidprice(), "mid-price", aOut, aTabs );
   XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

// ResidueBiomassOutput::XMLParse **************************************

// Documentation is inherited.
bool ResidueBiomassOutput::XMLParse( const xercesc::DOMNode* aNode )
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

// end of residue_biomass_output.cpp ***********************************




