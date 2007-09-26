/*!
 * residue_biomass_output.h
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

#if !defined( __RESIDUEBIOMASSOUTPUT_H )
#define __RESIDUEBIOMASSOUTPUT_H    // prevent multiple includes

// include files ***********************************************************

#include "technologies/include/ioutput.h"
#include "util/base/include/value.h"
#include "util/curves/include/cost_curve.h"
#include <vector>

// namespaces **************************************************************

// class: ResidueBiomassOutput *********************************************

/*!
 * \ingroup objects::biomass
 * \brief A class to output residue biomass supply to the energy
 *        market
 * \details This class contains a set of routines that implement
 *          output of residue biomass supply to the energy
 *
 *   <b>XML specification for ResidueBiomassOutput</b>
 *   - XML name: \c residue-biomass
 *   - Contained by: Technology
 *   - Parsing inherited from class: None.
 *   - Attributes: none
 *   - Elements:
 *   - \c curve-exponent ResidueBiomassOutput::mCostCurve.get/setCurveExponent()
 *   - \c eros-ctrl ResidueBiomassOutput::mErosCtrl
 *   - \c harvest-index ResidueBiomassOutput::mHarvestIndex
 *   - \c mass-conversion ResidueBiomassOutput::mMassConversion
 *   - \c mass-to-energy ResidueBiomassOutput::mMassToEnergy
 *   - \c mid-price ResidueBiomassOutput::mCostCurve.get/setMidprice()
 *
 * \author Kevin Walker
 * \date $ Date $
 * \version $ Revision $
 */
class ResidueBiomassOutput : public IOutput
{
public :

   typedef IOutput parent;

   /*!
    * Constructors
    * \pre Assigned(sectorName)
    */
   ResidueBiomassOutput( const std::string& sectorName = std::string() )
      : parent(),
        mPhysicalOutputs(),
        mName(sectorName),
        mCachedCO2Coef(),
        mLandAllocator( 0 ),
        mTechnologyName(),
        mLandType(),
        mHarvestIndex( -1 ),
        mRootToShoot( -1 ),
        mErosCtrl( -1 ),
        mMassConversion( -1 ),
        mMassToEnergy( -1 ),
        mCostCurve() {}
   //! \pre Assigned(other)
   ResidueBiomassOutput( const ResidueBiomassOutput& other )
      : parent(),
        mPhysicalOutputs( other.mPhysicalOutputs ),
        mName( other.mName ),
        mCachedCO2Coef( other.mCachedCO2Coef ),
        mLandAllocator( other.mLandAllocator ),
        mTechnologyName( other.mTechnologyName ),
        mLandType( other.mLandType ),
        mHarvestIndex( other.mHarvestIndex ),
        mRootToShoot( other.mRootToShoot ),
        mErosCtrl( other.mErosCtrl ),
        mMassConversion( other.mMassConversion ),
        mMassToEnergy( other.mMassToEnergy ),
        mCostCurve( other.mCostCurve ) {}

   //! Destructor
   virtual ~ResidueBiomassOutput(void) {}

   /*!
    * Assignment operator
    * \pre Assigned(other)
    * \post Copy other to *this and return *this
    */
   ResidueBiomassOutput& operator = ( const ResidueBiomassOutput& other )
   {
      if ( &other != this )
      {
         mPhysicalOutputs = other.mPhysicalOutputs;
         mName            = other.mName;
         mCachedCO2Coef   = other.mCachedCO2Coef;
         mLandAllocator   = other.mLandAllocator;
         mTechnologyName  = other.mTechnologyName;
         mLandType        = other.mLandType;

         mHarvestIndex    = other.mHarvestIndex;
         mRootToShoot     = other.mRootToShoot;
         mErosCtrl        = other.mErosCtrl;
         mMassConversion  = other.mMassConversion;
         mMassToEnergy    = other.mMassToEnergy;
         mCostCurve       = other.mCostCurve;
      }
      return *this;
   }

   // Documentation is inherited.
   virtual void accept(
      IVisitor* aVisitor,
      const int aPeriod ) const;

   // Documentation is inherited.
   virtual double calcPhysicalOutput(
      const double             aPrimaryOutput,
      const std::string&       aRegionName,
      const ICaptureComponent* aCaptureComponent,
      const int                aPeriod ) const;

   // Documentation is inherited.
   virtual ResidueBiomassOutput* clone( void ) const
   {
      return new ResidueBiomassOutput( *this );
   }

   // Documentation is inherited.
   virtual void completeInit(
      const std::string& aSectorName,
      DependencyFinder*  aDependencyFinder,
      const bool         aIsTechOperating );

   // Documentation is inherited.
   virtual double getEmissionsPerOutput(
      const std::string& aGHGName,
      const int          aPeriod ) const;

   // Documentation is inherited.
   virtual const std::string& getName( void ) const
   {
      return mName;
   }

   // Documentation is inherited.
   virtual double getPhysicalOutput( const int aPeriod ) const;

   // Documentation is inherited.
   virtual double getValue(
      const std::string&       aRegionName,
      const ICaptureComponent* aCaptureComponent,
      const int                aPeriod ) const;

   //! Return the XML tag name for residue biomass
   static const std::string& getXMLNameStatic( void );

   // Documentation is inherited.
   virtual void initCalc(
      const std::string& aRegionName,
      const int          aPeriod );

   // Documentation is inherited.
   virtual bool isSameType( const std::string& aType ) const
   {
      return getXMLNameStatic().compare( aType ) == 0;
   }

   // Documentation is inherited.
   virtual void postCalc(
      const std::string& aRegionName,
      const int          aPeriod );

   // Documentation is inherited.
   virtual void scaleCoefficient( const double aScaler );

   // Documentation is inherited.
    virtual void setLandAllocator(
       const ILandAllocator*    aLandAllocator,
       const std::string& aName,
       const std::string& aLandType );

   // Set the sector name
   virtual void setName( const std::string& sectorName )
   {
      mName = sectorName;
   }

   // Documentation is inherited.
   virtual void setPhysicalOutput(
      const double       aPrimaryOutput,
      const std::string& aRegionName,
      ICaptureComponent* aCaptureComponent,
      const int          aPeriod );

   // Documentation is inherited.
   virtual void toDebugXML(
      const int     aPeriod,
      std::ostream& aOut,
      Tabs*         aTabs ) const;

   // Documentation is inherited.
   virtual void toInputXML(
      std::ostream& aOut,
      Tabs*         aTabs ) const;

   // Documentation is inherited.
   virtual bool XMLParse( const xercesc::DOMNode* aNode );

private :

   typedef std::vector<Value> value_vector_type;

   //! Physical output by period.
   mutable value_vector_type mPhysicalOutputs;

   /*!
    * Name of the secondary output. Corresponds to a market for this good
    * and a supply sector which supplies this good as its primary output.
    */
   std::string mName;

   //! CO2 emissions coefficient cached from the marketplace.
   Value mCachedCO2Coef;

   //! The land allocator
   const ILandAllocator* mLandAllocator;

   //! The name of the technology
   std::string mTechnologyName;

   // - the land type
   std::string mLandType;

   //! The harvest index
   double mHarvestIndex;

   //! The root to shoot ratio
   double mRootToShoot;

   //! The mass per unit area of biomass to be retained to prevent erosion
   double mErosCtrl;

   //! The energy to mass conversion of the crop
   double mMassConversion;

   //! The mass to energy conversion of the crop residue
   double mMassToEnergy;

   //! The cost curve calculator
   ObjECTS::TCostCurve<> mCostCurve;

   // These variables are for debugging purposes. Values are written to debug.xml
   // Are made mutable so values can be saved. Are used only for debugging so are not violating const functions.
   //! Mass in crop residue
   mutable double mResMass;

   //! Mass in crop
   mutable double mCropMass;

   //! Residue Available
   mutable double mResAvail;

   //! Mass of biomass to be retained to prevent erosion
   mutable double mMeanErosCtrl;

   //! Max biomass energy supply
   mutable double mMaxBioEnergySupply;

   //! Fraction of max available residue harvested for energy
   mutable double mFPrice;

};

#endif   // __RESIDUEBIOMASSOUTPUT_H

// end of residue_biomass_output.h *************************************


