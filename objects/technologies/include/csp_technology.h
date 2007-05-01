/*!
 * csp_technology.h
 * Created: 02/27/2007
 * Version: 04/23/2007
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

#if !defined( __CSP_TECHNOLOGY_H )
#define __CSP_TECHNOLOGY_H     // prevent multiple includes

// include files ***********************************************************

#include "technologies/include/technology.h"

// namespaces **************************************************************

// class: CSPTechnology ****************************************************

/*!
 * \ingroup objects::biomass
 * \brief A technology class for Concentrated Solar Power (CSP)
 * \details This class contains a set of routines that implement
 *          technology for Concentrated Solar Power (CSP)
 *
 *   <b>XML specification for CSPTechnology</b>
 *   - XML name: \c csp-technology
 *   - Contained by: Technology
 *   - Parsing inherited from class: None.
 *   - Attributes: none
 *   - Elements:
 *   - \c capital-cost CSPTechnology::mCapitalCost
 *   - \c csp-capacity-factor CSPTechnology::mCSPCapacityFactor
 *   - \c fcr CSPTechnology::mFCR
 *   - \c grid-connection-cost CSPTechnology::mGridConnectionCost
 *   - \c om CSPTechnology::mOM
 *   - \c solar-field-fraction CSPTechnology::mSolarFieldFraction
 *   - \c solar-field-area CSPTechnology::mSolarFieldArea
 *
 * \author Kevin Walker
 * \date $ Date $
 * \version $ Revision $
 */
class CSPTechnology : public Technology
{
public :

   typedef Technology   parent;

   /*! Default constructor
    *  \param aName the name of the technology
    *  \param aYear the year
    */
   CSPTechnology(
      const std::string& aName = std::string(),
      const int          aYear = -1 );
   /*! Copy constructor
    *  \param other the instance to copy
    */
   CSPTechnology( const CSPTechnology& other );

   //! Destructor
   virtual ~CSPTechnology(void);

   /*! Assignment operator
    *  \param other the instance to copy
    *  \return *this
    */
   CSPTechnology& operator = ( const CSPTechnology& other );

   // Documentation is inherited
   virtual void calcCost(
      const std::string& aRegionName,
      const std::string& aSectorName,
      const int          aPeriod );

   // Documentation is inherited
   virtual double calcShare(
      const std::string& aRegionName,
      const std::string& aSectorName, 
      const GDP*         aGDP,
      const int          aPeriod ) const;

   // Documentation is inherited
   virtual CSPTechnology* clone( void ) const;

   // Documentation is inherited
   virtual void completeInit(
      const std::string&              aRegionName,
      const std::string&              aSectorName,
      DependencyFinder*               aDepFinder,
      const IInfo*                    aSubsectorIInfo,
      ILandAllocator*                 aLandAllocator,
      const GlobalTechnologyDatabase* aGlobalTechDB );

   // Documentation is inherited
   virtual double getEfficiency( const int aPeriod ) const;

   // Documentation is inherited
   virtual double getFuelCost(
      const std::string& aRegionName,
      const std::string& aSectorName,
      const int          aPeriod ) const;

   // Documentation is inherited
   virtual double getNonEnergyCost( const int aPeriod ) const;

   // Documentation is inherited
   static const std::string& getXMLNameStatic1D( void );

   // Documentation is inherited
   virtual void initCalc(
      const std::string& aRegionName,
      const std::string& aSectorName,
      const IInfo*       aSubsectorIInfo,
      const Demographic* aDemographics,
      const int          aPeriod );

   // Documentation is inherited
   virtual void postCalc(
      const std::string& aRegionName,
      const int          aPeriod );

   // Documentation is inherited
   virtual void production(
      const std::string& aRegionName,
      const std::string& aSectorName, 
      double             aVariableDemand,
      double             aFixedOutputScaleFactor,
      const GDP*         aGDP,
      const int          aPeriod );

protected :

   /*! Calculate the resource area in km^2
    *  \param aRegionName the region name
    *  \param aSectorName the sector name
    *  \param aVariableDemand the variable demand
    *  \param aPeriod the period
    *  \return the resource area
    */
   virtual double calcResourceArea(
      const std::string& aRegionName,
      const std::string& aSectorName,
      double             aVariableDemand,
      const int          aPeriod );

   // Documentation is inherited
   virtual const std::string& getXMLName1D( void ) const;

   // Documentation is inherited
   virtual void toDebugXMLDerived(
      const int     period,
      std::ostream& out,
      Tabs*         tabs ) const;

   // Documentation is inherited
   virtual void toInputXMLDerived(
      std::ostream& out,
      Tabs*         tabs ) const;

   // Documentation is inherited
   virtual bool XMLDerivedClassParse(
      const std::string&      nodeName,
      const xercesc::DOMNode* curr );

private :

   static const double      kWhrtoGJ;
   static const std::string ELECTRIC_SECTOR_NAME_KEY;
   static const std::string NO_SUN_DAYS_KEY;
   static const std::string TOTAL_ANNUAL_IRRADIANCE_KEY;

   //! Capital cost [$/MW Capacity]
   double mCapitalCost;

   //! Connection cost (used internally)
   double mCConnect;

   //! Generation cost (used internally)
   double mCGeneration;

   //! The average capacity factor for the CSP plant. [unitless] (used internally)
   double mCSPCapacityFactor;

   //! The current electrical sector name (cached)
   std::string mElectricSectorName;

   //! [unitless]
   double mFCR;

   //! The unit connection cost [$/km/MW capacity]. 
   double mGridConnectionCost;

   //! [$]
   double mOM;

   //! The current region name (cached)
   std::string mRegionName;

   //! The current sector name (cached)
   std::string mSectorName;

   /*! The fraction of the resource area that is actually occupied
    *  by the solar field. [unitless]
    */
   double mSolarFieldFraction;

   double mSolarFieldArea;
};

#endif   // __CSP_TECHNOLOGY_H

// end of csp_technology.h *************************************************




