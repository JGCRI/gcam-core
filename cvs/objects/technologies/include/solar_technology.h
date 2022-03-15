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
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


/*!
 * solar_technology.h
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

#if !defined( __SOLAR_TECHNOLOGY_H )
#define __SOLAR_TECHNOLOGY_H     // prevent multiple includes

// include files ***********************************************************

#include "technologies/include/intermittent_technology.h"

// namespaces **************************************************************

// class: SolarTechnology ****************************************************

/*!
 * \ingroup objects::biomass
 * \brief A technology class for Concentrated Solar Power (CSP)
 * \details This class contains a set of routines that implement
 *          technology for Concentrated Solar Power (CSP)
 *
 *   <b>XML specification for SolarTechnology</b>
 *   - XML name: \c solar-technology
 *   - Contained by: Technology
 *   - Parsing inherited from class: None.
 *   - Attributes: none
 *   - Elements:
 *   - \c capital-cost SolarTechnology::mCapitalCost
 *   - \c csp-capacity-factor SolarTechnology::mCSPCapacityFactor
 *   - \c fcr SolarTechnology::mFCR
 *   - \c grid-connection-cost SolarTechnology::mGridConnectionCost
 *   - \c om SolarTechnology::mOM
 *   - \c solar-field-fraction SolarTechnology::mSolarFieldFraction
 *   - \c solar-field-area SolarTechnology::mSolarFieldArea
 *
 * \author Kevin Walker
 * \date $ Date $
 * \version $ Revision $
 */
class SolarTechnology : public IntermittentTechnology
{
public :

   typedef IntermittentTechnology   parent;

   /*! Default constructor
    *  \param aName the name of the technology
    *  \param aYear the year
    */
   SolarTechnology(
      const std::string& aName = std::string(),
      const int          aYear = -1 );

   //! Destructor
   virtual ~SolarTechnology(void);

   // Documentation is inherited
   virtual void calcCost(
      const std::string& aRegionName,
      const std::string& aSectorName,
      const int          aPeriod );

   // Documentation is inherited
   virtual SolarTechnology* clone( void ) const;

   // Documentation is inherited
   virtual void completeInit(
      const std::string&              aRegionName,
      const std::string&              aSectorName,
      const std::string&              aSubsectorName,
      const IInfo*                    aSubsectorIInfo,
      ILandAllocator*                 aLandAllocator );

    virtual double getResourceToEnergyRatio( const std::string& aRegionName,
                                             const std::string& aSectorName,
                                             const int aPeriod );

   // Documentation is inherited
   double getSolarEfficiency( const int aPeriod ) const;

   // Documentation is inherited
   static const std::string& getXMLNameStatic1D( void );

   // Documentation is inherited
   virtual void initCalc(
      const std::string& aRegionName,
      const std::string& aSectorName,
      const IInfo*       aSubsectorIInfo,
      const Demographic* aDemographics,
      PreviousPeriodInfo& aPrevPeriodInfo,
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

   static const double      kWhrtoGJ;
   static const std::string ELECTRIC_SECTOR_NAME_KEY;
   static const std::string NO_SUN_DAYS_KEY;

   double getSolarPenetration( const int aPeriod ) const;
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        IntermittentTechnology,
    
       //! The key used for total annual irradiance
       DEFINE_VARIABLE( SIMPLE, "irradiance-tagname", mTotalAnnualIrradianceKey, std::string ),

       //! Capital cost [$/MW Capacity]
       DEFINE_VARIABLE( SIMPLE, "capital-cost", mCapitalCost, double ),

       //! Connection cost (used internally)
       DEFINE_VARIABLE( SIMPLE, "c-connect", mCConnect, double ),

       //! Net plant solar to electricity conversion efficiency
       DEFINE_VARIABLE( SIMPLE, "net-solar-conversion-efficiency", mCSPEfficiency, double ),

       //! Plant availability (% of year plant is available at scheduled time)
       DEFINE_VARIABLE( SIMPLE, "plant-availability-fraction", mPlantAvailability, double ),

       //! Plant Maintenance Fraction (% of year plant need to be down for scheduled maintenance)
       DEFINE_VARIABLE( SIMPLE, "plant-scheduled-maintenance-fraction", mScheduledMaintenance, double ),

       //! Fraction of scheduled maintenance that can be accomplished during no-sun day outages
       DEFINE_VARIABLE( SIMPLE, "random-maintence-fraction", mRandomMaintenanceFraction, double ),

       //! Generation cost (used internally)
       DEFINE_VARIABLE( SIMPLE, "c-generation", mCGeneration, double ),

       //! The average capacity factor for the CSP plant. [unitless] (used internally)
       DEFINE_VARIABLE( SIMPLE, "csp-capacity-factor", mCSPCapacityFactor, double ),

       //! [unitless]
       DEFINE_VARIABLE( SIMPLE, "fcr", mFCR, double ),

       //! The unit connection cost [$/km/MW capacity]. 
       DEFINE_VARIABLE( SIMPLE, "grid-connection-cost", mGridConnectionCost, double ),

       //! [$]
       DEFINE_VARIABLE( SIMPLE, "om", mOM, double ),

       //! The current region name (cached)
       DEFINE_VARIABLE( SIMPLE, "region-name", mRegionName, std::string ),

       //! The current sector name (cached)
       DEFINE_VARIABLE( SIMPLE, "sector-name", mSectorName, std::string ),

       /*! The fraction of the resource area that is actually occupied
        *  by the solar field. [unitless]
        */
       DEFINE_VARIABLE( SIMPLE, "solar-field-fraction", mSolarFieldFraction, double ),

       DEFINE_VARIABLE( SIMPLE, "solar-field-area", mSolarFieldArea, double ),

       //! The maximum energy loss fraction
       DEFINE_VARIABLE( SIMPLE, "max-solar-loss", mMaxLoss, double ),

       //! Exponent for energy loss as a function of penetration
       DEFINE_VARIABLE( SIMPLE, "loss-exponent", mEfficiencyLossExponent, double ),

       //! The max potential fraction of the sector's load that can be served by this technology
       DEFINE_VARIABLE( SIMPLE, "max-sector-load-served", mMaxSectorLoadServed, double ),

       //! Number of no sun days
       DEFINE_VARIABLE( SIMPLE, "no-sun-days", mNoSunDays, double )
    )
    
    void copy( const SolarTechnology& aOther );

   virtual const std::string& getTechCostName( ) const;
};

#endif   // __SOLAR_TECHNOLOGY_H

// end of solar_technology.h *************************************************



