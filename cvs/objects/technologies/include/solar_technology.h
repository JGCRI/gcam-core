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

   // Documentation is inherited
   virtual void toInputXMLDerived(
      std::ostream& out,
      Tabs*         tabs ) const;

   // Documentation is inherited
   virtual bool XMLDerivedClassParse(
      const std::string&      nodeName,
      const xercesc::DOMNode* curr );

   static const double      kWhrtoGJ;
   static const std::string ELECTRIC_SECTOR_NAME_KEY;
   static const std::string NO_SUN_DAYS_KEY;

   double getSolarPenetration( const int aPeriod ) const;
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        IntermittentTechnology,
    
       //! The key used for total annual irradiance
       CREATE_SIMPLE_VARIABLE( mTotalAnnualIrradianceKey, std::string, "irradiance-tagname" ),

       //! Capital cost [$/MW Capacity]
       CREATE_SIMPLE_VARIABLE( mCapitalCost, double, "capital-cost" ),

       //! Connection cost (used internally)
       CREATE_SIMPLE_VARIABLE( mCConnect, double, "c-connect" ),

       //! Net plant solar to electricity conversion efficiency
       CREATE_SIMPLE_VARIABLE( mCSPEfficiency, double, "net-solar-conversion-efficiency" ),

       //! Plant availability (% of year plant is available at scheduled time)
       CREATE_SIMPLE_VARIABLE( mPlantAvailability, double, "plant-availability-fraction" ),

       //! Plant Maintenance Fraction (% of year plant need to be down for scheduled maintenance)
       CREATE_SIMPLE_VARIABLE( mScheduledMaintenance, double, "plant-scheduled-maintenance-fraction" ),

       //! Fraction of scheduled maintenance that can be accomplished during no-sun day outages
       CREATE_SIMPLE_VARIABLE( mRandomMaintenanceFraction, double, "random-maintence-fraction" ),

       //! Generation cost (used internally)
       CREATE_SIMPLE_VARIABLE( mCGeneration, double, "c-generation" ),

       //! The average capacity factor for the CSP plant. [unitless] (used internally)
       CREATE_SIMPLE_VARIABLE( mCSPCapacityFactor, double, "csp-capacity-factor" ),

       //! [unitless]
       CREATE_SIMPLE_VARIABLE( mFCR, double, "fcr" ),

       //! The unit connection cost [$/km/MW capacity]. 
       CREATE_SIMPLE_VARIABLE( mGridConnectionCost, double, "grid-connection-cost" ),

       //! [$]
       CREATE_SIMPLE_VARIABLE( mOM, double, "om" ),

       //! The current region name (cached)
       CREATE_SIMPLE_VARIABLE( mRegionName, std::string, "region-name" ),

       //! The current sector name (cached)
       CREATE_SIMPLE_VARIABLE( mSectorName, std::string, "sector-name" ),

       /*! The fraction of the resource area that is actually occupied
        *  by the solar field. [unitless]
        */
       CREATE_SIMPLE_VARIABLE( mSolarFieldFraction, double, "solar-field-fraction" ),

       CREATE_SIMPLE_VARIABLE( mSolarFieldArea, double, "solar-field-area" ),

       //! The maximum energy loss fraction
       CREATE_SIMPLE_VARIABLE( mMaxLoss, double, "max-solar-loss" ),

       //! Exponent for energy loss as a function of penetration
       CREATE_SIMPLE_VARIABLE( mEfficiencyLossExponent, double, "loss-exponent" ),

       //! The max potential fraction of the sector's load that can be served by this technology
       CREATE_SIMPLE_VARIABLE( mMaxSectorLoadServed, double, "max-sector-load-served" ),

       //! Number of no sun days
       CREATE_SIMPLE_VARIABLE( mNoSunDays, double, "no-sun-days" )
    )
    
    void copy( const SolarTechnology& aOther );

   virtual const std::string& getTechCostName( ) const;
};

#endif   // __SOLAR_TECHNOLOGY_H

// end of solar_technology.h *************************************************



