/*!
 * wind_technology.h
 * Created: 03/20/2007
 * Version: 03/20/2007
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

#if !defined( __WIND_TECHNOLOGY_H )
#define __WIND_TECHNOLOGY_H      // prevent multiple includes

// include files ***********************************************************

#include "technologies/include/technology.h"

// namespaces **************************************************************

// class: WindTechnology ***************************************************

/*!
 * \ingroup objects::biomass
 * \brief A technology class for wind power
 * \details This class contains a set of routines that implement
 *          technology for wind power
 *
 *   <b>XML specification for WindTechnology</b>
 *   - XML name: \c wind-technology
 *   - Contained by: Technology
 *   - Parsing inherited from class: None.
 *   - Attributes: none
 *   - Elements:
 *   - \c  WindTechnology::
 *
 * \author Kevin Walker
 * \date $ Date $
 * \version $ Revision $
 */
class WindTechnology : public Technology
{
public :

   typedef Technology   parent;

   /*! Default constructor
    *  \param aName the name of the technology
    *  \param aYear the year
    */
   WindTechnology(
      const std::string& aName = std::string(),
      const int          aYear = -1 );
   /*! Copy constructor
    *  \param other the instance to copy
    */
   WindTechnology(const WindTechnology& other);

   //! Destructor
   virtual ~WindTechnology(void);

   /*! Assignment operator
    *  \param other the instance to copy
    *  \return *this
    */
   WindTechnology& operator = (const WindTechnology&);

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
   virtual WindTechnology* clone( void ) const;

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

   //! XML tag name indices
   enum
   {
      AIR_DENSITY_KEY,
      AVERAGE_WIND_SPEED_KEY,
      CAPITAL_COST_KEY,
      CUTOUT_SPEED_KEY,
      FCR_KEY,
      GRID_CONNECTION_COST_KEY,
      OM_KEY,
      REFERENCE_HEIGHT_KEY,
      ROTOR_DIAMETER_KEY,
      TURBINE_DENSITY_KEY,
      TURBINE_DERATING_KEY,
      TURBINE_HUB_HEIGHT_KEY,
      TURBINE_RATING_KEY,
      WIND_FARM_LOSS_KEY,
      WIND_VELOCITY_EXPONENT_KEY,

      //! Number of keys
      NUM_KEYS
   };

   static std::string sXMLTagNames[NUM_KEYS];

   /*! Compute the ideal turbine output
    *  \param aAveWindSpeed the average Wind Speed
    *  \param aDiameter the turbine blade diameter (in meters)
    *  \param aAirDensity the average Air density (in g/m^3)
    */
   static double calcIdealTurbineOutput(
      double aAveWindSpeed,
       double aDiameter,
       double aAirDensity );

   /*! Compute the realized turbine output
    *  \param apInfo pointer to the market info
    */
   virtual double calcRealizedTurbineOutput( const IInfo* apInfo ) const;

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

   /*! Compute the capture coefficient for a turbine with a finite power rating.
    *  \param aAveWindSpeed the average Wind Speed
    *  \param aRating the turbine Rating (in MW)
    *  \param aDiameter the turbine Blade Diameter (in meters)
    *  \param aAirDensity the average Air density (in g/m^3)
    *  \param aCutoutSpeed the cut-out Speed (m/s)
    */
   static double calcTurbineCoefficient(
      double aAveWindSpeed,
       double aRating,
       double aDiameter,
       double aAirDensity,
       double aCutoutSpeed );

   double computeWindPowerVariance(
      double aAveWindSpeed,
      double aRating,
      double aDiameter,
      double aAirDensity,
      double aCutoutSpeed ) const;

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

   static const double kWhrtoGJ;

   //! Capital cost [2004 $/kW] based on rated capacity
   double mCapitalCost;

   //! Connection cost
   double mConnectCost;

   //! Turbine cutout speed [m/s]
   double mCutOutSpeed;

   //! fixed charge rate [unitless]
   double mFCR;

   //! Generation cost
   double mGenerationCost;

   //! Grid connection cost [$/km/MW capacity] based on rated capacity
   double mGridConnectionCost;

   //! [2004 $/kW/Yr] based on rated capacity
   double mOM;

   //! Realized turbine output
   double mRealizedTurbineOutput;

   //! Turbine rotor diameter [m]
   double mRotorDiameter;

   //! Land per turbine [MW/km2]
   double mTurbineDensity;

   //! Percentage of loss from ideal operation [unitless]
   double mTurbineDerating;

   //! Turbine hub height [m]
   double mTurbineHubHeight;

   //! Turbine output rating [unitless]
   double mTurbineRating;

   //! Wind capacity factor
   double mWindCapacityFactor;

   //! Percentage of loss from turbine to grid [unitless]
   double mWindFarmLoss;

   //! Wind Power Variance
   mutable double mWindPowerVariance;
};

#endif   // __WIND_TECHNOLOGY_H

// end of wind_technology.h ************************************************

