#ifndef _ICARBON_CALC_H_
#define _ICARBON_CALC_H_
#if defined(_MSC_VER)
#pragma once
#endif

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
 * \file icarbon_calc.h
 * \ingroup Objects
 * \brief The ICarbonCalc interface file.
 * \author James Blackwood, Josh Lurz
 */

#include <boost/core/noncopyable.hpp>

#include "util/base/include/ivisitable.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include "util/base/include/data_definition_util.h"

// Forward declarations
class IInfo;
class Tabs;
class LandUseHistory;
class LandLeaf;

// Need to forward declare the subclasses as well.
class ASimpleCarbonCalc;
class LandCarbonDensities;
class NoEmissCarbonCalc;

/*!
 * \brief An interface to an object responsible for determining the annual
 *        carbon content of a land area.
 * \details Carbon content calculators are used to determine how much carbon is
 *          contained in a land leaf over time, and any net carbon emissions
 *          This calculation can be simple (net carbon accounting) or complex 
 *          (full carbon cycle with feedbacks). The carbon content may change as 
 *          a result of internal processes (re-growth or feedbacks), as a result
 *          of land-use changes, or due changes in the technologies used to grow
 *          and harvest crops. Changes in the carbon content of the land will
 *          result in net carbon emissions. The calculated carbon content of the
 *          land is also used to determine the carbon subsidy. Carbon content
 *          calculators do not calculate land use change, this is given to them
 *          by the land leaf.
 */
class ICarbonCalc: public IVisitable,
                   private boost::noncopyable
{
public:
    
    /*!
     * \brief An enum containing the possible "modes" in which to calculate this
     *        carbon calc, i.e. calling ICarbonCalc::calc.
     * \details Given that these calculations can be relative expensive to calculate
     *          and results in one period directly affecting many future years we give
     *          the users the ability to call "calc" in the following modes:
     *
     *            - CarbonCalcMode::eStoreResults Save all results for reporting.
     *              This mode is typically used in postCalc to ensure LUC emissions are
     *              saved and available for calculating emissions in the next time periods,
     *              feeding into the climate model, reporting, etc.
     *            - CarbonCalcMode::eReturnTotal Avoid saving any results and intead
     *              simply return the total emissions.  This mode is typically used during
     *              World.calc and allows us to do the minimum computations in case we are
     *              intending to add them to a CO2 constraint policy for instance, which is
     *              the only time the value would be required during World.calc.
     *            - CarbonCalcMode::eReverseCalc Run the calculation just to back
     *              out the emissions, etc from the currently saved results.  This is called
     *              during initCalc which allows us to re-run any model period, such as during
     *              target finder.
     */
    enum CarbonCalcMode {
        /*!
         * \brief Run the calculation and save all results.
         */
        eStoreResults,
        
        /*!
         * \brief Run the calculation but do not store any results.
         * \details Flag used as an optimization to avoid storing the
         *          full LUC emissins during World.calc and instead only
         *          return the total emissions in the given year.
         */
        eReturnTotal,
        
        /*!
         * \brief Run the calculation with the intent of backing out all the
         *        emissions, etc for the given year from all of the future saved
         *        results.
         * \details Such a mode is required to properly calculate emissions if we
         *          need to re-run some model period, such as for target finder.
         */
        eReverseCalc
    };
    
    //! Constructor
    inline ICarbonCalc();

    //! Destructor.
    inline virtual ~ICarbonCalc();

    // Documentation is inherited.
    virtual void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const = 0;

    //! Get element name used for XML parsing.
    virtual const std::string& getXMLName() const = 0;
    
    /*!
     * \brief Complete the initialization of the carbon calculator.
     */
    virtual void completeInit( const double aPrivateDiscountRateLand ) = 0;
    
    /*!
     * \brief Period specific initiliazations before calculations begin.
     * \param aPeriod The period which is about to begin.
     */
    virtual void initCalc( const int aPeriod ) = 0;

    /*!
     * \brief Sets objects to retrieve historial and future land use.
     * \details Initializes the carbon calculation with objects for the historical
     *          land use and the leaf containing the carbon calculator.
     * \param aHistory Historical land use object which may be null.
     * \param aLandLeaf The containing land leaf which must exist.
     */
    virtual void setLandUseObjects( const LandUseHistory* aHistory, const LandLeaf* aLandLeaf ) = 0;

    /*!
     * \brief Conduct carbon calculations for a period.
     * \details Conduct carbon calculations for a period once all information is
     *          set. This should be called once per iteration, but there may be
     *          multiple iterations per period. This may perform historical
     *          calculations up to the current period if the historical years
     *          have not been calculated yet.  Future emissions will only be
     *          calculated to the given end year.
     * \param aPeriod The current model period that is being calculated.
     * \param aEndYear The year to calculate future emissions to.
     * \param aCalcMode The "mode" in which to run the calculation.
     * \return The total LUC emissions in aEndYear.
     * \sa CarbonCalcMode
     */
    virtual double calc( const int aPeriod, const int aEndYear, const CarbonCalcMode aCalcMode ) = 0;

    /*!
     * \brief Get the net land use change emissions for a given year.
     * \details Returns the net land use change emission at the year. This is an
     *          annual emission. The value is a sum of the above and below
     *          ground uptake and emissions. A positive return value represents
     *          an emission, a negative represents net uptake.
     * \param aYear Year for which to get the land use change emission.
     * \return Annual net land use change emission for the year.
     */
    virtual double getNetLandUseChangeEmission( const int aYear ) const = 0;
    
    virtual double getNetLandUseChangeEmissionAbove( const int aYear ) const = 0;
    
    virtual double getNetLandUseChangeEmissionBelow( const int aYear ) const = 0;

    virtual double getActualAboveGroundCarbonDensity( const int aYear ) const = 0;
    
    virtual void setActualAboveGroundCarbonDensity( const double aAboveGroundCarbonDensity,
                                           const int aPeriod ) = 0;

    virtual double getActualBelowGroundCarbonDensity( const int aYear ) const = 0;

    virtual void setActualBelowGroundCarbonDensity( const double aBelowGroundCarbonDensity,
                                           const int aPeriod ) = 0;
    
    virtual int getMatureAge( ) const = 0;

    virtual double getAboveGroundCarbonSubsidyDiscountFactor( ) = 0;

    virtual double getBelowGroundCarbonSubsidyDiscountFactor( ) = 0;

	virtual double getAboveGroundCarbonStock( const int aYear ) const = 0;
	
    virtual double getBelowGroundCarbonStock( const int aYear ) const = 0;
	
    virtual void setSoilTimeScale( const int aTimeScale ) = 0;
    /*!
     * \brief Set the mature age of this land cover type; used by simple carbon calculator.
     * \details Sets the mature age.  For crops this is 1 (the default), but forests
     *            grow slowly, and their emission curve is calculated using this.
     * \param aMatureAge The age (in years) this land cover type reaches full biomass.
     * \param aPeriod Model period.
     */
    virtual void setMatureAge( const int aMatureAge ) = 0;

    // Documentation is inherited.
    virtual void accept( IVisitor* aVisitor,
                         const int aPeriod ) const = 0;

    virtual void acceptDerived( IVisitor* aVisitor,
                         const int aPeriod ) const = 0;
    
    protected:
    
    /* We must declare all subclasses of ICarbonCalc in this interface to allow
     * automatic traversal of the hierarchy under introspection.
     */
    DEFINE_DATA(
        DEFINE_SUBCLASS_FAMILY( ICarbonCalc, ASimpleCarbonCalc, LandCarbonDensities, NoEmissCarbonCalc )
    )
};

// Inline function definitions.
ICarbonCalc::ICarbonCalc(){
}

ICarbonCalc::~ICarbonCalc(){
}

#endif // _ICARBON_CALC_H_
