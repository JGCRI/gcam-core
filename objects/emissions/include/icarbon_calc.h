#ifndef _ICARBON_CALC_H_
#define _ICARBON_CALC_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file icarbon_calc.h
 * \ingroup Objects
 * \brief The ICarbonCalc interface file.
 * \author James Blackwood, Josh Lurz
 */

#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/ivisitable.h"
#include "util/base/include/iparsable.h"
#include "util/base/include/iround_trippable.h"
// Forward declarations
class IInfo;
class Tabs;

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
                   public IRoundTrippable,
                   public IParsable 
{
public:
    //! Constructor
    inline ICarbonCalc();

    //! Destructor.
    inline virtual ~ICarbonCalc();

    // Documentation is inherited.
    virtual bool XMLParse( const xercesc::DOMNode* aNode ) = 0;

    // Documentation is inherited.
    virtual void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const = 0;
    
    // Documentation is inherited.
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const = 0;
    
    /*!
     * \brief Complete the initialization of the carbon calculator.
     */
	virtual void completeInit() = 0;

    /*!
     * \brief Conduct carbon calculations for a period.
     * \details Conduct carbon calculations for a period once all information is
     *          set. This should be called once per iteration, but there may be
     *          multiple iterations per period. This may perform historical
     *          calculations up to the current period if the historical years
     *          have not been calculated yet.
     * \param aPeriod Model period.
     */
    virtual void calc( const int aPeriod ) = 0;

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

    /*!
     * \brief Get the net terrestrial emission for a given year.
     * \details Returns the annual net terrestrial emission for the year. The
     *          net terrestrial emission is similar to the net land use change
     *          emission but includes any feedbacks and re-growth.
     * \note Carbon calculators may choose not to implement this function and
     *       should return DBL_MAX to indicate that.
     * \param aYear Year for which to get the land use change emission.
     * \return Net terrestrial emission.
     */
    virtual double getNetTerrestrial( const int aYear ) const = 0;

    /*!
     * \brief Set the total land use for a period.
     * \details Sets the total land area used by the containing land leaf for a
     *          period.
     * \param aLandUse Amount of land used.
     * \param aPeriod Model period.
     */
	virtual void setTotalLandUse( const double aLandUse,
                                  const int aPeriod ) = 0;

    /*!
     * \brief Get the total amount of carbon which would be contained by the
     *        land above ground at steady state.
     * \details Returns the total carbon contained by the land area above ground
     *          if the land area were to remain constant and reach steady state.
     *          Since carbon uptake and emissions may have a long timescale,
     *          this potential carbon contained may not equal the actual carbon
     *          contained. Above ground carbon is the carbon content of
     *          vegitation.
     * \param aYear Year.
     * \return Total potential above ground carbon.
     */
	virtual double getPotentialAboveGroundCarbon( const int aYear ) const = 0;
    
    /*!
     * \brief Set the potential above ground carbon per unit of land area for a
     *        given period.
     * \details Sets the carbon which will be contained above ground per unit of
     *          land if the land is allowed to reach steady state.
     * \param aAboveGroundCarbon Potential above ground carbon content.
     * \param aPeriod Model period.
     */
	virtual void setUnitAboveGroundCarbon( const double aAboveGroundCarbon,
                                           const int aPeriod ) = 0;

    /*!
     * \brief Get the total amount of carbon which would be contained by the
     *        land below ground at steady state.
     * \details Returns the total carbon contained by the land area below ground
     *          if the land area were to remain constant and reach steady state.
     *          Since carbon uptake and emissions may have a long timescale,
     *          this potential carbon contained may not equal the actual carbon
     *          contained. Below ground carbon is the carbon content of
     *          soils.
     * \param aYear Year.
     * \return Total potential below ground carbon.
     */
	virtual double getPotentialBelowGroundCarbon( const int aYear ) const = 0;

    /*!
     * \brief Set the potential below ground carbon per unit of land area for a
     *        given period.
     * \details Sets the carbon which will be contained below ground per unit of
     *          land if the land is allowed to reach steady state.
     * \param aBelowGroundCarbon Potential below ground carbon content.
     * \param aPeriod Model period.
     */
	virtual void setUnitBelowGroundCarbon( const double aBelowGroundCarbon,
                                           const int aPeriod ) = 0;

    // Documentation is inherited.
	virtual void accept( IVisitor* aVisitor,
                         const int aPeriod ) const = 0;
};

// Inline function definitions.
ICarbonCalc::ICarbonCalc(){
}

ICarbonCalc::~ICarbonCalc(){
}

#endif // _ICARBON_CALC_H_
