#ifndef _UNMANAGED_CARBON_CALC_H_
#define _UNMANAGED_CARBON_CALC_H_
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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */


/*! 
 * \file unmanaged_carbon_calc.h
 * \ingroup Objects
 * \brief The UnmanagedCarbonCalc class header file.
 * \author James Blackwood
 */

#include <xercesc/dom/DOMNode.hpp>
#include "emissions/include/asimple_carbon_calc.h"

/*!
 * \brief A simple carbon content calculator used for unmanaged land leaves.
 * \details The unmanaged land carbon calculator reads-in value for above and
 *          below ground carbon for all years. These read-in values are then
 *          used to calculate the carbon value of the land and the net land use
 *          change emissions.
 *
 *          <b>XML specification for UnmanagedCarbonCalc</b>
 *          - XML name: \c unmanaged-carbon-calc
 *          - Contained by: UnmanagedLandLeaf
 *          - Parsing inherited from class: None
 *          - Attributes: None
 *          - Elements:
 *              - \c above-ground-carbon UnmanagedCarbonCalc::mAboveGroundCarbon
 *              - \c below-ground-carbon UnmanagedCarbonCalc::mBelowGroundCarbon
 */
class UnmanagedCarbonCalc : public ASimpleCarbonCalc {
public:
    UnmanagedCarbonCalc();
    virtual ~UnmanagedCarbonCalc();
    virtual UnmanagedCarbonCalc* clone() const;

    static const std::string& getXMLNameStatic();

    virtual bool XMLParse( const xercesc::DOMNode* aNode );
    virtual void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const;
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const;

    virtual void completeInit( int aKey );

    virtual double getPotentialAboveGroundCarbon( const int aYear ) const;
    
    virtual void setUnitAboveGroundCarbon( const double aAboveGroundCarbon,
                                           const int aPeriod );

    virtual double getPotentialBelowGroundCarbon( const int aYear ) const;

    virtual void setUnitBelowGroundCarbon( const double aBelowGroundCarbon,
                                           const int aPeriod );

    virtual double getActualAboveGroundCarbon( const int aYear ) const;
    
    virtual void setActualAboveGroundCarbon( const double aAboveGroundCarbon,
                                           const int aPeriod );

    virtual double getActualBelowGroundCarbon( const int aYear ) const;

    virtual void setActualBelowGroundCarbon( const double aBelowGroundCarbon,
                                           const int aPeriod );

    virtual void setMatureAge( const int aMatureAge );    

protected:
    //! Actual above ground carbon content by period. Varies with the amount of land
    objects::PeriodVector<double> mAboveGroundCarbon;

    //! Actual below ground carbon content by period. Varies with the amount of land
    objects::PeriodVector<double> mBelowGroundCarbon;

    //! Average above ground carbon content by year (read in).
    objects::YearVector<double> mAvgAboveGroundCarbon;

    //! Average below ground carbon content by year (read in).
    objects::YearVector<double> mAvgBelowGroundCarbon;
    
};

#endif // _UNMANAGED_CARBON_CALC_H_
