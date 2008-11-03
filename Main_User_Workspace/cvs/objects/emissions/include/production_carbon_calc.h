#ifndef _PRODUCTION_CARBON_CALC_H_
#define _PRODUCTION_CARBON_CALC_H_
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
* \file production_carbon_calc.h
* \ingroup Objects
* \brief The ProductionCarbonCalc class header file.
* \author James Blackwood
*/
#include <xercesc/dom/DOMNode.hpp>

#include "emissions/include/asimple_carbon_calc.h"

/*!
 * \brief A simple carbon content calculator used for production land leaves.
 * \details The production land carbon calculator has above and below ground
 *          carbon contents set by the agricultural technology which makes use
 *          of the land. The technologies set these values by period, and the
 *          production carbon calculator is responsible for determining annual
 *          values. These annual values are then used to calculate the carbon
 *          value of the land and the net land use change emissions.
 *
 *          <b>XML specification for ProductionCarbonCalc</b>
 *          - XML name: Not parsed
 *          - Contained by: LandLeaf
 *          - Parsing inherited from class: None
 *          - Attributes: None
 *          - Elements: None
 */
class ProductionCarbonCalc : public ASimpleCarbonCalc {
public:
    ProductionCarbonCalc();
    virtual ~ProductionCarbonCalc();
    virtual ProductionCarbonCalc* clone() const;

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
        
    virtual double getActualAboveGroundCarbonDensity( const int aYear ) const;
    
    virtual void setActualAboveGroundCarbonDensity( const double aAboveGroundCarbonDensity,
                                           const int aPeriod );

    virtual double getActualBelowGroundCarbonDensity( const int aYear ) const;

    virtual void setActualBelowGroundCarbonDensity( const double aBelowGroundCarbonDensity,
                                           const int aPeriod );
    
    virtual void setMatureAge(const int aMatureAge );    
    
protected:
    //! Potential above ground carbon content set by the Technology by period.
    objects::PeriodVector<double> mPotentialAboveGroundCarbon;

    //! Potential below ground carbon content set by the Technology by period.
    objects::PeriodVector<double> mPotentialBelowGroundCarbon;

    //! Actual above ground carbon content. This carbon content varies by yield
    objects::PeriodVector<double> mActualAboveGroundCarbon;

    //! Actual below ground carbon content. This carbon content varies by yield
    objects::PeriodVector<double> mActualBelowGroundCarbon;
};

#endif // _PRODUCTION_CARBON_CALC_H_
