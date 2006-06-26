#ifndef _PRODUCTION_CARBON_CALC_H_
#define _PRODUCTION_CARBON_CALC_H_
#if defined(_MSC_VER)
#pragma once
#endif

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

    static const std::string& getXMLNameStatic();
    
 	virtual bool XMLParse( const xercesc::DOMNode* aNode );
	virtual void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const;
	virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const;

	virtual void completeInit();

	virtual double getPotentialAboveGroundCarbon( const int aYear ) const;
	
    virtual void setUnitAboveGroundCarbon( const double aAboveGroundCarbon,
                                           const int aPeriod );

	virtual double getPotentialBelowGroundCarbon( const int aYear ) const;

	virtual void setUnitBelowGroundCarbon( const double aBelowGroundCarbon,
                                           const int aPeriod );
protected:
    //! Potential above ground carbon content set by the Technology by period.
    objects::PeriodVector<double> mPotentialAboveGroundCarbon;

    //! Potential below ground carbon content set by the Technology by period.
    objects::PeriodVector<double> mPotentialBelowGroundCarbon;
};

#endif // _PRODUCTION_CARBON_CALC_H_
