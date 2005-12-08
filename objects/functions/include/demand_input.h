#ifndef _DEMAND_INPUT_H_
#define _DEMAND_INPUT_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Laboratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responsibility for the 
	use of this software.
*/

/*! 
* \file demand_input.h
* \ingroup Objects
* \brief DemandInput class header file.
* \author Pralit Patel
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>

#include "functions/include/input.h"

class Tabs;
class IVisitor;

/*! 
* \ingroup Objects
* \brief Defines a single input to a demand function.
* \note Not all demand functions use DemandInputs, as some are modified
*       production functions.
* \details TODO
* \author Pralit Patel, Sonny Kim
*/
class DemandInput : public Input
{
public:
    DemandInput();
    DemandInput* clone() const;
    void copyParam( const Input* aInput );
    void copyParamsInto( DemandInput& aDemandInput ) const;
    void copyParamsInto( ProductionInput& aProductionInput ) const { assert( false ); }

    double getPriceElasticity() const;
    double getIncomeElasticity() const;
    
    void setExpPricePaid( double aExpPricePaid ){};
    double getExpPricePaid() const { return 0; }
	static const std::string& getXMLNameStatic();
    void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    const std::string& getXMLName() const;
private:
    //! Income Elasticity
    Value mIncomeElasticity;

    //! Price Elasticity
    Value mPriceElasticity;
};

#endif // _DEMAND_INPUT_H_
