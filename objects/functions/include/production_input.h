#ifndef _PRODUCTION_INPUT_H_
#define _PRODUCTION_INPUT_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Labratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responsibility for the 
	use of this software.
*/

/*! 
* \file production_input.h
* \ingroup Objects
* \brief ProductionInput class header file.
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

/*! 
* \ingroup Objects
* \brief Defines a single input to a production function.
* \details TODO
* \note Some demand functions also use production inputs.
* \author Pralit Patel, Sonny Kim
*/
class ProductionInput : public Input
{
public:
	ProductionInput();
    ProductionInput* clone() const ;
    void copyParam( const Input* aInput );
    void copyParamsInto( ProductionInput& aProductionInput ) const;
    void copyParamsInto( DemandInput& aDemandInput ) const { assert( false ); }
	static const std::string& getXMLNameStatic();
    double getPriceElasticity() const;
    double getIncomeElasticity() const;
    void updateOutputContainer( OutputContainer* outputContainer, const int period ) const;
protected:
    const std::string& getXMLName() const;
    bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
private:
	void copy( const ProductionInput& prodInput );

};

#endif // _PRODUCTION_INPUT_H_

