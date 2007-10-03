#ifndef _SUMMER_CARBON_STOCK_H_
#define _SUMMER_CARBON_STOCK_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

/*! 
 * \file summer_carbon_stock.h
 * \ingroup Objects
 * \brief SummerCarbonStock class header file.
 * \author Jim Naslund and Ming Chang
 */

#include "ccarbon_model/include/acarbon_stock.h"
#include "util/base/include/time_vector.h"

/*!
 * \brief An implementation of the ICarbonStock Interface.
 * \details A basic implementation of the ICarbonStock Interface that supports
 *          all interface functions.  This class is essentially a wrapper around
 *          a double representing a carbon stock.
 */
class SummerCarbonStock : public ACarbonStock {

public:
	SummerCarbonStock();
	SummerCarbonStock( const SummerCarbonStock& aSummerCarbonStock );
	virtual ~SummerCarbonStock();
	virtual SummerCarbonStock* clone() const;

    // IParsable Interface
    virtual bool XMLParse( const xercesc::DOMNode* aNode );

    virtual double transfer( const EnvironmentalInfo* aEnvInfo,
                             FlowType aFlowType, const int aYear );
};

#endif // _SUMMER_CARBON_STOCK_H_
