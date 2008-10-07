#ifndef _CARBON_STOCK_H_
#define _CARBON_STOCK_H_
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
 * \file carbon_stock.h
 * \ingroup Objects
 * \brief CarbonStock class header file.
 * \author Jim Naslund and Ming Chang
 */

#include "ccarbon_model/include/acarbon_stock.h"
#include "util/base/include/time_vector.h"

/*!
 * \brief Carbon stock for ordinary carbon boxes
 * \details This carbon stock object represents a stock of physical carbon in one
 *          specific carbon box.
 */
class CarbonStock : public ACarbonStock {

public:
    CarbonStock();
    CarbonStock( const CarbonStock& aCarbonStock );
    ~CarbonStock();

    virtual CarbonStock* clone() const;

    // IParsable Interface
    virtual bool XMLParse( const xercesc::DOMNode* aNode );
    static const std::string& getXMLNameStatic();

    virtual double transfer( const EnvironmentalInfo* aEnvInfo,
                             FlowType aFlowType, const int aYear );
    virtual void addToStock( double aValueToAdd, const int aYear );
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const;
    virtual void toDebugXML( const int aPeriod, std::ostream& aOut,
                             Tabs* aTabs ) const;
     virtual double getTurnoverTimescale();
private:
    //! The turnover timescale for changes in this carbon stock
    double mTurnoverTimescale;
};

#endif // _CARBON_STOCK_H_
