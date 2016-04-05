#ifndef _MARKET_TAX_H_
#define _MARKET_TAX_H_
#if defined(_MSC_VER_)
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
* \file market_tax.h
* \ingroup Objects
* \brief The MarketTax class header file.
* \author Sonny Kim
*/

#include "marketplace/include/market_RES.h"

/*!
* \ingroup Objects
* \brief A class which defines a MarketTax object which is used for GHG and fuel constraint markets. 
* \author Sonny Kim
*/

class MarketTax: public MarketRES {
public:
    MarketTax( const MarketContainer* aContainer );
    virtual IMarketType::Type getType() const;

    virtual void nullSupply();
protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        MarketRES
    )
    
    virtual void toDebugXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual double getDefaultPrice() const;
};

#endif // _MARKET_TAX_H_
