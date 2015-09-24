#ifndef _PRECONDITIONER_HPP_
#define _PRECONDITIONER_HPP_
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
* \file preconditioner.hpp
* \ingroup Objects
* \brief This is the header file for the Preconditioner solver component class.
*
* \author Robert Link
*/
#include <string>

class CalcCounter; 
class Marketplace;
class World;
class SolutionInfoSet;
class ISolutionInfoFilter;

/*! 
* \ingroup Objects
* \brief A class interface which defines the Preconditioner solver component.
*
* \details The preconditioner is intended to run before any other
*          solver, and just once per period.  The goal is to filter
*          out any price inputs that are in ranges that will cause the
*          Newton-type solvers to flip out (e.g. by providing
*          misleading derivatives).  We use heuristics to adjust
*          prices but the main ones include:
*          1) supply < epsilon && demand > supply:  increase prices by
*             the increase-price-increment until one of these conditions
*             is no longer true.
*          2) supply > demand:  decrease prices by the decrease-price-increment
*             until supply changes (probably a decrease) by at least 10% of
*             its original value, or until demand > supply
*
*
* \author Robert Link
*/

class Preconditioner: public SolverComponent {
public:        
    Preconditioner( Marketplace* marketplaceIn, World* worldIn, CalcCounter* calcCounterIn );
    static const std::string& getXMLNameStatic();
    
    // SolverComponent methods
    virtual void init();
    virtual ReturnCode solve( SolutionInfoSet& aSolutionSet, const int aPeriod );
    virtual const std::string& getXMLName() const;

    // IParsable methods
    virtual bool XMLParse( const xercesc::DOMNode* aNode );

protected:
    unsigned int mItmax;                  // default = 30
    double mPriceIncreaseFac; // default = 0.25
    double mPriceDecreaseFac; // default = 0.1

    double mLargePrice;         // default = 1e6
    double mFTOL;                // default = getSmallNumber()
    
    //! A filter which will be used to determine which SolutionInfos this solver component
    //! will work on.
    std::auto_ptr<ISolutionInfoFilter> mSolutionInfoFilter;
    
};

#endif // _PRECONDITIONER_HPP_
