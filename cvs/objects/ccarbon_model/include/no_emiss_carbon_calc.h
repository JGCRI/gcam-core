#ifndef _NO_EMISS_CARBON_CALC_H_
#define _NO_EMISS_CARBON_CALC_H_
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
 * \file no_emiss_carbon_calc.h
 * \ingroup Objects
 * \brief The NoEmissCarbonCalc header file.
 * \author Pralit Patel
 */

#include "ccarbon_model/include/land_carbon_densities.h"

/*!
 * \brief A carbon calculation which does not directly calculate emissions.
 * \details Leaves with type of carbon calculation work in conjunction with the
 *          NodeCarbonCalc to calculate emissions.  The calc method of this class
 *          will do nothing however emissions appropriate for this leaf will be
 *          set and be retrievable by the time calc would be called.
 * \sa NodeCarbonCalc
 */
class NoEmissCarbonCalc : public LandCarbonDensities
{
public:
    NoEmissCarbonCalc();
    virtual ~NoEmissCarbonCalc();
    
    static const std::string& getXMLNameStatic();

    virtual const std::string& getXMLName() const;
    
    virtual void calc( const int aPeriod, const int aEndYear );
    
    virtual void acceptDerived( IVisitor* aVisitor, const int aPeriod ) const;
private:

};

#endif // _NO_EMISS_CARBON_CALC_H_
