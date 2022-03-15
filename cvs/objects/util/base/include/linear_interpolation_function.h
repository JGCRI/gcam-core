#ifndef _LINEAR_INTERPOLATION_FUNCTION_H_
#define _LINEAR_INTERPOLATION_FUNCTION_H_
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
 * \file linear_interpolation_function.h
 * \ingroup Objects
 * \brief Header file for the LinearInterpolationFunction class.
 * \author Pralit Patel
 */
#include <string>

#include "util/base/include/iinterpolation_function.h"

class DataPoint;

/*!
 * \ingroup Objects
 * \brief A linear interpolation function.
 * \details Linearly interpolate a value by constructing a line which connects
 *          the left and right data points and evaluating that line at the
 *          given x-value. TODO: what do we do about errors?
 *          <b>XML specification for LinearInterpolationFunction</b>
 *          - XML name: \c interpolation-function
 *          - Contained by:
 *          - Parsing inherited from class: None.
 *          - Attributes:
 *              - \c name = linear
 *                      The XML name attribute value which differentiates this
 *                      IInterpolationFunction from the others.
 *          - Elements:
 *
 * \author Pralit Patel
 * \author Sonny Kim
 */
class LinearInterpolationFunction : public IInterpolationFunction {
public:
    LinearInterpolationFunction();
    ~LinearInterpolationFunction();
    
    static const std::string& getXMLNameStatic();
    
    // IInterpolationFunction methods
    virtual IInterpolationFunction* clone() const;
    
    virtual double interpolate( const DataPoint* aLeftPoint, const DataPoint* aRightPoint,
        const double aXValue ) const;
    
     virtual void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const;
    
protected:
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        IInterpolationFunction
    )
};

#endif // _LINEAR_INTERPOLATION_FUNCTION_H_
