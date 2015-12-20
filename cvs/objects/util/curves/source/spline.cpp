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

#include "util/curves/include/spline.hpp"
#include "util/logger/include/ilogger.h"
#include <stdlib.h>



/*!
 * \brief Constructor that fits a natural spline to an input table
 */
Spline::Spline(const std::vector<double> &ax, const std::vector<double> &ay)
{
    fit_natural(ax, ay);
}

/*!
 * \brief Constructor that fits a boundary value spline to an input table
 */
Spline::Spline(const std::vector<double> &ax, const std::vector<double> &ay,
               double yp0, double ypn)
{
    fit_boundary(ax, ay, yp0, ypn);
}

/*!
 * \brief Fit a natural spline to an input table 
 * \details There must be at least two entries in the table (in which
 *          case you will just get a straight line).  Fewer than that
 *          will cause an abort.
 */
void Spline::fit_natural(const std::vector<double> &ax, const std::vector<double> &ay)
{
    int n = ax.size();
    if(n<2 || ay.size() != n) {
        log_and_abort("Invalid input table.");
    }
    //! temporary work space for fit_internal
    std::vector<double> u(n);

    x = ax;
    y = ay;
    ypp.resize(n);

    // the following initializations distinguish a natural spline from
    // the other sort
    ypp[0] = u[0] = ypp[n-1] = 0.0;
    
    fit_internal(u);
}

/*!
 * \brief Fit a spline with specified boundary derivatives to an input table
 * \details Same limitations apply as with fit_natural()
 */
void Spline::fit_boundary(const std::vector<double> &ax, const std::vector<double> &ay,
                          double yp0, double ypn)
{
    int n = ax.size();
    if(n<2 || ay.size() != n) {
        log_and_abort("Invalid input table.");
    }
    //! temporary work space for fit_internal
    std::vector<double> u(n);

    x = ax;
    y = ay;
    ypp.resize(n);

    // this part is what makes a boundary spline different from a natural spline
    ypp[0] = -0.5;
    u[0]   = (3.0/(x[1]-x[0])) * ((y[1]-y[0])/(x[1]-x[0]) - yp0);

    ypp[n-1] = 0.5;
    u[n-1]   = (3.0/(x[n-1]-x[n-2])) * (ypn - (y[n-1]-y[n-2])/(x[n-1]-x[n-2]));

    fit_internal(u);
}


/*!
 * \brief Solve for the second derivative values for the spline
 * \details See Numerical Recipes section 3.3
 */
void Spline::fit_internal(std::vector<double> &u)
{
    int n = x.size();
    //tridiagonal decomposition.  We do this inline because it is
    //relatively simple
    for(int i=1; i<n-1; ++i) {
        double sig= (x[i]-x[i-1])/(x[i+1]-x[i-1]);
        double p = sig*ypp[i-1]+2.0;

        ypp[i] = (sig-1.0)/p;
        u[i] = (y[i+1]-y[i])/(x[i+1]-x[i]) -
            (y[i]-y[i-1])/(x[i]-x[i-1]);
        u[i] = (6.0*u[i]/(x[i+1]-x[i-1]) - sig*u[i-1])/p;
    }

    // Calculate upper boundary.  At this point, ypp[n-1] and u[n-1]
    // still have the values that were initialized in fit_boundary or
    // fit_natural.
    ypp[n-1] = (u[n-1]-ypp[n-1]*u[n-2])/(ypp[n-1]*ypp[n-2]+1.0);

    // back-substitution loop for the tridiagonal solver
    for(int i=n-2; i>=0; --i) {
        ypp[i] = ypp[i]*ypp[i+1]+u[i];
    }

}


/*!
 * \brief Evaluate a spline interpolation at an input x value 
 * \details This function does not extrapolate beyond the ends of the
 *          table that was input.  Passing a value outside the range
 *          of the original table is invalid and will result in an
 *          abort.
 */
double Spline::interpolate(double ax) const
{
    int n = x.size();
    if(ax < x[0] || ax > x[n-1])
        log_and_abort("x-value out of range!");

    int ilo = 0;
    int ihi = n-1;

    // bisection search
    while(ihi-ilo > 1) {
        int i = (ihi+ilo)/2;

        if(x[i] > ax)
            ihi = i;
        else
            ilo = i;
    }

    double dx = x[ihi]-x[ilo];
    if(dx <= 0.0)
        log_and_abort("x values must be strictly ascending.");
    double dxi = 1.0/dx;

    double a = (x[ihi]-ax)*dxi;
    double b = (ax-x[ilo])*dxi;

    double val = a*y[ilo]+b*y[ihi] + // this is the linear interpolation term
        ((a*a*a-a)*ypp[ilo] + (b*b*b-b)*ypp[ihi])*dx*dx/6.0; // second derivative correction

    return val;
}

/*!
 * \brief return minimum valid x-value
 * \details does not abort if the spline is not valid; just returns 0
 */
double Spline::xmin(void) const
{
    if(isValid())
        return x[0];
    else
        return 0.0;
}


/*!
 * \brief return maximum valid x-value
 * \details does not abort if the spline is not valid; just returns 0
 */
double Spline::xmax(void) const
{
    if(isValid())
        return x[x.size()-1];
    else
        return 0.0;
}

void Spline::log_and_abort(const std::string &msg) const
{
    ILogger &mainlog = ILogger::getLogger("main_log");
    mainlog.setLevel(ILogger::SEVERE);

    mainlog << "In class Spline:  " << msg << std::endl;

    abort();
}
