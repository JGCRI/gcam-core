#ifndef _SPLINE_HPP_
#define _SPLINE_HPP_
#if defined (_MSC_VER)
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

#include <vector>
#include <string>

/*!
 * \file spline.hpp
 * \ingroup Util
 * \brief Header file for spline class
 * \author Robert Link
 */

/*!
 * \class Spline
 * \brief Class for computing cubic spline interpolations
 *
 * \details Provides functions for computing cubic spline
 *          interpolations for tabulated functions.  This class
 *          provides only the basic mathematical machinery, not any of
 *          the GCAM-specific functionality, such as XML input
 *          parsing.  In particular, the Spline class doesn't inherit
 *          from the Curve class.  It is therefore intended to be
 *          included as a member in other GCAM classes, rather than to
 *          stand on its own as a first-class member of the GCAM
 *          framework.
 *          Important methods: 
 *          fit_natural -- compute the coefficients for a spline with
 *                         "natural" boundary conditions 
 *          fit_boundary -- compute the coefficients for a spline with
 *                          specified derivatives at the boundary. 
 *          interpolate -- use the coefficients computed by the
 *                         fitting procedures to compute an
 *                         interpolated y-value for an input x.  For
 *                         convenience, this function is aliased to
 *                         operator(). 
 * \note In theory one might want to fit a "mixed" spline; i.e., one
 *       with a natural boundary condition at one end and a specified
 *       derivative at the other.  It wouldn't be too hard to add a
 *       hook for this kind of fit, but I haven't done so because it
 *       seems unlikely to crop up in GCAM.
 */
class Spline {
    //! tabulated x values
    std::vector<double> x;
    //! tabulated y values
    std::vector<double> y;
    //! y'' values (computed in fit()
    std::vector<double> ypp;

    //! portion of the fitting procedure that is common to natural and
    //  boundary fits.
    void fit_internal(std::vector<double> &u);
    //! write an error to main_log and abort
    void log_and_abort(const std::string &msg) const;

public:
    //! Default constructor does nothing
    Spline() {}
    //! Constructor with input table (natural spline)
    Spline(const std::vector<double> &ax, const std::vector<double> &ay);
    //! Constructor with input table (boundary spline)
    Spline(const std::vector<double> &ax, const std::vector<double> &ay, double yp0, double ypn);

    void fit_natural(const std::vector<double> &ax, const std::vector<double> &ay);
    void fit_boundary(const std::vector<double> &ax, const std::vector<double> &ay,
                      double yp0, double ypn);
    double interpolate(double ax) const;
    //! alias for interpolate
    double operator()(double ax) const {return interpolate(ax);}
    //! minimum allowable x-value
    double xmin(void) const;
    //! maximum allowable x-value
    double xmax(void) const;
    //! indicate whether or not the spline is valid
    bool isValid(void) const {return x.size() >= 2;}
    //! clear the fitted values.  After this the spline will be invalid
    void clear(void) {x.clear(); y.clear(); ypp.clear();}
};


#endif
