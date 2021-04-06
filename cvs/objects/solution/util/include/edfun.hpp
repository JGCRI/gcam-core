#ifndef EDFUN_HPP_
#define EDFUN_HPP_


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
 * @file functor-subs.hpp
 * @ingroup Solution
 * @brief Some specific functor subclasses for use in GCAM
 */

#include <vector>
#include "solution/util/include/solution_info_set.h"
#include "solution/util/include/functor.hpp"

class Marketplace;
class World;


/*!
 * \class LogEDFun "solution/util/include/edfun.hpp"
 * \brief Functor for computing the GCAM excess demand in log space
 *
 * An instance of this class computes a vector of log relative excess
 * demand (log(demand) - log(supply) as a function of a vector of
 * inputs, for a set of markets determined at the time the object is
 * instantiated.  The inputs may be either prices or log(prices),
 * depending on a flag set in the constructor.
 *
 * \author Robert Link
 *
 * \todo: EDFun and LogEDFun share a lot of functionality.  We should
 * factor those shared functions into a base class so that both
 * classes can use it, rather than writing it independently in each
 * class.
 *
 * \sa EDFun
 */
class LogEDFun : public VecFVec
{
  // information needed to compute ED(p)
  
  //!
  //! Solution info objects for markets being manipulated.
  
  //! \details This need not (and generally will not) include all of
  //! the markets in the model.  Markets not included in the list will
  //! have their prices held constant.
  std::vector<SolutionInfo> mkts;

  SolutionInfoSet &solnset;         //!< All SolutionInfo objects,
                                    //!including ones not being
                                    //!solved.  We use this to reset
                                    //!the model to the base values
                                    //!when calculating a Jacobian.

  World *world;
  Marketplace *mktplc; //!< Marketplace holds *all* markets. We need
                       //!this to reset their supply and demand as
                       //!required.
  int period;
  bool mLogPricep;               //!< Flag indicating whether inputs are prices or log-prices

  // diagnostic variables
  std::vector<double> mstate;
public:
  LogEDFun(SolutionInfoSet &sisin, World *w, Marketplace *m, int per, bool aLogPricep=true);
  
  // basic vector function interface
  virtual void operator()(const UBVECTOR &x, UBVECTOR &fx, const int partj=-1);
  virtual void partial(int ip);
  virtual double partialSize(int ip) const;
  void scaleInitInputs(UBVECTOR &ax);
  void setSlope(UBVECTOR &adx);

  // Constants to protect against overflow: 
  static const double PMAX;            //!< Greatest allowable price
  static const double ARGMAX;          //!< log of greatest allowable price
  //! smallest value allowed for rescaling x values
  static const double MINXSCL;

protected:
  // scale factors for input and output
  UBVECTOR mxscl;
  UBVECTOR mfxscl;
  // supply correction slope to use for prices below the "lower bound"
  UBVECTOR slope;
    
};  


#endif
