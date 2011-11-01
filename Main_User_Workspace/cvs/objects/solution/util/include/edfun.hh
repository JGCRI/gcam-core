#ifndef EDFUN_HH_
#define EDFUN_HH_


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
 * @file functor-subs.hh
 * @ingroup Solution
 * @brief Some specific functor subclasses for use in GCAM
 */

#include <map>
#include <set>
#include <vector>
#include "marketplace/include/marketplace.h"
#include "containers/include/world.h"
#include "solution/util/include/solution_info_set.h"
#include "solution/util/include/functor.hh"
#include "util/base/include/atom.h"

#define UBVECTOR boost::numeric::ublas::vector

/*!
 * \class LogEDFun "solution/util/include/edfun.hh"
 * \brief Functor for computing the GCAM excess demand in log space
 *
 * An instance of this class computes a vector of log relative excess
 * demand (log(demand) - log(supply) as a function of a vector of
 * log(prices), for a set of markets determined at the time the object
 * is instantiated.  
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
class LogEDFun : public VecFVec<double,double>
{
  // information needed to compute ED(p)
  
  //!
  //! Solution info objects for markets being manipulated.
  
  //! @details This need not (and generally will not) include all of
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
  int partj; //!< flag indicating which variable in the input vector
             //!has changed in a partial derivative calculation

  // diagnostic variables
  std::vector<double> mstate;
public:
  // constructor
  LogEDFun(SolutionInfoSet &sisin,
           World *w, Marketplace *m, int per) :
    solnset(sisin),mkts(sisin.getSolvableSet()),
    world(w), mktplc(m), period(per), partj(-1) {na=nr=mkts.size();}
  
  // basic vector function interface
  virtual void operator()(const UBVECTOR<double> &x, UBVECTOR<double> &fx);
  virtual void partial(int ip) {partj = ip;}
};  


#undef UBVECTOR

#endif
