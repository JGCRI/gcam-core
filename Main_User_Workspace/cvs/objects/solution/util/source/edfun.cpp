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

#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <set>
#include <vector>
#include "solution/util/include/edfun.hpp"
#include "util/base/include/fltcmp.hpp"
#include "containers/include/iactivity.h"
#include "util/base/include/util.h"

#include "util/base/include/timer.h"

#define UBVECTOR boost::numeric::ublas::vector 

const double LogEDFun::PMAX = 1.0e24;
const double LogEDFun::ARGMAX = 55.262042; // log(PMAX)


void LogEDFun::partial(int ip)
{
    partj = ip;
}


double LogEDFun::partialSize(int ip) const
{
  return double(mkts[ip].getDependencies().size()) / double(world->global_size());
}

void LogEDFun::operator()(const UBVECTOR<double> &x, UBVECTOR<double> &fx)
{
  assert(x.size() == mkts.size());
  assert(fx.size() == mkts.size());

  Timer& edfunMiscTimer = TimerRegistry::getInstance().getTimer( TimerRegistry::EDFUN_MISC );
  Timer& edfunPreTimer = TimerRegistry::getInstance().getTimer( TimerRegistry::EDFUN_PRE );
  edfunMiscTimer.start();
  edfunPreTimer.start();
  
  if(partj < 0) {               // not a partial derivative calculation
    mktplc->nullSuppliesAndDemands(period);

    for(size_t i=0; i<x.size(); ++i) {
      // It would be better to have a list of markets and operate on
      // them directly; however, that's not easy to get, starting from
      // a SolutionInfoSet instance, so we will work through the
      // SolutionInfo class.
      if(x[i] > ARGMAX)
        mkts[i].setPrice(PMAX);
      else
        mkts[i].setPrice(exp(x[i])); // input vector = log(price)
    }
    edfunMiscTimer.stop();
    edfunPreTimer.stop();
    Timer& evalFullTimer = TimerRegistry::getInstance().getTimer( TimerRegistry::EVAL_FULL );
    evalFullTimer.start();
#if GCAM_PARALLEL_ENABLED
    world->calc(period, world->getGlobalFlowGraph());
#else
    world->calc(period);
#endif
    evalFullTimer.stop();
  }
  else {                        // partial derivative calculation
    mktplc->mIsDerivativeCalc = true;
    solnset.storeValues();      // store all market values
    // In theory this loop is unnecessary, and we need only to set mkts[partj].
    for(size_t i=0; i<x.size(); ++i) {
      if(x[i] > ARGMAX)
        mkts[i].setPrice(PMAX);
      else
        mkts[i].setPrice(exp(x[i])); // input vector = log(price)
    }

    const std::vector<IActivity*>& affectedNodes = mkts[partj].getDependencies();
    /* \invariant At least one node is affected */
    assert(!affectedNodes.empty());
    edfunMiscTimer.stop();
    edfunPreTimer.stop();
    Timer& evalPartTimer = TimerRegistry::getInstance().getTimer( TimerRegistry::EVAL_PART );
    evalPartTimer.start();
#if GCAM_PARALLEL_ENABLED
    world->calc(period, mkts[partj].getFlowGraph(), &affectedNodes);
#else
    world->calc(period, affectedNodes);
#endif
    evalPartTimer.stop();
  }

  edfunMiscTimer.start();
  Timer& edfunPostTimer = TimerRegistry::getInstance().getTimer( TimerRegistry::EDFUN_POST );
  edfunPostTimer.start();
  // at this point we've recalculated all the supplies and demands.
  // Retrieve them, calculate log relative excesses, and store them in fx
  for(size_t i=0; i<mkts.size(); ++i) {
    const double TINY = util::getTinyNumber();
    double d = std::max(mkts[i].getDemand(), TINY);
    double s = std::max(mkts[i].getSupply(), TINY);
    fx[i] = log(d/s);
  }
  edfunPostTimer.stop();

  Timer& edfunAnResetTimer = TimerRegistry::getInstance().getTimer( TimerRegistry::EDFUN_AN_RESET );
  edfunAnResetTimer.start();
  
  if(partj >= 0) {
    // reset flags
      const std::vector<IActivity*>& affectedNodes = mkts[partj].getDependencies();
      for( size_t nodeIndex = 0 ; nodeIndex < affectedNodes.size(); ++nodeIndex ) {
          affectedNodes[ nodeIndex ]->setStale();
      }
    partj = -1;
    mktplc->mIsDerivativeCalc = false;
    solnset.restoreValues();    // reset all markets to values stored above
  }
  edfunAnResetTimer.stop();
  edfunMiscTimer.stop();
}
