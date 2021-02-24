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
#include <vector>

#include "util/base/include/definitions.h"
#include "solution/util/include/edfun.hpp"
#include "containers/include/world.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/util.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/scenario.h"
#include "util/base/include/manage_state_variables.hpp"

#include "util/base/include/timer.h"

extern Scenario* scenario;

const double LogEDFun::PMAX = 1.0e24;
const double LogEDFun::ARGMAX = 55.262042; // log(PMAX)
const double LogEDFun::MINXSCL = 1.0e-5;

// constructor
LogEDFun::LogEDFun(SolutionInfoSet &sisin,
                   World *w, Marketplace *m, int per, bool aLogPricep) :
    mkts(sisin.getSolvableSet()),
    solnset(sisin),
    world(w), mktplc(m), period(per),
    mLogPricep(aLogPricep),
    slope(UBVECTOR::Constant(mkts.size(), 1.0))
{
    na=nr=mkts.size();
    mdiagnostic=false;

    // set up the scale vectors
    mxscl.resize(na);
    mfxscl.resize(nr);          // note na==nr

    if(!mLogPricep) {
        // linear prices & outputs, so x0 is the price, and fx0 is
        // 1/demand(forecast), for all markets
        for(int i=0; i<na; ++i) {
            slope[i] = mkts[i].getCorrectionSlope();
            // forecast demands has been constrained so that it
            // doesn't give nonsensical results here, but forecast
            // price is used for other things and so hasn't been
            // so-modified.
            if( mkts[i].getType() == IMarketType::TAX && ( mkts[i].getForecastPrice() < mkts[i].getLowerBoundSupplyPrice() ||
                mkts[i].getForecastPrice() > mkts[i].getUpperBoundSupplyPrice() ) )
            {
                mxscl[i] = mkts[i].getUpperBoundSupplyPrice();
            }
            else if( (mkts[i].getType() == IMarketType::RES || mkts[i].getType() == IMarketType::SUBSIDY) && ( mkts[i].getForecastPrice() <= mkts[i].getLowerBoundSupplyPrice() ) )
            {
                mxscl[i] = 1.0;
            } else {
                mxscl[i] = std::max(fabs(mkts[i].getForecastPrice()), MINXSCL);
            }
            mfxscl[i] = 1.0/std::max(fabs(mkts[i].getForecastDemand()), MINXSCL);
        }
    } else {
        // for log prices & outputs the situtation is more
        // complicated.  We can probably leave the (log) inputs alone.
        // On the output side, we can probably leave the normal market
        // outputs alone, but we need to rescale the outputs for other
        // market types (which are returned as linear).  (Why don't we
        // do the prices for constraint markets as linear too?)
        for(int i=0; i<na; ++i) {
            mxscl[i] = 1.0;
            if(mkts[i].getType() != IMarketType::NORMAL)
                mfxscl[i] = 1.0/mkts[i].getForecastDemand();
            else
                mfxscl[i] = 1.0;
        }
    } 
}

/*!
 * \brief scale a solver's initial inputs as necessary using the xscl vector 
 * \details When a solver sets up its initial-guess input vector, we
 *          need to scale those values so that when this function
 *          applies the scale vector we get back the initial guess
 *          values that the solver intended to use.
 */
void LogEDFun::scaleInitInputs(UBVECTOR &ax)
{
    for(unsigned i=0; i<ax.size(); ++i)
        ax[i] /= mxscl[i];
}


void LogEDFun::partial(int ip)
{
    Timer& edfunAnResetTimer = TimerRegistry::getInstance().getTimer( TimerRegistry::EDFUN_AN_RESET );
    edfunAnResetTimer.start();
    if(ip >= 0) {
        // We are about to perform partial derviatives so snap back *all* state
        // including prices/supplies/demands to a "base" state before we perform
        // this partial derivative
        scenario->mManageStateVars->copyState();
    }
    else if(ip == -1 ) {
        // We are calculating a full model run so ensure the partial derivative
        // flags are turned off and the "base" state is being updated.
        mktplc->mIsDerivativeCalc = false;
        scenario->mManageStateVars->setPartialDeriv(false);
    }
    edfunAnResetTimer.stop();
}


double LogEDFun::partialSize(int ip) const
{
  return double(mkts[ip].getDependencies().size()) / double(world->getGlobalOrderingSize());
}


/*!
 * \brief Set the slope to use for the negative correction supply which
 *        is applied when prices are below the lower bound of supply behavior.
 * \details In theory we would want the correction slope to match the slope
 *          just above the lower bound price point so that the function is
 *          continous.  However to explicitly calculate this would require
 *          a lot of extra iterations just to get this information.  Instead
 *          this method will be called each time we have a new derivative and
 *          we will update the slope if the price happens to be "close" to just
 *          above the lower bound price.
 * \param adx The vector of self derivatives for all markets which was just calculated.
 */
void LogEDFun::setSlope(UBVECTOR& adx) {
    assert(adx.size() == mkts.size());
    for(int i = 0; i < mkts.size(); ++i) {
        double newSlope = abs(adx[i]);
        // compare scaled prices to ensure consistent hueristics accross markets
        double p0 = mkts[i].getLowerBoundSupplyPrice() / mxscl[i];
        double p = mkts[i].getPrice() / mxscl[i];
        if( newSlope == 0.0) {
            // ignore zero slope which is obviously problematic
        }
        else if(( mkts[i].getType() == IMarketType::RES  // (constraint type markets only)
            || mkts[i].getType() == IMarketType::TAX
            || mkts[i].getType() == IMarketType::SUBSIDY ) && (p > p0 && p < (p0 + 0.5)))
        {
            slope[i] = newSlope;
            mkts[i].setCorrectionSlope(newSlope);
        }
        else if(( mkts[i].getType() == IMarketType::NORMAL) && (p > p0 && p < (p0 + 0.1)))
        {
            slope[i] = newSlope;
            mkts[i].setCorrectionSlope(newSlope);
        }
        // other markets would be PRICE and DEMAND type markets for which the slope should
        // always be 1
    }
}

void LogEDFun::operator()(const UBVECTOR &ax, UBVECTOR &fx, const int partj)
{
  assert(ax.size() == mkts.size());
  assert(fx.size() == mkts.size());

  Timer& edfunMiscTimer = TimerRegistry::getInstance().getTimer( TimerRegistry::EDFUN_MISC );
  Timer& edfunPreTimer = TimerRegistry::getInstance().getTimer( TimerRegistry::EDFUN_PRE );
  edfunMiscTimer.start();
  edfunPreTimer.start();

  // copy x so we can scale it without destroying the original.  This
  // is probably going to incur enough overhead that we will
  // eventually want to do the scaling inline when we assign the
  // prices, but this is easier for now.
  UBVECTOR x(ax.size()); 
  for(unsigned int i=0; i<x.size(); ++i)
      x[i] = ax[i]*mxscl[i];
  

  /**** The way we do this is kind of ugly.  We have two procedures
   **** that are almost but not quite identical, so we have two blocks
   **** (one for partial derivatives, one for regular evals) that are
   **** mostly duplicative.  Should really be cleaned up at some
   **** point.
   ****/
  
  if(partj < 0) {               // not a partial derivative calculation
    /****
     * 1A Set the model inputs using the solutionInfo objects (full eval version)
     ****/

    mktplc->nullSuppliesAndDemands(period);

    /* set prices into the marketplace. If the inputs are log-prices,
       we have to exp() them first*/
    if(mLogPricep) {
      /***** In part 3 we make some exceptions for certain market
       ***** types.  Perhaps we should consider doing that here too.
       ***** E.g., we could make the inputs for price and demand
       ***** markets always linear.
       *****/
      for(size_t i=0; i<x.size(); ++i) {
        if(x[i] > ARGMAX)
          mkts[i].setPrice(PMAX);
        else
          mkts[i].setPrice(exp(x[i])); // input vector = log(price)
      }
    }
    else {
      for(size_t i=0; i<x.size(); ++i) {
        mkts[i].setPrice(x[i]); // input vector = price
      }
    }
    edfunMiscTimer.stop();
    edfunPreTimer.stop(); 

    /****
     * 2A Evaluate the model (full eval version)
     ****/ 
    Timer& evalFullTimer = TimerRegistry::getInstance().getTimer( TimerRegistry::EVAL_FULL );
    evalFullTimer.start();
#if GCAM_PARALLEL_ENABLED
    world->calc(period, world->getGlobalFlowGraph());
#else
    world->calc(period);
#endif
    evalFullTimer.stop();
    // Proceed to part 3 below.
  }
  else {                        // partial derivative calculation
    /****
     * 1B Set the model inputs using the solutionInfo objects (partial derivative version)
     ****/ 
    mktplc->mIsDerivativeCalc = true;


    if(mdiagnostic) {
      ILogger &solverlog = ILogger::getLogger("solver_log");
      solverlog.setLevel(ILogger::DEBUG);

      solverlog << "j= " << partj <<"\tprice  \tsupply \tdemand\tmarket"
                << "old   \t" << mkts[partj].getPrice() << "\t" << mkts[partj].getSupply()
                << "\t" << mkts[partj].getDemand()
                << "\t" << mkts[partj].getName() << "\n";
    }
    
    // In theory the loop over markets is unnecessary, and we need
    // only to set mkts[partj].  We should try that sometime.
    if(mLogPricep) {            
      /***** In part 3 we make some exceptions for certain market
       ***** types.  Perhaps we should consider doing that here too.
       ***** E.g., we could make the inputs for price and demand
       ***** markets always linear.
       *****/
      for(size_t i=0; i<x.size(); ++i) {
        if(x[i] > ARGMAX)
          mkts[i].setPrice(PMAX);
        else
          mkts[i].setPrice(exp(x[i])); // input vector = log(price)
      }
    }
    else {
        // During a partial calc only the price of the partj'th element should
        // change and the rest were reset from stored values.  In theory
        // those reset prices are the same as in x however there may be some
        // slight differences due to roundoff error.
        mkts[partj].setPrice(x[partj]);
    }

    /****
     * 2B Evaluate the model (partial derivative version)
     ****/
    const std::vector<IActivity*>& affectedNodes = mkts[partj].getDependencies();
    /* \invariant At least one node is affected */
    assert(!affectedNodes.empty());
    edfunMiscTimer.stop();
    edfunPreTimer.stop();
    Timer& evalPartTimer = TimerRegistry::getInstance().getTimer( TimerRegistry::EVAL_PART );
    evalPartTimer.start();
    // Note even when running with GCAM_PARALLEL_ENABLED we still run in serial
    // mode for partial derivatives.  This is because the loop over each partial
    // derivative to run is a parallel_for.
    world->calc(period, affectedNodes);
    evalPartTimer.stop();

    if(mdiagnostic) {
      ILogger &solverlog = ILogger::getLogger("solver_log");
      solverlog.setLevel(ILogger::DEBUG);
      
      solverlog << "new   \t" << mkts[partj].getPrice() << "\t" << mkts[partj].getSupply()
                << "\t" << mkts[partj].getDemand()
                << "\t" << mkts[partj].getName() << "\n";
    }
  }

  
  edfunMiscTimer.start();
  Timer& edfunPostTimer = TimerRegistry::getInstance().getTimer( TimerRegistry::EDFUN_POST );
  edfunPostTimer.start();

  /****
   * 3 Collect the outputs from the solutionInfo objects and repack them in the
   *   output vector
   ****/
  
  // at this point we've recalculated all the supplies and demands.
  // Retrieve them, calculate output according to market type, and
  // store them in fx
  for(size_t i=0; i<mkts.size(); ++i) {
    const double TINY = util::getTinyNumber();
    if(mLogPricep && mkts[i].getType() == IMarketType::NORMAL) { // LOG CASE (NORMAL markets only)
      // for normal markets, output log(demand/supply), if we are using log prices
      double d = std::max(mkts[i].getDemand(), TINY);
      double s = std::max(mkts[i].getSupply(), TINY);
      double p0 = mkts[i].getLowerBoundSupplyPrice();
      double p  = x[i]>=ARGMAX ? PMAX : exp(x[i]);
      double c  = std::max(0.0, p0-p);
      fx[i] = log(d/s)+c;
    }
    else if(mkts[i].getType() == IMarketType::NORMAL) { // LINEAR CASE (NORMAL markets only)
        double d = mkts[i].getDemand();
        double s = mkts[i].getSupply();

        // generate a correction if the input price is less than the
        // supply curve lower bound.  This is most effective if we transform the
        // price correction from it's price scale into a scale relevant for the demands.
        // Note that the lower bound limit is an estimate
        // and in some cases may not be exact, thus we will only apply the correction
        // if the supply was indeed zero.  If the actual lower bound price is significantly
        // different than the estimated this may generate a discontinuity.
        double p0 = mkts[i].getLowerBoundSupplyPrice();
        double c = s == 0 ? std::max(0.0, (p0-x[i])/mfxscl[i]/mxscl[i]) * slope[i] : 0;
        // give difference as a fraction of demand
        fx[i] = d - s + c;          // == d-(s-c); i.e., the correction subtracts from supply
    }
    else if(!mLogPricep && ( mkts[i].getType() == IMarketType::RES  // LINEAR CASE (constraint type markets only)
            || mkts[i].getType() == IMarketType::TAX
            || mkts[i].getType() == IMarketType::SUBSIDY
            || mkts[i].getType() == IMarketType::TRIAL_VALUE ) )
    {
        double d = mkts[i].getDemand();
        double s = mkts[i].getSupply();

        // generate a correction if the input price is less than the
        // supply curve lower bound.  This is most effective if we transform the
        // price correction from it's price scale into a scale relevant for the demands.
        // Note that for constraint type markets this lower bound is the price (typically
        // zero) below which the policy is considered non-binding in which case the correction
        // is essentially adding extra demand to meet the constraint.
        double p0 = mkts[i].getLowerBoundSupplyPrice();
        double c = std::max(0.0, (p0-x[i])/mfxscl[i]/mxscl[i]) * slope[i];
        // give difference as a fraction of demand
        fx[i] = d - s + c;          // == d-(s-c); i.e., the correction subtracts from supply
        /*if(c>0.0) {
          ILogger &solverlog = ILogger::getLogger("solver_log");
          solverlog.setLevel(ILogger::DEBUG);
          solverlog << "\t\tAdding supply correction: i= " << i << "  p= " << x[i]
                    << "  p0= " << p0 << "  c= " << c << "  s= " << s << " d= " << d << " modified F(x)= " << fx[i]
                    << "\n";
        }*/
    }
    else {                      // Markets that are neither normal nor constraint types.
      // for other types of markets (mostly price, demand, and
      // trial-value), output fractional demand - supply
        fx[i] = mkts[i].getDemand() - mkts[i].getSupply();
    }
  }

  // Do the scaling for fx
  for(unsigned i=0; i<fx.size(); ++i)
      fx[i] *= mfxscl[i];
  
  edfunPostTimer.stop();

  edfunMiscTimer.stop();
}

