/*! 
* \file BisectionNRSolver.cpp
* \ingroup CIAM
* \brief BisectionNRSolver class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include <iostream>
#include <fstream>

#include "BisectionNRSolver.h"
#include "World.h"
#include "Modeltime.h"
#include "Scenario.h"
#include "SolverLibrary.h"
#include "Marketplace.h"
#include "Configuration.h"
#include "Util.h"

using namespace std;

extern Scenario* scenario;
extern ofstream bugoutfile, logfile;

//! Constructor
BisectionNRSolver::BisectionNRSolver( Marketplace* marketplaceIn ):Solver( marketplaceIn ) {
   bugTracking = true;
   bugMinimal = true;
   trackED = false;
   totIter = 0;
}

//! Destructor
BisectionNRSolver::~BisectionNRSolver() {
}

//! Solution method for all markets for one period
bool BisectionNRSolver::solve( const int period ) {
   
   World* world = scenario->getWorld();
   bool allbracketed = false;
   bool firsttime = true;
   int i = 0; // some index
   double worldCalcCount = 0; // index for solution iteration
   int bn = 0; // counter for bisection routine
   int code = 2; // code that reports success 1 or failure 0
   int solved = 0; // code that reports success 1 or failure 0
   const double solTolerance = 0.001; // tolerance for solution criteria
   const double excessDemandSolutionFloor = 0.01; // minimum value below which solution is assumed to be found.
   // Extra high tolerance to get to solution faster
   //double solTolerance = 0.1; // tolerance for solution criteria
   double maxSolVal; // temporary maximum value of equality condition			 
   bool calibrationStatus = world->getCalibrationSetting();
   
   const double BRACKET_INTERVAL = 0.5;
   
   Configuration* conf = Configuration::getInstance();
   trackED = conf->getBool( "trackMaxED" ); //!< Get parameter to turn on (or not) solution mechanism tracking (to cout)
   
   vector<SolverLibrary::SolutionInfo> sol = SolverLibrary::getMarketsToSolve( marketplace, period );
   const int marketsToSolve =  static_cast<int>( sol.size() );
   
   logfile << ",Starting Solution. Solving for " << marketsToSolve << " markets." << endl;
   
   // if marketsToSolve = 0, no markets to solve, break out of solution.
   if ( marketsToSolve == 0 ) {
      cout << "Model solved with last period's prices"; 
      return true;
   }
   
   SolverLibrary::update( marketplace, sol, period );
   
   // initialize solution information
   for (i = 0; i < marketsToSolve; i++ ) {
      sol[i].XL = sol[i].XR = sol[i].X;
      sol[i].EDL = sol[i].EDR = sol[i].ED; 
      sol[i].bracketed = 0;
   }
   
   // for debugging
   if ( bugMinimal ) {
      bugoutfile << endl << "Solution() Begin. Per " << period << endl;
      bugoutfile <<"Number of Markets: "<< marketsToSolve << endl;
      bugoutfile << endl << "Market,X,XL,XR,ED,EDL,EDR,Tolerance" << endl;
      
      for ( i = 0; i < marketsToSolve; i++ ) {
         bugoutfile << sol[ i ].marketName << sol[ i ].marketGood <<"," << sol[ i ].X << "," << sol[ i ].XL << "," << sol[ i ].XR
            << "," << sol[ i ].ED << "," << sol[ i ].EDL << "," << sol[ i ].EDR << "," << solTolerance << endl;
      }
      bugoutfile << endl;
      
   }
   
   // Loop is done at least once.
   do {
      if ( !allbracketed ) {
         Bracket( solTolerance, excessDemandSolutionFloor, BRACKET_INTERVAL, sol,allbracketed,firsttime,worldCalcCount,period );
      }
      
      // Bisect method
      if (allbracketed ) {
         if (bn < 1) {
            const int maxIter = 30;
            solved = Bisection_all( solTolerance, excessDemandSolutionFloor, maxIter,sol,worldCalcCount,period );
            
            if (!solved) {
               for (i=0;i<marketsToSolve;i++)
                  SolverLibrary::checkBracket(solTolerance, excessDemandSolutionFloor, sol,allbracketed );
            }
            ++bn;
         }
      }
      
      // Ron's version of the NR routine
      if ( allbracketed  ) {
         const int maxIter = 30;
         solved = Bisection_all( solTolerance, excessDemandSolutionFloor, maxIter, sol, worldCalcCount, period );
         
         logfile << ",Number of iterations: worldCalcCount = " << worldCalcCount << endl;
         int worstMarketIndex = 0;
         maxSolVal = SolverLibrary::findMaxExcessDemand( sol, excessDemandSolutionFloor, worstMarketIndex, period );
         
         if( !solved && maxSolVal < 1500 ) {
            
            // turn end-use calibrations off for NR
            world->turnCalibrationsOff();
            solved = NR_Ron( solTolerance, excessDemandSolutionFloor, sol, worldCalcCount, period );
            if ( calibrationStatus ) { // turn end-use calibrations back on if were on originally
               world->turnCalibrationsOn();
            }  
            
            if ( bugMinimal ) { 
               bugoutfile << "After Ron_NR "<< worldCalcCount;
            }
         }
         
         if ( !solved ) {
            SolverLibrary::checkBracket( solTolerance, excessDemandSolutionFloor, sol, allbracketed );
            
         }
      }
      
      // make sure that ED, NOT Log of ED, is checked against tolerance
      int worstMarketIndex = 0;
      maxSolVal =  SolverLibrary::findMaxExcessDemand( sol, excessDemandSolutionFloor, worstMarketIndex, period );
      
      // for debugging
      if ( bugTracking ) {
         bugoutfile << endl << "Solution() loop. N: " << worldCalcCount << endl;
         bugoutfile << endl << "Market,X,XL,XR,ED,EDL,EDR,Tolerance" << endl;
         
         for (i = 0; i < marketsToSolve; ++i ) {
            bugoutfile << sol[ i ].marketName << sol[ i ].marketGood << "," << sol[ i ].X << "," << sol[ i ].XL << "," << sol[ i ].XR 
               << ","<< sol[ i ].ED << "," << sol[ i ].EDL <<","<<sol[ i ].EDR << "," << solTolerance << endl;
         } 
      }
      
   } // end do loop		
   while ( maxSolVal >= solTolerance && ++worldCalcCount < 1000 );			// report success, 0
   code = ( maxSolVal < solTolerance ? 0 : -1 );				// or failure, -1, 
   
   if ( !marketplace->checkMarketSolution( solTolerance, excessDemandSolutionFloor, period ) && ( code == 0 ) ) {
      cerr << "ERROR: Supplies and Demands are NOT equal" << endl;
   }
   
   totIter += worldCalcCount;
   
   switch (code) {
   case 0:
      cout << "Model solved normally: worldCalcCount = " << int( worldCalcCount ) << "; Cumulative = "<< int( totIter ) << endl;
      logfile << ",Model solved normally: worldCalcCount = " << int( worldCalcCount ) << "; Cumulative = "<< int( totIter ) << endl;
      break;
   case -1:
      cout << "Model did not solve within set iteration	" << int( worldCalcCount )<< endl;
      logfile << ",***Model did not solve within set iteration " << int( worldCalcCount ) << endl;
      
      logfile <<",Market,X,XL,XR,ED,EDL,EDR,Tolerance" << endl;
      for ( i = 0; i < marketsToSolve; i++ ) {
         logfile << "," << sol[ i ].marketName << sol[ i ].marketGood << ","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
            <<","<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<solTolerance<< endl;
      }
      break;
   case 2:
      cout << "Original code has not been changed" << endl;
      logfile << ",Original code has not been changed" << endl;
      break;
   default:
      cout << "Case for code not found" << endl;
      logfile << ",Case for code not found" << endl;
      break;
   }
   return true;
}

//! Bracketing function only
/* Function finds bracket interval for each market and puts this information into sol vector
* \author Sonny Kim, Josh Lurz, Steve Smith
* \param solutionTolerance Target value for maximum relative solution for worst market 
* \param excessDemandSolutionFloor *Absolute value* beneath which market is ignored 
* \param bracketInterval Relative multipliciatve interval by which trail values are moved
* \param sol Vector of market solution information 
* \param allbracketed Boolean that holds bracketing state 
* \param firsttime Boolean that marks first time bracket is performed 
* \param worldCalcCount Counter for number of worldcalc model calls 
* \param per Model period
*/
int BisectionNRSolver::Bracket( const double solutionTolerance, const double excessDemandSolutionFloor, 
                               const double bracketInterval, vector<SolverLibrary::SolutionInfo>& sol, bool& allbracketed, 
                               bool& firsttime, double& worldCalcCount, const int per ) {
   
   World* world = scenario->getWorld();
   int i;
   const int numCurrMarkets = static_cast<int>( sol.size() ); // number of markets to solve
   int numIterations = 0; // number of iterations
   int code = 2; // code that reports success 1 or failure 0
   
   cout << "Entering bracketing..." << endl;
   
   // Loop is done at least once.
   do {
      
      SolverLibrary::setPricesToMarkets( marketplace, sol, per ); // set new prices
      marketplace->nulldem( per );	// null demand
      marketplace->nullsup( per ); // null supply
      
      world->calc( per ); // call world object to recalculate supply and demand
      
      SolverLibrary::update( marketplace, sol, per ); // set the new excess demands.
      // bracketed array is either 0 or 1
      
      allbracketed = true;
      
      for ( i = 0; i < numCurrMarkets; i++ ) {
         if ( !sol[ i ].bracketed ) {
            allbracketed = false;
            break;
         }
      }
      
      // Bracketing of prices; done first regardless of choice of solution algorithm.
      if ( !allbracketed ) {
         
         // Iterate through each market.
         for ( i = 0; i < numCurrMarkets; i++ ) {
            
            // If the market is not bracketed.
            if ( !sol[ i ].bracketed ) {
               
               // If ED at X and L are the same sign.
               if ( util::sign( sol[ i ].ED ) == util::sign( sol[ i ].EDL ) ) {
                  
                  // If Supply > Demand at point X. Price needs to decrease.
                  if ( sol[ i ].ED < 0 ) { 
                     
                     // Move Left Bracket to X
                     sol[ i ].XL = sol[ i ].X; 
                     sol[ i ].EDL = sol[ i ].ED;
                     
                     // If L and R do not span solution.
                     if ( util::sign( sol[ i ].EDL ) == util::sign( sol[ i ].EDR ) ) {
                        
                        // Decrease X.
                        
                        // If X is positive.
                        if( sol[ i ].X > util::getSmallNumber() ) {
                           sol[ i ].X *= 1 - bracketInterval; 
                        }
                        
                        // If X is near 0. 
                        else if ( fabs( sol[ i ].X ) < util::getSmallNumber() ) {
                           sol[ i ].X = 0;
                        }
                        
                        // X is negative.
                        else {
                           assert( false );
                        }
                        
                     } // if ( util::sign( sol[ i ].EDL ) == util::sign( sol[ i ].EDR ) )
                     
                     // If L and R span solution.
                     else {
                        
                        // This market is bracketed.
                        sol[ i ].bracketed = true;
                     } 
                  } // if( sol[ i ].ED < 0 )
                  
                  // If Supply <= Demand. Price needs to increase.
                  else { // ED >= 0
                     
                     // Move R Bracket to X
                     sol[ i ].XR = sol[ i ].X; 
                     sol[ i ].EDR = sol[ i ].ED;
                     
                     // If L and R do not span solution.
                     if ( util::sign( sol[ i ].EDL ) == util::sign( sol[ i ].EDR ) ) {
                        
                        // Increase X.
                        
                        // If X is positive.
                        if( sol[ i ].X > util::getSmallNumber() ) {
                           sol[ i ].X *= ( 1 + bracketInterval );
                        }
                        
                        // If X is 0
                        else if ( fabs( sol[ i ].X ) < util::getSmallNumber() ) {
                           sol[ i ].X = 0.05;
                        }
                        
                        // X is negative.
                        else {
                           assert( false );
                        }
                     }
                     
                     // If L and R span solution.
                     else {
                        
                        // This market is bracketed.
                        sol[ i ].bracketed = true;
                     }
                  }
               }
               
               // ED at X and R are the same sign.
               else { 
                  
                  // If Supply > Demand at X. Price needs to decrease.
                  if ( sol[ i ].ED < 0 ) {
                     
                     // Move the left bracket to X.
                     sol[ i ].XL = sol[ i ].X; 
                     sol[ i ].EDL = sol[ i ].ED;
                     
                     // If L and R do not span solution.
                     if ( util::sign( sol[ i ].EDL ) == util::sign( sol[ i ].EDR ) ) {
                        
                        // Decrease X.
                        
                        // If X is positive.
                        if( sol[ i ].X > util::getSmallNumber() ) {
                           sol[ i ].X *= 1 - bracketInterval;
                        }
                        
                        // If X is 0
                        else if ( fabs( sol[ i ].X ) < util::getSmallNumber() ) {
                           sol[ i ].X = 0;
                        }
                        
                        // X is negative.
                        else {
                           assert( false );
                        }
                     }
                     else {
                        
                        // The market is bracketed.
                        sol[ i ].bracketed = true;
                     }
                  }
                  
                  // If Supply <= Demand at X. Prices need to increase.
                  else {
                     // Move the right bracket to X.
                     sol[ i ].XR = sol[ i ].X; 
                     sol[ i ].EDR = sol[ i ].ED; 
                     
                     // If L and R do not span solution.
                     if ( util::sign( sol[ i ].EDL ) == util::sign( sol[ i ].EDR ) ) {
                        
                        // Increase X.
                        
                        // If X is positive.
                        if( sol[ i ].X > util::getSmallNumber() ) {
                           sol[ i ].X *= ( 1 + bracketInterval );
                        }
                        
                        // If X is 0
                        else if ( fabs( sol[ i ].X ) < util::getSmallNumber() ) {
                           sol[ i ].X = 0;
                        }
                        
                        // X is negative.
                        else {
                           assert( false );
                        }
                     }
                     else {
                        
                        // The market is bracketed.
                        sol[ i ].bracketed = true;
                     }
                  }
               }
            } // if( !sol[ i ].bracketed )
            
            // Check if the bracket is empty.
            // if ( fabs( sol[ i ].XL - sol[ i ].XR ) < util::getVerySmallNumber() ) {
            //    sol[ i ].bracketed = false;
            //    sol[ i ].XL = sol[ i ].XR = sol[ i ].X;
            //    sol[ i ].EDL = sol[ i ].EDR = sol[ i ].ED;
            // }
            
            // Check if X was exogenously forced outside the bracket.
            if( !( sol[ i ].X <= sol[ i ].XL ) || !( sol[ i ].X >= sol[ i ].XR ) ) {
               // sol[ i ].XR = sol[ i ].XL = sol[ i ].X;
               // sol[ i ].EDR = sol[ i ].EDL = sol[ i ].ED;
               sol[ i ].bracketed = false;
            }
            
            // Check if the ED is below the solution tolerance.
            if( SolverLibrary::isWithinTolerance( sol[ i ].ED, sol[ i ].demand, solutionTolerance, excessDemandSolutionFloor ) ) {
               sol[ i ].bracketed = true;
            }
            
            // Check if the market is unbracketable. This check is needed for GHG markets. 
            if( ( sol[ i ].X < util::getVerySmallNumber() ) && ( sol[ i ].ED < util::getSmallNumber() ) ) {
               sol[ i ].bracketed = true;
               sol[ i ].XR = sol[ i ].X = 0;
            }
            
         } // for 
         
         allbracketed = true;
         for ( i = 0; i < numCurrMarkets; i++ ) {
            if ( !sol[ i ].bracketed ) {
               allbracketed = false;
               break;
            }
         }
      }
   } while ( ++numIterations < 30 && !allbracketed );	
   code = ( allbracketed ? 1 : 0 );	// Report success, 1 or failure, 0
   
   worldCalcCount += numIterations - 1;
   return code;
}

//! Bisection Solution Mechanism (all markets)
/*! This solution mechanism bisects all markets at once. 
* Bisection is always performed at least a few times. Bisection stops if the maximum 
* relative ED does not change at a rate larger than BREAK_OUT_THRESHOLD.
* If the maximum relative "ED" is larger than BRACKET_THRESHOLD, then the unsolved markets
* are re-bracketed. Then bisection continues. The bracketing interval is smaller than that
* used initially so as to not perturb trial values too much. If a further re-bracket
* is necessary, the bracketing interval is decreased further. 
*
* Also, the price and demand markets are very prone to move outside their brackets.
* A check for that is performed each time and the brackets are adjusted accordingly.
* This check is critical for solution with simultuantey.
*
* Useful TrackED writeout to screen is turned on by toggle in configuration file.
* \author Sonny Kim, Josh Lurz, Steve Smith
* \warning Unless stated otherwise, ED values are normalized (i.e., that 10 == 10% difference).
* \todo need more general way to reset price and demand market types within bisect
* \todo implement check on price and demand markets within bracket?
* \param solutionTolerance Target value for maximum relative solution for worst market 
* \param excessDemandSolutionFloor *Absolute value* beneath which market is ignored
* \param IterLimit Maximum number of iterations the subroutine will perform. 
* \param sol Vector of market solution information 
* \param worldCalcCount Counter for number of worldcalc model calls 
* \param per Model period
*/
int BisectionNRSolver::Bisection_all( const double solutionTolerance, const double excessDemandSolutionFloor, const int IterLimit, vector<SolverLibrary::SolutionInfo>& sol, double& worldCalcCount, const int per ) {
   
   World* world = scenario->getWorld();
   int i;
   int numIterations = 0; // number of iterations
   int interLimitAdd = 0; // Variable to allow more iterations if re-bisect
   int numbRebrackets = 0; // Variable to allow more iterations if re-bisect
   int code = 2; // code that reports success 1 or failure 0
   const int numCurrMarkets = static_cast<int>( sol.size() ); // number of markets to solve
   double maxSolVal; // maximum equality value
   bool breakout;	// var to allow various conditions to exit bisection routine
   const int MAX_REBRACKETS = 3;
   const double BREAK_OUT_THRESHOLD = 0.001; // leave bracketing if not improving by at least this much
   const double BRACKET_THRESHOLD = 10;	// if breakout & relative maxED is > this then try re-bracketing
   double bracketInterval = 0.05; // starting value for re-bracketing interval
   double previousEDvalue = -1;
   double previousEDint = 0;
   double maxEDvalue = 0;
   bool reBracketingDone = false;
   
   if ( trackED ) { 
      cout << endl << "Bisection begin" << endl; 
   }
   logfile << ",,Bisection_all function called." << endl;
   // solve all markets
   do {
      breakout = false; //default is not to breakout of bisection routine
      maxEDvalue = 0;
      if (bugMinimal) {
         bugoutfile << " Bisect " << numIterations;
      }
      
      for (i=0; i<numCurrMarkets; ++i) {
         if ( !SolverLibrary::isWithinTolerance( sol[ i ].ED, sol[ i ].demand, solutionTolerance, excessDemandSolutionFloor )) { // if haven't solved
            // Move the left bracket in if Supply > Demand
            if (sol[i].ED < 0) {
               sol[i].XL = sol[i].X;
               sol[i].EDL = sol[i].ED;
            }
            // Move the right bracket in if Demand >= Supply
            else {
               sol[i].XR = sol[i].X;
               sol[i].EDR = sol[i].ED;
            }
            // Set new trial value to center
            sol[i].X = (sol[i].XL + sol[i].XR)/2;
            // Set difference in bracketed price
            sol[i].dX = sol[i].XR - sol[i].XL;
         }	
         // price=0 and supply>demand
         // only true for constraint case
         // other markets cannot have supply>demand as price->0
         if (fabs(sol[i].X)< util::getSmallNumber() && sol[i].ED<0) { 
            sol[i].X = 0; 
            sol[i].dX = 0;
         } 
      }
      
      SolverLibrary::setPricesToMarkets( marketplace, sol, per );
      marketplace->nulldem(per);	// null demand
      marketplace->nullsup(per); // null supply
      
      world->calc(per); // call world object to recalculate supply and demand
      SolverLibrary::update( marketplace, sol, per );
      
      
      
      // The  price and demand markets are very prone to moving beyond their brackets. 
      // So check and adjust if needed. Lines below check if XL < Demand, or XR > Demand, 
      // and move brackets if necessary. A more general  bracket check is below, but this
      // is needed more often and can be done simply.
      SolverLibrary::adjustPriceAndDemandMarkets( marketplace, sol, per );
      
      int maxIntSol = 0;
      maxSolVal = SolverLibrary::findMaxExcessDemand( sol, excessDemandSolutionFloor, maxIntSol, per );
      
      if ( trackED ) {
         cout << "Bisection-maxRelED: "<< maxSolVal <<" ("<< sol[ maxIntSol ].marketName + sol[ maxIntSol ].marketGood << ") -> ";
         cout << "S: "<< sol[ maxIntSol ].supply << " , " << " D: "<< sol[ maxIntSol ].demand;
         cout << ",  P: "<< sol[ maxIntSol ].X;
         cout << endl; 
      }
      
      // If the worst ED is not changing too much then breakout of bisection
      if (numIterations > 5 ) {	// always bisect a few times         
         if ( fabs(maxSolVal-previousEDvalue)/previousEDvalue < BREAK_OUT_THRESHOLD && maxIntSol == previousEDint )  {  
            breakout = true; 
         }
      }
      
      previousEDvalue = maxSolVal;
      previousEDint = maxIntSol;
      
      // If have not solved, then try bracketing again.
      // This helps when some market has fell out of its bracketing range
      // This helps if the maxED is still somewhat large (otherwise, NR should be fine)
      // This needs to be below maxED printout so that inconsistent TrackED printout does not occur
      if ( breakout && !reBracketingDone && ( fabs( maxSolVal ) > BRACKET_THRESHOLD ) ) {
         ++numbRebrackets;
         
         if ( numbRebrackets > MAX_REBRACKETS ) {
            reBracketingDone = true; // only try this once
         }
         
         // reset bracket flag for any markets not solved
         for(int i=0;i< static_cast<int>( sol.size() );i++) {
            if (fabs(sol[i].ED) > solutionTolerance) {
               sol[i].bracketed = false;
               sol[i].XL = sol[i].XR = sol[i].X; 
               sol[i].EDL = sol[i].EDR = sol[i].ED; 
            }
            else {
               sol[i].bracketed = true;
            }
         }
         logfile << ",";
         bool tempAllBracketed = false;
         bool firstTime = false;
         Bracket( solutionTolerance, excessDemandSolutionFloor, bracketInterval, 
            sol,tempAllBracketed,firstTime,worldCalcCount,per);
         
         // Reduce bracket interval for next re-bracket so do not perturb prices as much
         bracketInterval = bracketInterval/2; 
         breakout = false;
         interLimitAdd = numIterations;
         logfile << ",,,Bisection_all continuing after re-bracketing." << endl;
         if (trackED) {
            cout << "Bisection continuing after re-bracketing" << endl; 
         }
      } // end re-bracket block                       
      
   } // end do loop		
   while (++numIterations < (IterLimit+interLimitAdd) && maxSolVal >= solutionTolerance && !breakout);
   worldCalcCount+=numIterations-1;
   code = (maxSolVal < solutionTolerance ? 1 : 0); // report success, 1 or failure, 0
   
   if ( numIterations >= (IterLimit+interLimitAdd) ) {
      logfile << ",,,***Exited Bisection_all. Max number of Iterations exceeded." << endl;
   }
   if (trackED) { cout << endl; }
   
   return code;
}

//! Ron's version of the Newton Raphson Solution Mechanism (all markets)
/*! Derivatives are taken once. They are not taken again unless:
a) The Max Relative ED after calculation is greater than MAXED_FOR_DERIV_RECALC
b) or 10 NR iterations have occurred.
As long as Bisection is close, one set of derivatives per period is sufficient.
* Useful TrackED writeout to screen is turned on by toggle in configuration file.
* \author Sonny Kim, Josh Lurz, Steve Smith
* \warning Unless stated otherwise, ED values are normalized (i.e., that 10 == 10% difference).
* \param solutionTolerance Target value for maximum relative solution for worst market 
* \param excessDemandSolutionFloor *Absolute value* beneath which market is ignored 
* \param sol Vector of market solution information 
* \param worldCalcCount Counter for number of worldcalc model calls 
* \param per Model period
*/
int BisectionNRSolver::NR_Ron( const double solutionTolerance, const double excessDemandSolutionFloor, vector<SolverLibrary::SolutionInfo>& sol, double& worldCalcCount, const int per ) {
   
   World* world = scenario->getWorld();
   const Modeltime* modeltime = scenario->getModeltime();
   int i;
   int numDerivativeCalcs = 0; // count number of times derivatives are calculated
   int iter = 0; // number of iterations through solution algorithm
   int code = 2; // code that reports success 1 or failure 0
   const int MAX_DERIVATIVE_CALC = 1; // count number of times derivatives are normally calculated
   const double MAXED_FOR_DERIV_RECALC = 25; // recalculate derivatives is ED is larger than this 
   double maxSolVal; // maximum equality value 
   
   // Initialize solution vector.
   sol = SolverLibrary::getMarketsToSolve( marketplace, per, true );
   SolverLibrary::update( marketplace, sol, per );
   
   const int marketsToSolve =  static_cast<int>( sol.size() );
   
   vector<double> NP( marketsToSolve ); // adjustment value
   vector<double> KD( marketsToSolve ); // k values demand
   vector<double> KS( marketsToSolve ); // k values supply
   vector<double> KDS( marketsToSolve ); // k values demand - supply
   
   bool breakout = false;	// var to allow various conditions to exit NR routine
   double beforeEDvalue = -1;
   double previousEDvalue = -1;
   double BREAK_OUT_THRESHOLD = 0.001;
   
   if ( trackED ) { 
      cout << "NR_Ron begin "; 
   }
   
   Matrix JF( marketsToSolve, marketsToSolve );
   Matrix JFDM( marketsToSolve, marketsToSolve );
   Matrix JFSM( marketsToSolve, marketsToSolve );
   
   logfile << ",,Ron's version of the Newton-Raphson function called." << endl;
   
   // solve all markets
   do {
      
      // control no of times derivatives are calculated
      if ( ( numDerivativeCalcs < MAX_DERIVATIVE_CALC ) && ( per < modeltime->getmaxper() ) ) { 
         if ( trackED ) {
            cout <<" ... "; 
         }
         
         // Recalculate Jacobian matrix, returns JF matrix
         SolverLibrary::derivatives( marketplace, world, sol, JFDM, JFSM, worldCalcCount, per ); 
         numDerivativeCalcs++; // increment count of derivative calculation
         
         for( i = 0; i < marketsToSolve; i++ ) {
            for( int j = 0; j < marketsToSolve; j++ ) {
               JF[ i ][ j ] = JFSM[ i ][ j ] - JFDM[ i ][ j ];
               assert( util::isValidNumber( JF[ i ][ j ] ) );
            }
         }
         
         SolverLibrary::invertMatrix( JF );
         logfile << ",,,Derivatives calculated" << endl;
         
         if ( trackED ) {
            cout <<" End Derivatives " <<endl;
         }
      }
      
      // initialize KD and KS as logs of original demand and supply
      for ( i = 0; i < marketsToSolve; i++ ) {
         KD[ i ] =  log( sol[ i ].demand );
         KS[ i ] =  log( sol[ i ].supply );
      }
      
      for ( i = 0; i < marketsToSolve; i++ ) {
         for ( int j = 0; j < marketsToSolve; j++ ) {
            double tempValue = log( max( sol[ j ].X, util::getSmallNumber() ) );
            KD[ i ] -= tempValue * JFDM[ i ][ j ];
            KS[ i ] -= tempValue * JFSM[ i ][ j ];
            assert( util::isValidNumber( KD[ i ] ) );
            assert( util::isValidNumber( KS[ i ] ) );
         }
         
         KDS[ i ] = KD[ i ] - KS[ i ];
         assert( util::isValidNumber( KDS[ i ] ) );
      }
      
      // Calculate new log price based on NR
      for ( i = 0; i < marketsToSolve; i++ ) {
         
         NP[ i ] = 0;
         
         for ( int j = 0; j < marketsToSolve; j++ ) {
            assert( util::isValidNumber( JF[ i ][ j ] ) );
            NP[ i ] += JF[ i ][ j ] * KDS[ j ];
            assert( util::isValidNumber( NP[ i ] ) );
         }
         
         sol[ i ].X = exp( NP[ i ] ); // new price
         
         // Check the validity of the price.
         assert( util::isValidNumber( sol[ i ].X ) );
      }
      
      SolverLibrary::setPricesToMarkets( marketplace, sol, per );
      marketplace->nulldem( per ); // null demand
      marketplace->nullsup( per ); // null supply
      
      world->calc( per ); // call world object to recalculate supply and demand
      
      SolverLibrary::update( marketplace, sol, per );
      
      int maxIntSol = 0;
      maxSolVal = SolverLibrary::findMaxExcessDemand( sol, excessDemandSolutionFloor, maxIntSol, per );
      
      
      if ( trackED ) {
         cout << "NR-maxRelED: "<< maxSolVal <<" ("<< sol[ maxIntSol ].marketName + sol[ maxIntSol ].marketGood << ") -> ";
         cout << "S: "<< sol[ maxIntSol ].supply << " , " << " D: "<< sol[ maxIntSol ].demand;
         cout << ",  P: "<< sol[ maxIntSol ].X;
         cout << endl; 
      }
      
      beforeEDvalue = previousEDvalue;
      previousEDvalue = maxSolVal;
      
      // if solution moves in wrong direction
      if( maxSolVal > 25) {
         logfile << ",,***Exit Newton-Raphson function maxSolVal > 25. "<< endl;
         //   logfile << ", Due to market " << getRegionName(maxInt)<< "-"<< getGoodName(maxInt) <<"\n";
         if ( trackED && per > 0) {
            cout << "Exit Newton-Raphson function maxSolVal > 25" << endl;
         }

         for ( i = 0; i < static_cast<int>( sol.size() ); i++ ) {
            if ( fabs( sol[ i ].ED ) > maxSolVal / 100 ) {
               if ( trackED ) {
                  cout << "ED: (" << sol[ i ].marketName + sol[ i ].marketGood  << ") - " << sol[ i ].ED << endl;
               }
            logfile << ",,,Due to market " << sol[ i ].getName()<< " - ED: " << sol[ i ].ED << endl;
            }
         }
         return 0; 
      }
      
      // If have iterated 10 times in NR without solving, then re-do derivatives
      if ( (iter > 3) && ((iter % 10) == 0) ) {
         numDerivativeCalcs = 0;
      }
      
      // IF ED is too high then re-calculate derivatives
      if ( maxSolVal > MAXED_FOR_DERIV_RECALC ) {
         numDerivativeCalcs = 0;
      }
      
   } // end do loop	
   
   while ( ++iter < 35 && maxSolVal >= solutionTolerance && !breakout );	
   code = ( maxSolVal < solutionTolerance ? 1 : 0 ); // report success 1 or failure 0, 
   
   worldCalcCount += iter - 1;
   logfile << ",Number of Newton-Raphson iterations: n =" << iter << endl;
   
   return code;
}
