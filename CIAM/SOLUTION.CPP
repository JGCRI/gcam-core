/* solution.cpp												*
 * Solution Mechanisms:										*
 * Bracketing Routine										*
 * Bisection Solution Mechanism								*
 * False Position Solution Mechanism						*
 * Secant Solution Mechanism								*
 * Neuton Rapson Solution Mechanism							*
 * Requires only array of market information				*
 * Only prices are adjusted for each market by the 			*
 * solution algorithm.							 			*
 * Coded by Sonny Kim 5/11/00								*/

// standard library
#include <math.h>
#include <iostream>
#include <fstream>
#include <time.h> // to use clock and time functions
// user defined headers
#include "world.h"
#include "market.h" //contains market no.,supply,demand,price,and period.
//#include "NRsol.h" // Newton-Raphson Function Protocol
//#include "MatrixC.h"
#include "modeltime.h"
// preprocessor function to determine sign of number
#define SIGN(num) ((num<0)?(-1):1)
// global variables
extern World world;
extern Marketplace marketplace;
extern ofstream bugoutfile, logfile, sdfile;	
extern clock_t start, intermediate, finish, afterdata;
extern bool timestamp;
extern Modeltime modeltime;

const double smnum = 1e-6; // constant small number to replace for null
const double verysmnum = 1e-8; // constant small number to replace for null

// object to hold unknowns and equality conditions
// for the solution mechanism
struct solinfo
{
	double X;		// unknown, prices
	double ED;		// excess demand for X
	double dX;		// change in excess demand
	double XL;		// left bracket
	double XR;		// right bracket
	double EDL;		// excess demand for left bracket
	double EDR;		// excess demand for right bracket
	double XL_org;	// original left bracket
	double XR_org;	// original right bracket
	double EDL_org;	// original excess demand for left bracket
	double EDR_org;	// original excess demand for right bracket
	int bracketed;	// 1 or 0 for bracketed or unbrackted
};

// Function to calculate maximum value of a vector
double Max(const vector<double>& v) 
{
	int low = 0;
	if (low>v.size()) return 0;
	double largest = fabs(v[0]);
	for (int i=0;i<v.size();i++) {
		if (fabs(v[i]) > largest) largest = fabs(v[i]);
	}
	return largest;
}

// Bracketing function only, does not find solution
int Bracket(double Tol, vector<solinfo>& sol, bool& allbracketed,
			 bool& firsttime,int& n,int per)
{
	int i, j;
	int nmrks = sol.size(); // number of markets to solve
	int nn = 0; // number of iterations
	int Code = 2; // Code that reports success 1 or failure 0
	vector<double> Xtemp(nmrks); // temporary prices
	vector<double> EDtemp(nmrks); // temporary excess demand

	logfile << "Bracketing function called.\n";

	// Loop is done at least once.
	do {
		// bracketed array is either 0 or 1
		j=0; // counter
		for (i=0;i<nmrks;i++)
			if (sol[i].bracketed) j++;
		if (j == nmrks) {
			allbracketed = true;
			// store original brackeded information
			if(firsttime) {
				for (i=0;i<nmrks;i++) {
					sol[i].XL_org = sol[i].XL; 
					sol[i].EDL_org = sol[i].EDL;
					sol[i].XR_org = sol[i].XR; 
					sol[i].EDR_org = sol[i].EDR;
				}
				firsttime = false;
			}
		}
		else
			allbracketed = false;

		// Bracketing of prices; done first regardless of choice of solution
		// algorithm.
		if (!allbracketed)
			for (i=0; i<nmrks; ++i) {
				if (!sol[i].bracketed) {
					if (SIGN(sol[i].ED) == SIGN(sol[i].EDL)) { //first will always be true
						if (sol[i].ED < 0) {
							sol[i].XL = sol[i].X; 
							sol[i].EDL = sol[i].ED;
							if (SIGN(sol[i].EDL) == SIGN(sol[i].EDR)) sol[i].X *= 0.5; 
							else sol[i].bracketed=true;
						}
						else {
							if (sol[i].X == 0) {
								sol[i].X = 10.0; // starting value
								int stop=1;
							}
							else {
								sol[i].XR = sol[i].X; 
								sol[i].EDR = sol[i].ED;
								if (SIGN(sol[i].EDL) == SIGN(sol[i].EDR)) sol[i].X *= 1.5;
								else sol[i].bracketed=true;
							}
						}
					}
					else { // branch for bracket
						if (sol[i].ED < 0) {
							sol[i].XL = sol[i].X; 
							sol[i].EDL = sol[i].ED;
							if (sol[i].XL<=sol[i].XR) { //XL is always greater than XR
								sol[i].X = sol[i].XL*0.9;
								sol[i].bracketed=false;
							}
							else
								sol[i].bracketed=true;
						}
						else {
							if (sol[i].X == 0) {
								sol[i].X = 10.0; // starting value
								int stop=1;
							}
							else {
								sol[i].XR = sol[i].X; 
								sol[i].EDR = sol[i].ED; 
								if (sol[i].XL<=sol[i].XR) { //XL is always greater than XR
									sol[i].X = sol[i].XR*1.1;
									sol[i].bracketed=false;}
								else
									sol[i].bracketed=true;
							}
						}
					}
					if (sol[i].X < 0) sol[i].X = smnum; // cannot have negative prices
				}
				if (sol[i].XL == sol[i].XR) sol[i].bracketed=false;

				// Check if market has fixed constraint and 
				// if demand is always below contraint
				if (sol[i].X < smnum && sol[i].ED < 0) {
					sol[i].X = sol[i].XL = sol[i].XR = 0;
					sol[i].bracketed = true;
				}

				// case when supply is zero and cannot bracket (ie. resource runs out)
				if(!sol[i].bracketed && (fabs(sol[i].ED)<Tol)) sol[i].bracketed=true;
			}
		// bracketed array is either 0 or 1
		j=0; // counter
		for (i=0;i<nmrks;i++) {
			if (sol[i].bracketed) j++;
		}
		if (j == nmrks) {
			allbracketed = true;
			// store original brackeded information
			if(firsttime) {
				for (i=0;i<nmrks;i++) {
					sol[i].XL_org = sol[i].XL; 
					sol[i].EDL_org = sol[i].EDL;
					sol[i].XR_org = sol[i].XR; 
					sol[i].EDR_org = sol[i].EDR;
				}
				firsttime = false;
			}
		}
		else
			allbracketed = false;

		for (i=0;i<nmrks;i++)
			Xtemp[i]=sol[i].X;
		
		marketplace.setPRC(Xtemp,per); // set new prices
		marketplace.nulldem(per);	// null demand
		marketplace.nullsup(per); // null supply
		
		world.calc(per); // call world object to recalculate supply and demand

		marketplace.logED(per); // calculate log of excess demand
		marketplace.excessdemand(per); // calculate excess demand
		// using log of ED does not matter for bracketing
		// since only sign matters
		//logED = marketplace.showlogED(per); // show log of excess demand
		EDtemp = marketplace.showED(per); // show excess demand
		for (i=0;i<nmrks;i++)
			sol[i].ED = EDtemp[i];
		

		// for debugging
		int bug = 0; // debugging on(1) or off(0)
		if (bug) {
			bugoutfile<<"\nMarket,X,XL,XR,ED,EDL,EDR,Tolerance\n";
			for (i=0; i<nmrks; ++i) {
				bugoutfile<<i<<","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
				<<","<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<Tol<<"\n";
			}
			marketplace.bugout(per,nn);
			marketplace.sdcurves(per,nn);
		}

	} // end do loop		
	while (++nn < 30 && !allbracketed);			// report sucess, 1
	Code = (allbracketed ? 1 : 0);				// or failure, 0, 
	n+=nn-1;
	return Code;
}

// function to check bracketing
// if not solving and bracketed prices are converging,
void CheckBracket(double Tol,vector<solinfo>& sol,bool& allbracketed)
{
	logfile << "Check brackets function called.\n";
	int nmrks = sol.size(); // number of markets to solve
	int reset = 0;
	// try rebracketing by setting bracketed array to false
	for(int i=0;i<nmrks;i++) {
		//if (fabs(sol[i].XL-sol[i].XR)<verysmnum && fabs(sol[i].ED>Tol)) {
		if (fabs(sol[i].dX) < smnum) {
			allbracketed = false;
			sol[i].bracketed = false;
			sol[i].XL = sol[i].XR = sol[i].X; 
			sol[i].EDL = sol[i].EDR = sol[i].ED; 
		}
	}
}

// Bisection Solution Mechanism (all markets)
int Bisection_all(double Tol,const int N,vector<solinfo>& sol,int& n,int per)
{
	int i;
	int nn = 0; // number of iterations
	int Code = 2; // Code that reports success 1 or failure 0
	int nmrks = sol.size(); // number of markets to solve
	double M; // maximum equality value
	vector<double> Xtemp(nmrks); // temporary prices
	vector<double> EDtemp(nmrks); // temporary excess demand

	logfile << "Bisection_all function called.\n";
	// solve all markets
	do {
		for (i=0; i<nmrks; ++i) {
			if (fabs(sol[i].ED) > Tol) {
				if (sol[i].ED < 0) {
					sol[i].XL = sol[i].X;
					sol[i].EDL = sol[i].ED; 
				}
				else {
					sol[i].XR = sol[i].X;
					sol[i].EDR = sol[i].ED; 
				}
				sol[i].X = (sol[i].XL + sol[i].XR)/2;
				sol[i].dX = sol[i].XR - sol[i].XL;
			}	
			// price=0 and supply>demand
			// only true for constraint case
			// other markets cannot have supply>demand as price->0
			if (fabs(sol[i].X)<smnum && sol[i].ED<0) { 
					sol[i].X = 0; 
					sol[i].dX = 0;
			} 
			Xtemp[i]=sol[i].X; // copy prices to temporary vector
		}
		marketplace.setPRC(Xtemp,per); // set new prices
		marketplace.nulldem(per);	// null demand
		marketplace.nullsup(per); // null supply
		
		world.calc(per); // call world object to recalculate supply and demand

		marketplace.logED(per); // calculate log of excess demand
		marketplace.excessdemand(per); // calculate excess demand
		EDtemp = marketplace.showED(per); // show excess demand
		for (i=0;i<nmrks;i++)
			sol[i].ED = EDtemp[i];

		M = marketplace.maxED(per);

		// for debugging
		int bug = 0; // debugging on(1) or off(0)
		if (bug) {
			bugoutfile<<"\nMarket,X,XL,XR,ED,EDL,EDR,Tolerance\n";
			for (i=0; i<nmrks; ++i) {
				bugoutfile<<i<<","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
				<<","<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<Tol<<"\n";
			}
			marketplace.bugout(per,nn);
			marketplace.sdcurves(per,nn);
		}

	} // end do loop		
	while (++nn < N && M >= Tol);			// report sucess, 1
	Code = (M < Tol ? 1 : 0);				// or failure, 0, 
	n+=nn-1;
	return Code;
}

// Bisection Solution Mechanism (single market)
int Bisection_i(const int i,double Tol,vector<solinfo>& sol,int& n,int per)
{
	int nn = 0; // number of iterations
	int Code = 2; // Code that reports success 1 or failure 0
	double M; // maximum equality value
	vector<double> Xtemp=marketplace.showPRC(per); // temporary prices
	vector<double> EDtemp=marketplace.showED(per); // temporary excess demand

	logfile << "Bisection_i function called.\n";
    // solve only one market
	do {
		if (fabs(sol[i].ED) > Tol) {
			if (sol[i].ED < 0) {
				sol[i].XL = sol[i].X;
				sol[i].EDL = sol[i].ED; 
			}
			else {
				sol[i].XR = sol[i].X;
				sol[i].EDR = sol[i].ED; 
			}
			sol[i].X = (sol[i].XL + sol[i].XR)/2;
			sol[i].dX = sol[i].XR - sol[i].XL;
		}			
		Xtemp[i]=sol[i].X; // copy prices to temporary vector
		// only current market should have different prices
		marketplace.setPRC(Xtemp,per); // set new prices
		marketplace.nulldem(per);	// null demand
		marketplace.nullsup(per); // null supply
		
		world.calc(per); // call world object to recalculate supply and demand

		marketplace.logED(per); // calculate log of excess demand
		marketplace.excessdemand(per); // calculate excess demand
		EDtemp = marketplace.showED(per); // show excess demand
		for (int j=0;j<EDtemp.size();j++)
			sol[j].ED = EDtemp[j];

		M = fabs(sol[i].ED); // only solving one market

		// for debugging
		int bug = 0; // debugging on(1) or off(0)
		if (bug) {
			bugoutfile<<"\nMarket,X,XL,XR,ED,EDL,EDR,Tolerance\n";
			bugoutfile<<i<<","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
				<<","<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<Tol<<"\n";
			marketplace.bugout(per,nn);
			marketplace.sdcurves(per,nn);
		}

	} // end do loop		
	while (++nn < 30 && M >= Tol);			// report sucess, 1
	Code = (M < Tol ? 1 : 0);				// or failure, 0, 
	n+=nn-1;
	return Code;
}

// False Position Solution Mechanism (all markets)
int FalsePos_all(double Tol,vector<solinfo>& sol,int& n,int per)
{
	int i;
	int nn = 0; // number of iterations
	int Code = 2; // Code that reports success 1 or failure 0
	double M; // maximum equality value
	int nmrks = sol.size(); // number of markets to solve
	vector<double> Xtemp(nmrks); // temporary prices
	vector<double> EDtemp(nmrks); // temporary excess demand

	logfile << "FalsePos_all function called.\n";
	// solve all markets
	do {
		for (i=0; i<nmrks; ++i) {
			if (fabs(sol[i].ED) > Tol) {
				if (sol[i].ED < 0) {
					sol[i].XL = sol[i].X;
					sol[i].EDL = sol[i].ED; 
				}
				else {
					sol[i].XR = sol[i].X;
					sol[i].EDR = sol[i].ED; 
				}
				sol[i].dX = sol[i].XR - sol[i].XL;
				sol[i].X = sol[i].XL + (sol[i].dX*sol[i].EDL)/(sol[i].EDL-sol[i].EDR);
			}		
			Xtemp[i] = sol[i].X;
		}
		marketplace.setPRC(Xtemp,per); // set new prices
		marketplace.nulldem(per);	// null demand
		marketplace.nullsup(per); // null supply
		
		world.calc(per); // call world object to recalculate supply and demand

		marketplace.logED(per); // calculate log of excess demand
		marketplace.excessdemand(per); // calculate excess demand
		EDtemp = marketplace.showED(per); // show excess demand
		for (i=0;i<EDtemp.size();i++)
			sol[i].ED = EDtemp[i];

		M = Max(EDtemp); // Max returns largest sol[i].ED

		// for debugging
		int bug = 0; // debugging on(1) or off(0)
		if (bug) {
			bugoutfile<<"\nMarket,X,XL,XR,ED,EDL,EDR,Tolerance\n";
			for (i=0; i<nmrks; ++i) {
				bugoutfile<<i<<","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
				<<","<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<Tol<<"\n";
			}
			marketplace.bugout(per,nn);
			marketplace.sdcurves(per,nn);
		}

	} // end do loop		
	while (++nn < 30 && M >= Tol);			// report sucess, 1
	Code = (M < Tol ? 1 : 0);				// or failure, 0, 
	n+=nn-1;
	return Code;
}

// Secant Solution Mechanism (all markets)
int Secant_all(double Tol,vector<solinfo>& sol,int& n,int per)
{
	int i;
	int iSec=0; 
	int nn = 0; // number of iterations
	int Code = 2; // Code that reports success 1 or failure 0
	double M; // maximum equality value
	int nmrks = sol.size(); // number of markets to solve
	vector<double> Xtemp(nmrks); // temporary prices
	vector<double> EDtemp(nmrks); // temporary excess demand

	logfile << "Secant_all function called.\n";
	// solve all markets
	do {
		if (iSec == 0) { // initial starting point; done once
			for (i=0; i<nmrks; ++i) {
				if (fabs(sol[i].ED) < fabs(sol[i].EDR)) {
					sol[i].X = sol[i].XL;
					sol[i].XL = sol[i].XR;
					double swap = sol[i].EDL;
					sol[i].EDL = sol[i].EDR;
					sol[i].ED = swap; 
				}
				else 
					sol[i].X = sol[i].XR;
			}
		}
		for (i=0; i<nmrks; i++) {
			if (fabs(sol[i].ED) > Tol) {
				sol[i].dX = (sol[i].XL - sol[i].X)*sol[i].ED/(sol[i].ED-sol[i].EDL);
				sol[i].XL = sol[i].X;
				sol[i].EDL = sol[i].ED;
				if (fabs(sol[i].dX)<fabs(sol[i].X))
					sol[i].X += sol[i].dX;
			}
			Xtemp[i] = sol[i].X;
		}
		iSec++;

		marketplace.setPRC(Xtemp,per); // set new prices
		marketplace.nulldem(per);	// null demand
		marketplace.nullsup(per); // null supply
		
		world.calc(per); // call world object to recalculate supply and demand

		marketplace.logED(per); // calculate log of excess demand
		marketplace.excessdemand(per); // calculate excess demand
		EDtemp = marketplace.showED(per); // show excess demand
		for (i=0;i<EDtemp.size();i++)
			sol[i].ED = EDtemp[i];

		M = Max(EDtemp); // Max returns largest sol[i].ED

		// for debugging
		int bug = 0; // debugging on(1) or off(0)
		if (bug) {
			bugoutfile<<"\nMarket,X,XL,XR,ED,EDL,EDR,Tolerance\n";
			for (i=0; i<nmrks; ++i) {
				bugoutfile<<i<<","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
				<<","<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<Tol<<"\n";
			}
			marketplace.bugout(per,nn);
			marketplace.sdcurves(per,nn);
		}

	} // end do loop		
	while (++nn < 30 && M >= Tol);			// report sucess, 1
	Code = (M < Tol ? 1 : 0);				// or failure, 0, 
	n+=nn-1;
	return Code;
}

// Function to calculate derivative
//void JFunction(valarray<double> prices, Matrix& JFDM, int per)
void JFunction(vector<double> prices,double** JFDM,int& n,int per)
{
	int m = prices.size();
	double deltap = 1e-4; // What is the proper value for delta?
	vector<double> tprices = prices; // define and initialize prices for storage
	vector<double> tmpJFD(m);

	marketplace.storeinfo(per); // store original market info before perturbing price
	for (int j=0;j<m;++j) {	// j is column index
		prices[j] *= (1 + deltap);	  // add price times deltap
		//prices[j] += deltap;	  // add price times deltap
		marketplace.setPRC_NR(prices,per);	// set new price for one market
		marketplace.nulldem(per);	// null demand
		marketplace.nullsup(per);	// null supply
		world.calc(per); // call world object to recalculate supply and demand
		int col = j; // function calculates rows for each column
		tmpJFD = marketplace.jacobian(col,per); // calculate elasticities or Jacobian
		for (int i=0;i<m;++i) // copy column vector to Jacobian Matrix
			JFDM[i][j] = tmpJFD[i]; // i is row index
		marketplace.restoreprc_NR(per); // restore perturbed market price
		prices[j] = tprices[j]; //  restore perturbed market price
		n++;
	}
}

// Function to calculate derivative for Ron's version
void Derivatives(vector<double> prices,double** JFDM,double** JFSM,int& n,int per)
{
	int m = prices.size();
	double deltap = 1e-4; // What is the proper value for delta?
	vector<double> tprices = prices; // define and initialize prices for storage
	vector<double> tmpJFD(m),tmpJFS(m);

	marketplace.storeinfo(per); // store original market info before perturbing price
	for (int j=0;j<m;++j) {	// j is column index
		prices[j] *= (1 + deltap);	  // add price times deltap
		marketplace.setPRC_NR(prices,per);	// set new price for one market
		marketplace.nulldem(per);	// null demand
		marketplace.nullsup(per);	// null supply
		world.calc(per); // call world object to recalculate supply and demand
		int col = j; // function calculates rows for each column
		tmpJFD = marketplace.dem_elas_NR(col,per); // calculate demand elasticities
		tmpJFS = marketplace.sup_elas_NR(col,per); // calculate supply elasticities
		for (int i=0;i<m;++i) {// copy column vector to Jacobian Matrix
			JFDM[i][j] = tmpJFD[i]; // i is row index
			JFSM[i][j] = tmpJFS[i]; // i is row index
		}
		marketplace.restoreprc_NR(per); // restore perturbed market price
		prices[j] = tprices[j]; //  restore perturbed market price
		n++;
	}
}

// Newton Raphson Solution Mechanism (all markets)
int NewtRap(double Tol,vector<solinfo>& sol,double** JF,double** bb,int& n,int per)
{
	int i;
	int NRn=0; // calls to calculate elasticities
	int nn = 0; // number of iterations
	int Code = 2; // Code that reports success 1 or failure 0
	double M; // maximum equality value
	int m = marketplace.setmrks_sol_NR(per,Tol); // number of markets to solve
	vector<double> DP(m); // adjustment value
	vector<double> logED = marketplace.showlogED_NR(per); // show log of excess demand
	vector<double> Xtemp=marketplace.showPRC_NR(per); // temporary prices
	vector<double> EDtemp(m); // temporary excess demand

	// function protocol
	//void gaussj(Matrix& a,int n,Matrix& b,int m); // function protocol
	void gaussj(double** a,int n,double** b,int m);

	logfile << "Newton-Raphson function called.\n";
	// solve all markets
	do {
		if ((NRn < 5) && (per < modeltime.getmaxper())) { // do only once for each period
			JFunction(Xtemp,JF,n,per); //recalculate Jacobian matrix, returns JF matrix
			gaussj(JF,m,bb,1); // inverse of JF matrix by reference
		}
		NRn++;
		for (i=0; i<m; ++i) {
			// Calculate new price based on NR
			DP[i] = 0;
			for (int j=0; j<m; ++j)
				DP[i] -= JF[i][j]*logED[j];
			if (DP[i] < -1) DP[i] = -0.9;
			Xtemp[i]*=(1+DP[i]); // new price
		}

		marketplace.setPRC_NR(Xtemp,per); // set new prices
		marketplace.nulldem(per);	// null demand
		marketplace.nullsup(per); // null supply
		
		world.calc(per); // call world object to recalculate supply and demand

		marketplace.logED(per); // calculate log of excess demand
		marketplace.excessdemand(per); // calculate excess demand
		logED = marketplace.showlogED_NR(per); // show log of excess demand
		EDtemp = marketplace.showED_NR(per); // show excess demand

		M = marketplace.maxED(per); // Max returns largest ED[i]

		// for debugging
		int bug = 0; // debugging on(1) or off(0)
		if (bug) {
			bugoutfile<<"\nMarket,X,DP,ED,Tolerance\n";
			for (i=0; i<m; ++i) {
				bugoutfile<<i<<","<<Xtemp[i]<<","<<DP[i]<<","<<EDtemp[i]<<","<<Tol<<"\n";
			}
			marketplace.bugout(per,nn);
			marketplace.sdcurves(per,nn); 
		}
		// if solution moves in wrong direction
		if(M>1500) {
			logfile << "Exit Newton-Raphson function M>1500.\n";
			return 0;
		}
	} // end do loop		
	while (++nn < 30 && M >= Tol);			// report sucess, 1
	Code = (M < Tol ? 1 : 0);				// or failure, 0, 
	// resize and reasign all solution prices and ED's
	// need to copy prices and ED to sol
	Xtemp = marketplace.showPRC(per);
	EDtemp = marketplace.showED(per);
	for (i=0; i<Xtemp.size(); i++) {
		sol[i].X = Xtemp[i];
		sol[i].ED = EDtemp[i];
	}
	n+=nn-1;
	return Code;
}

// Ron's version of the Newton Raphson Solution Mechanism (all markets)
int NR_Ron(double Tol,vector<solinfo>& sol,double** JF,double** bb,int& n,int per)
{
	int i;
	int NRn=0; // calls to calculate elasticities
	int nn = 0; // number of iterations
	int Code = 2; // Code that reports success 1 or failure 0
	double M; // maximum equality value
	int m = marketplace.setmrks_sol_NR(per,Tol); // number of markets to solve
	vector<double> NP(m); // adjustment value
	vector<double> KD(m); // k values demand
	vector<double> KS(m); // k values supply
	vector<double> KDS(m); // k values demand - supply
	vector<double> Xtemp=marketplace.showPRC_NR(per); // temporary prices
	vector<double> EDtemp(m); // temporary excess demand

	double aaa[145][145], bbb[145][145];
	double **JFDM, **JFSM;
	JFDM = new double*[145*sizeof(double*)];
	for (i=0;i<145;i++) JFDM[i] = aaa[i];
	JFSM = new double*[145*sizeof(double*)];
	for (i=0;i<145;i++) JFSM[i] = bbb[i];

	// function protocol
	//void gaussj(Matrix& a,int n,Matrix& b,int m); // function protocol
	void gaussj(double** a,int n,double** b,int m);

	logfile << "Ron's version of the Newton-Raphson function called.\n";
	// solve all markets
	do {
		marketplace.logDem(per); // calculate log of demand
		marketplace.logSup(per); // calculate log of supply

		if ((NRn < 2) && (per < modeltime.getmaxper())) { // control no of times derivatives are calculated
			Derivatives(Xtemp,JFDM,JFSM,n,per); //recalculate Jacobian matrix, returns JF matrix
			for(i=0;i<m;++i) {
				for(int j=0;j<m;++j)
					JF[i][j] = JFSM[i][j]-JFDM[i][j];
			}
			gaussj(JF,m,bb,1); // inverse of JF matrix by reference
		}
		NRn++;

		// initialize KD and KS as logs of original demand and supply
		KD = marketplace.showlogDem_NR(per); // return log of demand
		KS = marketplace.showlogSup_NR(per); // return log of supply
		for (i=0; i<m; ++i) {
			for (int j=0; j<m; ++j) {
				KD[i] -= (log(_MAX(Xtemp[j],smnum))*JFDM[i][j]);
				KS[i] -= (log(_MAX(Xtemp[j],smnum))*JFSM[i][j]);
			}
			KDS[i] = KD[i] - KS[i];
		}

		// Calculate new log price based on NR
		for (i=0; i<m; ++i) {
			NP[i] = 0;
			for (int j=0; j<m; ++j)
				NP[i] += JF[i][j]*KDS[j];
			Xtemp[i] = exp(NP[i]); // new price
		}

		marketplace.setPRC_NR(Xtemp,per); // set new prices
		marketplace.nulldem(per);	// null demand
		marketplace.nullsup(per); // null supply
		
		world.calc(per); // call world object to recalculate supply and demand

		marketplace.excessdemand(per); // calculate excess demand
		EDtemp = marketplace.showED_NR(per); // show excess demand

		M = marketplace.maxED(per); // Max returns largest ED[i]

		// for debugging
		int bug = 0; // debugging on(1) or off(0)
		if (bug) {
			bugoutfile<<"\nMarket,X,DP,ED,Tolerance\n";
			for (i=0; i<m; ++i) {
				bugoutfile<<i<<","<<Xtemp[i]<<","<<NP[i]<<","<<EDtemp[i]<<","<<Tol<<"\n";
			}
			marketplace.bugout(per,nn);
			marketplace.sdcurves(per,nn); 
		}
		// if solution moves in wrong direction
		if(M>1500) {
			logfile << "Exit Newton-Raphson function M>1500.\n";
			return 0;
		}
	} // end do loop		
	while (++nn < 35 && M >= Tol);			// report sucess, 1
	Code = (M < Tol ? 1 : 0);				// or failure, 0, 
	// resize and reasign all solution prices and ED's
	// need to copy prices and ED to sol
	Xtemp = marketplace.showPRC(per);
	EDtemp = marketplace.showED(per);
	for (i=0; i<Xtemp.size(); i++) {
		sol[i].X = Xtemp[i];
		sol[i].ED = EDtemp[i];
	}
	n+=nn-1;
	logfile << "Number of Newton-Raphson iterations: n="<<nn<<"\n";
	// free up memory when done
	delete [] JFDM;
	delete [] JFSM;
	return Code;
}


void solution(int per)
{
	bool allbracketed = false;
	bool prod_not_null = false;
	bool useBisect = false;
	bool useFP = false;
	bool useSecant = false;
	bool useNR = false;
	bool useNR_Ron = true;
	bool firsttime = true;
	int i=0; // some index
	int n=0; // index for solution iteration
	int bn=0; // counter for bisection routine
	int Code = 2; // Code that reports success 1 or failure 0
	int solved = 0; // Code that reports success 1 or failure 0
	//double T = 0.0001; // Tolerance for solution criteria
	double T = 0.01; // Tolerance for solution criteria
	double M; // temporary maximum value of equality condition			 
	vector<double> X,ED,logED; // price, excess demand and log of excess demand vectors
	// For Newton-Raphson Solution Mechanism
	double a[145][145], b[145][145];
	double **JF, **bb;
	JF = new double*[145*sizeof(double*)];
	for (i=0;i<145;i++) JF[i] = a[i];
	bb = new double*[145*sizeof(double*)];
	for (i=0;i<145;i++) bb[i] = b[i];
	
	marketplace.excessdemand(per); // first calculate excess demand for all markets
	const int m = marketplace.setmrks_sol(per,T); // set number of markets to solve
	// if m = 0, no markets to solve, break out of solution.
	if (m==0) {cout<< "Model solved with last period's prices"; return;}

	marketplace.logED(per); // calculate log of excess demand
	X = marketplace.showPRC(per); // showPRC returns a vector of prices	
	logED = marketplace.showlogED(per); // showlogED returns a vector of logED
	ED = marketplace.showED(per); // showED returns a vector of ED
	
	vector<solinfo> sol(m); // create vector of solution information
	// initialize solution information
	for (i=0;i<m;i++) {
		sol[i].XL = sol[i].XR = sol[i].X = X[i];
		sol[i].EDL = sol[i].EDR = sol[i].ED = ED[i]; 
		sol[i].bracketed = 0;
	}

	// Loop is done at least once.
	do {
		if (!allbracketed) {
			solved = Bracket(T,sol,allbracketed,firsttime,n,per);
		}

		// Bisect method
		if (allbracketed && useBisect) {
			if (bn < 1) {
				int iter = 30;
				solved = Bisection_all(T,iter,sol,n,per);
				if (!solved) {
					for (i=0;i<m;i++)
						CheckBracket(T,sol,allbracketed);
				}
				++bn;
			}
			else {
				i = marketplace.worstED(per);
				solved = Bisection_i(i,T,sol,n,per);
				if (solved)
					bn = 2;
				else
					for (i=0;i<m;i++)
						CheckBracket(T,sol,allbracketed);
			}
		}
		// False position method
		if (allbracketed && useFP) {
			if (bn < 1) {
				solved = FalsePos_all(T,sol,n,per);
				if (!solved) {
					for (i=0;i<m;i++)
						CheckBracket(T,sol,allbracketed);
				}
				++bn;
			}
			else {
				i = marketplace.worstED(per);
				solved = Bisection_i(i,T,sol,n,per);
				if (solved)
					bn = 0;
				else
					CheckBracket(T,sol,allbracketed);
			}
		}

		// Secant method
		if (allbracketed && useSecant) {
			if (bn < 1) {
				solved = Secant_all(T,sol,n,per);
				if (!solved) {
					for (i=0;i<m;i++)
						CheckBracket(T,sol,allbracketed);
				}
				++bn;
			}
			else {
				i = marketplace.worstED(per);
				solved = Bisection_i(i,T,sol,n,per);
				if (solved)
					bn = 0;
				else
					CheckBracket(T,sol,allbracketed);
			}
		}

		// Use Newton-Raphson only if all markets are bracketed
		// and production is not zero.
		if (allbracketed && useNR) {
			int iter = 30;
			solved = Bisection_all(T,iter,sol,n,per);
			logfile <<"Number of iterations: n = "<<n<<"\n";
			M = marketplace.maxED(per); // Max returns largest ED[i]
			// Bisection returns ED, not log of ED
			if(!solved && M<1500) 
				solved = NewtRap(T,sol,JF,bb,n,per);
			if (!solved) CheckBracket(T,sol,allbracketed);
		}

		// Ron's version of the NR routine
		if (allbracketed && useNR_Ron) {
			int iter = 30;
			solved = Bisection_all(T,iter,sol,n,per);
			logfile <<"Number of iterations: n = "<<n<<"\n";
			M = marketplace.maxED(per); // Max returns largest ED[i]
			if(!solved && M<1500) 
				solved = NR_Ron(T,sol,JF,bb,n,per);
			if (!solved) CheckBracket(T,sol,allbracketed);
		}

		//sdfile<<"Iteration: "<<n<<"\n"; //supply & demand info

		// make sure that ED, NOT Log of ED, is checked against tolerance
		ED = marketplace.showED(per);
		M = marketplace.maxED(per); // Max returns largest ED[i]

		// for debugging
		int bug = 1; // debugging on(1) or off(0)
		if (bug) {
			bugoutfile<<"\nMarket,X,XL,XR,ED,EDL,EDR,Tolerance\n";
			for (i=0; i<m; ++i) {
				bugoutfile<<i<<","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
				<<","<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<T<<"\n";
			}
			marketplace.bugout(per,n);
			marketplace.sdcurves(per,n);
		}

	} // end do loop		
	while (M >= T && ++n < 1000);			// report sucess, 0

	Code = (M < T ? 0 : -1);				// or failure, -1, 

	switch (Code) {
	case 0:
		cout<< "Model solved normally:	World Calc n = " << n;
		logfile<< "Model solved normally:  World Calc n = " << n <<"\n";
		break;
	case -1:
		cout<< "Model did not solve within set iteration	" << n;
		logfile<< "Model did not solve within set iteration " << n <<"\n";

		logfile<<"Market,X,XL,XR,ED,EDL,EDR,Tolerance\n";
		for (i=0; i<m; ++i) {
			logfile<<i<<","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
			<<","<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<T<<"\n";
		}
		break;
	case 2:
		cout<< "Original Code has not been changed";
		logfile<< "Original Code has not been changed\n";
		break;
	default:
		cout<< "Case for Code not found";
		logfile<< "Case for Code not found\n";
		break;
	}

	// free up memory when done
	delete [] JF;
	delete [] bb;
}

#undef SIGN
