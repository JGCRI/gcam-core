#ifndef _NEWTON_RAPHSON_SD_H_
#define _NEWTON_RAPHSON_SD_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file log_newton_raphson_sd.h
* \ingroup objects
* \brief This is the header file for the LogNewtonRaphsonSaveDeriv solver component class.
*
* \author Josh Lurz
*/

#include "solution/solvers/include/log_newton_raphson.h"

/*! 
* \ingroup Objects
* \brief A SolverComponent based on the Newton-Raphson algorithm using logarithmic values. 
* \author Josh Lurz
*/

class LogNewtonRaphsonSaveDeriv: public LogNewtonRaphson {
public:
    LogNewtonRaphsonSaveDeriv( Marketplace* aMarketplaceIn, World* aWorld, CalcCounter* aCalcCounter,
                               double aDeltaPrice );
    ~LogNewtonRaphsonSaveDeriv();
    static const std::string& getNameStatic();
    void init();
    
protected:
    const std::string& getName() const;
    static const std::string SOLVER_NAME;

    virtual ReturnCode calculateDerivatives( SolverInfoSet& solverSet, Matrix& JFSM, Matrix& JFDM, Matrix& JF,
                                             int period );

    bool derivativesCalculated;
    Matrix JFSave;
    Matrix JFDMSave;
    Matrix JFSMSave;
    unsigned int savedMatrixSize;
};

#endif // _NEWTON_RAPHSON_SD_H_
