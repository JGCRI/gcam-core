#ifndef _NEWTON_RAPHSON_SD_H_
#define _NEWTON_RAPHSON_SD_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file log_newton_raphson.h
* \ingroup objects
* \brief This is the header file for the LogNewtonRaphsonSaveDeriv solver component class.
*
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "solution/solvers/include/log_newton_raphson.h"

/*! 
* \ingroup Objects
* \brief A SolverComponent based on the newton raphson algorithm using logarithmic values. 
* \author Josh Lurz
*/

class LogNewtonRaphsonSaveDeriv: public LogNewtonRaphson {
public:
    LogNewtonRaphsonSaveDeriv( Marketplace* marketplaceIn, World* worldIn, CalcCounter* calcCounterIn );
    ~LogNewtonRaphsonSaveDeriv();
    static const std::string& getNameStatic();
    void init();
    
protected:
    const std::string& getName() const;
    static const std::string SOLVER_NAME;
    virtual ReturnCode calculateDerivatives( SolverInfoSet& solverSet, Matrix& JFSM, Matrix& JFDM, Matrix& JF, int period );

    bool derivativesCalculated;
    std::auto_ptr <Matrix> JFSave;
    std::auto_ptr <Matrix> JFDMSave;
    std::auto_ptr <Matrix> JFSMSave;
    int savedMatrixSize;
};

#endif // _NEWTON_RAPHSON_SD_H_
