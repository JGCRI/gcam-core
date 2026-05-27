#ifndef _NESTED_CES_PRODUCTION_FUNCTION_MACRO_H_
#define _NESTED_CES_PRODUCTION_FUNCTION_MACRO_H_
#if defined(_MSC_VER)
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


/*! 
* \file nested_ces_production_function_macro.h
* \ingroup Objects
* \brief Nested CES Production Function class header file for GCAM Macro.
* \author Sonny Kim
*/

#include <string>
#include <vector>
#include "functions/include/iinput.h"
#include "util/base/include/ivisitable.h"
#include "util/base/include/inamed.h"
#include "util/base/include/time_vector.h"
#include "util/base/include/value.h"
#include "util/base/include/data_definition_util.h"

class NationalAccount;
class NestedCESProductionFunctionMacro;

class FactorInputLeaf: public IVisitable, public INamed {
    friend class IVisitor;
    friend class XMLDBOutputter;
    friend class NestedCESProductionFunctionMacro;
public:
    FactorInputLeaf();
    static const gcamstr& getXMLNameStatic();
    const gcamstr& getXMLName() const;
    virtual const gcamstr& getName() const;
    void completeInit( const gcamstr& aRegionName, const gcamstr& aGDPActName );

    void initCalc( const gcamstr& aRegionName, NationalAccount* aNationalAccount, const int aPeriod );
    
    void updateMarkets(const gcamstr& aRegionName, NationalAccount* aNationalAccount, const int aPeriod);

    double getCalShares(const gcamstr& aRegionName, NationalAccount* aNationalAccount, const double aGrossOutput, const int aPeriod);

    void calcCoef(const gcamstr& aRegionName, NationalAccount* aNationalAccount, const double aGrossOutput, const double aShareAdj, const double aSlackShare, const double aParentValue, const double aParentPrice, const double aGamma, const int aPeriod);

    double getCoefficient(const double aTFP, const int aPeriod) const;

    double getPrice(const gcamstr& aRegionName, const double aTFP, const int aPeriod);

    void calcQuantity(const gcamstr& aRegionName,
                        NationalAccount* aNationalAccount,
                        const double aTFP,
                        const double aParentQuantity,
                        const double aParentPrice,
                        const double aGamma,
                        const bool aSaveResults,
                        const int aPeriod);

    double getQuantity(const gcamstr& aRegionName,
            const int aPeriod);

    void calcPricesForReporting(NationalAccount* aNationalAccount,
                                const double aTFP,
                                const double aQuantityAbove,
                                const double aPriceAbove,
                                const double aGamma,
                                const int aPeriod);

    void reportResults(const gcamstr& aRegionName,
                       const double aTFP,
                       const double aGrossOutput,
                       NationalAccount* aNationalAccount,
                       const int aPeriod);

    void postCalc( const gcamstr& aRegionName, NationalAccount* aNationalAccount, const int aPeriod );
    void accept( IVisitor* aVisitor, const int aPeriod ) const;
    
    
protected:
    
    DEFINE_DATA(
        /* Declare all subclasses of Sector to allow automatic traversal of the
         * hierarchy under introspection.
         */
        DEFINE_SUBCLASS_FAMILY( FactorInputLeaf ),

        //! factor input name
        DEFINE_VARIABLE( SIMPLE, "name", mName, gcamstr ),
    
        DEFINE_VARIABLE( SIMPLE, "is-capital", mIsCapital, bool ),
        DEFINE_VARIABLE( SIMPLE, "is-labor", mIsLabor, bool ),
        DEFINE_VARIABLE( SIMPLE, "is-energy", mIsEnergy, bool ),
        //! Vector of total productivity.
        DEFINE_VARIABLE( ARRAY, "productivity", mProductivity, objects::PeriodVector<Value> ),

        //! scaler for factor inputs
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "scaler", mScaler, Value ),
                
        //! A state value to add labor / capital demands to market
        DEFINE_VARIABLE( SIMPLE | STATE, "demand", mDemand, Value ),
        
        //! market name for output
        DEFINE_VARIABLE( SIMPLE, "output-market-name", mOutputMrkName, gcamstr )
    )
    
    double getCalibrationQuantity(const gcamstr& aRegionName, NationalAccount* aNationalAccount, const int aPeriod) const;
};

class FactorInputNode: public IVisitable, public INamed {
    friend class IVisitor;
    friend class XMLDBOutputter;
    friend class NestedCESProductionFunctionMacro;
public:
    FactorInputNode();
    static const gcamstr& getXMLNameStatic();
    const gcamstr& getXMLName() const;
    virtual const gcamstr& getName() const;
    void completeInit( const gcamstr& aRegionName, const gcamstr& aGDPActName );
    void initCalc( const gcamstr& aRegionName, NationalAccount* aNationalAccount, const int aPeriod );
    
    void updateMarkets(const gcamstr& aRegionName, NationalAccount* aNationalAccount, const int aPeriod);

    void getCalShares(std::vector<double>& aShares, const gcamstr& aRegionName, NationalAccount* aNationalAccount, const double aGrossOutput, const int aPeriod);

    void calcCoef(const gcamstr& aRegionName, NationalAccount* aNationalAccount, const double aGrossOutput, const double aShareAdj, const double aSlackShare, const double aParentValue, const double aParentPrice, const double aGamma, const int aPeriod);

    double getCoefficient() const { return mScaler; }

    double getPrice(const gcamstr& aRegionName, const double aTFP, const int aPeriod);

    void calcQuantity(const gcamstr& aRegionName,
                        NationalAccount* aNationalAccount,
                        const double aTFP,
                        const double aQuantityAbove,
                        const double aPriceAbove,
                        const double aGamma,
                        const bool aSaveResults,
                        const int aPeriod);

    void reportResults(const gcamstr& aRegionName,
                       const double aTFP,
                       const double aGrossOutput,
                       NationalAccount* aNationalAccount,
                       const int aPeriod);

    void postCalc( const gcamstr& aRegionName, NationalAccount* aNationalAccount, const int aPeriod );
    void accept( IVisitor* aVisitor, const int aPeriod ) const;
    
protected:
    
    DEFINE_DATA(
        /* Declare all subclasses of Sector to allow automatic traversal of the
         * hierarchy under introspection.
         */
        DEFINE_SUBCLASS_FAMILY( FactorInputNode ),

        //! factor input node name
        DEFINE_VARIABLE( SIMPLE, "name", mName, gcamstr ),
        
        //! modified elasticity for nest
        DEFINE_VARIABLE( SIMPLE, "node-gamma", mFactorNodeGamma, Value ),

        //! Calibration coefficient
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "scaler", mScaler, Value ),

        //! The current node price, just for reporting
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "price", mPrice, Value ),

        //! The current node quantity, just for reporting
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "demand", mDemand, Value ),
        
        //! Vector of factor inputs to GDP macro
        DEFINE_VARIABLE( CONTAINER, "factor-input-leaf", mFactorInputLeaf, std::vector<FactorInputLeaf*> )
    )
};


class NestedCESProductionFunctionMacro: public IVisitable, public INamed {
    friend class IVisitor;
    friend class XMLDBOutputter;
public:
    NestedCESProductionFunctionMacro();
    static const gcamstr& getXMLNameStatic();
    const gcamstr& getXMLName() const;
    const gcamstr& getName() const;
    void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const;
    void completeInit( const gcamstr& aRegionName, const gcamstr& aGDPActName );
    void initCalc( const gcamstr& aRegionName, NationalAccount* aNationalAccount, const int aPeriod );
    void updateMarkets(const gcamstr& aRegionName, NationalAccount* aNationalAccount, const int aPeriod);
    void setTotalFactorProductivity( const double aTotalFactorProd );
    double calcGrossOutput( const gcamstr& aRegionName, NationalAccount* aNationalAccount, const int aPeriod, const bool aSaveResults );
    void postCalc( const gcamstr& aRegionName, NationalAccount* aNationalAccount, const int aPeriod );
    void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    DEFINE_DATA(
        /* Declare all subclasses of Sector to allow automatic traversal of the
         * hierarchy under introspection.
         */
        DEFINE_SUBCLASS_FAMILY( NestedCESProductionFunctionMacro ),

        //! rho elasticity for CES
        DEFINE_VARIABLE( SIMPLE, "rho", mRho, Value ),
        
        //! A state value to add capital investment supply to market
        DEFINE_VARIABLE( SIMPLE | STATE, "capital-supply", mCapitalSupply, Value ),
                
        //! The current total factor productivity to apply to the gross output
        DEFINE_VARIABLE( SIMPLE | STATE | NOT_PARSABLE, "total-factor-productivity", mTotalFactorProd, Value ),
        
        //! A intermediate node of input to the CES, typically for the value-added nest
        DEFINE_VARIABLE( CONTAINER, "factor-input-node", mFactorInputNode, FactorInputNode* ),
                
        //! Vector of direct factor inputs to the root of the CES
        DEFINE_VARIABLE( CONTAINER, "factor-input-leaf", mFactorInputLeaf, std::vector<FactorInputLeaf*> )
    )

};

#endif // _NESTED_CES_PRODUCTION_FUNCTION_MACRO_H_
