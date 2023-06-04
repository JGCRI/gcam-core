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
    static const std::string& getXMLNameStatic();
    const std::string& getXMLName() const;
    virtual const std::string& getName() const;
    void completeInit( const std::string& aRegionName, const std::string& aGDPActName );

    void initCalc( const std::string& aRegionName, NationalAccount* aNationalAccount, const int aPeriod );

    bool isPrimaryFactor() const;

    void postCalc( const std::string& aRegionName, NationalAccount* aNationalAccount, const int aPeriod );
    void accept( IVisitor* aVisitor, const int aPeriod ) const;
    
    void grabInputs(FactorInputLeaf*& aEnergyInput, FactorInputLeaf*& aLaborInput, FactorInputLeaf*& aCapitalInput);
    
protected:
    
    DEFINE_DATA(
        /* Declare all subclasses of Sector to allow automatic traversal of the
         * hierarchy under introspection.
         */
        DEFINE_SUBCLASS_FAMILY( FactorInputLeaf ),

        //! factor input name
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),
    
        DEFINE_VARIABLE( SIMPLE, "is-primary-factor", mIsPrimaryFactor, bool ),
        DEFINE_VARIABLE( SIMPLE, "is-capital", mIsCapital, bool ),
        DEFINE_VARIABLE( SIMPLE, "is-labor", mIsLabor, bool ),
        DEFINE_VARIABLE( SIMPLE, "is-energy", mIsEnergy, bool ),
        //! Vector of total productivity.
        DEFINE_VARIABLE( ARRAY, "productivity", mProductivity, objects::PeriodVector<Value> ),

        //! scaler for factor inputs
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "scaler", mScaler, Value ),
        //! market name for output
        DEFINE_VARIABLE( SIMPLE, "output", mOutputMrkName, std::string )
    )

};

class FactorInputNode: public IVisitable, public INamed {
    friend class IVisitor;
    friend class XMLDBOutputter;
public:
    FactorInputNode();
    static const std::string& getXMLNameStatic();
    const std::string& getXMLName() const;
    virtual const std::string& getName() const;
    void completeInit( const std::string& aRegionName, const std::string& aGDPActName );
    void initCalc( const std::string& aRegionName, NationalAccount* aNationalAccount, const int aPeriod );
    
    bool isPrimaryFactor() const;

    void postCalc( const std::string& aRegionName, NationalAccount* aNationalAccount, const int aPeriod );
    void accept( IVisitor* aVisitor, const int aPeriod ) const;
    
    double getNodeGamma() const { return mFactorNodeGamma; }
    void grabInputs(FactorInputLeaf*& aEnergyInput, FactorInputLeaf*& aLaborInput, FactorInputLeaf*& aCapitalInput) {
        for(auto child : mFactorInputLeaf) {
            child->grabInputs(aEnergyInput, aLaborInput, aCapitalInput);
        }
    }

protected:
    
    DEFINE_DATA(
        /* Declare all subclasses of Sector to allow automatic traversal of the
         * hierarchy under introspection.
         */
        DEFINE_SUBCLASS_FAMILY( FactorInputNode ),

        //! factor input node name
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),
        
        //! modified elasticity for nest
        DEFINE_VARIABLE( SIMPLE, "node-gamma", mFactorNodeGamma, Value ),
        
        //! Vector of factor inputs to GDP macro
        DEFINE_VARIABLE( CONTAINER, "factor-input-leaf", mFactorInputLeaf, std::vector<FactorInputLeaf*> )
    )

};


class NestedCESProductionFunctionMacro: public IVisitable, public INamed {
    friend class IVisitor;
    friend class XMLDBOutputter;
public:
    NestedCESProductionFunctionMacro();
    static const std::string& getXMLNameStatic();
    const std::string& getXMLName() const;
    const std::string& getName() const;
    void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const;
    void completeInit( const std::string& aRegionName, const std::string& aGDPActName );
    void initCalc( const std::string& aRegionName, NationalAccount* aNationalAccount, const int aPeriod );
    void setTotalFactorProductivity( const double aTotalFactorProd );
    double calcGrossOutput( const std::string& aRegionName, NationalAccount* aNationalAccount, const int aPeriod, const bool aSaveResults );
    void postCalc( const std::string& aRegionName, NationalAccount* aNationalAccount, const int aPeriod );
    void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    DEFINE_DATA(
        /* Declare all subclasses of Sector to allow automatic traversal of the
         * hierarchy under introspection.
         */
        DEFINE_SUBCLASS_FAMILY( NestedCESProductionFunctionMacro ),

        //! rho elasticity for CES
        DEFINE_VARIABLE( SIMPLE, "rho", mRho, Value ),
                
        //! The current total factor productivity to apply to the gross output
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "total-factor-productivity", mTotalFactorProd, Value ),
        
        //! Vector of factor inputs to GDP macro.
        DEFINE_VARIABLE( CONTAINER, "factor-input-node", mFactorInputNode, std::vector<FactorInputNode*> )
    )

};

#endif // _NESTED_CES_PRODUCTION_FUNCTION_MACRO_H_
