#ifndef _NODE_INPUT_H_
#define _NODE_INPUT_H_
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
* \file node_input.h
* \ingroup Objects
* \brief NodeInput class header file.
* \author Pralit Patel
* \author Ron Sands
*/

#include "util/base/include/definitions.h"

#include "functions/include/inested_input.h"
#include "util/base/include/value.h"

class IFunction;

/*! 
 * \ingroup Objects
 * \brief A node class to implement nested production inputs.
 *
 * \details A node input which facillitates nesting of inputs by calling a contained
 *          production function methods on each of it's children then instructing them
 *          to forward the call since they themselves may be a node input.  Thus all
 *          recursion required for calculations are handled by node inputs and the leaves
 *          are not required to do anything (they are the stop point for the recursion).
 *
 *          <b>XML specification for NodeInput</b>
 *          - XML name: \c nodeInput
 *          - Contained by: BaseTechnology, NodeInput
 *          - Parsing inherited from class: None
 *          - Attributes: \c name NodeInput::mName
 *          - Elements:
 *              - \c prodDmdFnType NodeInput::mProdDmdFnType
 *                   The production function type string which can be used by FunctionManager.
 *              - \c price-received NodeInput::mPricePaid
 *                   Sets the initial price to use during coefficient calibration.
 *                   TODO: calling this price-received does not seem correct.
 *              - \c Sigma1 NodeInput::mSigmaNewCapital
 *                   Sigma exponent to use for new capital vintage technology.
 *              - \c Sigma2 NodeInput::mSigmaOldCapital
 *                   Sigma exponent to use for old capital vintage technology.
 *              - \c technicalChange NodeInput::mTechChange
 *                   Tech change specific to this node.  This tech change would only be 
 *                   applied in the intial period this nest operates.
 *              - \c ProductionInput::getXMLNameStatic(),
                     NodeInput::getXMLNameStatic() NodeInput::mNestedInputs
 *                   Inputs which can be parsed a children to this node.
 *                   TODO: shouldn't there be a factory for these?
 *
 * \author Pralit Patel
 * \author Ron Sands
 */
class NodeInput : public INestedInput
{
public:
    NodeInput();
    ~NodeInput();

    static const std::string& getXMLNameStatic();

    // INestedInput methods
    virtual void removeEmptyInputs();

    virtual void initialize();

    virtual void calcCoefficient( const std::string& aRegionName, const std::string& aSectorName,
        const int aTechPeriod );

    virtual void changeElasticity( const std::string& aRegionName, const int aPeriod,
        const double aAlphaZero );

    virtual void changeSigma( const std::string& aRegionName, const int aPeriod,
        const double aAlphaZero );

    virtual void calcLevelizedCost( const std::string& aRegionName, const std::string& aSectorName,
        const int aPeriod, const double aAlphaZero );

    virtual double calcInputDemand( const std::string& aRegionName, const std::string& aSectorName,
        const int aPeriod, const double aPhysicalOutput, const double aUtilityParameterA,
        const double aAlphaZero );

    virtual double calcCapitalOutputRatio( const std::string& aRegionName, const std::string& aSectorName,
        const int aPeriod, const double aAlphaZero );

    virtual void calcVariableLevelizedCost( const std::string& aRegionName, const std::string& aSectorName,
        const int aPeriod, const double aAlphaZero );

    virtual const IFunction* getFunction() const;
    
    virtual double getLevelizedCost( const std::string& aRegionName, const std::string& aSectorName,
        const int aPeriod ) const;

    virtual void applyTechnicalChange( const std::string& aRegionName, const std::string& aSectorName,
        const int aPeriod, const TechChange& aTechChange );

    virtual void resetCalcLevelizedCostFlag();

    // IInput methods
    virtual IInput* clone() const;
    
    virtual void copyParam( const IInput* aInput,
                            const int aPeriod );

    virtual void copyParamsInto( NodeInput& aInput,
        const int aPeriod ) const;

    virtual bool isSameType( const std::string& aType ) const;
    
    virtual const std::string& getName() const;

    virtual const std::string& getMarketName( const std::string& aRegionName ) const { return aRegionName; }

    virtual const std::string& getXMLReportingName() const;
    
    virtual const std::string& getXMLName() const;

    virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const;
    
    virtual bool hasTypeFlag( const int aTypeFlag ) const;

    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               const std::string& aSubsectorName,
                               const std::string& aTechName,
                               const IInfo* aTechInfo );

    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const bool aIsNewInvestmentPeriod,
                           const bool aIsTrade,
                           const IInfo* aTechInfo,
                           const int aPeriod );

    virtual double getCurrencyDemand( const int aPeriod ) const;

    virtual void setCurrencyDemand( const double aCurrencyDemand,
                                    const std::string& aRegionName, 
                                    const int aPeriod );

    virtual double getPhysicalDemand( const int aPeriod ) const;
    
    virtual double getCarbonContent( const int aPeriod ) const;
    
    virtual void setPhysicalDemand( const double aPhysicalDemand,
                                    const std::string& aRegionName, 
                                    const int aPeriod );

    virtual double getPrice( const std::string& aRegionName,
                             const int aPeriod ) const;

    virtual void setPrice( const std::string& aRegionName,
                           const double aPrice,
                           const int aPeriod );

    virtual double getPriceAdjustment() const;

    virtual double getPricePaid( const std::string& aRegionName,
                                 const int aPeriod ) const;

    virtual void setPricePaid( const double aPricePaid,
                               const int aPeriod );

    virtual double getCoefficient( const int aPeriod ) const;

    virtual void setCoefficient( const double aCoefficient,
                                 const int aPeriod );

    virtual double getConversionFactor( const int aPeriod ) const;

    virtual double getCO2EmissionsCoefficient( const std::string& aGHGName,
                                             const int aPeriod ) const;

    virtual void tabulateFixedQuantity( const std::string& aRegionName,
                                        const double aFixedOutput,
                                        const bool aIsInvestmentPeriod,
                                        const int aPeriod );

    virtual void scaleCalibrationQuantity( const double aScaleFactor );

    virtual double getCalibrationQuantity( const int aPeriod ) const;

    virtual double getPriceElasticity( const int aPeriod ) const;

    virtual double getIncomeElasticity( const int aPeriod ) const;

    virtual double getTechChange( const int aPeriod ) const;

    // TODO: put methods that NodeInput will not implement under here
    virtual void calcPricePaid( const std::string& aRegionName,
                                const std::string& aSectorName,
                                const std::vector<AGHG*>& aGhgs,
                                const ICaptureComponent* aSequestrationDevice,
                                const int aLifetimeYears,
                                const int aPeriod ) {}

    virtual double calcTaxes( const std::string& aRegionName,
                            NationalAccount* aNationalAccount,
                            Expenditure* aExpenditure,
                            const int aPeriod ) const { return 0; }

    virtual void doInterpolations( const int aYear, const int aPreviousYear,
                                   const int aNextYear, const IInput* aPreviousInput,
                                   const IInput* aNextInput ) {}
    
    virtual void copyParamsInto( EnergyInput& aInput,
        const int aPeriod ) const {}

    virtual void copyParamsInto( NonEnergyInput& aInput,
        const int aPeriod ) const {}

    virtual void copyParamsInto( RenewableInput& aInput,
        const int aPeriod ) const {}

    virtual void copyParamsInto( InputSubsidy& aInput,
        const int aPeriod ) const {}

    virtual void copyParamsInto( InputTax& aInput,
        const int aPeriod ) const {}
	
	virtual void copyParamsInto( InputOMVar& aInput,
								const int aPeriod ) const {}
	
	virtual void copyParamsInto( InputOMFixed& aInput,
								const int aPeriod ) const {}
	
	virtual void copyParamsInto( InputCapital& aInput,
								const int aPeriod ) const {}

    // IVisitable interface.
    virtual void accept( IVisitor* aVisitor,
                        const int aPeriod ) const;


protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        INestedInput,
                            
        //! Vector of child inputs
        DEFINE_VARIABLE( CONTAINER, "nodeInput", mNestedInputs, std::vector<INestedInput*> ),
        
        //! The name of this input
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),
                                
        //! Type of function used
        DEFINE_VARIABLE( SIMPLE, "prodDmdFnType", mProdDmdFnType, std::string ),

        /*! 
         * Value used for initialization.  This is the same thing as the sum
         * of the children's currency demand
         */
        DEFINE_VARIABLE( SIMPLE | STATE | NOT_PARSABLE, "demandCurrency", mNodeCurrencyDemand, Value ),
                                
        //! Sigma used to operate new capital
        DEFINE_VARIABLE( SIMPLE, "Sigma1", mSigmaNewCapital, Value ),

        //! Sigma used to operate old capital
        DEFINE_VARIABLE( SIMPLE, "Sigma2", mSigmaOldCapital, Value ),

        //! The current Sigma to use
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "current-Sigma", mCurrentSigma, Value ),

        //! Alpha, for the root node this would be Alpha zero
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "coefficient", mAlphaCoef, Value ),

        //! Price paid, for the root this would be price recieved
        DEFINE_VARIABLE( SIMPLE | STATE, "price-received", mPricePaid, Value ),

        //! Price paid in the base year, for the root this would be price recieved
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "base-price-paid", mBasePricePaid, Value ),

        /*! 
         * The technical change that would get applied to the node.  Note that it
         * only gets applied during the initial vintage of a technology and will
         * never get passed forward.
         */
        DEFINE_VARIABLE( SIMPLE, "technicalChange", mTechChange, Value )
    )
    
    //! Pointer to function this class will use
    const IFunction* mProdDmdFn;
                           
    //! Cache the vector of children as IInput* which is needed for the mProdDmdFn
    std::vector<IInput*> mChildInputsCache;
                           
    //! Hack to avoiding excessive levelized cost calcs to help performance.
    bool mNodePriceSet;
                           
    typedef std::vector<INestedInput*>::iterator NestedInputIterator;
    typedef std::vector<INestedInput*>::const_iterator CNestedInputIterator;
    
    void copy( const NodeInput& aNodeInput );
};

#endif // _NODE_INPUT_H_
