#ifndef _NATIONAL_ACCOUNT_CONTAINER_H_
#define _NATIONAL_ACCOUNT_CONTAINER_H_
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
* \file national_account_container.h
* \ingroup Objects
* \brief The NationalAccountContainer class header file.
* \author Sonny Kim
*/

#include <vector>
#include <string>
#include "util/base/include/ivisitable.h"
#include "util/base/include/data_definition_util.h"
#include "util/base/include/value.h"

class Tabs;
class NestedCESProductionFunctionMacro;
class Demographic;
class NationalAccount;

/*! 
 * \ingroup Objects
 * \brief This objects calculates regional GDPs and contains national economic accounts for all periods.
 * \details This object initiates the calculation of GDPs, calibration of GDPs or returns fixed GDP
 *          pathways. It provides an interface for retriving GDP and GDP/capital use to drive overall
 *          economic activity and other uses of GDP and GDP/capita.  The object contains the macro-economic
 *          production function (alternative functions possible) for calculating the Gross Economic Output and
 *          GDP.  It also holds the yearly national accounts for all periods.  The national accounts are used
 *          for both reporting and for passing aggregated financial accounts during model operation.
 *
 * \author Sonny Kim
 */
class NationalAccountContainer: public IVisitable
{
    friend class IVisitor;
    friend class XMLDBOutputter;
public:
    NationalAccountContainer();
    virtual ~NationalAccountContainer();
    static const std::string& getXMLNameStatic();
    const std::string& getGDPMarketName() const;
    const std::string& getGDPActivityName() const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    void completeInit( const std::string& aRegionName, const Demographic* aDemographics );
    void initCalc( const Demographic* aDemographics, const int aPeriod );
    
    // the main calc method
    void calcGDP( const int aPeriod );
    
    // accessor functions for GDP, typically interacted with via SectorUtils
    double getMarketGDP( const int aPeriod ) const;
    double getMarketGDPperCapita( const int aPeriod ) const;
    double getMarketGDPperCapitaNorm( const int aPeriod ) const;
    double getGDPPPP( const int aPeriod ) const;
    double getPop( const int aPeriod ) const;
    
    void postCalc( const int aPeriod );
    void accept( IVisitor* aVisitor, const int aPeriod ) const;

    protected:
    
    DEFINE_DATA(
        /*! \brief NationalAccountContainer */
        DEFINE_SUBCLASS_FAMILY( NationalAccountContainer ),

        /*! \brief vector of national accounts */
        DEFINE_VARIABLE( CONTAINER, "NationalAccount", mNationalAccounts, std::vector<NationalAccount*> ),
                
        /*! \brief region name */
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "regionName", mRegionName, std::string ),
                
        /*! \brief gdp trial market name */
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "GDPMrkName", mGDPMrkName, std::string ),
                        
        /*! \brief gdp activity name */
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "GDPActName", mGDPActName, std::string ),
                
        /*! \brief negative emissions budget policy name */
        DEFINE_VARIABLE( SIMPLE, "negative-emiss-budget-name", mNegEmissBudgetName, std::string ),
                
        /*! \brief negative emissions budget GDP fraction */
        DEFINE_VARIABLE( SIMPLE, "negative-emiss-budget-fraction", mNegEmissBudgetFraction, Value ),
                
        /*! \brief negative emissions budget GDP supply state variable */
        DEFINE_VARIABLE( SIMPLE | STATE, "negative-emiss-budget-supply", mNegEmissBudgetSupply, Value ),
                
        /*! \brief curr GDP value state variable */
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE | STATE, "curr-GDP", mCurrGDP, Value ),
                                
        /*! \brief net energy export market name */
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "EnergyNetExportMrkName", mEnergyNetExportMrkName, std::string ),
    
        /*! \brief energy servicemarket name */
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "EnergyServiceMrkName", mEnergyServiceMrkName, std::string ),

        //! MER to PPP scale conversion
        DEFINE_VARIABLE( SIMPLE, "gdp-ppp-conversion", mPPPConversion, Value ),
                
        //! Savings rate regression paramter: base value (intercept)
        DEFINE_VARIABLE( SIMPLE, "saving-rate-param-base", mSavingsRParamBase, Value ),
                
        //! Savings rate regression paramter: previous savings rate coef
        DEFINE_VARIABLE( SIMPLE, "saving-rate-param-SR-coef", mSavingsRParamSR, Value ),
        
        //! Savings rate regression paramter: GDP per Capita growth rate coef
        DEFINE_VARIABLE( SIMPLE, "saving-rate-param-GR-coef", mSavingsRParamGR, Value ),
        
        //! Macro model for GDP
        DEFINE_VARIABLE( CONTAINER, "gdp-macro-function", mGdpMacroFunction, NestedCESProductionFunctionMacro* )
    )

private:
    
    void setGDPTrialMarket();
};

#endif // _NATIONAL_ACCOUNT_CONTAINER_H_
