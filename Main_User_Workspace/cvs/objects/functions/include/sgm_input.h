#ifndef _SGM_INPUT_H_
#define _SGM_INPUT_H_
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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*! 
* \file SGMInput.h
* \ingroup Objects
* \brief SGMInput class header file.
* \author Pralit Patel, Sonny Kim
* \date $Date: 2005/06/01 22:01:13 $
* \version $Revision: 1.3 $
*/

#include <string>
#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/value.h"
#include "functions/include/iinput.h"

class Tabs;
class DemandInput;
class ProductionInput;

/*! 
* \ingroup Objects
* \brief Defines a single SGMInput to a production or demand function.
* \details TODO
* \author Pralit Patel, Sonny Kim, Josh Lurz
*/
class SGMInput: public IInput
{
    friend class SocialAccountingMatrix;
    friend class DemandComponentsTable;
    friend class SectorReport;
    friend class SGMGenTable;
    friend class XMLDBOutputter;
public:
    SGMInput();
    virtual ~SGMInput();
    virtual SGMInput* clone() const = 0;
    virtual void copyParam( const IInput* aInput, const int aPeriod ) = 0;

    virtual const std::string& getXMLReportingName() const;

    void XMLParse( const xercesc::DOMNode* node );
    bool isSameType( const std::string& aType ) const;

    void completeInit( const std::string& aRegionName,
                       const std::string& aSectorName,
                       const std::string& aSubsectorName,
                       const std::string& aTechName,
                       DependencyFinder* aDependencyFinder ,
                       const IInfo* aTechInfo);

    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const bool aIsNewInvestmentPeriod,
                           const bool aIsTrade,
                           const int aPeriod );

    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    const std::string& getName() const;
    double getConversionFactor( const int aPeriod ) const;
    double getCO2EmissionsCoefficient( const std::string& aGHGName, const int aPeriod ) const;
    double getPhysicalDemand( const int aPeriod ) const;
    double getCarbonContent( const int aPeriod ) const;
    double getCurrencyDemand( const int aPeriod ) const;
    double getTechChange( const int aPeriod ) const;

    double getPrice( const std::string& aRegionName, const int aPeriod ) const;

    virtual void setPrice( const std::string& aRegionName,
                           const double aPrice,
                           const int aPeriod );

    double getPricePaid( const std::string& aRegionName, const int aPeriod ) const;
    void setPricePaid( const double aPricePaid, const int aPeriod );
    double getPriceReceived( const std::string& aRegionName, const int aPeriod ) const;

    void setCurrencyDemand( const double aCurrencyDemand, const std::string& aRegionName, const int aPeriod );

    void setPhysicalDemand( const double aPhysicalDemand, const std::string& aRegionName, const int aPeriod );

    double getCoefficient( const int aPeriod ) const;

    void setCoefficient( const double aCoefficient, const int aPeriod );
    double getPriceAdjustment() const;
    bool hasTypeFlag( const int aTypeFlag ) const;
    virtual double getPriceElasticity() const = 0;
    virtual double getIncomeElasticity() const = 0;

    void tabulateFixedQuantity( const std::string& aRegionName,
                                const double aFixedOutput,
                                const bool aIsInvestmentPeriod,
                                const int aPeriod );

    void scaleCalibrationQuantity( const double aScaleFactor );

    virtual double getCalibrationQuantity( const int aPeriod ) const;

    void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
    virtual void accept( IVisitor* aVistior, const int aPeriod ) const = 0;
    virtual void copyParamsInto( ProductionInput& aInput, const int aPeriod ) const = 0;
    virtual void copyParamsInto( DemandInput& aInput, const int aPeriod ) const = 0;

    // MiniCAM input types which SGM inputs should ignore.
    virtual void copyParamsInto( EnergyInput& aInput, const int aPeriod ) const {}
    virtual void copyParamsInto( NonEnergyInput& aInput, const int aPeriod ) const {}
    virtual void copyParamsInto( BuildingDemandInput& aInput, const int aPeriod ) const {}
    virtual void copyParamsInto( RenewableInput& aInput, const int aPeriod ) const {}
    virtual void copyParamsInto( InputSubsidy& aInput, const int aPeriod ) const {}
    virtual void copyParamsInto( InputTax& aInput, const int aPeriod ) const {}
protected:
    //! Name of the SGMInput.
    std::string mName;

    //! Coefficient for production or demand function.
    Value mCoefficient;

    //! Currency Demand.
    Value mCurrencyDemand;

    //! Price adjustment factor.
    Value mPriceAdjustFactor;

    //! Price paid for SGMInput, adjusted from market price.
    Value mPricePaid;

    //! Technical Change.
    Value mTechnicalChange;

    //! Conversion factor.
    Value mConversionFactor;

    //! CO2 coefficient.
    Value mCO2Coefficient;

    //! Type flags.
    int mTypeFlags;

    void initializeTypeFlags( const std::string& aRegionName );
    void initializeCachedCoefficients( const std::string& aRegionName );

    bool isEnergyGood( const std::string& aRegionName ) const;

    bool isPrimaryEnergyGood( const std::string& aRegionName ) const;

    bool isSecondaryEnergyGood( const std::string& aRegionName ) const;

    virtual const std::string& getXMLName() const = 0;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ) = 0;
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const = 0;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const = 0;

private:
    const static std::string XML_REPORTING_NAME; //!< tag name for reporting xml db 

};

#endif // _SGM_INPUT_H_
