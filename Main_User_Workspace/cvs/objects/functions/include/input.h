#ifndef _INPUT_H_
#define _INPUT_H_
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
* \file input.h
* \ingroup Objects
* \brief Input class header file.
* \author Pralit Patel, Sonny Kim
*/

#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/value.h"
#include "util/base/include/ivisitable.h"
#include "util/base/include/iround_trippable.h"
#include "util/base/include/time_vector.h"

class IVisitor;
class DemandInput;
class ProductionInput;

/*! 
* \ingroup Objects
* \brief Defines a single input to a production or demand function.
* \details TODO
* \author Pralit Patel, Sonny Kim, Josh Lurz
*/
class Input: public IVisitable, public IRoundTrippable
{
    friend class SocialAccountingMatrix;
    friend class DemandComponentsTable;
    friend class SectorReport;
    friend class SGMGenTable;
    friend class XMLDBOutputter;
public:
    Input();
    virtual ~Input();
    virtual Input* clone() const = 0;
    virtual void copyParam( const Input* aInput,
                            const int aPeriod ) = 0;

    virtual void copyParamsInto( ProductionInput& aInput ) const = 0;
    virtual void copyParamsInto( DemandInput& aInput ) const = 0;
    void XMLParse( const xercesc::DOMNode* node );

    void completeInit( const unsigned int aFirstOperationalPeriod );

    void initCalc( const std::string& aRegionName,
                   const bool aIsTrade,
                   const int aPeriod );

    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    const std::string& getName() const;
    double getConversionFactor( const std::string& aRegionName ) const;
    double getGHGCoefficient( const std::string& aGHGName, const std::string& aRegionName ) const;
    
    double getDemandPhysical( const std::string& aRegionName,
                              const int aPeriod ) const;
    
    double getDemandCurrency( const int aPeriod ) const;

    double getTechChange( double aEnergyTechChange, double aMaterialTechChange,
                          const std::string& aRegionName ) const;
    double getPrice( const std::string& aRegionName, const int aPeriod ) const;
    
    double getPricePaid( const int aPeriod ) const;
    
    void setPricePaid( double aPricePaid,
                       const int aPeriod );
    
    double getPriceReceived( const std::string& aRegionName, const int aPeriod ) const;
    void setDemandCurrency( const double aDemand, const std::string& aRegionName, 
        const std::string& aSectorName, int aPeriod );
    virtual void scaleCoefficient( double scaleValue );
    double getCoefficient() const;
    void setCoefficient( double coef );
    double getPriceAdjustment() const;
    virtual double getPriceElasticity() const = 0;
    virtual double getIncomeElasticity() const = 0;
    virtual void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
    bool isFactorSupply() const;
    inline bool isCapital() const;
    inline bool isNumeraire() const;
    static bool isInputEnergyGood( const std::string& aInputName, const std::string& aRegionName );
    static bool isInputPrimaryEnergyGood( const std::string& aInputName, const std::string& aRegionName );
    static bool isInputSecondaryEnergyGood( const std::string& aInputName, const std::string& aRegionName );
    static double getMarketConversionFactor( const std::string& aInputName, const std::string& aRegionName );
protected:
    enum Type {
        ENERGY,
        MATERIAL,
        LAND,
        LABOR,
        CAPITAL,
        NOT_SET
    };
    std::string mName; //!< Name of Input
    Value mCoefficient; //!< Coefficient for production or demand function
    
    //! Read in demand currency.
    Value mInitialDemandCurrency;

    //! Currency demand by period.
    objects::PeriodVector<Value> mDemandCurrency;

    //! Price paid for input, adjusted from market price
    objects::PeriodVector<Value> mPricePaid;

    mutable Value mConversionFactor; //!< Conversion Factor
    //! Cached GHG coefficient, this will have to be fixed for multiple gases.
    mutable Value mGHGCoefficient;

    Value mPriceAdjustFactor; //!< Price adjustment factor

    Value mTechnicalChange; //!< Technical Change

    bool mIsNumeraire; //!< Whether the input is the numeraire input.
    bool mIsCapital; //!< Whether the input is capital.

    //! Whether the currency demand represents fixed trade in the good.
    bool mIsFixedTrade;

    virtual const std::string& getXMLName() const = 0;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ) = 0;
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const = 0;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const = 0;
    Type getType( const std::string& aRegionName ) const;
private:
};

/*! \brief Return whether the input is Capital.
* \author Josh Lurz
* \return Whether the input is capital.
*/
bool Input::isCapital() const {
    return mIsCapital;
}

/*! \brief Return whether the input is the numeraire.
* \author Josh Lurz, Sonny Kim
* \return Whether the input is the numeraire.
*/
bool Input::isNumeraire() const {
    return mIsNumeraire;
}

#endif // _INPUT_H_
