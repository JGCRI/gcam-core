#ifndef _FOREST_DEMAND_SECTOR_H_
#define _FOREST_DEMAND_SECTOR_H_
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
* \file forest_demand_sector.h
* \ingroup Objects
* \brief The ForestDemandSector class header file.
* \author James Blackwood
*/

#include "sectors/include/energy_final_demand.h"

// Forward declarations
class GDP;

class ForestDemandSector: public EnergyFinalDemand {
friend class ForestSupplySector; // allow ForestSupplySector to access future forest prefix. Prevents defining  this in two places.
public:
    ForestDemandSector();
    static const std::string& getXMLNameStatic();
    
    virtual void completeInit( const std::string& aRegionName,
                               const IInfo* aRegionInfo );

    virtual void initCalc( const std::string& aRegionName,
                           const GDP* aGDP,
                           const Demographic* aDemographics,
                           const int aPeriod );
    
    virtual void setFinalDemand( const std::string& aRegionName,
                             const Demographic* aDemographics,
                             const GDP* aGDP,
                             const int aPeriod );

    class PerCapitaNotAdjGDPDemandFunction: public IDemandFunction {
    public:
        virtual bool isPerCapitaBased() const {
            // This object overrides with its own per capita
            // based demand function regardless of this boolean.
            // Setting it to false prevents writing out
            // boolean to output.xml.
            return false;
        }

        virtual double calcDemand( const Demographic* aDemographics,
                                   const GDP* aGDP,
                                   const double aPriceElasticity,
                                   const double aIncomeElasticity,
                                   const double aPriceRatio,
                                   const int aPeriod ) const;
    };

protected:
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ); 
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual const std::string& getXMLName() const;

    virtual void scaleCalibratedValues( const std::string& aFuelName,
                                        const double aScaleValue,
                                        const int aPeriod );
    double calcFutureForestDemand(  const std::string& aRegionName,
                                    const Demographic* aDemographics,
                                    const GDP* aGDP,
                                    std::string& aMarketName,
                                    const int aPeriod,
                                    std::string& aBaseMarketName, 
                                    const int aBasePeriod );

private:
    int mRotationPeriod;
    static const std::string& futureMarketPrefix();

    //! Save future demand so can be written out for debugging
    double mFutureForestDemand; 
};

#endif // _FOREST_DEMAND_SECTOR_H_

