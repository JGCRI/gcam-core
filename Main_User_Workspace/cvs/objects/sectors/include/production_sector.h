#ifndef _PRODUCTION_SECTOR_H_
#define _PRODUCTION_SECTOR_H_
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
* \file production_sector.h
* \ingroup Objects
* \brief The ProductionSector class header file.
* \author Sonny Kim
*/
#include <string>
#include <memory>
#include <map>
#include "sectors/include/sector.h"
// There are some missing includes here!
class IInvestor;
class Demographic;
class NationalAccount;
class GDP;
class IInfo;
class DependencyFinder;

/*! 
* \ingroup Objects
* \brief This class represents a single production sector.
* \details TODO
* \author Sonny Kim
*/
class ProductionSector: public Sector
{

public:
    explicit ProductionSector ( const std::string& aRegionName );
    virtual ~ProductionSector();
    void calcFinalSupplyPrice( const GDP* aGDP, const int aPeriod ){};
    void supply( const GDP* aGDP, const int aPeriod ){};
    static const std::string& getXMLNameStatic();
    
    virtual void completeInit( const IInfo* aRegionInfo,
                               DependencyFinder* aDepFinder,
                               ILandAllocator* aLandAllocator,
                               const GlobalTechnologyDatabase* aGlobalTechDB );

    double getOutput( const int aPeriod ) const;
    
    virtual void initCalc( NationalAccount* aNationalAccount,
                           const Demographic* aDemographics,
                           const int aPeriod );

    virtual void tabulateFixedDemands( const int period, const GDP* aGDP ) {}

    virtual void setCalSuppliesAndDemands( const int aPeriod ) const {}

    virtual void operate( NationalAccount& aNationalAccount, const Demographic* aDemographic, const int aPeriod ); // Passing demographic here is not good.
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;

    virtual void dbOutput( const GDP* aGDP,
                           const IndirectEmissionsCalculator* aIndEmissCalc ) const {}
protected:
    std::map<std::string,double> ghgEmissCoefMap; //! Map of ghg name to emission coefficient
    void setMarket();

    virtual double getPrice( const GDP* aGDP,
                             const int aPeriod ) const;

    virtual const std::string& getXMLName() const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ); 
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
private:
    //! Vector of read-in prices for the production sector which will be used as
    //! fixed prices if mIsFixedPrice is true.
    std::vector<double> mFixedPrices;

    //! The market region into which the sector is exporting.
    std::string mMarketName;

    //! Whether this sector is on a fixed price path.
    bool mIsFixedPrice;

    //! If the sector has an energy product.
    bool mIsEnergyGood;

    //! If the sector has a primary energy product.
    bool mIsPrimaryEnergyGood;

    //! If the sector has a secondary energy product.
    bool mIsSecondaryEnergyGood;

    //! Object responsible for determining levels of investment for this sector
    //! in its various technologies. Different types of investment objects may
    //! be read in to change the investment behavior.
    std::auto_ptr<IInvestor> mInvestor;
    
    void calcInvestment( const Demographic* aDemographic, NationalAccount& aNationalAccount, const int period );
    void operateOldCapital( const Demographic* aDemographic, NationalAccount& aNationalAccount, const int period );
    void operateNewCapital( const Demographic* aDemographic, NationalAccount& aNationalAccount, const int period );
    void calcPriceReceived( const int period );
};

#endif // _PRODUCTION_SECTOR_H_
