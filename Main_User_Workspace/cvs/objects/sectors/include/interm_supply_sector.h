#ifndef _INTERM_SUPPLY_SECTOR_H_
#define _INTERM_SUPPLY_SECTOR_H_
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
* \file interm_supply_sector.h
* \ingroup Objects
* \brief The Intermittent SupplySector class header file.
* \details Intended for computing and demanding a backup supply sector to add to
*          the supply of an intermittent resource.
* \author Marshall Wise
*/
#include <string>
#include "sectors/include/supply_sector.h"
class IInfo;
class DependencyFinder;

/*!
 * \ingroup Objects
 * \brief A SupplySector which consumes an intermittent resource and produces a
 *        non-intermittent good.
 * \details Intended for wind and solar. Takes an intermittent resource and
 *          determines the demand for supply from a back-up sector, especially
 *          if needed for electricity.
 *
 *          <b>XML specification for IntermittentSupplySector</b>
 *          - XML name: \c intermittent-supplysector
 *          - Contained by: Region
 *          - Parsing inherited from class: SupplySector
 *          - Elements:
 *              - \c electricity-reserve-margin mElectricityReserveMargin
 *              - \c average-grid-capacity-factor mAverageGridCapacityFactor
 *              - \c backup-capacity-factor mBackupCapacityFactor
 *              - \c backup-cost mBackupCost
 *
 * \author Marshall Wise
 * \todo Remove this class once all elements are moved.
 */
class IntermittentSupplySector: public SupplySector
{
public:
    explicit IntermittentSupplySector( const std::string& aRegionName );
    static const std::string& getXMLNameStatic();
    
    virtual void completeInit( const IInfo* aRegionInfo,
                               DependencyFinder* aDepFinder,
                               ILandAllocator* aLandAllocator,
                               const GlobalTechnologyDatabase* aGlobalTechDB );

    virtual void initCalc( NationalAccount* nationalAccount,
                           const Demographic* aDemographics,
                           const int aPeriod );
protected:
    bool XMLDerivedClassParse( const std::string& aNodeName, const xercesc::DOMNode* aCurr );
    void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    
	const std::string& getXMLName() const;
    
    //! Name of the electricity sector for the trial market.
    std::string mElectricSectorName; 

    // TODO: Read this in the electricity sector.
	//! Electricity reserve margin for regional electricity sector.
    double mElectricityReserveMargin;
    
    // TODO: Read this in the electricity sector.
	//! Average capacity factor of total electric system to convert to total
    //! grid capacity.
    double mAverageGridCapacityFactor;
    
    // TODO: Read this in the backup electricity sector.
	//! Capacity factor for backup capacity (to convert energy output to
    //! capacity.
    double mBackupCapacityFactor;
    
    // TODO: Make a non-energy input.
	//! Resource backup cost in 1975 $/kW/yr(value is and should be annualized)
    double mBackupCost;
};

#endif // _INTERM_SUPPLY_SECTOR_H_
