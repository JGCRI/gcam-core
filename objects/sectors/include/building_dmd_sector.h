#ifndef _BUILDLING_DMD_SECTOR_H_
#define _BUILDLING_DMD_SECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Labratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

/*! 
 * \file building_dmd_sector.h
 * \ingroup Objects
 * \brief BuildingDemandSector class header file.
 * \author Steve Smith
 */

#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/demand_sector.h"
#include "util/base/include/value.h"

// Forward declarations
class GDP;
class NationalAccount;
class Demographics;
class IInfo;

/*! 
 * \ingroup Objects
 * \brief A class which defines a single building demand sector.
 * \details The building demand sector calculates the demand for building
 *          servicies in terms of square feet. The actual building service is
 *          supplied by a number of separate supply sectors. No energy is used
 *          directly by this sector or its technologies. The building model first
 *          calculates a per capita demand for the service, and then converts
 *          that to a total demand using the regional population. Service demand
 *          per capita is calculated as:
 *
 *          \f[ D_{per capita} = \gamma * \frac{i^{\alpha}}{1 + ({\frac{i}{Xc}})^\beta} \f]
 *
 *          where \n
 *          \f$D\f$ is the per capita demand for square footage. \n
 *          \f$\gamma\f$ is the calibrated base scaler. \n
 *          \f$i\f$ is the MER per capita GDP. \n
 *          \f$\alpha\f$ is the income elasticity. \n
 *          \f$\beta\f$ is the saturation elasticity. \n
 *          \f$Xc\f$ is the saturation control point. \n
 *
 *          <b>XML specification for BuildingDemandSector</b>
 *          - XML name: \c building-demand-sector
 *          - Contained by: Region
 *          - Parsing inherited from class: DemandSector
 *          - Attributes:
 *              - \c name DemandSector::mName
 *          - Elements:
 *              - \c base-service BuildingDemandSector::mBaseService
 *              - \c saturation-elasticity BuildingDemandSector::mSaturationElasticity
 *              - \c saturation-point BuildingDemandSector::mSaturationPoint
 *
 * \author Steve Smith, Josh Lurz
 */
class BuildingDemandSector: public DemandSector
{
public:
    static const std::string& getXMLNameStatic();

    explicit BuildingDemandSector( const std::string& aRegionName );

    ~BuildingDemandSector();

    virtual void completeInit( const IInfo* aRegionInfo,
                               DependencyFinder* aDepFinder,
                               ILandAllocator* aLandAllocator,
                               const GlobalTechnologyDatabase* aGlobalTechDB );

    virtual void initCalc( NationalAccount* aNationalAccount,
                           const Demographic* aDemographics,
                           const int aPeriod );

    virtual void calcAggregateDemand( const GDP* aGDP,
                                      const Demographic* aDemographics,
                                      const int aPeriod );
protected:
    bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ); 
    void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    const std::string& getXMLName() const;

    double getBaseScaler( const int aPeriod ) const;
    
    // TODO: This could be a vectror of Value if XMLHelper supported it.
    //! Per capita service for each period to which to calibrate.
    std::vector<double> mBaseService;
    
    //! Scaler for determing demand for future years.
    std::vector<Value> mBaseScaler;

    //! Elasticity for saturation.
    Value mSaturationElasticity;

    //! Saturation point of the demand function.
    Value mSaturationPoint;
};

#endif // _BUILDLING_DMD_SECTOR_H_
