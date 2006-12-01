#ifndef _REGION_MINICAM_H_
#define _REGION_MINICAM_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
* \file region_minicam.h
* \ingroup Objects
* \brief The RegionMiniCAM class header file.
* \author Sonny Kim
*/

#include <map>
#include <vector>
#include <memory>
#include <string>
#include <list>
#include <xercesc/dom/DOMNode.hpp>

#include "containers/include/region.h"
#include "util/base/include/ivisitable.h"
#include "util/base/include/iround_trippable.h"

// Forward declarations.
class Population;
class Demographic;
class Resource;
class Sector;
class SupplySector;
class DemandSector;
class AgSector;
class ILandAllocator;
class GHGPolicy;
class Summary;
class ILogger;
class GDP;
class Curve;
class DependencyFinder;
class TotalSectorEmissions;
class IInfo;
class GlobalTechnologyDatabase;

#define SORT_TESTING 0

/*! 
* \ingroup Objects
* \brief This class defines a single region of the model and contains other
*        regional information such as demographics, resources, supply and demand
*        sectors, and GDPs. The classes contained in the Region are the
*        Populations, Resource, Sector, DemandSector.  Since this particular
*        implementation of the model is based on a partial equilibrium concept,
*        it is not mandatory to instantiate all of these classes.  The region
*        can contain just one of these objects or any combination of each of
*        these objects.  The demand sector object, however, requires Populations
*        information to drive the demand for goods and services.  An agriculture
*        class is included in the Region class, but the agriculture class is an
*        interface to the Fortran based AGLU module. The Region class also
*        contains the GhgMarket class which is instantiated only when a market
*        for ghg emissions is needed. Member functions of the Region class call
*        functions of contained objects and trigger a series of events cascading
*        down to the lowest set of classes.  The sequences of calls to other
*        functions of the contained objects are likely to important in each of
*        these member functions. 
*
* \author Sonny Kim
*/

class RegionMiniCAM: public Region
{
    friend class InputOutputTable;
    friend class SocialAccountingMatrix;
    friend class DemandComponentsTable;
    friend class SectorReport;
    friend class SGMGenTable;
    friend class XMLDBOutputter;
public:
    RegionMiniCAM();
    virtual ~RegionMiniCAM();
    static const std::string& getXMLNameStatic();
    virtual void completeInit( const GlobalTechnologyDatabase* aGlobalTechDB );
    virtual void calc( const int period, const bool doCalibrations );
    virtual void initCalc( const int period );

    virtual void csvOutputFile() const;
    virtual void dbOutput( const std::list<std::string>& aPrimaryFuelList ) const;
    virtual void initializeAgMarketPrices( const std::vector<double>& pricesIn );
    virtual void updateSummary( const std::list<std::string>& aPrimaryFuelList, const int period );
    virtual const Summary& getSummary( const int period ) const;

    virtual bool isAllCalibrated( const int period, double calAccuracy, const bool printWarnings ) const;
    virtual void setCalSuppliesAndDemands( const int period );
    virtual void initializeCalValues( const int period );
    virtual bool setImpliedCalInputs( const int period );
    virtual int scaleCalInputs( const int period );
    virtual void updateAllOutputContainers( const int period );
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    std::auto_ptr<GDP> gdp; //!< GDP object.
    std::auto_ptr<AgSector> agSector; //!< Agricultural sector

    //! Regional land allocator.
    std::auto_ptr<ILandAllocator> mLandAllocator;

    //! The region's information store.
    std::auto_ptr<IInfo> mRegionInfo;

    //! Map of fuel relationships used for calibration consistency adjustments
    typedef std::map<std::string, std::vector<std::string> > FuelRelationshipMap;
    std::auto_ptr<FuelRelationshipMap> fuelRelationshipMap;
    std::vector<Resource*> resources; //!< vector of pointers toresource objects
    std::vector<DemandSector*> demandSector; //!< vector of pointers to demand sector objects

    //! Container of objects which calculate an aggregate emissions coefficient
    //! for a set of sectors.
    std::vector<TotalSectorEmissions*> mAggEmissionsCalculators;

    std::vector<double> calibrationGDPs; //!< GDPs to calibrate to
    std::vector<double> GDPcalPerCapita; //!< GDP per capita to calibrate to

    //! Aggregate price for demand services.
    std::vector<double> mEnergyServicePrice;

    std::vector<double> carbonTaxPaid; //!< total regional carbon taxes paid
    std::vector<double> TFEcalb;  //!< Total Final Energy Calibration value (cannot be equal to 0)
    std::vector<double> TFEPerCapcalb;  //!< Total Final Energy per Capita Calibration GJ/cap (cannot be equal to 0)
#if SORT_TESTING
    std::vector<std::string> sectorOrderList; //!< A vector listing the order in which to process the sectors.
#endif
    std::vector<Summary> summary; //!< summary values and totals for reporting
    std::map<std::string,int> supplySectorNameMap; //!< Map of supplysector name to integer position in vector.
    std::map<std::string, double> primaryFuelCO2Coef; //!< map of CO2 emissions coefficient for primary fuels only
    double heatingDegreeDays; //!< heatingDegreeDays for this region (used to drive heating/cooling demands -- to be replaced in the future with specific set points)
    double coolingDegreeDays; //!< coolingDegreeDays for this region (used to drive heating/cooling demands -- to be replaced in the future with specific set points)

    //! The rotation period of forests in this region in years.
    // TODO: If this is in years it shouldn't be called rotation period.
    int mRotationPeriod;

    //! Interest rate for the region.
    double mInterestRate;

    typedef std::vector<DemandSector*>::iterator DemandSectorIterator;
    typedef std::vector<DemandSector*>::const_iterator CDemandSectorIterator;
    typedef std::vector<Resource*>::iterator ResourceIterator;
    typedef std::vector<Resource*>::const_iterator CResourceIterator;
    typedef std::vector<Sector*>::iterator SectorIterator;
    typedef std::vector<Sector*>::reverse_iterator SectorReverseIterator;
    typedef std::vector<Sector*>::const_iterator CSectorIterator;

    virtual const std::string& getXMLName() const;
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    bool isEnergyDemandAllCalibrated( const int period ) const;
    void initElementalMembers();
    void setupCalibrationMarkets();
    void checkData( const int period );
    double getTotFinalEnergy( const int period ) const;
    bool reorderSectors( const std::vector<std::string>& orderList );
    void calcGDP( const int period );
    void calcResourceSupply( const int period );
    void calcFinalSupplyPrice( const int period );
    void calcEndUsePrice( const int period );
    void adjustGDP( const int period );
    void calcEndUseDemand( const int period );
    void setFinalSupply( const int period );
    void calcAgSector( const int period );
    void calibrateRegion( const bool doCalibrations, const int period );
    double calcTFEscaleFactor( const int period ) const;
    const std::vector<double> calcFutureGDP() const;
    void calcEmissions( const int period );
    void calcEmissFuel( const std::list<std::string>& aPrimaryFuelList, const int period );
    void calcTotalCarbonTaxPaid( const int period );
    void setCO2CoefsIntoMarketplace( const int aPeriod );
    void calibrateTFE( const int period );
private:
    void clear();
    bool ensureGDP() const;
    bool ensureDemographics() const;
};

#endif // _REGION_H_

