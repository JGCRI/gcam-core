#ifndef _REGION_H_
#define _REGION_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file region.h
* \ingroup CIAM
* \brief The Region class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <map>
#include <vector>
#include <memory>
#include <string>

// Forward declarations.
class Population;
class Resource;
class SupplySector;
class DemandSector;
class AgSector;
class GHGPolicy;
class Summary;
class Emcoef_ind;
class ILogger;
class GDP;
class Curve;
class Tabs;

/*! 
* \ingroup CIAM
* \brief This class defines a single region of the model and contains other regional information such as demographics, resources, supply and demand sectors, and GDPs.
*
* The classes contained in the Region are the Populations, Resource, Sector, Demsector.  Since this particular implementation of the model is based on a partial equilibrium concept,
* it is not mandatory to instantiate all of these classes.  The region can contain just one of these objects or any combination of each of these objects.  The demand sector object, however, requires Populations
* information to drive the demand for goods and services.  An agriculture class is included in the Region class, but the agriculture class is an interface to the Fortran based AGLU module.
* The Region class also contains the GhgMarket class which is instantiated only when a market for ghg emissions is needed.
*
* Member functions of the Region class call functions of contained objects and trigger a series of events cascading down to the lowest set of classes.  The sequences of
* calls to other functions of the contained objects are likely to important in each of these member functions. 
*
* \author Sonny Kim
* \todo Change the way fixed carbon taxes are implemented by using the ghgMarket.  A market for the gas and the tax is created but market does not need to be solved.  
* The applycarbontax function can be removed once this is completed.
*/

class Region
{
public:
    Region();
    ~Region(); 
    void initElementalMembers();
    void XMLParse( const xercesc::DOMNode* node );
    void completeInit();
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    const std::string& getXMLName() const;
    static const std::string& getXMLNameStatic();
    std::string getName() const;
    void calc( const int period, const bool doCalibrations );
    bool isDemandAllCalibrated( const int period ) const;
    void calibrateTFE( const int period ); 
    void initCalc( const int period );
    void calcEmissions( const int period );
    void calcEmissFuel( const int period );
    void emissionInd( const int period );
    void calcTotalCarbonTaxPaid( const int period );
    void csvOutputFile() const;
    void dbOutput() const;
    void findSimul( const int period );
    void initializeAgMarketPrices( const std::vector<double>& pricesIn );
    void updateSummary( const int period );
    void printGraphs( std::ostream& outStream, const int period ) const;
    double getPrimaryFuelCO2Coef( const std::string& fuelName ) const;
    double getCarbonTaxCoef( const std::string& fuelName ) const;
    const Summary getSummary( const int period ) const;
    std::vector<std::string> getSectorDependencies( const std::string& sectorName ) const;
    void printSectorDependencies( ILogger& aLog ) const;
    void setFixedTaxes( const std::string& policyName, const std::string& marketName, const std::vector<double>& taxes );
    const Curve* getEmissionsQuantityCurve( const std::string& ghgName ) const;
    const Curve* getEmissionsPriceCurve( const std::string& ghgName ) const;
    void checkData( const int period );
    void setupCalibrationMarkets();
    bool isAllCalibrated( const int period, double calAccuracy, const bool printWarnings ) const;
    void setCalSuppliesAndDemands( const int period );
    bool setImpliedCalInputs( const int period );
    void scaleCalInputs( const int period );

private:
    const static std::string XML_NAME; //!< node name for toXML method.
    std::string name; //!< Region name
    std::auto_ptr<Population> population; //!< Population object
    std::auto_ptr<GDP> gdp; //!< GDP object.
    std::auto_ptr<AgSector> agSector; //!< Agricultural sector
    std::vector<Resource*> resources; //!< vector of pointers toresource objects
    std::vector<SupplySector*> supplySector; //!< vector of pointers to supply sector objects
    std::vector<DemandSector*> demandSector; //!< vector of pointers to demand sector objects
    std::vector<GHGPolicy*> mGhgPolicies; //!< vector of pointers to ghg market objects, container for constraints and emissions
    std::vector<double> iElasticity; //!< income elasticity
    std::vector<double> calibrationGDPs; //!< GDPs to calibrate to
    std::vector<double> GDPcalPerCapita; //!< GDP per capita to calibrate to
    std::vector<double> priceSer; //!< aggregate price for demand services
    std::vector<double> carbonTaxPaid; //!< total regional carbon taxes paid
    std::vector<double> TFEcalb;  //!< Total Final Energy Calibration value (cannot be equal to 0)
    std::vector<double> TFEPerCapcalb;  //!< Total Final Energy per Capita Calibration GJ/cap (cannot be equal to 0)
    std::vector<Summary> summary; //!< summary values and totals for reporting
    std::map<std::string,int> resourceNameMap; //!< Map of resource name to integer position in vector. 
    std::map<std::string,int> supplySectorNameMap; //!< Map of supplysector name to integer position in vector. 
    std::map<std::string,int> demandSectorNameMap; //!< Map of demandsector name to integer position in vector. 
    std::map<std::string,int> mGhgPoliciesNameMap; //!< Map of GhgPolicy name to integer position in vector. 
    std::vector<Emcoef_ind> emcoefInd; //!< vector of objects containing indirect emissions coefficients
    std::map<std::string, double> primaryFuelCO2Coef; //!< map of CO2 emissions coefficient for primary fuels only
    std::map<std::string, double> carbonTaxFuelCoef; //!< map of CO2 emissions coefficient for all fossil fuels
    bool anySupplyFixedOutput( const int sectorNumber ) const;
    void checkSectorCalData( const int period );
    void initializeCalValues( const int period );
    double getTotFinalEnergy( const int period ) const;
    std::vector<std::string> sectorOrderList; //!< A vector listing the order in which to process the sectors. 
    typedef std::vector<SupplySector*>::iterator SupplySectorIterator;
    typedef std::vector<SupplySector*>::const_iterator CSupplySectorIterator;
    typedef std::vector<DemandSector*>::iterator DemandSectorIterator;
    typedef std::vector<DemandSector*>::const_iterator CDemandSectorIterator;
    typedef std::vector<Resource*>::iterator ResourceIterator;
    typedef std::vector<Resource*>::const_iterator CResourceIterator;
    typedef std::vector<GHGPolicy*>::iterator GHGPolicyIterator;
    typedef std::vector<GHGPolicy*>::const_iterator CGHGPolicyIterator;
    typedef std::map<std::string, std::vector<std::string> > FuelRelationshipMap; //!< map for fuel relationships
    std::auto_ptr<FuelRelationshipMap> fuelRelationshipMap; //!< ptr to map of fuel relationships used for calibration consistency adjustments
    void clear();
    bool reorderSectors( const std::vector<std::string>& orderList );
    bool sortSectorsByDependency();
    bool isRegionOrderedCorrectly() const;
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
};

#endif // _REGION_H_

