#ifndef _REGION_H_
#define _REGION_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file Region.h
* \ingroup CIAM
* \brief The Region class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <map>

// Forward declarations.
class demographic;
class Resource;
class Sector;
class DemandSector;
class AgSector;
class ghg_mrk;
class Summary;
class Emcoef_ind;
class Logger;

/*! 
* \ingroup CIAM
* \brief This class defines a single region of the model and contains other regional information such as demographics, resources, supply and demand sectors, and GNPs.
*
* The classes contained in the Region are the Demographics, Resource, Sector, Demsector.  Since this particular implementation of the model is based on a partial equilibrium concept,
* it is not mandatory to instantiate all of these classes.  The region can contain just one of these objects or any combination of each of these objects.  The demand sector object, however, requires demographics
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
private:
    std::string name; //!< Region name
    int noGhg; //!< number of ghg for market solution in each region
    int numResources; //!< number of depletable resources in each region
    int noSSec; //!< number of supply sectors in each region
    int noDSec; //!< number of demand sectors in each region
    int noRegMrks; //!< number of markets in each region
    double EnergyGNPElas; //!< elasticity for energy price feedback on GNP
    demographic* population; //!< demographic object
    std::vector<Resource*> resources; //!< vector of pointers toresource objects
    std::vector<Sector*> supplySector; //!< vector of pointers to supply sector objects
    std::vector<DemandSector*> demandSector; //!< vector of pointers to demand sector objects
    AgSector* agSector; //!< Agricultural sector
    std::vector<ghg_mrk*> ghgMarket; //!< vector of pointers to ghg market objects, container for constraints and emissions
    std::vector<double> iElasticity; //!< income elasticity
    std::vector<double> gnpDol; //!< regional gross national product in dollar value
    std::vector<double> calibrationGNPs; //!< GNPs to calibrate to
    std::vector<double> gnp; //!< regional gross national product normalized
    std::vector<double> gnpAdj; //!< regional gross national product normalized and adjusted for energy
    std::vector<double> gnpCap; //!< regional gross national product per capita normalized 
    std::vector<double> input; //!< total fuel consumption in energy units
    std::vector<double> priceSer; //!< aggregate price for demand services
    std::vector<double> carbonTax; //!< regional carbon tax
    std::vector<double> carbonTaxPaid; //!< total regional carbon taxes paid
    std::vector<double> TFEcalb;  //!< Total Final Energy Calibration value (cannot be equal to 0)
    std::vector<Summary> summary; //!< summary values and totals for reporting
    std::map<std::string,int> resourceNameMap; //!< Map of resource name to integer position in vector. 
    std::map<std::string,int> supplySectorNameMap; //!< Map of supplysector name to integer position in vector. 
    std::map<std::string,int> demandSectorNameMap; //!< Map of demandsector name to integer position in vector. 
    std::map<std::string,int> ghgMarketNameMap; //!< Map of ghgmarket name to integer position in vector. 
    std::vector<Emcoef_ind> emcoefInd; //!< vector of objects containing indirect emissions coefficients
    std::map<std::string, double> primaryFuelCO2Coef; //!< map of CO2 emissions coefficient for primary fuels only
    std::map<std::string, double> carbonTaxFuelCoef; //!< map of CO2 emissions coefficient for all fossil fuels
    void clear();
public:
   Region();
    ~Region(); 
    void initElementalMembers();
    void XMLParse( const xercesc::DOMNode* node );
    void completeInit();
    void toXML( std::ostream& out ) const;
    void toDebugXML( const int period, std::ostream& out ) const;
    std::string getName() const;
    void writeBackCalibratedValues( const int period );
    void setupCalibrationMarkets();
    void calibrateRegion( const bool doCalibrations, const int period );
    bool isDemandAllCalibrated( const int period ) const;
    void calibrateTFE( const int period ); 
    void initCalc( const int period );
    void setGhgSupply( const int period );
    void setGhgDemand( const int period );
    void addGhgTax( const int period );
    void rscSupply( const int period );
    void finalSupplyPrc( const int period );
    void calcGnp( const int period );
    const std::vector<double> calcFutureGNP() const;
    void calcGNPlfp( const int period );
    void calcEnduseprice( const int period );
    void calcEndUsePrice( const int period );
    void adjustGnp( const int period );
    void enduseDemand( const int period );
    void finalSupply( const int period );
    void emission( const int period );
    void calcEmissFuel( const int period );
    void emissionInd( const int period );
    void applycarbontax( const int period );
    void outputFile() const;
    void MCoutput() const;
    void findSimul( const int period );
    void calcAgSector( const int period );
    void initializeAgMarketPrices( const std::vector<double>& pricesIn );
    void updateSummary( const int period );
    void printGraphs( std::ostream& outStream, const int period ) const;
    double getPrimaryFuelCO2Coef( const std::string& fuelName ) const;
    double getCarbonTaxCoef( const std::string& fuelName ) const;
    const Summary getSummary( const int period ) const;
    std::vector<std::string> getSectorDependencies( const std::string& sectorName ) const;
    void printSectorDependencies( Logger* logger ) const;
};

#endif // _REGION_H_

