#ifndef _WORLD_H_
#define _WORLD_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file world.h
* \ingroup Objects
* \brief The World class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <map>
#include <vector>
#include <xercesc/dom/DOMNode.hpp>

// Forward declarations
class Region;
class Logger;
class Curve;
class Tabs;

/*! 
* \ingroup Objects
* \brief A class which contains all the model's regions.
* \author Sonny Kim
*/

class World
{
public:
    World();
    ~World();
    void XMLParse( const xercesc::DOMNode* node );
    void completeInit();
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
	const std::string& getXMLName() const;
	static const std::string& getXMLNameStatic();
    void initCalc( const int period ); 
    void calc( const int period, const std::vector<std::string>& regionsToSolve = std::vector<std::string>( 0 ) );
    void updateSummary( const int period ); 
    void emiss_ind( const int period );
    void calculateEmissionsTotals();
    void csvOutputFile() const; 
    void dbOutput() const; 
    double getGHGEmissions( const std::string& ghgName, const int period ) const;
    const std::map<std::string,int> getOutputRegionMap() const;
    const std::vector<std::string> getRegionVector() const;
    void turnCalibrationsOn(); 
    void turnCalibrationsOff();
    bool getCalibrationSetting() const;
    void printGraphs( std::ostream& outStream, const int period ) const;
    const std::vector<std::string> getPrimaryFuelList() const;
    double getPrimaryFuelCO2Coef( const std::string& regionName, const std::string& fuelName ) const;
    double getCarbonTaxCoef( const std::string& regionName, const std::string& fuelName ) const;
    void printSectorDependencies( Logger* logger ) const;
    void setFixedTaxes( const std::string& policyName, const std::string& marketName, const std::vector<double> taxes, const std::vector<std::string>& regionsToSet = std::vector<std::string>( 0 ) );
    const std::map<const std::string, const Curve*> getEmissionsQuantityCurves( const std::string& ghgName ) const;
    const std::map<const std::string, const Curve*> getEmissionsPriceCurves( const std::string& ghgName ) const;
private:
    typedef std::vector<Region*>::iterator RegionIterator;
    typedef std::vector<Region*>::const_iterator ConstRegionIterator;
    std::map<std::string, int> regionNamesToNumbers; //!< Map of region name to indice. 
    std::vector<Region*> regions; //!< array of pointers to Region objects
    std::vector<std::map<std::string,double> > ghgs; //!< maps containing ghg emissions
    std::vector<std::string> primaryFuelList; //!< vector of names of primary fuels. 
    bool doCalibrations; //!< turn on or off calibration routines
	static const std::string XML_NAME; //!< node name for toXML methods
    
    void initAgLu(); 
    void clear();
    const std::vector<int> getRegionIndexesToCalculate( const std::vector<std::string>& regionsToSolve );
};

#endif // _WORLD_H_

