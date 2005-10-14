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
#include <memory>
#include <xercesc/dom/DOMNode.hpp>

// Forward declarations
class Region;
class ILogger;
class Curve;
class Tabs;
class CalcCounter;
class IClimateModel;
namespace objects {
	class Atom;
}
template <class T, class U> class HashMap;

/*! 
* \ingroup Objects
* \brief A class which contains all the model's regions.  These regions may be MiniCAM (partial
* eqilibrium) regions or SGM (general equilibrium) regions as they are derived from the Region
* base class.
*
* The World class object is contained by the Scenario class object.  The world object controls
* the calling of the regions which it has been told to solve (passed in an argument of the
* method world.calc()) by calling region.calc() to run the model for one iteration for these
* regions.
*
* The world object includes a switch for running the model in calibration mode, methods for 
* determining the chain of sector dependecies (necessary for solving partial equilibrium model),
* and the methods for setting global fixed GHG taxes.
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

    //! The type of the vector containing region atoms.
    typedef std::vector<const objects::Atom*> AtomVector;
	void calc( const int period, const AtomVector& aRegionsToCalc = AtomVector() );
    
    void updateSummary( const int period ); 
    void emiss_ind( const int period );
    void runClimateModel();
    void csvOutputFile() const; 
    void dbOutput() const; 
    const std::map<std::string,int> getOutputRegionMap() const;
	const AtomVector getRegionIDs() const;
    void turnCalibrationsOn(); 
    void turnCalibrationsOff();
    bool getCalibrationSetting() const;
    bool isAllCalibrated( const int period, double calAccuracy, const bool printWarnings ) const;
    void printGraphs( std::ostream& outStream, const int period ) const;
    const std::vector<std::string> getPrimaryFuelList() const;
    void setFixedTaxes( const std::string& policyName, const std::string& marketName, const std::vector<double> taxes, const std::vector<std::string>& regionsToSet = std::vector<std::string>( 0 ) );
    const std::map<const std::string, const Curve*> getEmissionsQuantityCurves( const std::string& ghgName ) const;
    const std::map<const std::string, const Curve*> getEmissionsPriceCurves( const std::string& ghgName ) const;
    void setCalcCounter( CalcCounter* calcCounter );
    void finalizePeriod( const int aPeriod );	
    void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
    void csvSGMGenFile( std::ostream& aFile, const int aPeriod ) const;
private:
    //! The type of an iterator over the Region vector.
    typedef std::vector<Region*>::iterator RegionIterator;

    //! The type of a constant iterator over the Region vector.
    typedef std::vector<Region*>::const_iterator CRegionIterator;

	//! The type of the fast Region lookup hashmap.
	typedef HashMap<const objects::Atom*, unsigned int> FastRegionLookupMap;
	
    //! A fast hashmap which stores a mapping of region ID atom to region
    //! location. This allows world calc calls for derivatives to be faster.
	std::auto_ptr<FastRegionLookupMap> mRegionLookupMap;

    std::map<std::string, int> regionNamesToNumbers; //!< Map of region name to indice used for XML parsing.
    std::vector<Region*> regions; //!< array of pointers to Region objects
    std::vector<std::string> primaryFuelList; //!< vector of names of primary fuels.
    std::auto_ptr<IClimateModel> mClimateModel; //!< The climate model.
    bool doCalibrations; //!< turn on or off calibration routines
    CalcCounter* calcCounter;
	static const std::string XML_NAME; //!< node name for toXML methods
    void initAgLu(); 
    void clear();

	const std::vector<unsigned int> getRegionIndexesToCalculate( const AtomVector& aRegionsToCalc );
	void createFastLookupMap();
    void csvGlobalDataFile() const; 
    bool checkCalConsistancy( const int period );
};

#endif // _WORLD_H_
