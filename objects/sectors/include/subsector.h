#ifndef _SUBSECTOR_H_
#define _SUBSECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file subsector.h
* \ingroup CIAM
* \brief The subsector class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <map>
#include <xercesc/dom/DOMNode.hpp>

// Forward declarations
class Summary;
class technology;
class Emcoef_ind;
class Tabs;
class GDP;

/*! 
* \ingroup CIAM
* \brief A class which defines a single Subsector of the model.

* The subsector contains a group of technology objects, which produce or consume commodities in the marketplace. Each sub-sector has attributes such as share, share weight, logit expoential, fixed capacity, and capacity limits. 

* \author Sonny Kim, Steve Smith, Josh Lurz
*/

class Subsector
{
private:
    static const std::string XML_NAME; //!< node name for toXML methods
    void clear();
protected:
    bool debugChecking; //!< General toggle to turn on various checks
    std::string name; //!< subsector name
    std::string regionName; //!< region name
    std::string sectorName; //!< sector name
    std::string unit; //!< unit of final product from subsector
    std::string fueltype; //!< each subsector has one fueltype
    int notech; //!< number of technologies in each subsector
    int scaleYear; //!< year to scale share weights to after calibration
    double tax; //!< subsector tax or subsidy
    double basesharewt; //! subsector base year consumption share weight
    double CO2EmFactor; //! CO2 emissions factor, calculated based on fuel input and share
    std::vector<std::vector<technology*> > techs; //!< vector of technology by period
    std::vector<double> capLimit; //!< subsector capacity limit
    std::vector<bool> capLimited; //!< true if subsector has hit its capacity limit
    std::vector<double> fixedShare; //!< share of this sub-sector that is fixed capacity -- set in sector
    std::vector<double> shrwts; //!< subsector logit share weights
    std::vector<double> lexp; //!< subsector logit exponential
    std::vector<double> share; //!< subsector shares
    std::vector<double> input; //!< subsector energy input
    std::vector<double> pe_cons; //!< subsector primary energy consumption
    std::vector<double> subsectorprice; //!< subsector price for all periods
    std::vector<double> fuelprice; //! subsector fuel price only for all periods
    std::vector<double> output; //!< total amount of final output from subsector
    std::vector<double> carbontaxpaid; //!< total subsector carbon taxes paid
    std::vector<double> fuelPrefElasticity; //!< Fuel preference elasticity
    std::vector<double> calOutputValue; // Calibration value
    std::vector<bool> doCalibration; // Flag set if calibration value is read-in
    std::vector<bool> calibrationStatus; // Set true if sector or any tech is calibrated
    std::vector<Summary> summary; //!< summary for reporting
    std::map<std::string,int> techNameMap; //!< Map of technology name to integer position in vector. 
    void interpolateShareWeights( const int period ); // Consistantly adjust share weights
    void sumOutput( const int period );
    void shareWeightLinearInterpFn( const int beginPeriod,  const int endPeriod );
    bool techHasInput( const technology* thisTech, const std::string& goodName ) const;
    virtual void MCDerivedClassOutput() const;
    virtual void csvDerivedClassOutput() const;
    virtual bool XMLDerivedClassParse( const std::string nodeName, const xercesc::DOMNode* curr );
    virtual const std::string& getXMLName() const;
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const{};
    virtual void toOutputXMLDerived( std::ostream& out, Tabs* tabs ) const{};
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const{};
    void normalizeTechShareWeights( const int period );
    void techShareWeightLinearInterpFn( const int beginPeriod,  const int endPeriod );
public:
    Subsector( const std::string regionName, const std::string sectorName );
    virtual ~Subsector();
    static double capLimitTransform( double capLimit, double orgShare ); 
    const std::string getName() const;
    void XMLParse( const xercesc::DOMNode* tempNode );
    void completeInit();
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    virtual void toOutputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    static const std::string& getXMLNameStatic();
    virtual void calcPrice( const int period );
    double getPrice( const int period ) const;
    double getCO2EmFactor(int period) const;
    virtual void initCalc( const int period );
    virtual void checkSubSectorCalData( const int period );
    bool getCalibrationStatus( const int period ) const;
    void setCalibrationStatus( const int period );
    void scaleCalibrationInput( const int period, const double scaleFactor );
    bool allOuputFixed( const int period ) const;
    double getFixedShare( const int period ) const;
    void setFixedShare( const int period, const double share );
    void setShareToFixedValue( const int period );
    double getfuelprice( const int period ) const; 
    double getwtfuelprice( const int period ) const;
    double getCapacityLimit( const int period ) const;
    virtual void calcShare( const int period, const GDP* gdp ); 
    void setShare( const double shareVal, const int period );
    void normShare( const double sum, const int period );
    double getShare( const int period ) const;
    double getShareWeight( const int period ) const;
    void scaleShareWeight( const double scaleValue, const int period );
    void limitShares( const double sum, const int period );
    void setCapLimitStatus( const bool value, const int period );
    bool getCapLimitStatus( const int period ) const;
    void calcTechShares ( const GDP* gdp, const int period );
    virtual void setoutput( const double demand, const int period, const GDP* gdp ); 
    double exogSupply( const int period );
    bool inputsAllFixed( const int period, const std::string& goodName ) const;
    void scalefixedOutput( const double scaleRatio, const int period );
    double getFixedOutput( const int period ) const;
    void resetfixedOutput( const int period );
    double getTotalCalOutputs( const int period ) const;
    double getCalAndFixedInputs( const int period, const std::string& goodName, const bool bothVals ) const;
    void csvOutputFile() const; 
    void MCoutputSupplySector() const; 
    void MCoutputDemandSector() const; 
    void MCoutputAllSectors() const; 
    void emission( const int period );
    void indemission( const int period, const std::vector<Emcoef_ind>& emcoef_ind );
    double getInput( const int period )  const;
    double getOutput( const int period );
    double getTotalCarbonTaxPaid( const int period ) const;
    std::map<std::string, double> getfuelcons( const int period ) const; 
    void clearfuelcons( const int period );
    std::map<std::string, double> getemission( const int period ) const;
    std::map<std::string, double> getemfuelmap( const int period ) const; 
    std::map<std::string, double> getemindmap( const int period ) const;
    void adjShares( const double demand, const double shareRatio, const double totalfixedOutput, const int period );
    void updateSummary( const int period );
    void adjustForCalibration( double sectorDemand, double totalfixedOutput, double totalCalOutputs, const bool allFixedOutput, const int period );
    void scaleCalibratedValues( const int period, const std::string& goodName, const double scaleValue );
    int getNumberAvailTechs( const int period ) const;
};
#endif // _SUBSECTOR_H_
