#ifndef _SUBSECTOR_H_
#define _SUBSECTOR_H_
#pragma once

/*! 
* \file subsector.h
* \ingroup CIAM
* \brief The subsector class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <string>
#include <map>

// xerces xml headers
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std;
using namespace xercesc;

// Forward declarations
class Summary;
class technology;
class hydro_tech;
class Emcoef_ind;

/*! 
* \ingroup CIAM
* \brief A class which defines a single Subsector of the model.
* \author Sonny Kim
*/

class subsector
{
protected:
    string name; //!< subsector name
    string unit; //!< unit of final product from subsector
    string fueltype; //!< each subsector has one fueltype
    int notech; //!< number of technologies in each subsector
    double tax; //!< subsector tax or subsidy
    double basesharewt; //! subsector base year consumption share weight
    vector<vector<technology*> > techs; //!< array of pointers to technology objects for each period
    vector<hydro_tech> hydro; //!< array of hydroelectricity by period
    vector<double> capLimit; //!< subsector capacity limit
    vector<double> shrwts; //!< subsector logit share weights
    vector<double> lexp; //!< subsector logit exponential
    vector<double> share; //!< subsector shares
    vector<double> input; //!< subsector energy input
    vector<double> pe_cons; //!< subsector primary energy consumption
    vector<double> subsectorprice; //!< subsector price for all periods
    vector<double> fuelprice; //! subsector fuel price only for all periods
    vector<double> output; //!< total amount of final output from subsector
    vector<double> carbontaxpaid; //!< total subsector carbon taxes paid
    vector<double> fuelPrefElasticity; //!< Fuel preference elasticity
    vector<double> calOutputValue; // Calibration value
    vector<bool> doCalibration; // Flag set if calibration value is read-in
    vector<bool> calibrationStatus; // Set true if sector or any tech is calibrated
    vector<Summary> summary; //!< summary for reporting
    map<string,int> techNameMap; //!< Map of technology name to integer position in vector. 
    void shareWeightScale( const int pmer ); // Consistantly adjust share weights
    
public:
    subsector();
    virtual ~subsector();
    virtual void clear();
    const string getName() const;
    void XMLParse( const DOMNode* tempNode );
    virtual void XMLDerivedClassParse( const string nodeName, const DOMNode* curr ); // for derived classes
    void completeInit();
    void toXML( ostream& out ) const;
    virtual void toOutputXML( ostream& out ) const;
    void toDebugXML( const int period, ostream& out ) const;
    void copytolast( const int period );
    virtual void calcPrice( const string regionName, const int period); // maw
    double getPrice( const int period ) const;
    void initCalc( const int per ); // Consistantly adjust share weights
    bool getCalibrationStatus( const int period ) const;
    void setCalibrationStatus( const int period );
    void scaleCalibrationInput( const int period, const double scaleFactor ); // scale calibration values
    double getfuelprice( const int period ) const; 
    double getwtfuelprice( const int period ) const;
    double getCapacityLimit( const int period ) const;
    void applycarbontax( const string& regionName, const double tax, const int period );
    void addghgtax( const string& ghgname, const string& regionName, const int period ); 
    virtual void calcShare( const string& regionName, const int period, const double gnp_cap = 1 ); 
    void shareWeightInterp( const int beginPeriod,  const int endPeriod );
    void normShare( const double sum, const int period );
    void limitShares( const double sum, const int period );
    // maw compute tech shares within subsector in seperate method
    void calcTechShares ( const string& regionName, const int period );
    // sets demand to output and output
    virtual void setoutput( const string& regionName, const string& prodName, const double dmd, const int period ); 
    void sumoutput( const int period );
    // calculates exogenous supply
    double exogSupply( const int period );
    void scaleFixedSupply( const double scaleRatio, const int per );
    double getFixedSupply( const int per ) const;
    void resetFixedSupply( const int per );
    void show_subsec() const;
    double getShare( const int period ) const;
    double getTotalCalOutputs( const int period ) const;
    void showtechs( const int period, const string ofile ) const;
    void showlabel( const string& ofile ) const;
    void outputfile( const string& regionName, const string& sectorName) const; 
    void MCoutputA( const string& regionName, const string& sectorName ) const; 
    void MCoutputB( const string& regionName, const string& sectorName ) const; 
    void MCoutputC( const string& regionName, const string& sectorName ) const; 
    int shownotech() const;
    void emission( const int period, const string& productName );
    void indemission( const int period, const vector<Emcoef_ind>& emcoef_ind );
    double showCO2( const int period ) const;
    double showCO2ind( const int period ) const; // returns indirect CO2 emissions
    double showCO2fuel( const int period ) const; // returns equivalent CO2 emissions from fuel input
    double showpe_cons( const int period ); // returns subsector primary energy consumption
    double showinput( const int period )  const; // returns subsector primary or final energy consumption
    double getoutput( const int period ) const; // returns subsector output
    double showcarbontaxpaid( const int period ) const; // returns subsector total carbon taxes paid
    map<string, double> getfuelcons( const int period ) const; 
    void clearfuelcons( const int period);  //  clears the fuelcons map in summary
    map<string, double> getemission( const int period) const;// get ghg emissions map in summary object 
    map<string, double> getemfuelmap( const int period) const;// get ghg emissions map in summary object 
    map<string, double> getemindmap( const int period ) const;// get ghg emissions map in summary object 
    void adjShares( const double dmd, const double varSectorSharesTot, const double totalFixedSupply, const int period);
    void updateSummary(const int per);
    void adjustForCalibration( double sectorDemand, double totalFixedSupply, double totalCalOutputs, const int period ); // Do sub-sector calibration
};
#endif // _SUBSECTOR_H_
