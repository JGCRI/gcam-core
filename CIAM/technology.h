#ifndef _TECHNOLOGY_H_
#define _TECHNOLOGY_H_
#pragma once

/*! 
* \file technology.h
* \ingroup CIAM
* \brief The technology class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

// Standard Library headers.
#include <vector>
#include <map>
#include <string>

// xerces xml headers
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std;
using namespace xercesc;

// Forward declaration
class Ghg;
class Emcoef_ind;
/*! 
* \ingroup CIAM
* \brief This technology class is based on the MiniCAM description of technology.
* \author Sonny Kim
*/

class technology
{
protected:
    int year; //!< period year or vintage
    double shrwts; //!< logit share weight
    double eff; //!< energy intensity
    double necost; //!< all non-fuel costs (levelized)
    double fuelcost; //!< fuel cost only
    double techcost; //!< total cost of technology
    double tax; //!< utility tax
    double fMultiplier; //!< multiplier on fuel cost or price
    double pMultiplier; //!< multiplier on total cost or price
    double carbontax; //!< carbon tax in $/TC
    double carbontaxgj; //!< carbon tax in $/GJ
    double carbontaxpaid; //!< total carbon taxes paid
    double lexp; //!< logit exponential
    double share; //!< technology shares
    double input; //!< total fuel input (fossil and uranium)
    double output; //!< technology output
    double techchange;  //!< technical change in %/year
    double fixedSupply; //!< amount of fixed supply (>0) for this tech, exclusive of constraints
    double fixedOutputVal; //!< A fixed amount of output.
    string name; //!< technology name
    string unit; //!< unit of final product from technology
    string fuelname; //!< name of fuel used
    bool doCalibration; // Flag set if calibration value is read-in
    double calInputValue; // Calibration value
    vector<Ghg*> ghg; //!< suite of greenhouse gases
    map<string,double> emissmap; //!< map of ghg emissions
    map<string,double> emfuelmap; //!< map of ghg emissions implicit in fuel
    map<string,double> emindmap; //!< map of indirect ghg emissions
    
    // attributes for hydroelectricity only!
    double resource; //!< available hydro resource in energy units
    double A; //!< logit function shape parameter
    double B; //!< logit function shape parameter
    map<string,int> ghgNameMap; //!< Map of ghg name to integer position in vector. 
    
public:
    technology(); // default construtor
    technology( const technology& techIn ); // copy constructor.
    technology& operator=( const technology& techIn ); // assignment operator.
    virtual ~technology();
    virtual void clear();
    virtual void copy( const technology& techIn );
    void initElementalMembers();
    virtual void XMLParse( const DOMNode* tempnode ); // initialize technology with xml data
    virtual void XMLDerivedClassParse( const string nodeName, const DOMNode* curr ); // for derived classes
    void completeInit();
    virtual void toXML( ostream& out ) const;
    virtual void toDebugXML( const int period, ostream& out ) const;
    void applycarbontax( const string& regionName, const double tax); // apply carbon tax to appropriate technology
    // sets ghg tax to technologies
    void addghgtax( const string ghgname, const string regionName, const int per ); 
    // calculates fuel and total cost of technology
    void calcCost( const string regionName, const int per); 
    // uses logit function to calculate technology share
    void calcShare( const string regionName, const int per); 
    void normShare(double sum); // normalize technology share
    void calcFixedSupply(int per); // calculate fixed supply
    void resetFixedSupply(int per); // reset fixed supply to max value
    void adjShares(double subsecdmd, double subsecFixedSupply, double varShareTot, int per);
    void scaleFixedSupply(const double scaleRatio); // scale fixed supply
    // calculates fuel input and technology output
    virtual void production(const string& regionName,const string& prodName,double dmd,const int per);
    void emission( const string prodname); // calculates GHG emissions from technology
    void indemission( const vector<Emcoef_ind>& emcoef_ind ); // calculates indirect GHG emissions from technology use
    void printTech( const string& outFile = "" ) const; // write technology information to file or screen
    // ****** return names and values ******
    string getName() const; // return technology name
    string getFName() const; // return fuel name
    double getEff() const; // return fuel efficiency
    double getShare() const; // return normalized share
    bool getCalibrationStatus( ) const; // return true if technology has calibration value
    void scaleCalibrationInput( const double scaleFactor ); // scale calibration value
    double getCalibrationInput() const; // return calibration input value
    double getCalibrationOutput() const; // return calibration output value
    bool ouputFixed() const; // return calibration output value
    double getInput() const; // return fuel input amount
    double getOutput() const; // return technology output
    double getFuelcost() const; // return fuel cost only
    double getTechcost() const; // return total technology cost
    double getNecost() const; // return non-fuel cost
    double getCarbontax() const; // return carbon taxes in $/TC
    double getCarbontaxgj() const; // return carbon taxes in $/GJ
    double getCarbontaxpaid() const; // return carbon taxes paid
    double getCO2() const; // return actual CO2 emissions from technology
    map<string,double> getemissmap() const; // return map of all ghg emissions
    map<string,double> getemfuelmap() const; // return map of all ghg emissions
    map<string,double> getemindmap() const; // return map of all ghg emissions
    double get_emissmap_second( const string& str ) const; // return value for ghg
    double getlexp() const; // return logit exponential for the technology
    double getFixedSupply() const; // return fixed supply
    void setYear( const int yearIn );
};

/*! 
* \ingroup CIAM
* \brief Hydroelectricity technology class derived from the technology class.
* \author Sonny Kim
* \date $ Date $
* \version $ Revision $
*/
class hydro_tech : public technology
{
private:
    double resource; //!< available hydro resource in energy units
    double A; //!< logit function shape parameter
    double B; //!< logit function shape parameter
    
public:
    hydro_tech();
    void clear();
    void production(double dmd,int per); // calculates fuel input and technology output
};

#endif // _TECHNOLOGY_H_