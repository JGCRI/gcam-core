#ifndef _POPULATION_H_
#define _POPULATION_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file population.h
* \ingroup CIAM
* \brief The demographic class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <xercesc/dom/DOM.hpp>

/*! 
* \ingroup CIAM
* \brief An object which contains the demographic information for a region.
* \author Sonny Kim
*/

class demographic
{
private:
    // vector of time period 
    // population has 1 more historical period 
    std::vector<double> malepop; //!< Total male population
    std::vector<double> femalepop; //!< Total female population.
    std::vector<double> totalpop; //!< Total population
    std::vector<double> laborprod; //!< labor productivity growth rate
    std::vector<double> laborforce_p; //!< labor force participation percent
    std::vector<double> laborforce; //!< actual labor force
public:
    demographic(); //default construtor
    void clear();
    void XMLParse( const xercesc::DOMNode* node );
    void toXML( std::ostream& out ) const;
    void toDebugXML( const int period, std::ostream& out ) const;
    void initData();
    // set size of population and labor productivity variables to max period
    double labor( const int per ) const; // return labor productivity growthrate
    double total( const int per ) const; // return total population
    const std::vector<double>& getTotalPopVec() const; 
    // return labor force (actual working)
    double getlaborforce( const int per ) const;
    void show( int per );
    // outputs to file
    void outputfile( const std::string& regname ); 
    // MiniCAM outputs to file
    void MCoutput( const std::string& regname ); 
    void setupCalibrationMarkets( const std::string& regionName );
    void writeBackCalibratedValues( const std::string& regionName, const int period );
    double getTotalLaborProductivity( const int period ) const;
};

#endif // _POPULATION_H_
