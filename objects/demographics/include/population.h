#ifndef _POPULATION_H_
#define _POPULATION_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file population.h
* \ingroup CIAM
* \brief The Population class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <xercesc/dom/DOMNode.hpp>

/*! 
* \ingroup CIAM
* \brief An object which contains the Population information for a region.
* \author Sonny Kim
*/

class Population
{
public:
    Population(); //default construtor
    void clear();
    void XMLParse( const xercesc::DOMNode* node );
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
	const std::string& getXMLName() const;
	static const std::string& getXMLNameStatic();
    void initData();
    double getTotal( const int per, const bool isPopPeriod = false ) const;
    const std::vector<double>& getTotalPopVec() const; 
    void csvOutputFile( const std::string& regionName ) const; 
    void dbOutput( const std::string& regionName ) const; 
private:
    // vector of time period 
    // population has 1 more historical period 
    std::vector<double> malepop; //!< Total male population
    std::vector<double> femalepop; //!< Total female population.
    std::vector<double> totalpop; //!< Total population
	static const std::string XML_NAME; //!< node name for toXML methods
};

#endif // _POPULATION_H_
