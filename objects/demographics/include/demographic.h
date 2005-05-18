#ifndef _DEMOGRAPHIC_H_
#define _DEMOGRAPHIC_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file demographic.h
* \ingroup Objects
* \brief The Demographic class header file.
* \author Sonny Kim
* \author Katherine Chung
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <map>

//forward declare class
class Population;
class Tabs;
class OutputContainer;

/*! 
* \ingroup Objects
* \brief Demographics model that calculates population by gender and age cohort.
*/

class Demographic {
    friend class SGMGenTable;
public:
    Demographic();
	~Demographic();

	void XMLParse( const xercesc::DOMNode* node );
	void toInputXML( std::ostream& out, Tabs* tabs ) const;
	void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
	void completeInit();
	void initCalc();

	static const std::string& getXMLNameStatic();

    void calcPop();
   
	double getWorkingAgePopulation( const int period ) const;
	double getWorkingAgePopulationMales( const int per ) const;
	double getWorkingAgePopulationFemales( const int per ) const; 
    double getTotal( const int per ) const;
	const std::vector<double> getTotalPopVec() const;

	void csvOutputFile( const std::string& regionName ) const; 
	void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
    void dbOutput( const std::string& regionName ) const; 
    void updateOutputContainer( OutputContainer* outputContainer, const int aPeriod ) const;

private:
    void clear();
    int convertPeriodToPopulationIndex( int aPeriod ) const;
    const std::string& getXMLName() const;
	std::vector<Population*> population; //!< array of pointers to Population objects
    std::map<std::string,int> yearToMapIndex; //!< Map of year to indice
    typedef std::map<std::string,int>::const_iterator CYearMapIterator;
	typedef std::vector<Population*>::iterator PopulationIterator;
	static const std::string XML_NAME; //!< node name for toXML methods
};

#endif // _DEMOGRAPHIC_H_

