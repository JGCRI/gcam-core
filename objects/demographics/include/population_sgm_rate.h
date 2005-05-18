#ifndef _POPULATION_SGM_RATE_H_
#define _POPULATION_SGM_RATE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file population_sgm_rate.h
* \ingroup Objects-SGM
* \brief The PopulationSGMRate class header file.
* \author Sonny Kim
* \author Katherine Chung
* \date $Date$
* \version $Revision$
*/

#include <vector>

#include "demographics/include/population.h"

// forward class declaration
class AgeCohort;

/*! 
* \ingroup Objects-SGM
* \brief An object which contains the PopulationSGMRate information for a region.
* \details PopulationSGMRate reads in population information for the first year
*  then uses survival rates, etc, to calculate the populations for future years.
*/

class PopulationSGMRate : public Population
{
public:
	PopulationSGMRate();
	~PopulationSGMRate();

    virtual void completeInit( const std::vector<double>& femalePopFromPrev = std::vector<double>(),
        const std::vector<double>& malePopFromPrev = std::vector<double>() );
	
    virtual void initCalc();
	static const std::string& getXMLNameStatic();
	
    const std::vector<double> getSurvMalePop() const;
    const std::vector<double> getSurvFemalePop() const;
    
	double getWorkingAgePop() const;
	double getWorkingAgePopMale() const;
	double getWorkingAgePopFemale() const;

	void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
protected:
    std::vector<AgeCohort*> ageCohort; //!< array of age cohorts
	typedef std::vector<AgeCohort*>::iterator AgeCohortIterator;
    typedef std::vector<AgeCohort*>::const_iterator CAgeCohortIterator;
	static const std::string XML_NAME; //!< node name for toXML methods

	virtual const std::string& getXMLName() const;
    virtual bool XMLDerivedClassParse( const std::string &nodeName, const xercesc::DOMNode* curr );
	virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
	virtual void toDebugXMLDerived( std::ostream& out, Tabs* tabs ) const;
private:
    void fillMalePop( const std::vector<double>& aMalePop );
    void fillFemalePop( const std::vector<double>& aFemalePop );
    void calcPop();
    void clear();
};

#endif // _POPULATION_SGM_RATE_H_

