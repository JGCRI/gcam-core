#ifndef _POPULATION_H_
#define _POPULATION_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file population.h
* \ingroup Objects
* \brief The Population class header file.
* \author Sonny Kim
* \author Katherine Chung
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <string>
#include <xercesc/dom/DOMNode.hpp>

class Tabs;

/*! 
* \ingroup Objects
* \brief An object which contains the Population information for a region.
* \details This is the base Population class. PopulationSGMRate, PopulationSGMFixed, and
*  populationMiniCAM derive from it. They all share a totalPopulation value and year.
*/

class Population
{
public:
	Population();
	virtual ~Population();
	void XMLParse( const xercesc::DOMNode* node );
	void toInputXML( std::ostream& out, Tabs* tabs ) const;
	void toDebugXML( std::ostream& out, Tabs* tabs ) const;

    virtual void completeInit( const std::vector<double>& femalePopFromPrev = std::vector<double>(), const std::vector<double>& malePopFromPrev = std::vector<double>() ) = 0;
    virtual const std::vector<double> getSurvMalePop() const = 0; // TEMP
    virtual const std::vector<double> getSurvFemalePop() const = 0; // TEMP
	virtual void initCalc() = 0;
	virtual void csvSGMOutputFile( std::ostream& aFile, const int period ) const = 0;

	double getTotal() const;
	int getYear() const;
	const std::string getName() const;
	virtual double getWorkingAgePop() const = 0;
	virtual double getWorkingAgePopMale() const = 0;
	virtual double getWorkingAgePopFemale() const = 0;

protected:
	int mYear; //!< year
	double mTotalPop; //!< total population for this year
    int mWorkingAgeMin; //!< minimum working age.
    int mWorkingAgeMax; //!< maximum working age.

	virtual const std::string& getXMLName() const = 0;
	virtual bool XMLDerivedClassParse( const std::string &nodeName, const xercesc::DOMNode* curr ) = 0;
	virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const = 0;
	virtual void toDebugXMLDerived( std::ostream& out, Tabs* tabs ) const = 0;
private:
    const static int WORKING_AGE_MIN_DEFAULT = 15; //!< Default minimum working age.
    const static int WORKING_AGE_MAX_DEFAULT = 65; //!< Default maximum working age.
};

#endif // _POPULATION_H_


