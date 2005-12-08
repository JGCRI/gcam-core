#ifndef _POPULATION_SGM_FIXED_H_
#define _POPULATION_SGM_FIXED_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file population_sgm_fixed.h
* \ingroup Objects-SGM
* \brief The PopulationSGMFixed class header file.
* \author Sonny Kim
* \author Katherine Chung
*/

#include <vector>

#include "demographics/include/population.h"

// forward class declaration
class AgeCohort;
class IVisitor;

/*! 
* \ingroup Objects-SGM
* \brief An object which contains the fixed population information for a region.
* \details PopulationSGMFixed calculates population by gender and age.
*
*/

class PopulationSGMFixed : public Population
{
public:
    PopulationSGMFixed();
    virtual ~PopulationSGMFixed();
    virtual void completeInit( const std::vector<double>& femalePopFromPrev = std::vector<double>(), 
        const std::vector<double>& malePopFromPrev = std::vector<double>() );
    virtual void initCalc();
    const std::vector<double> getSurvMalePop() const { return std::vector<double>(); } // TEMP
    const std::vector<double> getSurvFemalePop() const { return std::vector<double>(); } // TEMP

    static const std::string& getXMLNameStatic();

    double getWorkingAgePop() const;
    double getWorkingAgePopMale() const;
    double getWorkingAgePopFemale() const;

    void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    virtual const std::string& getXMLName() const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( std::ostream& out, Tabs* tabs ) const;

private:
    void calcPop();
    std::vector<AgeCohort*> ageCohort; //!< array of age cohort pointers
    typedef std::vector<AgeCohort*>::iterator AgeCohortIterator;
    typedef std::vector<AgeCohort*>::const_iterator CAgeCohortIterator;
    
    static const std::string XML_NAME; //!< node name for toXML methods
    void clear();
};

#endif // _POPULATION_SGM_FIXED_H_

