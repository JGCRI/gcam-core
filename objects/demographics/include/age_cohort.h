#ifndef _AGE_COHORT_H_
#define _AGE_COHORT_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file age_cohort.h
* \ingroup Objects-SGM
* \brief The AgeCohort class header file.
* \author Sonny Kim
* \author Katherine Chung
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include <memory>

// forward declare headers
class Male;
class Female;
class Gender;
class Tabs;

/*! 
* \ingroup Objects
* \brief A class which stores population information for a more specific age range
*/

class AgeCohort {
public:
	AgeCohort();
    ~AgeCohort();
	void XMLParse( const xercesc::DOMNode* node );
	void toInputXML( std::ostream& out, Tabs* tabs ) const;
	void toDebugXML( std::ostream& out, Tabs* tabs ) const;
	void completeInit();
	virtual void initCalc();
	
	static const std::string& getXMLNameStatic();

	double calcMaleBirth();
	double calcFemaleBirth();
	double calcSurvMalePop();
	double calcSurvFemalePop();

	void setMalePop( double aMalePopulation );
	void setFemalePop( double aFemalePopulation );
	double getMalePop() const;
	double getFemalePop() const;
	const std::string& getAgeGroup() const;
    int getLowerAgeBound() const;
    int getUpperAgeBound() const;
protected:
	virtual const std::string& getXMLName() const;
	static const std::string XML_NAME; //!< node name for toXML methods

	std::string ageGroup; //!< age group name
	std::auto_ptr<Male> male; //!< male gender object
	std::auto_ptr<Female> female; //!< male gender object
private:
    bool parseGender( xercesc::DOMNode* aNode );
    mutable int mLowerAgeBound;
    mutable int mUpperAgeBound;
};

#endif // _AGE_COHORT_H_
