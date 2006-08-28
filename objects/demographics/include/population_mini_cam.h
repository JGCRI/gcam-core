#ifndef _POPULATION_MINI_CAM_H_
#define _POPULATION_MINI_CAM_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file population_mini_cam.h
* \ingroup Objects
* \brief The PopulationMiniCAM class header file.
* \author Sonny Kim
* \author Katherine Chung
*/

#include <vector>
#include <xercesc/dom/DOMNode.hpp>

#include "demographics/include/population.h"

class IVisitor;

/*! 
* \ingroup Objects
* \brief An object which contains the PopulationMiniCAM information for a
*        region.
* \details PopulationMiniCAM only holds a total that is read in. There is no
*          separation between gender or age.
*/

class PopulationMiniCAM : public Population
{
public:
    PopulationMiniCAM();
    virtual void completeInit( const std::vector<double>& femalePopFromPrev = std::vector<double>(), 
        const std::vector<double>& malePopFromPrev = std::vector<double>() );
    const std::vector<double> getSurvMalePop() const { return std::vector<double>(); } // TEMP
    const std::vector<double> getSurvFemalePop() const { return std::vector<double>(); } // TEMP
    virtual void initCalc();

    static const std::string& getXMLNameStatic();
    double getWorkingAgePop() const;
    double getWorkingAgePopMale() const { return 0; } // minicam only has total
    double getWorkingAgePopFemale() const { return 0; } // minicam only has total

    virtual void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    virtual const std::string& getXMLName() const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( std::ostream& out, Tabs* tabs ) const;
private:
    static const std::string XML_NAME; //!< node name for toXML methods
};

#endif // _POPULATION_MINI_CAM_H_

