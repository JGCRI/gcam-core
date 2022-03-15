#ifndef _POPULATION_H_
#define _POPULATION_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/



/*! 
* \file population.h
* \ingroup Objects
* \brief The Population class header file.
* \author Sonny Kim
* \author Katherine Chung
*/

#include <vector>
#include <string>
#include <boost/core/noncopyable.hpp>

#include "util/base/include/iyeared.h"
#include "util/base/include/ivisitable.h"
#include "util/base/include/data_definition_util.h"

class IVisitor;
class Tabs;

// Need to forward declare the subclasses as well.
class PopulationMiniCAM;

/*! 
* \ingroup Objects
* \brief An object which contains the Population information for a region.
* \details This is the base Population class. PopulationMiniCAM derives from it.
*/

class Population: public IYeared, public IVisitable, private boost::noncopyable
{
    friend class XMLDBOutputter; // For getXMLName()
public:
    Population();
    virtual ~Population();
    virtual Population* cloneAndInterpolate( const int aNewYear, const Population* aNextPopulation ) const = 0;
    void toDebugXML( std::ostream& out, Tabs* tabs ) const;

    virtual void completeInit( const std::vector<double>& femalePopFromPrev = std::vector<double>(), const std::vector<double>& malePopFromPrev = std::vector<double>() ) = 0;
    virtual const std::vector<double> getSurvMalePop() const = 0; // TEMP
    virtual const std::vector<double> getSurvFemalePop() const = 0; // TEMP
    virtual void initCalc() = 0;

    double getTotal() const;
    int getYear() const;
    const std::string getName() const;
    virtual double getWorkingAgePop() const = 0;
    virtual double getWorkingAgePopMale() const = 0;
    virtual double getWorkingAgePopFemale() const = 0;
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const = 0;
    bool mIsParsed; //!< whether population has been read-in
protected:
    
    DEFINE_DATA(
        /* Declare all subclasses of Population to allow automatic traversal of the
         * hierarchy under introspection.
         */
        DEFINE_SUBCLASS_FAMILY( Population, PopulationMiniCAM ),

        //! year
        DEFINE_VARIABLE( SIMPLE, "year", mYear, int ),
        
        //! total population for this year
        DEFINE_VARIABLE( SIMPLE, "totalPop", mTotalPop, double ),
        
        //! unit of population numbers
        DEFINE_VARIABLE( SIMPLE, "population-unit", mPopulationUnit, std::string ),
        
        //! minimum working age.
        DEFINE_VARIABLE( SIMPLE, "min-working-age", mWorkingAgeMin, int ),
        
        //! maximum working age.
        DEFINE_VARIABLE( SIMPLE, "max-working-age", mWorkingAgeMax, int )
    )

    virtual const std::string& getXMLName() const = 0;
    virtual void toDebugXMLDerived( std::ostream& out, Tabs* tabs ) const = 0;
private:
    const static int WORKING_AGE_MIN_DEFAULT = 15; //!< Default minimum working age.
    const static int WORKING_AGE_MAX_DEFAULT = 65; //!< Default maximum working age.
};

#endif // _POPULATION_H_


