#ifndef _POPULATION_MINI_CAM_H_
#define _POPULATION_MINI_CAM_H_
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
* \file population_mini_cam.h
* \ingroup Objects
* \brief The PopulationMiniCAM class header file.
* \author Sonny Kim
* \author Katherine Chung
*/

#include <vector>

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
    virtual Population* cloneAndInterpolate( const int aNewYear, const Population* aNextPopulation ) const;
    virtual void completeInit( const std::vector<double>& femalePopFromPrev = std::vector<double>(), 
        const std::vector<double>& malePopFromPrev = std::vector<double>() );
    const std::vector<double> getSurvMalePop() const { return std::vector<double>(); } // TEMP
    const std::vector<double> getSurvFemalePop() const { return std::vector<double>(); } // TEMP
    virtual void initCalc();

    static const std::string& getXMLNameStatic();
    double getWorkingAgePop() const;
    double getWorkingAgePopMale() const { return 0; } // minicam only has total
    double getWorkingAgePopFemale() const { return 0; } // minicam only has total

    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        Population,
        
        //! Fraction of the total population which is assumed to be of working age
        DEFINE_VARIABLE( SIMPLE, "fraction-working", mFractionWorking, double )
    )
    
    virtual const std::string& getXMLName() const;
    virtual void toDebugXMLDerived( std::ostream& out, Tabs* tabs ) const;
private:
    static const std::string XML_NAME; //!< node name for toXML methods
};

#endif // _POPULATION_MINI_CAM_H_


