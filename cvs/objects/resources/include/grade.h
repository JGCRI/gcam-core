#ifndef _GRADE_H_
#define _GRADE_H_
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
* \file grade.h
* \ingroup Objects
* \brief The Grade class header file.
* \author Sonny Kim
*/

#include <memory>
#include <boost/core/noncopyable.hpp>

#include "util/base/include/inamed.h"
#include "util/base/include/ivisitable.h"
#include "util/base/include/time_vector.h"
#include "util/base/include/data_definition_util.h"

class Tabs;
class IInfo;

// Forward declarations.
class AccumulatedGrade;
class AccumulatedPostGrade;

/*! 
* \ingroup Objects
* \brief Technologies representing a Grade for each resource.
*
* grade is an object that contains technologies that characterize each grade.
*
* \author Sonny Kim
*/

class Grade: public INamed, public IVisitable, private boost::noncopyable
{
    friend class XMLDBOutputter;
public:
    Grade();
     virtual void completeInit( const IInfo* aSubresourceInfo );
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    static const std::string& getXMLNameStatic();

    virtual void initCalc( const std::string& aRegionName, const std::string& aResourceName, const int aPeriod );
    virtual void postCalc( const std::string& aRegionName, const std::string& aResourceName, const int aPeriod );

    void calcCost( const double cumTechChange, const int per );
    double getAvail() const;
    double getCost( const int per ) const;
    double getExtCost() const;
    const std::string& getName() const;
    void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    
    DEFINE_DATA(
        /* Declare all subclasses of SubResource to allow automatic traversal of the
         * hierarchy under introspection.
         */
        DEFINE_SUBCLASS_FAMILY( Grade, AccumulatedGrade, AccumulatedPostGrade ),
        
        //! Grade name.
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),
        
        //! amount of Grade for each Grade
        DEFINE_VARIABLE( SIMPLE, "available", mAvailable, double ),
        
        //! extraction cost of each Grade
        DEFINE_VARIABLE( SIMPLE, "extractioncost", mExtractCost, double ),
        
        //! total cost
        DEFINE_VARIABLE( ARRAY | NOT_PARSABLE, "totalcost", mTotalCost, objects::PeriodVector<double> )
    )

    //! The Grade's information store.
    std::auto_ptr<IInfo> mGradeInfo;
    
    virtual const std::string& getXMLName() const;
};

#endif // _GRADE_H_
