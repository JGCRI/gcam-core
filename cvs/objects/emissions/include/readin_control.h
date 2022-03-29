#ifndef _READIN_CONTROL_H_
#define _READIN_CONTROL_H_
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
 * \file readin_control.h
 * \ingroup Objects
 * \brief ReadInControl class header file.
 * \author Kate Calvin
 */

#include <memory>

#include "emissions/include/aemissions_control.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"

/*! 
 * \ingroup Objects
 * \brief An class that represents read-in emissions controls. You can read in future emissions factors for vintaged
 *        technologies and it will compute the necessary emissions reduction.
 * \author Kate Calvin
 */
class ReadInControl: public AEmissionsControl {
public:
    ReadInControl();
    
    virtual ~ReadInControl();
    
    virtual ReadInControl* clone() const;
    
    static const std::string& getXMLNameStatic();
    
    virtual const std::string& getXMLName() const;
    
    bool XMLParse( rapidxml::xml_node<char>* & aNode );
    
    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               const IInfo* aTechIInfo );

    virtual void initCalc( const std::string& aRegionName,
                           const IInfo* aTechInfo,
                           const NonCO2Emissions* aParentGHG,
                           const int aPeriod );

protected: 
    ReadInControl( const ReadInControl& aOther );
    ReadInControl& operator=( const ReadInControl& aOther );
    
    virtual void toDebugXMLDerived( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const;

    virtual void calcEmissionsReduction( const std::string& aRegionName, const int aPeriod, const GDP* aGDP );

    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        AEmissionsControl,
        
        //! Technology build period -- this is the period that the vintage was constructed
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "tech-build-period", mTechBuildPeriod, int )
    )
    
    //! Future emissions factors -- this vector sets future emissions factors for vintaged technologies
    // Note ideally this would be included for GCAMFusion with the following definition however it is
    // not currently able to handle smart pointers.
    // DEFINE_VARIABLE( ARRAY, "future-emiss-factor", mFutureEmissionsFactors, std::shared_ptr<objects::PeriodVector<double> > ),
    std::shared_ptr<objects::PeriodVector<double> > mFutureEmissionsFactors;

    void copy( const ReadInControl& aOther );
};

#endif // _READIN_CONTROL_H_
