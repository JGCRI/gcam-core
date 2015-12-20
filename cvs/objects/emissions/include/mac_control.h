#ifndef _MAC_CONTROL_H_
#define _MAC_CONTROL_H_
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
 * \file mac_control.h
 * \ingroup Objects
 * \brief MACControl class header file.
 * \author Kate Calvin
 */

#include <memory>

#include "emissions/include/aemissions_control.h"

class PointSetCurve;

/*! 
 * \ingroup Objects
 * \brief An class that represents MAC Controls.
 * \author Kate Calvin
 */
class MACControl: public AEmissionsControl {
public:
    MACControl();
    
    virtual ~MACControl();
    
    virtual MACControl* clone() const;
    
    static const std::string& getXMLNameStatic();
    
    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               const IInfo* aTechIInfo );

    virtual void initCalc( const std::string& aRegionName,
                           const IInfo* aLocalInfo,
                           const int aPeriod );

protected:
    MACControl( const MACControl& aOther );
    MACControl& operator=( const MACControl& aOther );
    
    virtual const std::string& getXMLName() const;
    virtual bool XMLDerivedClassParse( const std::string& aNodeName, const xercesc::DOMNode* aCurrNode );
    virtual void toInputXMLDerived( std::ostream& aOut, Tabs* aTabs ) const;
    virtual void toDebugXMLDerived( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const;

    virtual void calcEmissionsReduction( const std::string& aRegionName, const int aPeriod, const GDP* aGDP );

private:
    //! Boolean indicating whether reductions should occur at a zero carbon price
    bool mNoZeroCostReductions;
    
    //! The underlying Curve (as read in)
    std::auto_ptr<PointSetCurve> mMacCurve;

    void copy( const MACControl& other );
    double getMACValue( const double aCarbonPrice ) const;
};

#endif // _MAC_CONTROL_H_
