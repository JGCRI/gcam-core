#ifndef _FEMALE_H_
#define _FEMALE_H_
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
* \file female.h
* \ingroup Objects
* \brief The Female class header file.
* \author Sonny Kim
* \author Katherine Chung
*/

#include "demographics/include/gender.h"

class IVisitor;
/*! 
* \ingroup Objects
* \brief Derived from Gender, a class which represents Females.
*/

class Female : public Gender {
public:
    Female();
    static const std::string& getXMLNameStatic();
    double calcMaleBirth();
    double calcFemaleBirth();
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    virtual void toDebugXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual const std::string& getXMLName() const;
    
    double mFertilityRate; //!< fertility rate
    double mMaleBirth; //!< male birth
    double mFemaleBirth; //!< female birth 
    double mMaleBirthFrac; //!< fraction of births that are male

    static const std::string XML_NAME; //!< node name for toXML methods
};

#endif // _FEMALE_H_

