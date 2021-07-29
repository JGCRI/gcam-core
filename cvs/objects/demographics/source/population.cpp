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
* \file population.cpp
* \ingroup Objects
* \brief Population class source file.
* \author Sonny Kim, Katherine Chung, Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <string>
#include <map>
#include <cassert>
#include <vector>

#include "containers/include/scenario.h"
#include "demographics/include/population.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/configuration.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/ivisitor.h"

using namespace std;

extern Scenario* scenario;

//! Default constructor.
Population::Population():
    mIsParsed(true)

{
    mYear = -1;
    mTotalPop = -1;
    mPopulationUnit = "thous";
    mWorkingAgeMin = WORKING_AGE_MIN_DEFAULT;
    mWorkingAgeMax = WORKING_AGE_MAX_DEFAULT;
}

//! Population destructor. 
Population::~Population(){
}

//! Returns total population for this year
double Population::getTotal() const {
    assert( mTotalPop != -1 );
    return mTotalPop;
}

//! Returns year of population
int Population::getYear() const {
    assert( mYear != -1 );
    return mYear;
}

//! Returns name (year as a string)
const std::string Population::getName() const {
    return util::toString( mYear );
}

//! Write out XML for debugging purposes.
void Population::toDebugXML( std::ostream& out, Tabs* tabs ) const {
    XMLWriteOpeningTag ( getXMLName(), out, tabs , "", mYear);

    XMLWriteElement( mPopulationUnit, "population-unit", out, tabs );
    XMLWriteElement( mTotalPop, "totalPop", out, tabs );
    XMLWriteElement( mWorkingAgeMin, "min-working-age", out, tabs );
    XMLWriteElement( mWorkingAgeMax, "max-working-age", out, tabs );
    // write out variables for derived classes
    toDebugXMLDerived( out, tabs );

    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

/*! \brief Update a Visitor with information about a Population.
* \param aVisitor Visitor to update.
* \param aPeriod Period for which to update.
*/
void Population::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitPopulation( this, aPeriod );
    aVisitor->endVisitPopulation( this, aPeriod );
}
