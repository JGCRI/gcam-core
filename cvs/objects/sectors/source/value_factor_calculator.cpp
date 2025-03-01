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
 * \file value_factor_calculator.cpp
 * \ingroup Objects
 * \brief ValueFactorCalculator class source file.
 * \author Matthew Binsted, Matt Mowers
 */

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <math.h>

#include "sectors/include/value_factor_calculator.h"
#include "util/base/include/util.h"
#include "util/base/include/xml_helper.h"
#include "sectors/include/sector_utils.h"
#include "marketplace/include/marketplace.h"

using namespace std;

/*!
 * \brief Constructor.
 */
ValueFactorCalculator::ValueFactorCalculator()
{
    mValueFactorIntercept = 1.0;
    mValueFactorSlope = 0.0;
}

// Documentation is inherited.
ValueFactorCalculator* ValueFactorCalculator::clone() const {
    ValueFactorCalculator* clone = new ValueFactorCalculator();
    clone->mValueFactorIntercept = mValueFactorIntercept;
    clone->mValueFactorSlope = mValueFactorSlope;
    
    return clone;
}

// Documentation is inherited.
bool ValueFactorCalculator::isSameType( const std::string& aType ) const {
    return aType == getXMLNameStatic();
}

// Documentation is inherited.
const gcamstr& ValueFactorCalculator::getName() const {
    const static gcamstr NAME(getXMLNameStatic());
    return NAME;
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. The "==" operator that is used when parsing, required this second
* function to return static.
* \note A function cannot be static and virtual.
* \return The constant XML_NAME as a static.
*/
const string& ValueFactorCalculator::getXMLNameStatic() {
    const static string XML_NAME = "value-factor-calculator";
    return XML_NAME;
}

const string& ValueFactorCalculator::getXMLName() const {
    return getXMLNameStatic();
}

// Documentation is inherited.
void ValueFactorCalculator::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElement(mValueFactorIntercept, "ValueFactorIntercept", aOut, aTabs );
    XMLWriteElement(mValueFactorSlope, "ValueFactorSlope", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

// Documentation is inherited.
void ValueFactorCalculator::initCalc( const IInfo* aTechInfo ) {
    // No information needs to be passed in
}

double ValueFactorCalculator::getMarginalBackupCapacity(const gcamstr& aSector,
    const gcamstr& aElectricSector,
    const gcamstr& aResource,
    const gcamstr& aRegion,
    const double aTechCapacityFactor,
    const double aReserveMargin,
    const double aAverageGridCapacityFactor,
    const int aPeriod) const
{
    // This is a placeholder function so we can use the IBackupCalculator class
    // For now, it will do nothing (and should not be called)

    return 0.0;
}

double ValueFactorCalculator::getAverageBackupCapacity(const gcamstr& aSector,
    const gcamstr& aElectricSector,
    const gcamstr& aResource,
    const gcamstr& aRegion,
    const double aTechCapacityFactor,
    const double aReserveMargin,
    const double aAverageGridCapacityFactor,
    const int aPeriod) const
{
    // This is a placeholder function so we can use the IBackupCalculator class
    // For now, it will do nothing (and should not be called)

    return 0.0;
}

/*!
 * \brief Compute value factor for electricity technology.
 * \details Compute value factor (VF) for electricity technology,
 *          which will decrease linearly as market share increases.
 *          This value factor will then be used to adjust technology
 *          LCOE into "profitability-adjusted LCOE" (PLCOE), where
 *          PLCOE = LCOE/VF.
 * \param aSector The name of the sector.
 * \param aElectricSector The name of the electricity sector.
 * \param aRegion Name of the containing region.
 * \param aPeriod Model period.
 * \return Value factor scalar for technology cost (range: 0-1).
 */
double ValueFactorCalculator::getValueFactor( const gcamstr& aSector,
                                              const gcamstr& aElectricSector,
                                              const gcamstr& aRegion,
                                              const int aPeriod ) const
{
    // Preconditions
    assert( !aSector.empty() );
    assert( !aElectricSector.empty() );
    assert( !aRegion.empty() );

    double renewElecShare = std::min( SectorUtils::getTrialSupply( aRegion, aSector, aPeriod ), 1.0 );

    double valueFactor = 1.0;
    valueFactor = std::max( ( mValueFactorIntercept + mValueFactorSlope * renewElecShare ), util::getVerySmallNumber() );

    return valueFactor;

}
