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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*! 
 * \file acarbon_stock.cpp
 * \ingroup Objects
 * \brief ACarbonStock class source file.
 * \author Jim Naslund and Ming Chang
 */

#include "util/base/include/definitions.h"
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "ccarbon_model/include/acarbon_stock.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include "ccarbon_model/include/acarbon_flow.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/xml_helper.h"
#include "ccarbon_model/include/environmental_info.h"
#include <typeinfo>

using namespace std;
using namespace xercesc;

/*! 
 * \brief Default Constructor
 */
ACarbonStock::ACarbonStock()
: mCarbonStock( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear(), 0 )
{
    this->mInitialStock = 0;
    this->mCarbonBoxType =eAnyBox;
}


/*! ACarbonStock( const ACarbonStock& aCarbonStock )
 * \brief a Copy Constructor for ACarbonFlow
 * \details create a new ACarbonStock object or derived class and initialize all the
              variables with either deep copy of the member object variable or shallow copy
              of the member variable.
 * \param aCarbonFlow const reference of ACarbonStock object
 * \author Ming Chang
 */
ACarbonStock::ACarbonStock( const ACarbonStock& aCarbonStock )
: mCarbonStock( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear(), 0 )
{
    // initialize the member variables with values from aCarbonStock
    this->mCarbonBoxType = aCarbonStock.mCarbonBoxType;
    this->mInitialStock = aCarbonStock.mInitialStock;
    
    // copy mCarbonStock vector
    std::copy( aCarbonStock.mCarbonStock.begin(), aCarbonStock.mCarbonStock.end(), this->mCarbonStock.begin() );
}


ACarbonStock::~ACarbonStock() {
}

void ACarbonStock::toInputXML( std::ostream& aOut, Tabs* aTabs ) const {
}

void ACarbonStock::toDebugXML( const int aPeriod, std::ostream& aOut,
                               Tabs* aTabs ) const {
}

void ACarbonStock::accept( IVisitor* aVisitor, const int aPeriod ) const {
}

void ACarbonStock::addToStock( double aValueToAdd, const int aYear ){
    mCarbonStock[ aYear ] += aValueToAdd;
}

double ACarbonStock::getStock( const int aYear ) {
    return mCarbonStock[ aYear ];
}

void ACarbonStock::setCurrentStock( const int aYear ) {
    mCarbonStock[ aYear ] = mCarbonStock[ max( CarbonModelUtils::getStartYear(), aYear - 1 ) ];
}

double ACarbonStock::getNPPOverAreaRatio( const int aYear ){
    return 0;
}

 
/*! deductFromStock
 * \brief reduce stock value.
 * \details This function takes in a deduct value as parameter and 
            reduces the stock value by this amount.
 
 * \param aDeductValue the value to subtract from the stock value
 *          aYear the year to perform the task
 * \author Ming Chang
 */
void ACarbonStock::deductFromStock( const double aDeductValue, const int aYear ) {
    mCarbonStock[ aYear ] -= aDeductValue;
}

/*! modifyCarbonStock
 * \brief replace stock value.
 * \details This function takes in a new value as parameter and 
            replaces the stock value with the new value.
 
 * \param aNewValue the value used to replace the stock value
           aYear the year to perform the task
 * \author Ming Chang
 */
void ACarbonStock::modifyCarbonStock(const double aNewValue, const int aYear) {
    mCarbonStock[ aYear ] = aNewValue;
}

/*! getTurnoverTimescale
 * \brief return turnover timescale.
 * \details This function returns the turnover timescale value of the stock
             if one exists. Otherwise, it returns 0
 * \author Ming Chang
 */
double ACarbonStock::getTurnoverTimescale() {
    return 0;
}

/*! copyBoxType( BoxType aCarbonBoxType )
 * \brief copy the current box type to the flow objects
 * \details store the Box Type enum to the carbon flow object
 * \param aCarbonBoxType : BoxType
 * \author Ming Chang
 */
void ACarbonStock::copyBoxType( BoxType aCarbonBoxType ){
    mCarbonBoxType = aCarbonBoxType ;
    return;
}
/*! returnBoxType( BoxType aCarbonBoxType )
 * \brief return the parent box type
 * \details return the parent box type
 * \author Ming Chang
 */
BoxType ACarbonStock::returnBoxType(){
    return mCarbonBoxType;
}
