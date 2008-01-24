/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
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
        //! initialize the member variables with values from aCarbonStock
        this->mCarbonBoxType = aCarbonStock.mCarbonBoxType;
        this->mInitialStock = aCarbonStock.mInitialStock;
    
        //! copy mCarbonStock vector
        if ( typeid( aCarbonStock.mCarbonStock ) ==  typeid(this->mCarbonStock) ){
            std::copy( aCarbonStock.mCarbonStock.begin(), aCarbonStock.mCarbonStock.end(), this->mCarbonStock.begin() );
        };
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

const double ACarbonStock::getStock( const int aYear ) {
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
