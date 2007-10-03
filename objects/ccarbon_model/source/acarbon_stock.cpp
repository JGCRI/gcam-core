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
: mCarbonStock( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear(), 0 ),
  mCurrentStock( 0 )
{
	this->mInitialStock = 0;
	this->mCarbonBoxType =eAnyBox;
}


ACarbonStock::ACarbonStock( const ACarbonStock& aCarbonStock )
: mCarbonStock( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear(), 0 )
{
		this->mCurrentStock = aCarbonStock.mCurrentStock;
		this->mCarbonBoxType = aCarbonStock.mCarbonBoxType;
		this->mInitialStock = aCarbonStock.mInitialStock;
	
		//! copy mCarbonStock vector
		/*
		objects::YearVector<double>::iterator currentYearIter = this->mCarbonStock.begin();
		for (objects::YearVector<double>::const_iterator yearIter =aCarbonStock.mCarbonStock.begin();
			 yearIter != aCarbonStock.mCarbonStock.end(); yearIter++ ){
				*currentYearIter = *yearIter;
		}*/
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
    mCurrentStock += aValueToAdd;
    mCarbonStock[ aYear ] = mCurrentStock;
}

double ACarbonStock::getStock( const int aYear ) {
    return mCarbonStock[ aYear ];
}

void ACarbonStock::setCurrentStock( const int aYear ) {
    mCurrentStock = mCarbonStock[ max( CarbonModelUtils::getStartYear(), aYear - 1 ) ];
}

double ACarbonStock::getNPPOverAreaRatio( const int aYear ){
	return 0;
}

 
/*! deductFromStock
 * \brief reduce stock value.
 * \details This function takes in a deduct value as parameter and 
 *          reduce the stock value with the deduct value.
 *
 * \param aDeductValue the value subtracts to the stock value
 *	      aYear - the year to perform the task
 */
void ACarbonStock::deductFromStock( const double aDeductValue, const int aYear ) {
	mCurrentStock -= aDeductValue;
	mCarbonStock[ aYear ] = mCurrentStock;
}

/*! modifyCarbonStock
 * \brief replace stock value.
 * \details This function takes in a new value as parameter and 
 *          replaces the stock value with the new value.
 *
 * \param aNewValue the value used to replace the stock value
 *	      aYear - the year to perform the task
 */
void ACarbonStock::modifyCarbonStock(const double aNewValue, const int aYear) {
	mCurrentStock = aNewValue;
	mCarbonStock[ aYear ] = aNewValue;
}

/*! getTurnoverTimescale
 * \brief return turnover timescale.
 * \details This function returns the turnover timescale value of the stock
 *			if one exists. Otherwise, it returns 0
 *
 */
double ACarbonStock::getTurnoverTimescale() {
	return 0;
}

void ACarbonStock::copyBoxType( BoxType aCarbonBoxType ){
	mCarbonBoxType = aCarbonBoxType ;
	return;
}

BoxType ACarbonStock::returnBoxType(){
	return mCarbonBoxType;
}