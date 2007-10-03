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
 * \file summer_sub_container.cpp
 * \ingroup Objects
 * \brief SummerSubContainer class source file.
 * \author Jim Naslund and Ming Chang
 */

#include "util/base/include/definitions.h"

#include "ccarbon_model/include/carbon_box.h"
#include "ccarbon_model/include/acarbon_flow.h"
#include "ccarbon_model/include/carbon_flow_factory.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/dependency_finder.h"
#include "ccarbon_model/include/icarbon_stock.h"
#include "ccarbon_model/include/npp.h"
#include "ccarbon_model/include/carbon_stock.h"
#include "ccarbon_model/include/summer_sub_container.h"
#include "ccarbon_model/include/carbon_model_utils.h"

#include <vector>

using namespace std;

typedef boost::ptr_list<ACarbonFlow>::iterator CarbonFlowIter;
typedef boost::ptr_list<ACarbonFlow>::const_iterator CarbonFlowConstIter;

/*!
 * \brief Default constructor.
 */
SummerSubContainer::SummerSubContainer()
: mTransferedIn( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear(), false )
{
    mCarbonBoxes.push_back( new CarbonBox( eVegetation ) );
    mCarbonBoxes.push_back( new CarbonBox( eSoil ) );
    mCarbonBoxes.push_back( new CarbonBox( eLitter ) );
}

/*!
 * \brief Destructor.
 * \details Deallocates all the boxes that were created on the heap.
 */
SummerSubContainer::~SummerSubContainer(){
    for( CarbonBoxConstIterator boxIter = mCarbonBoxes.begin();
         boxIter != mCarbonBoxes.end(); ++boxIter ){
        delete *boxIter;
    }
}

void SummerSubContainer::resetTransferedIn(){
    mTransferedIn = objects::YearVector<bool>( CarbonModelUtils::getStartYear(),
                                               CarbonModelUtils::getEndYear(), false );
}

void SummerSubContainer::doTransfers( const EnvironmentalInfo* aEnvInfo, FlowType aFlowType,
                                      const int aYear ){
    // This check is needed because this function is called from
    // CarbonBoxModel::calcLandUseChange.  Multiple CarbonBoxModels will
    // be under the same conceptual root which means this function will be called
    // multiple times for each conceptual root.  See CarbonBoxModel::calcLandUseChange
    // for details.
	
    if( !mTransferedIn[ aYear ] ) {
        for( CarbonBoxIterator boxIter = mCarbonBoxes.begin();
             boxIter != mCarbonBoxes.end(); ++boxIter ){
            (*boxIter)->doTransfers( aEnvInfo, aFlowType, aYear );
        }
        mTransferedIn[ aYear ] = true;
    }
}

void SummerSubContainer::acceptTransfer( double aCarbonValue, const int aYear,
                                         const BoxType aBoxType ){
    for( CarbonBoxIterator boxIter = mCarbonBoxes.begin();
         boxIter != mCarbonBoxes.end(); ++boxIter ){
        if( (*boxIter)->matches( aBoxType ) ) {
			(*boxIter)->acceptTransfer( aCarbonValue, aYear, aBoxType );
        }
	}
}

void SummerSubContainer::addFlow( auto_ptr<ACarbonFlow> aCarbonFlow, const BoxType aBoxType ){
    for( CarbonBoxIterator boxIter = mCarbonBoxes.begin();
         boxIter != mCarbonBoxes.end(); ++boxIter ){
        if( (*boxIter)->matches( aBoxType ) ) {
            (*boxIter)->addFlow( aCarbonFlow, aBoxType );
        }
    }
}

void SummerSubContainer::addDependencies( DependencyFinder& aDepFinder ) const {
    assert(false);
}
