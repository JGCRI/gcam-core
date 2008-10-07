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
#include "ccarbon_model/include/luc_carbon_summer_sub_container.h"
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

void SummerSubContainer::resetTransferedIn( const double aYear ){
    mTransferedIn[ aYear ] = false;
}

void SummerSubContainer::doTransfers( const EnvironmentalInfo* aEnvInfo, FlowType aFlowType,
                                      const int aYear ){
    // This check is needed because this function is called multiple CarbonBoxModels
    // under the same conceptual root. Since the summer contains flows to all box models (within
    // the same conceptual root), only want to do transfers once.

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
