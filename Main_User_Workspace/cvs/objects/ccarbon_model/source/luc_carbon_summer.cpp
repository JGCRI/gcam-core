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
 * \file summer_box.cpp
 * \ingroup objects
 * \brief CarbonSummer class source file.
 * \author Jim Naslund and Ming Chang
 */
#include "util/base/include/definitions.h"

#include "ccarbon_model/include/luc_carbon_summer.h"
#include "ccarbon_model/include/luc_carbon_summer_sub_container.h"
using namespace std;


// Static initializations.
std::auto_ptr<CarbonSummer> CarbonSummer::gInstance;

/*!
 * \brief Default constructor.
 */
CarbonSummer::CarbonSummer()
: mContainers( HashMap<int,SummerSubContainer*>() )
{
}

/*!
 * \brief Adds a container to this box.
 * \details Adds a container for the unique key that is passed in
            if one does not already exist.
 * \param aKey the key to add a container for.
 */
void CarbonSummer::addContainer( const int aKey ){
    if( mContainers.find( aKey ) == mContainers.end() ){
        auto_ptr<SummerSubContainer> newContainer( new SummerSubContainer() );
        mContainers.insert( 
            pair<int, SummerSubContainer*>( aKey, newContainer.get() ) );
        mContainerPointers.push_back( newContainer.release() );
        mLandLostSums.insert( pair<int, double>( aKey, 0 ) );
    }
}

/*!
 * \brief Returns the correct container for the key.
 * \details Returns the container that corresponds to the key that is passed
 *          in.
 * \warning This function does not check to see if the container exists.
 * \param aKey the key to get a container with.
 * \return a pointer to the container.
 */
ICarbonContainer* CarbonSummer::getContainer( const int aKey ){
    // uncertain add up
    // add a container for the key if it is not already existed

    return ( *( mContainers.find( aKey ) ) ).second;
}

//TODO: do something here?
//      This is currently not used because the transfers will be
//      invoked from CarbonBoxModels.  They are invoked on the appropriate
//      ICarbonContainer from CarbonBoxModel::calcLandUseChange.  See
//      comment there for details and explanation.
void CarbonSummer::calcLandUseChange( const int aYear, FlowType aFlowType ){
    //doTransfers( aYears, aFlowType );
}

/*!
 * \brief Get a pointer to the instance of the Summer object.
 * \details If the static instance of the Summer class has not been created,
 *          get Instance() will create it. Otherwise getInstance will return
 *          a pointer to the instance of the Configuration class.
 * \return A pointer to the single instance of the Configuration class.
 */
CarbonSummer* CarbonSummer::getInstance(){
    if( !gInstance.get() ){
        gInstance.reset( new CarbonSummer() );
    }
    return gInstance.get();
}

/*!
 * \brief Add the amount of land lost to the total for this conceptual root
 * \details The land loss amount is added to the existing value. 
 *          Separate values are maintained for each conceptual root.
 * \param aLandLost Amount of land lost
* \param aKey the unique key for each conceptual root
 */
void CarbonSummer::sumLandGain( const double aLandLost, const int aKey ){
    double currentValue = 0;
    HashMap<int, double>::const_iterator entry = mLandLostSums.find( aKey );
    if( entry != mLandLostSums.end() ) {
        currentValue = mLandLostSums.find( aKey )->second;
    }
    mLandLostSums.insert( pair<int, double>( aKey, currentValue + aLandLost ) );
}

/*!
 * \brief Return total amount of land lost for conceptual root matching key.
 * \return A double representing total amount of land lost
 */
double CarbonSummer::getTotalLandGain( const int aKey ) const {
    HashMap<int, double>::const_iterator entry = mLandLostSums.find( aKey );
    if( entry != mLandLostSums.end() ) {
        double landLost = mLandLostSums.find( aKey )->second;
        return landLost;
    }
    return 0;
}

/*!
 * \brief Resets the summer's state.
 * \details Resets the summer's state so it can be called again.  This function
 *          sets the land lost sums for each container to 0.  It also sets the
 *          mTransferredInFlag to false in each container.
 */
void CarbonSummer::resetState( const double aYear ){
    mLandLostSums = HashMap<int,double>();
    for( HashMap<int,SummerSubContainer*>::iterator iter = mContainers.begin();
         iter != mContainers.end(); ++iter ){
        iter->second->resetTransferedIn( aYear );
    }
}
