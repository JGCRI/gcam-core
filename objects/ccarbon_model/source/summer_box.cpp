/*! 
 * \file summer_box.cpp
 * \ingroup objects
 * \brief SummerBox class source file.
 * \author Jim Naslund and Ming Chang
 */
#include "util/base/include/definitions.h"

#include "ccarbon_model/include/summer_box.h"
#include "ccarbon_model/include/summer_sub_container.h"
using namespace std;


// Static initializations.
std::auto_ptr<SummerBox> SummerBox::gInstance;

/*!
 * \brief Default constructor.
 */
SummerBox::SummerBox()
: mContainers( HashMap<int,SummerSubContainer*>() )
{
}

/*!
 * \brief Adds a container to this box.
 * \details Adds a container for the unique key that is passed in
            if one does not already exist.
 * \param aKey the key to add a container for.
 */
void SummerBox::addContainer( const int aKey ){
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
ICarbonContainer* SummerBox::getContainer( const int aKey ){
	// uncertain add up
	// add a container for the key if it is not already existed
	return ( *( mContainers.find( aKey ) ) ).second;
}

//TODO: do something here?
//      This is currently not used because the transfers will be
//      invoked from CarbonBoxModels.  They are invoked on the appropriate
//      ICarbonContainer from CarbonBoxModel::calcLandUseChange.  See
//      comment there for details and explanation.
void SummerBox::calcLandUseChange( const int aYear, FlowType aFlowType ){
    //doTransfers( aYears, aFlowType );
}

/*!
 * \brief Get a pointer to the instance of the Summer object.
 * \details If the static instance of the Summer class has not been created,
 *          get Instance() will create it. Otherwise getInstance will return
 *          a pointer to the instance of the Configuration class.
 * \return A pointer to the single instance of the Configuration class.
 */
SummerBox* SummerBox::getInstance(){
    if( !gInstance.get() ){
        gInstance.reset( new SummerBox() );
    }
    return gInstance.get();
}

void SummerBox::sumLandLoss( const double aLandLost, const int aKey ){
    double currentValue = 0;
    HashMap<int, double>::const_iterator entry = mLandLostSums.find( aKey );
    if( entry != mLandLostSums.end() ) {
        double currentValue = mLandLostSums.find( aKey )->second;
    }
    mLandLostSums.insert( pair<int, double>( aKey, currentValue + aLandLost ) );
}

double SummerBox::getLandLoss( const int aKey ) const {
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
void SummerBox::resetState(){
    mLandLostSums = HashMap<int,double>();
    for( HashMap<int,SummerSubContainer*>::iterator iter = mContainers.begin();
         iter != mContainers.end(); ++iter ){
        iter->second->resetTransferedIn();
    }
}
