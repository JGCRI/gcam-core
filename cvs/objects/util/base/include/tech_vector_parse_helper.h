#ifndef _TECH_VECTOR_PARSE_HELPER_H_
#define _TECH_VECTOR_PARSE_HELPER_H_
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



#include "util/base/include/definitions.h"
#include <map>
#include <algorithm>

#include <boost/mpl/vector.hpp>
#include <boost/fusion/include/map.hpp>
#include <boost/fusion/include/at_key.hpp>
#include <boost/fusion/include/mpl.hpp>
#include <boost/mpl/zip_view.hpp>
#include <boost/mpl/transform_view.hpp>
#include <boost/mpl/transform.hpp>
#include <boost/mpl/unpack_args.hpp>
#include <boost/fusion/include/vector.hpp>
#include <boost/fusion/include/for_each.hpp>

#include "util/base/include/time_vector.h"
#include "util/base/include/value.h"





/*!
 * \brief A helper class that provides a means for temporary storage of data for vectors
 *        of type TechVintageVector when users needs to set values in them prior to it
 *        being initialized with the proper technology size.
 * \details This class contains a temporary storage for TechVintageVector by mapping it's
 *          unique ID to a PeriodVector in which data can be stored.  Note PeriodVectors are
 *          only allocated on demand so as to keep memory usage down.
 * \author Pralit Patel
 */
template<typename T>
class TechVectorParseHelper {
public:
    /*!
     * \brief Get the temporary storage PeriodVector associated with a given TechVintageVector.
     * \details Note if no entry has been previously made for the given TechVintageVector then
     *          a new PeriodVector will be allocated for it.
     * \param aTecVector The TechVintageVector instance to find the temporary storage for.
     * \return The associated temporary storage for aTecVector.
     */
    objects::PeriodVector<T>& getPeriodVector( objects::TechVintageVector<T>& aTecVector ) {
        // Note that in order to save memory usage space in TechVintageVector we do not have
        // an explicit ID member variable in aTecVector.  Instead it has been stashed in
        // aTecVector.mData so we must reinterpret that as size_t which is the ID type.
        size_t tempDataKey = reinterpret_cast<size_t>( aTecVector.mData );
        auto iter = mTempStore.find( tempDataKey );
        if( iter == mTempStore.end() ) {
            iter = mTempStore.insert( std::make_pair( tempDataKey, objects::PeriodVector<T>() ) ).first;
        }
        
        return (*iter).second;
    }
    
    static void setDefaultValue( const T& aDefaultValue, objects::TechVintageVector<T>& aTechVector );
    
    static void initializeVector( const unsigned int aStartPeriod, const unsigned int aSize, objects::TechVintageVector<T>& aV );
    
private:
    //! The map from a TechVintageVector's ID to it's temporary storage.
    std::map<size_t, objects::PeriodVector<T> > mTempStore;
};

/*!
 * \brief A list of all of the data container types TechVectorParseHelper might contain
 *        to minimize the number of maps we have to make.
 */
using TechVectorParseHelperTempStoreTypes = boost::mpl::vector<double, Value>;
/*!
 * \brief Create a list of all the type of TechVectorParseHelper we will create.
 * \details We transform TechVectorParseHelperTempStoreTypes (i.e. double) to make it the
 *          template argument to TechVectorParseHelper as a pointer (i.e. TechVectorParseHelper<double>*)
 */
using TechVectorParseHelperTempStorePtrTypes = typename boost::mpl::transform<TechVectorParseHelperTempStoreTypes, boost::add_pointer<TechVectorParseHelper<boost::mpl::_> > >::type;
/*!
 * \brief Generate the type for our boost::fusion::map from storage type (i.e. double) to an
 *        pointer of TechVectorParseHelper of that type (i.e. TechVectorParseHelper<double>*)
 *        resulting in fusion::map<double, TechVectorParseHelper<double>*>
 */
using TechVectorParseHelperTempStoreMapType = typename boost::fusion::result_of::as_map<
    typename boost::fusion::result_of::as_vector<
        typename boost::mpl::transform_view<
            boost::mpl::zip_view<
                boost::mpl::vector<TechVectorParseHelperTempStoreTypes, TechVectorParseHelperTempStorePtrTypes> >,
                boost::mpl::unpack_args<boost::fusion::pair<boost::mpl::_1, boost::mpl::_2> >
            >
        >::type
    >::type;

/*!
 * \brief We make just one static instance of the TechVectorParseHelper temporary
 *        storafge instances as we really do not want to make copies of all of these
 *        arrays.
 */
extern TechVectorParseHelperTempStoreMapType sTechVectorParseHelperMap;

/*!
 * \brief A helper method for class that use aTechVector to cover the commmon case that
 *        they want to initialize the vector with some default value.
 * \param aDefaultValue The default value to set.
 * \param aTechVector The TechVintageVector which when initialized should have this default
 *                    value.
 */
template<typename T>
void TechVectorParseHelper<T>::setDefaultValue( const T& aDefaultValue, objects::TechVintageVector<T>& aTechVector ) {
    objects::PeriodVector<T>& periodVector = boost::fusion::at_key<T>( sTechVectorParseHelperMap )->getPeriodVector( aTechVector );
    std::fill( periodVector.begin(), periodVector.end(), aDefaultValue );
}

/*!
 * \brief Initialize the given TechVintageVector instance.
 * \details Initialize the given aTechVec if not already initialized with the given start period
 *          and size, then copy in any data that may have been set in temporary storage for the
 *          given instance.  Note that the temporary storage does not get removed yet in case there
 *          are other instances that are sharing that temporary storage.  Instead all temporary storage
 *          will go away after completeInit.
 * \param aStartPeriod The start period to initialize aTechVec.
 * \param aSize The size to initialize aTechVec.
 * \param aTechVec The instance of TechVintageVector to initialize.
 */
template<typename T>
void TechVectorParseHelper<T>::initializeVector( const unsigned int aStartPeriod, const unsigned int aSize, objects::TechVintageVector<T>& aTechVec ) {
    // Do not re-initialize an instance that has already been initialized.
    if( !aTechVec.isInitialized() ) {
        // Save temp data key before we overwrite it.
        size_t tempDataKey = reinterpret_cast<size_t>( aTechVec.mData );

        // Fill the the vector parameters and allocate it's memory.
        aTechVec.mStartPeriod = aStartPeriod;
        aTechVec.mSize = aSize;
        // Note an unititialized TechVintageVector will not have allocated any
        // memory for mData so we do not need to worry about freeing that here
        aTechVec.mData = new T[ aSize ];
        
        // Attempt to copy in data from temporary storage
        TechVectorParseHelper<T>* currTVParseHelper = boost::fusion::at_key<T>( sTechVectorParseHelperMap );
        
        if( currTVParseHelper ) {
            auto tempData = currTVParseHelper->mTempStore.find( tempDataKey );
            if( tempData != currTVParseHelper->mTempStore.end() ) {
                // Data was found in temporary storage for this instance so copy those values
                // over (for valid model periods only).
                const objects::PeriodVector<T>& pv = (*tempData).second;
                for(auto per = aStartPeriod; per < (aStartPeriod + aSize); ++per ) {
                    aTechVec[ per ] = pv[ per ];
                }
            }
            else {
                // No temporary data so just fill it with default values.
                std::fill( aTechVec.begin(), aTechVec.end(), T() );
            }
        }
        else {
            // The temporary storage has already been deleted.  All TechVintageVector should have
            // already been initialized by this point HOWEVER we do have some vectors in objects
            // that get created during the model simulation, such as in AGHG.
            // The temporary storage will not be available to them and if they attempted to store
            // values in it they would have gotten an error at that point.
            // So.. with all the being said if we get to this point we can assume there should
            // be no temporary storage and can just fill in default values.
            std::fill( aTechVec.begin(), aTechVec.end(), T() );
        }
    }
}

#endif // _TECH_VECTOR_PARSE_HELPER_H_

