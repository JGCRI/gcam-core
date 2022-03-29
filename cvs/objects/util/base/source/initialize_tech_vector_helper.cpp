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
* \file initialize_tech_vector_helper.cpp
* \ingroup Objects
* \brief The InitializeTechVectorHelper class source file.
*
* \author Pralit Patel
*/

#include <vector>

#include "util/base/include/definitions.h"
#include "util/base/include/initialize_tech_vector_helper.hpp"
#include "util/base/include/time_vector.h"
#include "util/base/include/value.h"
#include "util/base/include/tech_vector_parse_helper.h"
#include "util/base/include/gcam_fusion.hpp"
#include "util/base/include/gcam_data_containers.h"

// We need some place to static initialize the TechVectorParseHelper temporary storage arrays
TechVectorParseHelperTempStoreMapType sTechVectorParseHelperMap(boost::fusion::make_pair<double>( static_cast<TechVectorParseHelper<double>*>( 0 ) ),
                                                       boost::fusion::make_pair<Value>( static_cast<TechVectorParseHelper<Value>*>( 0 ) ) );

namespace objects {

/*!
 * \brief Constructor which takes the first model period the technology is available and
 *        the number of model period for which it will operate.
 * \param aStartPeriod The first period the technology operates.
 * \param aNumPeriodsActive The total number of model periods the technology operates.
 */
InitializeTechVectorHelper::InitializeTechVectorHelper( const int aStartPeriod, const int aNumPeriodsActive ):
mStartPeriod( aStartPeriod ),
mNumPeriodsActive( aNumPeriodsActive )
{
}

/*!
 * \brief Starts the process of initializing any uninitialized TechVintageVectors found in
 *        the given technology.
 * \param aContainer The current technology vintage in which to search for vectors.
 */
void InitializeTechVectorHelper::initializeTechVintageVector( ITechnology* aContainer ) {
    initializeTechVintageVectorImpl( aContainer );
}

/*!
 * \brief Starts the process of initializing any uninitialized TechVintageVectors found in
 *        the given GCAMConsumer.
 * \details We need have a method for GCAMConsumer even though it does not have vintaging
 *          since it contains some objects such as IOutput that are typicaly found in
 *          Technology.
 * \param aContainer The current technology vintage in which to search for vectors.
 */
void InitializeTechVectorHelper::initializeTechVintageVector( GCAMConsumer* aContainer ) {
    initializeTechVintageVectorImpl( aContainer );
}

/*!
 * \brief The implementation for initializeTechVintageVector which is templated simply to reduce
 *        code duplication from having the same code for each of the container type this class
 *        supports.
 * \details This method will use GCAMFusion to search for any Data with ARRAY flag contained anywhere
 *          in the given container (i.e. could be several conainers down).  The GCAMFusion callback
 *          processData will then handle any TechVintageVector by calling TechVectorParseHelper::initializeVector.
 * \param aContainer The GCAM Data CONTAINER from which to start the GCAMFusion search.
 */
template<typename ContainerType>
void InitializeTechVectorHelper::initializeTechVintageVectorImpl( ContainerType* aContainer ) {
    std::vector<FilterStep*> findArraySteps( 2, 0 );
    findArraySteps[ 0 ] = new FilterStep( "" );
    findArraySteps[ 1 ] = new FilterStep( "", DataFlags::ARRAY );
    GCAMFusion<InitializeTechVectorHelper, false, false, true> findTechVec( *this, findArraySteps );
    findTechVec.startFilter( aContainer );

    // clean up GCAMFusion related memory
    for( auto filterStep : findArraySteps ) {
        delete filterStep;
    }
}

template<typename T>
void InitializeTechVectorHelper::processData( T& aData ) {
    // ignore
}

template<>
void InitializeTechVectorHelper::processData<objects::TechVintageVector<double> >( objects::TechVintageVector<double>& aData ) {
    TechVectorParseHelper<double>::initializeVector( mStartPeriod, mNumPeriodsActive, aData );
}

template<>
void InitializeTechVectorHelper::processData<objects::TechVintageVector<Value> >( objects::TechVintageVector<Value>& aData ) {
    TechVectorParseHelper<Value>::initializeVector( mStartPeriod, mNumPeriodsActive, aData );
}

} // namespace objects
