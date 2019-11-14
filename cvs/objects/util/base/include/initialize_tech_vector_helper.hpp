#ifndef _INITIALIZE_TECH_VECTOR_HELPER_HPP_
#define _INITIALIZE_TECH_VECTOR_HELPER_HPP_
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


/*! 
* \file initialize_tech_vector_helper.hpp
* \ingroup util
* \brief Header file for the objects::InitializeTechVectorHelper class.
* \author Pralit Patel
*/

class ITechnology;
class GCAMConsumer;

namespace objects {
    /*!
    * \ingroup util
    * \brief A helper class to initialize objects::TechVintageVector once the
    *        containing classes know what their initial model year and lifetime
    *        will be.
    * \details This helper does a deep search using GCAMFusion for any class member
    *          Data tagged with ARRAY and type objects::TechVintageVector then initialize it by:
    *          - Setting the start period and sizing the array appropriately for the lifetime.
    *          - Copy in any parameters set as default values / XMLParse via TechVectorParseHelper.
    * \author Pralit Patel
    */
    class InitializeTechVectorHelper {
	public:
		InitializeTechVectorHelper( const int aStartPeriod, const int aNumPeriodsActive );

        void initializeTechVintageVector( ITechnology* aContainer );
        void initializeTechVintageVector( GCAMConsumer* aContainer );

        // GCAMFusion callbacks
        template<typename T>
        void processData( T& aData );

	private:
        //! The start period to set into the TechVintageVector instances.
        int mStartPeriod;

        //! The appropriate length to size the TechVintageVector instances.
        int mNumPeriodsActive;
        
        template<typename ContainerType>
        void initializeTechVintageVectorImpl( ContainerType* aContainer );
        
	};

}
#endif // _INITIALIZE_TECH_VECTOR_HELPER_HPP_
