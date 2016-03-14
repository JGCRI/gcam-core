#ifndef _FACTORY_H_
#define _FACTORY_H_
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
 * \file factory.h
 * \ingroup util
 * \brief Factory class header file.
 * \author Pralit Patel
 */

#include <string>

#include <boost/type_traits.hpp>

#include <boost/mpl/vector.hpp>
#include <boost/mpl/front.hpp>
#include <boost/mpl/transform.hpp>
#include <boost/mpl/remove_if.hpp>

#include <boost/fusion/include/vector.hpp>
#include <boost/fusion/include/mpl.hpp>
#include <boost/fusion/include/any.hpp>
#include <boost/fusion/include/accumulate.hpp>

//#include "util/logger/include/ilogger.h"

/*!
 * \brief A generic factory that can create any member of a SubClassFamilyVector
 *        given the XML name.
 * \details The factory provides two static methods: First to see if a given XML
 *          name is part of the SubClassFamilyVector and the second to create an
 *          instance of that type (provided it was in fact a part of the family).
 *
 * \author Pralit Patel
 */
template<typename SubClassFamilyVector>
struct Factory {
    //! Create a subset of SubClassFamilyVector that can actually be created (non-abstract).
    using CreatableVector = typename boost::mpl::remove_if<SubClassFamilyVector, boost::is_abstract<boost::mpl::_> >::type;
    //! The SubClassFamilyVector as pointer types.
    using SubClassFamilyVectorPtr = typename boost::mpl::transform<SubClassFamilyVector, boost::add_pointer<boost::mpl::_> >::type;
    //! The createable members of SubClassFamilyVector as pointers.
    using CreatableVectorPtr = typename boost::mpl::transform<CreatableVector, boost::add_pointer<boost::mpl::_> >::type;
    //! We assume the base type of SubClassFamilyVector is the first member of the list.
    using FamilyBasePtr = typename boost::mpl::front<SubClassFamilyVectorPtr>::type;

    /*!
     * \brief Checks if the given XML name is an XML name of any of the members of
     *        SubClassFamilyVector.
     * \details Loops over the creatable types of the family and checks if it's
     *          getXMLNameStatic() matches the given aXMLName.
     * \param aXMLName The XML name to check.
     * \return If aXMLName is the XML name of one of the family members.
     */
    static bool canCreateType( const std::string& aXMLName ) {
        // TODO: no early exit option in any/for_each etc
        typename boost::fusion::result_of::as_vector<CreatableVectorPtr>::type asFusionVec;
        return boost::fusion::any( asFusionVec,
            [&aXMLName] ( auto aType ) -> bool {
                return aType->getXMLNameStatic() == aXMLName;
            } );
    }

    /*!
     * \brief Creats an instance of the family member who's XML name matches the
     *        given aXMLName.
     * \details Loops over the creatable types of the family and creates a new
     *          instance of the member who's getXMLNameStatic() matches the given
     *          aXMLName.  If no names match a warning is generated and a null
     *          pointer is returned.
     * \param aXMLName The XML name to check.
     * \return If aXMLName is the XML name of one of the family members.
     */
    static FamilyBasePtr createType( const std::string& aXMLName ) {
        // TODO: no early exit option in any/for_each etc
        FamilyBasePtr nullPtr = 0;
        typename boost::fusion::result_of::as_vector<CreatableVectorPtr>::type asFusionVec;
        // There is no way to break out of a loop over the fusion types so instead
        // we must "accumulate" the created type starting with a null pointer and
        // only replacing that null result with a new instance if the XML names match.
        FamilyBasePtr ret = boost::fusion::accumulate( asFusionVec, nullPtr,
            [&aXMLName] ( const FamilyBasePtr& aCurrResult, auto aType ) -> FamilyBasePtr {
                return !aCurrResult && aType->getXMLNameStatic() == aXMLName ?
                    new typename boost::remove_pointer<decltype( aType )>::type : aCurrResult;
            } );

        // If we still have a null pointer then no names matched.
        if( !ret ) {
            /*
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::SEVERE );
            mainLog << "Could not create unknown type " << aXMLName << std::endl;
            */
        }
        return ret;
    }
};

#endif // _FACTORY_H_
