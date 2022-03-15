#ifndef _EXPAND_DATA_VECTOR_H_
#define _EXPAND_DATA_VECTOR_H_
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
 * \file expand_data_vector.h
 * \ingroup util
 * \brief ExpandDataVector class header file.
 * \author Pralit Patel
 */

#include <boost/type_traits.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/utility/enable_if.hpp>

#include <boost/mpl/not.hpp>
#include <boost/mpl/transform.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/zip_view.hpp>
#include <boost/mpl/transform_view.hpp>
#include <boost/mpl/unpack_args.hpp>
#include <boost/mpl/has_xxx.hpp>

#include <boost/fusion/include/vector.hpp>
#include <boost/fusion/include/map.hpp>
#include <boost/fusion/include/at_key.hpp>
#include <boost/fusion/include/for_each.hpp>
#include <boost/fusion/include/mpl.hpp>
#include <boost/fusion/include/join.hpp>
#include <boost/fusion/include/list.hpp>


#if XMLPARSE_DEBUG_ACTIVE
// forward declare needed for debug XMLParse workaround
template<typename SubClassFamilyVector>
class ExpandDataVector;
template<typename SubClassFamilyVector>
bool callDebugXMLParse( ExpandDataVector<SubClassFamilyVector> aSubClassContainer, const rapidxml::xml_node<char>* aNode );
#endif // XMLPARSE_DEBUG_ACTIVE

// Helper meta-functions
/*!
 * \brief A boost supplied macro that defines a meta-function in this case has_ParentClass<T>
 *        which returns a value type of true if the template parameter T has a defined member type
 *        ParentClass.
 */
BOOST_MPL_HAS_XXX_TRAIT_DEF( ParentClass );

/*!
 * \brief Helper meta-function that recursively traverses the sub-class heirarchy following
 *        the ParentClass typedefs.
 */
template<typename T, typename Enable=void>
struct get_base_class {
    using type = typename get_base_class<typename T::ParentClass>::type;
};

/*!
 * \brief The specialization where we have arrived to a class that has not defined the ParentClass
 *        type aka the BaseClass.  This is the stop point for the recursion.
 */
template<typename T>
struct get_base_class<T, typename boost::enable_if<boost::mpl::not_<has_ParentClass<T> > >::type> {
    using type = T;
};

/*!
 * \brief A utility for ensuring that we get the complete data vector from a
 *        data container taking into account the data vectors inherited from
 *        any base classes.
 * \details Expanding the full data vector is more tricky than it would first
 *          appear since we need to be able to determine which SubClass we are
 *          dealing with at runtime as we only ever store instances with the
 *          Base class pointer (this is typically accomplished with virtual
 *          methods).  However the return type of each SubClass would be different
 *          for each SubClass.  Thus we need to use a double dispatch based approach
 *          with a visitor that will collect the full data vector.  In order for this
 *          visitor to be generic it needs to be templated however mixing virtual
 *          methods with templated argument is not allowed by the compiler due to
 *          possibly infinite method combinations.
 *          Thus the solution we have here.  We create a templated ExpandDataVector
 *          visitor that is limited to work with only a specific subset of classes
 *          which is defined as the set of classes that are included the inheritance
 *          tree of interest (SubClassFamilyVector). We assume the first class in the
 *          list is the Base of the inheritance tree.
 *          We then use the visitor patter approach to call setSubClass to determine
 *          which member of the SubClassFamilyVector we are looking to expand.
 *          Then gatherDataVector can then be used to walk the inheritance tree to
 *          create a joint_view of the full data vector.
 *          Storing this joint_view proves tricky so instead we provide the 
 *          getFullDataVector which takes as an argument some templated class that
 *          provides a processDataVector method as a call back to do something useful with
 *          the joint_view of the full data vector.
 *
 * \author Pralit Patel
 */
template<typename SubClassFamilyVector>
class ExpandDataVector  {
#if XMLPARSE_DEBUG_ACTIVE
    // Special access for debugging XML Parse
    friend bool callDebugXMLParse<SubClassFamilyVector>( ExpandDataVector aSubClassContainer, const rapidxml::xml_node<char>* aNode );
#endif // XMLPARSE_DEBUG_ACTIVE
    
    public:

    /*!
     * \brief Default constructor.  Initializes that no member of SubClassFamilyVector
     *        is set.
     */
    ExpandDataVector() {
        reset();
    }

    /*!
     * \brief Clears any state about which member of SubClassFamilyVector has been
     *        set.
     */
    void reset() {
        boost::fusion::for_each( mSubClassPtrMap, [] ( auto& aPair ) {
            aPair.second = 0;
        } );
    }

    /*!
     * \brief Part of the double dispatch visitor pattern where SubClass will call back
     *        to this method determining at runtime which member of SubClassFamilyVector
     *        we are dealing with.
     * \details We will just track which subclass we have for now and wait to gather the
     *          full data vector until we have some object that can handle the data.
     * \tparam SubClass The type of the member of SubClassFamilyVector we are working with.
     * \param aSubClass The pointer to the instance of SubClass we want to get the data from.
     * \note This method will reset any previously set SubClass enabling this class to be
     *       re-used multiple times.
     */
    template<typename SubClass>
    void setSubClass( SubClass* aSubClass ) {
        reset();
        boost::fusion::at_key<SubClass>( mSubClassPtrMap ) = aSubClass;
    }

    /*!
     * \brief The accessor method to get the full datavector once setSubClass has been
     *        called.
     * \details The full data vector is gathered and passed as an argument to the processDataVector
     *          call back on the aDataHandler argument which can then do something useful with
     *          the information.
     * \tparam DataVecHandler Any object type that can provide the processDataVector call back which
     *                        takes the full data vector as an argument.
     * \param aDataHandler The instance of DataVecHandler to call back on.
     */
    template<typename DataVecHandler>
    void getFullDataVector( DataVecHandler& aDataHandler ) const {
        boost::fusion::for_each( mSubClassPtrMap, [this, &aDataHandler] ( auto& aPair ) {
            if( aPair.second ) {
                aDataHandler.processDataVector( this->gatherDataVector( aPair.second ) );
            }
        } );
    }

    protected:
    //! Alias a type that adds a pointer to each type in SubClassFamilyVector
    using SubClassVecPtr = typename boost::mpl::transform<SubClassFamilyVector, boost::add_pointer<boost::mpl::_> >::type;

    /*!
     * \brief Define the type that maps each type in SubClassFamilyVector to a pointer of that type.
     * \details In order to construct this type we need to zip and re-pack the type vectors using
     *          the tools provided to use by the MPL library such that with two vectors:
     *          [ C1, C2, C3 ... ] and [ C1*, C2*, C3* ... ] we get pairs of:
     *          [ pair( C1, C1* ), pair( C2, C2* ), pair( C3, C3* ) ... ]
     */
    using SubClassPtrMap = typename boost::fusion::result_of::as_map<
            typename boost::fusion::result_of::as_vector<
                typename boost::mpl::transform_view<
                    boost::mpl::zip_view<
                        boost::mpl::vector<SubClassFamilyVector, SubClassVecPtr>
                    >, boost::mpl::unpack_args<boost::fusion::pair<boost::mpl::_1, boost::mpl::_2> >
                >
            >::type
        >::type;

    //! The map which is used to track which SubClass of the SubClassFamilyVector we are dealing
    //! with and will keep a pointer to the instance of that type.
    SubClassPtrMap mSubClassPtrMap;

    /*!
     * \brief Helper meta-function that generates the type of the full data vector by recursively creating
     *        joint_views with the ParentClass' data vector until we reach the BaseClass.  Note since the
     *        data vectors are generated upon request a "view" upon a temporary will not be sufficient so
     *        we will need to copy the view into a list as a last step.
     */
    template<typename T, typename Enable=void>
    struct get_full_datavector_type {
        using type = typename boost::fusion::result_of::as_list<
                boost::fusion::joint_view<
                    typename get_full_datavector_type<typename T::ParentClass>::type,
                    typename T::DataVectorType
                >
            >::type;
    };

    /*!
     * \brief The specialization where we have arrived to a class that has not defined the ParentClass
     *        type aka the BaseClass.  Here the full data vector is simply the BaseClass data vector.
     *        This is the stop point for the recursion.
     */
    template<typename T>
    struct get_full_datavector_type<T, typename boost::enable_if<boost::mpl::not_<has_ParentClass<T> > >::type> {
        using type = typename T::DataVectorType;
    };

    /*!
     * \brief Creates a joint_view of the data vector from aSubClass and of aSubClass's parent.
     * \details This specialization is for the SubClass types in SubClassFamilyVector that have a
     *          parent class.  It will recursively call itself to fully gather the data vector of
     *          the parent class before creating the joint_view.  Note since the data vectors are
     *          generated upon request a "view" upon a temporary will not be sufficient so we will
     *          need to copy the view into a list as a last step.
     * \param aSubClass A SubClass in SubClassFamilyVector that has a parent from which to gather
     *                  the data vector from.
     * \return The full data vector of aSubClass including all of it's parent class' data members.
     */
    template<typename SubClass>
    typename boost::enable_if<has_ParentClass<SubClass>, typename get_full_datavector_type<SubClass>::type>::type gatherDataVector( SubClass* aSubClass ) const {
        // recursive call to get the full data vector of the parent class
        auto parentDataVec = gatherDataVector( static_cast<typename SubClass::ParentClass*>( aSubClass ) );
        // have this subclass generate it's own data vector
        auto subclassDataVec = aSubClass->generateDataVector();
        // join the parent and child data vectors together than copy to a list since
        // a view would just hold reference to otherwise temporary objects.
        // TODO: is there some way to "move" to avoid make extra copies.
        return boost::fusion::as_list( boost::fusion::join( parentDataVec, subclassDataVec ) );
    }

    /*!
     * \brief Gets the data vector from aSubClass.
     * \details This specialization is for the SubClass types in SubClassFamilyVector that have no
     *          parent class.  It is the stop point for recursion.
     * \param aSubClass A SubClass in SubClassFamilyVector that has no parent from which to gather
     *                  the data vector from.
     * \return The data vector of aSubClass.
     */
    template<typename SubClass>
    typename boost::enable_if<boost::mpl::not_<has_ParentClass<SubClass> >, typename get_full_datavector_type<SubClass>::type>::type gatherDataVector( SubClass* aSubClass ) const {
        return aSubClass->generateDataVector();
    }
};

#endif // _EXPAND_DATA_VECTOR_H_
