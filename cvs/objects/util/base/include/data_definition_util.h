#ifndef _DATA_DEFINITION_UTIL_H_
#define _DATA_DEFINITION_UTIL_H_
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
 * \file data_definition_util.h
 * \ingroup util
 * \brief Header file for that provide Macros and other utilities to generate member
 *        variable definitions which can be manipulated at compile time and accessed
 *        in a generic way.
 * \author Pralit Patel
 */

#include <string>

#include <boost/mpl/vector.hpp>
#include <boost/fusion/include/vector.hpp>
#include <boost/fusion/include/at.hpp>
#include <boost/fusion/include/mpl.hpp>

#include <boost/preprocessor/seq.hpp>
#include <boost/preprocessor/variadic/to_seq.hpp>
#include <boost/preprocessor/variadic/elem.hpp>
#include <boost/preprocessor/variadic/size.hpp>
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/punctuation/is_begin_parens.hpp>

/*
#include <boost/static_assert.hpp>
#include <boost/type_traits/is_copy_constructible.hpp>
#include <boost/type_traits/is_copy_assignable.hpp>
*/
#include <type_traits>

#include "util/base/include/expand_data_vector.h"

struct NamedFilter;
struct NoFilter;
struct YearFilter;

/*!
 * \brief Basic structure for holding data members for GCAM classes.
 * \details The idea behind this structure is that every data member
 *          has three important properties: the data itself, a name
 *          used to refer to it (e.g., in XML inputs), and a type.
 *          This structure makes all three of those available for
 *          inspection by other objects and functions.
 */
template<typename T>
struct Data {
    Data( const char* aDataName ):mDataName( aDataName ) {}
    Data( const std::string& aDataName ):mDataName( aDataName ) {}
    /*! \warning The Data struct will not attempt to manage memory allocation/de-allocation
     *           automatically and users will still be responsible for releasing memory
     *           themselves.
     */
    virtual ~Data() { }

    /*! \brief Type for this data item */
    typedef T value_type;
    /*! \brief The human readable name for this data. */
    const std::string mDataName;

    /*! \brief The actual data stored. */
    T mData;
};

template<typename T, typename Filter>
struct ContainerData : public Data<T> {
    ContainerData( const char* aDataName ):Data<T>( aDataName ) {}
    ContainerData( const std::string& aDataName ):Data<T>( aDataName ) {}
    virtual ~ContainerData() { }
    typedef Filter filter_type;
};

template<typename T>
struct ArrayData : public Data<T> {
    ArrayData( const char* aDataName ):Data<T>( aDataName ) {}
    ArrayData( const std::string& aDataName ):Data<T>( aDataName ) {}
    virtual ~ArrayData() { }
};

//! The name to call the variable which will hold all Data structs in a vector.
#define DATA_VECTOR_NAME mDataVector

/*!
 * \brief Macro to add to a class a method to accept the ExpandDataVector visitor
 * \details The ExpandDataVector visitor works with this method to
 *          produce a vector of all of the data elements for an object
 *          (including those inherited from base classes).  Each class
 *          needs to have such a method, and this macro produces it.
 */
#define ACCEPT_EXPAND_DATA_VECTOR_METHOD( aTypeDef ) \
    friend class ExpandDataVector<aTypeDef>; \
    virtual void doDataExpansion( ExpandDataVector<aTypeDef>& aVisitor ) { \
        using ThisType = std::remove_reference<decltype( *this )>::type; \
        static_assert( !std::is_copy_constructible<ThisType>::value && \
                       !std::is_copy_assignable<ThisType>::value, \
                       "Container classes must not be copyable. They should use the clone() idiom if need be." ); \
        aVisitor.setSubClass( this ); \
    }

/*!
 * \brief A Macro to define a Data struct for simple data such as an int.
 
 * \details This macro produces a declaration for a struct Data (see
 *          above).  The arguments to the macro are the name of the
 *          member (i.e., the token that will be used to refer to it
 *          in the class's methods) the type, and the string that
 *          gives the human-readable name (i.e., what will be used to
 *          refer to the data outside of the class, for example in XML
 *          input files).  For example, the declaration:
 *
 *          ```
 *          DEFINE_SIMPLE_VARIABLE( int, "year" )
 *          ```
 *          will expand to
 *          ```
 *          Data< int >
 *          ```
 *          The variable name is thrown away.  The reason that the
 *          variable name is included at all is that the type name
 *          could have commas in it, which will make it look to the
 *          macro like a sequence of arguments.  This macro does the
 *          work of separating the name from the type and uses the
 *          latter to generate the declaration.
 *
 * \warning We need to be careful about commas being in the type definition (such
 *          as with map definitions).  We therfore assume the last argument is the
 *          data name and the rest is the type definition.  We also avoid adding commas
 *          now and instead just keep it as a SEQ.
 * \param ...[0:n-1] The type definition.
 * \param ...[n:n] The human readable name.
 * \param The Data definition as a sequence of tokens.
 */
#define DEFINE_SIMPLE_DATA_STRUCT( aTypeAndName... ) \
    BOOST_PP_VARIADIC_TO_SEQ( Data< BOOST_PP_SEQ_ENUM( BOOST_PP_SEQ_POP_BACK( BOOST_PP_VARIADIC_TO_SEQ( aTypeAndName ) ) ) > )

#define DEFINE_CONTAINER_DATA_STRUCT( aTypeAndName... ) \
    BOOST_PP_VARIADIC_TO_SEQ( ContainerData< BOOST_PP_SEQ_ENUM( BOOST_PP_SEQ_POP_BACK( BOOST_PP_VARIADIC_TO_SEQ( aTypeAndName ) ) ) > )

#define DEFINE_ARRAY_DATA_STRUCT( aTypeAndName... ) \
    BOOST_PP_VARIADIC_TO_SEQ( ArrayData< BOOST_PP_SEQ_ENUM( BOOST_PP_SEQ_POP_BACK( BOOST_PP_VARIADIC_TO_SEQ( aTypeAndName ) ) ) > )

/*!
 * \brief Gathers the definiiton for a piece of data to be collected as a sequence
 *        of tokens to be stiched together into a vector of definitions.
 
 * \details This macro takes a sequence of (variable-name, type, human-name) and
 *          transforms the type in the sequence to a Data struct.  Thus:
 *          ```
 *          CREATE_SIMPLE_VARIABLE( mYear, int, "year")
 *          ```
 *          will expand to:
 *          ```
 *          (mYear, Data< int > , "year")
 *          ```
 *          This sequence will be used by other macros to generate the
 *          declarations needed by the class declaration.
 * \param aVarName The variable the user of the class will use for direct access to this Data.
 * \param ...[0:n-1] The type definition.
 * \param ...[n:n] The human readable name.
 */
#define CREATE_SIMPLE_VARIABLE( aVarName, aTypeAndName... ) \
    ( aVarName, DEFINE_SIMPLE_DATA_STRUCT( aTypeAndName ), BOOST_PP_VARIADIC_ELEM( BOOST_PP_DEC( BOOST_PP_VARIADIC_SIZE( aTypeAndName ) ), aTypeAndName ) )

#define CREATE_CONTAINER_VARIABLE( aVarName, aTypeAndName... ) \
    ( aVarName, DEFINE_CONTAINER_DATA_STRUCT( aTypeAndName ), BOOST_PP_VARIADIC_ELEM( BOOST_PP_DEC( BOOST_PP_VARIADIC_SIZE( aTypeAndName ) ), aTypeAndName ) )

#define CREATE_ARRAY_VARIABLE( aVarName, aTypeAndName... ) \
    ( aVarName, DEFINE_ARRAY_DATA_STRUCT( aTypeAndName ), BOOST_PP_VARIADIC_ELEM( BOOST_PP_DEC( BOOST_PP_VARIADIC_SIZE( aTypeAndName ) ), aTypeAndName ) )

/*!
 * \brief Identity transformation. To be used with FOR_EACH metafunction to Flatten the nesting of sequences one level.

 * \details Example.  Starting with a sequence like this
 *          ```
 *          (a) (b) (c)
 *          ```
 *          will become:
 *          ```
 *          a b c
 *          ```
 * \param s The next available BOOST_PP_FOR repetition.
 * \param data A base token to be always passed in (not used).
 * \param elem The sequence of tokens represetning the current data definition.
 */
#define FLATTEN( r, data, elem ) \
    elem

/*!
 * \brief Creates the direct access variable definition.
 * \details All of the members for a class will be stored in a
 *          Boost::Fusion vector.  This Macro will be called by
 *          BOOST_PP_SEQ_FOR_EACH_I as it loops over all the requested
 *          Data definitions.  Each time it is called it will create a
 *          reference with the requested variable name pointing to the
 *          corresponding element in the Boost::Fusion vector.  This
 *          bit of programming legerdemain is what allows us to access
 *          a member variable equally well through the variable name
 *          or through the vector of member variables.
 * \param r The next available BOOST_PP_FOR repetition.
 * \param data A base token to be always passed in (note used).
 * \param i The current iteration of the data definitions.
 * \param elem The variable name to define for direct access to the current Data.
 */
#define MAKE_VAR_REF( r, data, i, elem ) \
    boost::mpl::at_c< DataVectorType, i >::type::value_type& elem= boost::fusion::at_c<i>( DATA_VECTOR_NAME ).mData;

/*!
 * \brief Cut accross sequences of Data declarations to organize as
 *        sequences of variable names, Data type definitions, and data names.
 * \details Implementation from: http://stackoverflow.com/questions/26475453/how-to-use-boostpreprocessor-to-unzip-a-sequence
 *          The "unzipped" sequences can be accessed by index.  For data definitions
 *          given like:
 *          ```
 *          #define DECLS  \
 *            ( VAR_1, TYPE_1, NAME_1), \
 *            ( VAR_2, TYPE_2, NAME_2)
 *          ```
 *          The macro calls:
 *          ```
 *          UNZIP(0, DECLS)
 *          UNZIP(1, DECLS)
 *          UNZIP(2, DECLS)
 *          ```   
 *          Would then be transformed to:
 *          ```
 *          (VAR_1) (VAR_2)
 *          (TYPE_1) (TYPE_2)
 *          (NAME_1) (NAME_2)
 *          ```
 */
#define UNZIP_MACRO(s, data, elem) BOOST_PP_TUPLE_ELEM(data, elem)

#define UNZIP(i, ...) \
    BOOST_PP_SEQ_TRANSFORM( \
        UNZIP_MACRO, \
        i, \
        BOOST_PP_VARIADIC_TO_SEQ(__VA_ARGS__) \
    )

/*!
 * \brief Collects each CREATE_*_VARIABLE definition and generates the full set of Data definitions.
 * \details The collected data generates the following definitions:
 *          1) A boost::mpl::vector that contains all of the Data definitions types in a
 *             vector and forms the type to base mDataVector off of.
 *          2) The boost::fusion::vector that contains all of the Data structs which
 *             contains the actual member variable data and data names, etc.  This
 *             will be defined in a member variable in the name defined by DATA_VECTOR_NAME.
 *          3) Aliases for direct access varible definitions such as mName, mYear so that
 *             users can continue to use the member variables as normal.
 */
#define DEFINE_DATA_INTERNAL( aDefList... ) \
    typedef boost::mpl::vector<BOOST_PP_SEQ_ENUM( BOOST_PP_SEQ_FOR_EACH( FLATTEN, BOOST_PP_EMPTY, UNZIP( 1, aDefList ) ) )> DataVectorType; \
    boost::fusion::result_of::as_vector<DataVectorType>::type DATA_VECTOR_NAME = boost::fusion::result_of::as_vector<DataVectorType>::type( BOOST_PP_SEQ_ENUM( UNZIP( 2, aDefList ) ) ); \
    BOOST_PP_SEQ_FOR_EACH_I( MAKE_VAR_REF,  BOOST_PP_EMPTY, UNZIP( 0, aDefList ) )

#define DEFINE_DATA_INTERNAL_EMPTY() \
    typedef boost::mpl::vector<> DataVectorType; \
    boost::fusion::result_of::as_vector<DataVectorType>::type DATA_VECTOR_NAME = boost::fusion::result_of::as_vector<DataVectorType>::type();

#define DEFINE_DATA_INTERNAL_CHECK_ARGS( aDefList... ) \
    BOOST_PP_IIF( BOOST_PP_IS_BEGIN_PARENS( aDefList ), DEFINE_DATA_INTERNAL, DEFINE_DATA_INTERNAL_EMPTY ) ( aDefList )

/*!
 * \brief Define data entry point.  In this definition a user must give a sequence
 *        of all of the possible members of the inheritance heirarchy this class
 *        belongs to starting with itself.  This is necessary in order to get the
 *        full data vector from sub-classes at runtime.
 * \details The first argument is used to typedef the SubClassFamilyVector, and the
 *          rest is given to DEFINE_DATA_INTERNAL to process the actual data definitions.
 *          The DEFINE_SUBCLASS_FAMILY macro can be used to create the SubClassFamilySeq
 *          argument.
 */
#define DEFINE_DATA( aSubClassFamilySeq, aDefList... ) \
    public: typedef boost::mpl::vector<BOOST_PP_SEQ_ENUM( aSubClassFamilySeq )> SubClassFamilyVector; \
    ACCEPT_EXPAND_DATA_VECTOR_METHOD( SubClassFamilyVector ) protected: \
    DEFINE_DATA_INTERNAL_CHECK_ARGS( aDefList )

/*!
 * \brief Define data entry point which adds a typdef to give reference to the direct
 *        parent class.  This is necessary since each definition of mDataVector is
 *        independent from it's parent classes.
 * \details The first argument is used to typeef the reference to the direct parent class,
 *          and the rest is given to DEFINE_DATA_INTERNAL to process the actual data definitions.
 */
#define DEFINE_DATA_WITH_PARENT( aParentClass, aDefList... ) \
    public: typedef aParentClass ParentClass; \
    ACCEPT_EXPAND_DATA_VECTOR_METHOD( get_base_class<ParentClass>::type::SubClassFamilyVector ) protected: \
    DEFINE_DATA_INTERNAL_CHECK_ARGS( aDefList )

/*!
 * \brief A helper to organize the subclass family members into a sequence.
 */
#define DEFINE_SUBCLASS_FAMILY( aClassList... ) \
    BOOST_PP_VARIADIC_TO_SEQ( aClassList )

#endif // _DATA_DEFINITION_UTIL_H_
