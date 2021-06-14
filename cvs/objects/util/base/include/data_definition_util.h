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

#include "util/base/include/expand_data_vector.h"

/*!
 * \brief An enumeration of flags that could be useful as tags on the various Data
 *        definitions.
 * \details All Data definitions will have a flag of SIMPLE, ARRAY, or CONTAINER
 *          however even more flags may be added to give even more context about
 *          the definition such as SIMPLE | STATE which could then also be used to
 *          for instance search by in GCAMFusion.
 */
enum DataFlags {
    /*!
     * \brief A flag to indicate this Data definition is a simple member variable.
     * \details A SIMPLE data would be something such as an int or std::string.
     *          Basically a kind of data that would not contain any further Data
     *          definitions (of interest during introspection) and would not make
     *          sense to be indexed into such as with ARRAY definitions.
     */
    SIMPLE = 1 << 0,
    
    /*!
     * \brief A flag to indicate this Data definition is a simple array member variable.
     * \details A ARRAY data would be something such as PeriodVector<double>.
     *          Basically a kind of data that would not contain any further Data
     *          definitions (of interest during introspection) however can be
     *          indexed, for instance to only get the double in 10th position.
     */
    ARRAY = 1 << 1,
    
    /*!
     * \brief A flag to indicate this Data definition which contains a GCAM class
     *        that itself would have futher data definitions within it.
     * \details A CONTAINER data would be something such as IDiscreteChoice* or
     *          std::vector<Subsector*> where by the choice function and subsector
     *          each have further Data member variables themselves.  As implied
     *          earlier both "single containers" or "arrays of containers" can be
     *          tagged with just this flag (i.e. no need to add the ARRAY flag too).
     */
    CONTAINER = 1 << 2,

    /*!
     * \brief A flag to indicate this Data is a "state" variable or in other words
     *        it's value changes during World.calc.
     * \details This flag would generally be used in conjunction with a SIMPLE or
     *          ARRAY who's data type is Value or an array of Values.  Data marked
     *          with this flag can be searched for and re-organized so that "state"
     *          can be managed centrally stored/restored for instance during partial
     *          derivative calculations.
     */
    STATE = 1 << 3,
    
    /*!
     * \brief A flag to indicate this Data should not be available to set via XML parse.
     * \details Such a flag is useful to make sure users to not inadvertently set some
     *          parameters they should not but also to avoid trying to generate the C++
     *          to try to parse it.
     */
    NOT_PARSABLE = 1 << 4
    
    // potentially more flags here
};

/*!
 * \brief Basic structure for holding data members for GCAM classes.
 * \details The idea behind this structure is that every data member
 *          has two important properties: the data itself and a name
 *          used to refer to it (e.g., in XML inputs).  In addition
 *          there may be some additional compile time meta data that
 *          would be useful to generate code or search by in GCAM
 *          Fusion such as the data type or some combination from the
 *          enumeration DataFlags.
 *          This structure makes all of those available for inspection
 *          by other objects and functions.
 */
template<typename T, int DataFlagsDefinition>
struct Data {
    Data( T& aData, const char* aDataName ):mDataName( aDataName ), mData( aData ){}
    Data( T& aData, const std::string& aDataName ):mDataName( aDataName.c_str() ), mData( aData ){}
    /*! \note The Data struct does not manage any of it's member variables and
     *        instead simply holds reference to some original source.
     */
    virtual ~Data() { }
    
    /*!
     * \brief The human readable name for this data. 
     */
    const char* mDataName;

    /*!
     * \brief A reference to the actual data stored.
     */
    T& mData;
    
    /*!
     * \brief Type for this data item
     */
    typedef T value_type;
    
    /*!
     * \brief A constexpr (compile time) function that checks if a given aDataFlag
     *        matches any of the flags set set in DataFlagsDefinition.
     * \param aDataFlag A Flag that may be some combination of the flags declared
     *                  in the enumeration DataFlags.
     * \return True if aTypeFlag was set in the data definition flags used to
     *         define this data structure.
     */
    static constexpr bool hasDataFlag( const int aDataFlag ) {
        return ( ( aDataFlag & ~DataFlagsDefinition ) == 0 );
    }
    
    /*!
     * \pre All Data definitions must at the very least be tagged as SIMPLE,
     *      ARRAY, or CONTAINER.
     */
    static_assert( hasDataFlag( SIMPLE ) || hasDataFlag( ARRAY ) || hasDataFlag( CONTAINER ),
                   "Invalid Data definition: failed to declare the kind of data." );
};

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
        aVisitor.setSubClass( this ); \
    }

/*!
 * \brief This Macro is how GCAM member variables definitions that should be
 *        available for introspection should be made.
 * \details Note that while this is how all Data definitions should be generated
 *          the result of this Macro will not make sense unless called from within
 *          DEFINE_DATA_INTERNAL (through it proxy Macro calls DEFINE_DATA or
 *          DEFINE_DATA_WITH_PARENT).  The purpose of this Macro then is simply to
 *          collect all of the required pieces of a Data definition, reorganize them,
 *          and put them into a sequence of tokens so that then can be safely processed
 *          and stiched together in DEFINE_DATA_INTERNAL.  Thus a call such as
 *          ```
 *          DEFINE_VARIABLE( CONTAINER, "period", mVintages, std::map<int, ITechnology*> )
 *          ```
 *          will expand to:
 *          ```
 *          ( "period", mVintages, (Data<std::map<int)(ITechnology*>)(CONTAINER>) )
 *          ```
 *          Note the special consideration given to ensure type definitions with
 *          commas in them get handled properly.  This sequence will be used by
 *          DEFINE_DATA_INTERNAL to generate the declarations needed by the class declaration.
 * \param aDataTypeFlags DataFlags used to determine properties about this data.  Note this flag
 *                       must at least contain one of SIMPLE, ARRAY, or CONTAINER.
 * \param aDataName The human readable name.
 * \param aVarName The variable the user of the class will use for direct access to this Data.
 * \param aTypeDef (... / __VA_ARGS__) The type definition of the member variable.
 */
#define DEFINE_VARIABLE( aDataTypeFlags, aDataName, aVarName, ... ) \
    ( aDataName, aVarName, BOOST_PP_VARIADIC_TO_SEQ( Data<__VA_ARGS__, aDataTypeFlags> ) )

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
 * \details This Macro will be called by BOOST_PP_SEQ_FOR_EACH_I as it loops
 *          over all the requested Data definitions.  Each time it is called
 *          it will create the acutal member variable definition although the
 *          type is pointing to the corresponding element in the Boost::Fusion
 *          vector such as:
 *          boost::mpl::at_c< DataVectorType, 0 >::type::value_type mName;
 * \param r The next available BOOST_PP_FOR repetition.
 * \param data A base token to be always passed in (not used).
 * \param i The current iteration of the data definitions.
 * \param elem The variable name to define for direct access to the current Data.
 */
#define MAKE_VAR_REF( r, data, i, elem ) \
    boost::mpl::at_c< DataVectorType, i >::type::value_type elem;

/*!
 * \brief Creates data vector initialization syntax.
 * \details This Macro will be called by BOOST_PP_SEQ_FOR_EACH_I as it loops
 *          over all the requested Data definitions.  Each time it is called
 *          it will create the call to the constructor to the corresponding
 *          element in the Boost::Fusion Data vector such as:
 *          Data<std::string, SIMPLE>( mName, "name" )
 *          To create this definition all elements of each DATA DEFINITION will
 *          need to be used.
 * \param r The next available BOOST_PP_FOR repetition.
 * \param data A base token to be always passed in (not used).
 * \param i The current iteration of the data definitions.
 * \param elem The full definition for a single data as generated by DEFINE_VARIABLE.
 */
#define MAKE_DATA_STRUCT_INIT( r, data, i, elem ) \
    ( ( BOOST_PP_SEQ_ENUM( BOOST_PP_TUPLE_ELEM( 2, elem ) )( BOOST_PP_TUPLE_ELEM( 1, elem ), BOOST_PP_TUPLE_ELEM( 0, elem ) ) ) )

/*!
 * \brief Cut accross sequences of Data declarations to organize as
 *        sequences of variable names, Data type definitions, and data names.
 * \details Implementation from: http://stackoverflow.com/questions/26475453/how-to-use-boostpreprocessor-to-unzip-a-sequence
 *          The "unzipped" sequences can be accessed by index.  For data definitions
 *          given like:
 *          ```
 *          #define DECLS  \
 *            ( NAME_1, VAR_1, TYPE_1 ), \
 *            ( NAME_2, VAR_2, TYPE_2 )
 *          ```
 *          The macro calls:
 *          ```
 *          UNZIP(0, DECLS)
 *          UNZIP(1, DECLS)
 *          UNZIP(2, DECLS)
 *          ```   
 *          Would then be transformed to:
 *          ```
 *          (NAME_1) (NAME_2)
 *          (VAR_1) (VAR_2)
 *          (TYPE_1) (TYPE_2)
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
 * \brief Collects each DEFINE_VARIABLE definition and generates the full set of Data definitions.
 *        Note that for perforamance reasons the data vector is only generated upon
 *        request through the function generateDataVector().
 * \details The collected data generates the following definitions:
 *          1) A boost::fusion::vector typedef that contains all of the Data definitions types in a
 *             vector.
 *          2) A member function generateDataVector() which will generate a
               boost::fusion::vector that contains all of the Data structs which
 *             contains reference to actual member variable data and data names, etc.
 *          3) The actual member varible definitions such as mName, mYear so that
 *             users can continue to use the member variables as normal.
 *
 *          For instance a call such as:
 *          ```
 *          DEFINE_DATA_INTERNAL( DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ) )
 *          ```
 *          Would then be transformed to (although perhaps not so well formatted):
 *          ```
 *          typedef boost::fusion::vector<Data<std::string, SIMPLE> > DataVectorType;
 *          DataVectorType generateDataVector() {
 *              return DataVectorType>(Data<std::string, SIMPLE>( mName, "name" ) );
 *          }
 *          boost::mpl::at_c< DataVectorType, 0 >::type::value_type mName;
 *          ```
 */
#define DEFINE_DATA_INTERNAL( ... ) \
    typedef boost::fusion::vector<BOOST_PP_SEQ_ENUM( BOOST_PP_SEQ_FOR_EACH( FLATTEN, BOOST_PP_EMPTY, UNZIP( 2, __VA_ARGS__ ) ) )> DataVectorType; \
    DataVectorType generateDataVector() { \
        return DataVectorType( BOOST_PP_SEQ_ENUM( BOOST_PP_SEQ_FOR_EACH_I( MAKE_DATA_STRUCT_INIT,  BOOST_PP_EMPTY, BOOST_PP_VARIADIC_TO_SEQ( __VA_ARGS__ ) ) ) ); \
    } \
    BOOST_PP_SEQ_FOR_EACH_I( MAKE_VAR_REF,  BOOST_PP_EMPTY, UNZIP( 1, __VA_ARGS__ ) )

/*!
 * \brief A Macro to handle the special case where there are no Data definitions
 *        to be made.  This will correctly make the data definitions in a way that
 *        won't result in compiler error.
 */
#define DEFINE_DATA_INTERNAL_EMPTY() \
    typedef boost::fusion::vector<> DataVectorType; \
    DataVectorType generateDataVector() { return DataVectorType(); }

/*!
 * \brief A helper Macro to detect if we do or do not actually have any DEFINE_VARIABLE
 *        definitions.  We need to check explicitly since DEFINE_DATA_INTERNAL would
 *        generate invalid syntax if it's argument was infact empty.
 */
#define DEFINE_DATA_INTERNAL_CHECK_ARGS( ... ) \
    BOOST_PP_IIF( BOOST_PP_IS_BEGIN_PARENS( __VA_ARGS__ ), DEFINE_DATA_INTERNAL, DEFINE_DATA_INTERNAL_EMPTY ) ( __VA_ARGS__ )

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
#define DEFINE_DATA( aSubClassFamilySeq, ... ) \
    public: typedef boost::mpl::vector<BOOST_PP_SEQ_ENUM( aSubClassFamilySeq )> SubClassFamilyVector; \
    ACCEPT_EXPAND_DATA_VECTOR_METHOD( SubClassFamilyVector ) protected: \
    DEFINE_DATA_INTERNAL_CHECK_ARGS( __VA_ARGS__ )

/*!
 * \brief Define data entry point which adds a typdef to give reference to the direct
 *        parent class.  This is necessary since each definition of generateDataVector() is
 *        independent from it's parent classes.
 * \details The first argument is used to typeef the reference to the direct parent class,
 *          and the rest is given to DEFINE_DATA_INTERNAL to process the actual data definitions.
 */
#define DEFINE_DATA_WITH_PARENT( aParentClass, ... ) \
    public: typedef aParentClass ParentClass; \
    ACCEPT_EXPAND_DATA_VECTOR_METHOD( get_base_class<ParentClass>::type::SubClassFamilyVector ) protected: \
    DEFINE_DATA_INTERNAL_CHECK_ARGS( __VA_ARGS__ )

/*!
 * \brief A helper to organize the subclass family members into a sequence.
 */
#define DEFINE_SUBCLASS_FAMILY( ... ) \
    BOOST_PP_VARIADIC_TO_SEQ( __VA_ARGS__ )

#endif // _DATA_DEFINITION_UTIL_H_
