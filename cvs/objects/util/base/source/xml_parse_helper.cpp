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
 * \file xml_parse_helper.cpp
 * \ingroup Objects
 * \brief XMLParseHelper class source file.
 * \author Pralit Patel
 */

#include <boost/iostreams/device/mapped_file.hpp>
#include <boost/fusion/include/filter_if.hpp>
#include <boost/preprocessor/tuple/enum.hpp>
#include <boost/preprocessor/seq.hpp>
#include <boost/preprocessor/control/iif.hpp>
#include <boost/preprocessor/punctuation/is_begin_parens.hpp>
#include <boost/preprocessor/tuple/enum.hpp>
#include <boost/mpl/set.hpp>

#include "util/base/include/xml_parse_helper.h"

// A tuple of GCAM Containers for which we want to debug the XML parsing
// such as `(World, ReserveSubResource)`.  Users can choose any class even
// if it is not the base class and they will need to add a `debugXMLParse`
// method to these classes (they do not need to add it to the base class
// however).  If no value is set here we avoid compiling the extra machinery
// required to support calling debugXMLParse.
#define XMLPARSE_DEBUG_CONTAINERS

/*!
 * \brief Figure out if we have any containers set to debug XML Parsing.
 * \details This is actually kind of tricky to do.  Not until the C++ 20 standard will there be a way
 *          to see if an "ENUM" is empty (length will still return 1).  So instead we see if there is an
 *          opening parenthesis and if so we _assume_ there is at least one container to debug.
 * \param .../__VA_ARGS__ The ENUM to check if is "empty"
 * \return 1 if we have containers to debug otherwise 0
 */
#define IS_XMLPARSE_DEBUG_ACTIVE(...) \
     BOOST_PP_IIF( BOOST_PP_IS_BEGIN_PARENS( __VA_ARGS__ ), 1, 0)

// We will set a preprocessor flag if we have have any containers to debug
// and we will check it to avoid compiling the extra code needed to do so
// if we have none to save compile / run time.
// Note we set this now as ExpandDataVector (included below) will want to check it.
#define XMLPARSE_DEBUG_ACTIVE IS_XMLPARSE_DEBUG_ACTIVE(XMLPARSE_DEBUG_CONTAINERS)

#include "util/logger/include/ilogger.h"
#include "util/base/include/data_definition_util.h"
#include "util/base/include/gcam_fusion.hpp"
#include "util/base/include/gcam_data_containers.h"
#include "util/base/include/factory.h"
#include "util/base/include/tech_vector_parse_helper.h"
#include "containers/include/scenario_runner_factory.h"
#include "containers/include/iscenario_runner.h"
#include "containers/include/single_scenario_runner.h"
#include "containers/include/mac_generator_scenario_runner.h"
#include "containers/include/batch_runner.h"
#include "target_finder/include/policy_target_runner.h"
#include "util/logger/include/logger_factory.h"
#include "util/logger/include/logger.h"
#include "util/base/include/configuration.h"

using namespace std;
using namespace rapidxml;

#if XMLPARSE_DEBUG_ACTIVE
// convert the list of containers to debug to a boost MPL set
using DebugContainersSet = boost::mpl::set<BOOST_PP_TUPLE_ENUM(XMLPARSE_DEBUG_CONTAINERS)>::type;

// A helper to use in our filter_if lambda expression below
template<typename T>
struct PairTypeHelper {
    using type = typename T::first_type;
};

/*!
 * \brief Call the debugXMLParse method if the container was included in the set to debug.
 * \details This gets tricky because we only ever hold a reference by the base class pointer.  Users may be
 *          interested in debugging one of the subclasses however.  We of course use the ExpandDataVector
 *          to do a double-dispatch to figure out which subclass we actually have so we take advantage of that
 *          machinery for this purpose as well.
 * \param aContainer A reference to the current parsing container vis-a-vis the ExpandDataVector so we can
 *                  use it to tell us which subclass is actually set and then check if that subclass is one of the
 *                  containers we are interested in debugging.
 * \param aNode The XML Node at the container level.  For instance if we are debugging SupplySector the aNode
 *              would be a reference to `<supplysector name="sector name">`
 * \return A flag from the debugging code if false signals the generic parsing code should continue its parsing, and
 *         if true no further processing will be attempted.
 */
template<typename SubClassFamilyVector>
bool callDebugXMLParse(ExpandDataVector<SubClassFamilyVector> aContainer, const rapidxml::xml_node<char>* aNode) {
    bool stopProcessing = false;
    // first, we need to filter all the possible subclasses to just those selected for debugging
    boost::fusion::for_each(boost::fusion::filter_if<
            boost::mpl::lambda<
                boost::mpl::has_key<
                    DebugContainersSet,
                    PairTypeHelper<boost::mpl::_1>
                >
            >::type >(aContainer.mSubClassPtrMap), [&stopProcessing, aNode] (auto& aPair)
    {
        // The fusion map where the `.first` is the subclass type and the `.second` is
        // a pointer to the actual instance of the subclass which would be non-NULL if
        // that is the type we have at runtime.
        if( aPair.second ) {
            // call the debugXMLParse method on this container as it was flagged to be
            // debugged
            stopProcessing = aPair.second->debugXMLParse(aNode);
        }
    });
    return stopProcessing;
}
#endif // XMLPARSE_DEBUG_ACTIVE

// A LOT of heavy lifting will go on in this cpp file.  The XML parse of all the
// GCAM classes will be generated here.
// Warning: The order of the function definitions here matters!  We need to ensure
// any template specializations are made before we start instantiating them.  Thus the
// file is sort of in reverse order where the top level parseXML functions are at the
// bottom and the lower level utilities are at the top.

//=============================================================================

void XMLParseHelper::initParser() {
    // At this point we should ensure we have a place to store temporary data for
    // TechVintageVectors.
    // Note we will clean up this memory (and all of the temporary arrays that it contains)
    // when we close the XML parser.
    boost::fusion::for_each(sTechVectorParseHelperMap, [] (auto& aPair) {
        using TVVHelperType = typename boost::remove_pointer<decltype( aPair.second )>::type;
        aPair.second = new TVVHelperType();
    });
}

void XMLParseHelper::cleanupParser() {
    // The "store" memory pool was used to store XML adjustments to be made in
    // stub technology during completeInit.  That should be done by now so we
    // should free the memory, which can be significant, to make room during
    // the model run.
    getStoreXMLMemoryPool().clear();
    
    // Clear out all temporary storage arrays for TechVintageVector.
    boost::fusion::for_each(sTechVectorParseHelperMap, [] (auto& aPair) {
        delete aPair.second;
        aPair.second = 0;
    });
}

//=============================================================================

// A specialization Factory for Technology Containers since they get parsed
// with the various technology tags or the stub-technology tag
template<>
struct Factory<ITechnologyContainer::SubClassFamilyVector> {
    using FamilyBasePtr = ITechnologyContainer*;
    static bool canCreateType( const std::string& aXMLName ) {
        return TechnologyContainer::hasTechnologyType( aXMLName ) ||
               StubTechnologyContainer::getXMLNameStatic() == aXMLName;
    }
    static ITechnologyContainer* createType( const std::string& aXMLName ) {
        ITechnologyContainer* ret = 0;
        if( TechnologyContainer::hasTechnologyType( aXMLName ) ) {
            ret = new TechnologyContainer();
        }
        else if( StubTechnologyContainer::getXMLNameStatic() == aXMLName ) {
            ret = new StubTechnologyContainer();
        }
        if( !ret ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Could not create " << aXMLName << " of type: " << typeid(ITechnologyContainer).name() << std::endl;
        }
        return ret;
    }
};

// A specialization Factory for Scenario Runners which just defers to the
// ScenarioRunnerFactory which has all kinds of specialized behavior.
// Wrapping it in the generic Factory allows us to use all the rest of the
// generic tagsMatch / parseData methods.
template<>
struct Factory<IScenarioRunner::SubClassFamilyVector> {
    using FamilyBasePtr = IScenarioRunner*;
    static bool canCreateType( const std::string& aXMLName ) {
        return ScenarioRunnerFactory::isOfType( aXMLName );
    }
    static IScenarioRunner* createType( const std::string& aXMLName ) {
        return ScenarioRunnerFactory::create( aXMLName ).release();
    }
};

//=============================================================================

/*!
 * \brief Some template meta programming that deduces the actual C++ data types to help clean up some
 *        syntax in parseData.
 * \details This helper will work through the various ways we hold references to CONTAINER classes which
 *          could be:
 *            - A single container: Region*
 *            - An array of containers: std::vector<Region*>
 *            - A map of containers: std::map<int, Region*>
 *
 *          For any of these types we determine the:
 *            - value_type: Region*
 *            - data_type: Region
 *            - FactoryType or the proper way of templating the generic factory: Factory<Region::SubClassFamilyVector>
 */
template<typename DataType, typename Enable = void>
struct GetActualContainerType;

// Specialization for a "Single" container
template<typename DataType>
struct GetActualContainerType<DataType, typename boost::enable_if<
    boost::mpl::and_<
        typename CheckDataFlagHelper<DataType>::is_container,
        boost::is_pointer<typename DataType::value_type>
    >
>::type> {
    using value_type = typename DataType::value_type;
    using data_type = typename boost::remove_pointer<value_type>::type;
    using FactoryType = Factory<typename data_type::SubClassFamilyVector>;
};

// Specialization for a vector (or any iterable array that is not a map) container
template<typename DataType>
struct GetActualContainerType<DataType, typename boost::enable_if<
    boost::mpl::and_<
        typename CheckDataFlagHelper<DataType>::is_container,
        boost::mpl::and_<has_iterator<typename DataType::value_type>, boost::mpl::not_<has_key_type<typename DataType::value_type> > >
    >
>::type> {
    using value_type = typename DataType::value_type::value_type;
    using data_type = typename boost::remove_pointer<value_type>::type;
    using FactoryType = Factory<typename data_type::SubClassFamilyVector>;
};

// Specialization for a map container
template<typename DataType>
struct GetActualContainerType<DataType, typename boost::enable_if<
    boost::mpl::and_<
        typename CheckDataFlagHelper<DataType>::is_container,
        has_key_type<typename DataType::value_type>
    >
>::type> {
    using value_type = typename DataType::value_type::mapped_type;
    using data_type = typename boost::remove_pointer<value_type>::type;
    using FactoryType = Factory<typename data_type::SubClassFamilyVector>;
};

//=============================================================================

// Note: we do our template specializations on tagsMatchI -- or internal to avoid
// function redefinition errors stemming from the original "promise" definition in
// the header.

// Specialization for non-parsable data
template<typename DataType>
typename boost::enable_if<typename CheckDataFlagHelper<DataType>::is_not_parsable,
bool>::type tagsMatchI(const std::string& aXMLTag, const DataType& aData) {
    // non-parsable should never match
    return false;
}

// Specialization for non-containers
template<typename DataType>
typename boost::disable_if<
    boost::mpl::or_<
        typename CheckDataFlagHelper<DataType>::is_container,
        typename CheckDataFlagHelper<DataType>::is_not_parsable
    >,
bool>::type tagsMatchI(const std::string& aXMLTag, const DataType& aData) {
    // SIMPLE and ARRAY Data are straightforward, just check if the data name matches
    // the tag
    return aXMLTag == aData.mDataName;
}

// Specialization for a containers
template<typename DataType>
typename boost::enable_if<
    boost::mpl::and_<
        typename CheckDataFlagHelper<DataType>::is_container,
        boost::mpl::not_<typename CheckDataFlagHelper<DataType>::is_not_parsable>
    >,
bool>::type tagsMatchI(const std::string& aXMLTag, const DataType& aData) {
    // For CONTAINER Data we do not check the Data name but instead need to check
    // the getXMLNameStatic of all the possible subclasses of this type and see
    // if any of them match. We can use our generic Factory to do the heavy lifting.
    using FactoryType = typename GetActualContainerType<DataType>::FactoryType;
    return FactoryType::canCreateType( aXMLTag );
}

/*!
 * \brief Check if the given XML tag corresponds to the given Data.
 * \tparam DataType The Data declaration such as `Data<double, SIMPLE>` or `Data<IClimateModel*, CONTAINER>`
 *                 which will drive specialized matching behavior depending on the type flags set on the Data.
 * \param aXMLTag The XML node name which we are trying to match up to some Data.
 * \param aData The current Data member variable definition which are checking against.
 * \return True if the XML tag matches this Data member variable, false otherwise.
 */
template<typename DataType>
bool XMLParseHelper::tagsMatch(const std::string& aXMLTag, const DataType& aData) {
    // defer to the tagsMatchI version to get the correct template specialization
    // based off of the flags set on the given Data
    return tagsMatchI(aXMLTag, aData);
}

//=============================================================================

// Note: we do our template specializations on parseDataI -- or internal to avoid
// function redefinition errors stemming from the original "promise" definition in
// the header.

// Specialization for non-parsable data
template<typename DataType>
typename boost::enable_if<typename CheckDataFlagHelper<DataType>::is_not_parsable,
void>::type parseDataI(const rapidxml::xml_node<char>* aNode, DataType& aData) {
    // the data is not parsable, we should not get here
    assert(false);
}

// Specialization for a "Single" container
template<typename DataType>
typename boost::enable_if<
    boost::mpl::and_<
        boost::mpl::not_<typename CheckDataFlagHelper<DataType>::is_not_parsable>,
        typename CheckDataFlagHelper<DataType>::is_container,
        boost::is_pointer<typename DataType::value_type>
    >,
void>::type parseDataI(const rapidxml::xml_node<char>* aNode, DataType& aData) {
    using namespace std;
    using data_type = typename GetActualContainerType<DataType>::data_type;
    using FactoryType = typename GetActualContainerType<DataType>::FactoryType;
    
    string nodeName(aNode->name(), aNode->name_size());
    map<string, string> attrs = XMLParseHelper::getAllAttrs(aNode);
    bool deleteFlagSet = XMLParseHelper::isAttrFlagSet( attrs, "delete" );
    bool noCreateFlagSet = XMLParseHelper::isAttrFlagSet( attrs, "nocreate" );
    if( !aData.mData ) {
        // The instance of the container has not been set yet.
        if( deleteFlagSet ) {
            // log delete set but container not found
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Could not delete node " << nodeName << " as it does not exist." << std::endl;
            return;
        } else if( noCreateFlagSet ) {
            // log nocreate
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::NOTICE );
            mainLog << "Did not create node " << nodeName << " as the nocreate input flag was set." << std::endl;
            return;
        }
        else {
            // No previous container so add a new one.
            
            // Some error checking to make sure the type of class that was created is
            // actually a subclass of the type aData was declared as.  For instance
            // LandAllocator has a base class ALandAllocatorItem however in
            // RegionMiniCAM::mLandAllocator we want to ensure only the type LandAllocator
            // is created and not for instance a LandLeaf.
            typename FactoryType::FamilyBasePtr temp = FactoryType::createType( nodeName );
            aData.mData = dynamic_cast<typename DataType::value_type>( temp );
            if( temp && !aData.mData ) {
                // log temp->getXMLName() is not a subclass of typename DataType::value_type::getXMLNameStatic()
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::ERROR );
                mainLog << "Attempted to set incompatible type " << nodeName << " as " << typeid(typename DataType::value_type).name() << std::endl;
                abort();
            }
        }
    }
    else {
        // There is already an instance set
        if( deleteFlagSet ) {
            // when we get a delete flag we simply delete and ignore the rest.
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::DEBUG );
            mainLog << "Deleting node: " << nodeName << std::endl;
            delete aData.mData;
            aData.mData = 0;
            return;
        }
        else {
            // else we can just use this instance
            // TODO: we should check to make sure the XML names are the same but we would currently fail this check often
        }
    }
    
    // parse child nodes
    ParseChildData parseChildHelper(aNode, attrs);
    parseChildHelper.setContainer(aData.mData);
    ExpandDataVector<typename data_type::SubClassFamilyVector> getDataVector;
    aData.mData->doDataExpansion( getDataVector );
#if XMLPARSE_DEBUG_ACTIVE
    // call debugXMLParse if applicable and check if the debugging code intends for
    // the generic parsing to continue or not
    bool stopProcessing = callDebugXMLParse<typename data_type::SubClassFamilyVector>(getDataVector, aNode);
    if(stopProcessing) {
        return;
    }
#endif //XMLPARSE_DEBUG_ACTIVE
    getDataVector.getFullDataVector(parseChildHelper);
}

// Specialization for a vector (or any iterable array that is not a map) container
template<typename DataType>
typename boost::enable_if<
    boost::mpl::and_<
        boost::mpl::not_<typename CheckDataFlagHelper<DataType>::is_not_parsable>,
        typename CheckDataFlagHelper<DataType>::is_container,
        boost::mpl::and_<has_iterator<typename DataType::value_type>, boost::mpl::not_<has_key_type<typename DataType::value_type> > >
    >,
void>::type parseDataI(const rapidxml::xml_node<char>* aNode, DataType& aData) {
    using namespace std;
    using data_type = typename GetActualContainerType<DataType>::data_type;
    using value_type = typename GetActualContainerType<DataType>::value_type;
    using FactoryType = typename GetActualContainerType<DataType>::FactoryType;
    
    string nodeName(aNode->name(), aNode->name_size());
    map<string, string> attrs = XMLParseHelper::getAllAttrs(aNode);
    bool deleteFlagSet = XMLParseHelper::isAttrFlagSet( attrs, "delete" );
    bool noCreateFlagSet = XMLParseHelper::isAttrFlagSet( attrs, "nocreate" );
    
    // We need to try to find in the array if the container exists
    auto dataIter = aData.mData.end();
    bool found = false;
    for( auto currIter = aData.mData.begin(); currIter != aData.mData.end() && !found; ++currIter ) {
        if( GetFilterForContainer<data_type>::filter_type::matchesXMLAttr( *currIter, attrs ) ) {
            found = true;
            dataIter = currIter;
        }
    }
    value_type currContainer = found ? *dataIter : 0;
    if( !found ) {
        // The instance of the container has not yet been set yet.
        if( deleteFlagSet ) {
            // log delete set but container not found
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING);
            mainLog << "Could not delete node " << nodeName << " as it does not exist." << std::endl;
            return;
        } else if( noCreateFlagSet ) {
            // log nocreate
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::NOTICE );
            mainLog << "Did not create node " << nodeName << " as the nocreate input flag was set." << std::endl;
            return;
        }
        else {
            // No previous container so add a new one.
            
            // Some error checking to make sure the type of class that was created is
            // actually a subclass of the type aData was declared as.  For instance
            // LandAllocator has a base class ALandAllocatorItem however in
            // RegionMiniCAM::mLandAllocator we want to ensure only the type LandAllocator
            // is created and not for instance a LandLeaf.
            typename FactoryType::FamilyBasePtr temp = FactoryType::createType( nodeName );
            currContainer = dynamic_cast<value_type>( temp );
            if( temp && !currContainer ) {
                // log temp->getXMLName() is not a subclass of typename DataType::value_type::getXMLNameStatic()
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::ERROR );
                mainLog << "Attempted to set incompatible type " << nodeName << " as " << typeid(typename DataType::value_type).name() << std::endl;
                abort();
            }
            aData.mData.push_back( currContainer );
        }
    }
    else {
        // There is already an instance set
        if( deleteFlagSet ) {
            // when we get a delete flag we simply delete and ignore the rest.
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::DEBUG );
            mainLog << "Deleting node: " << nodeName << std::endl;
            delete currContainer;
            currContainer = 0;
            aData.mData.erase( dataIter );
            return;
        }
        else {
            // else we can just use this instance
            // TODO: we should check to make sure the XML names are the same but we would currently fail this check often
        }
    }
    
    // parse child nodes
    ParseChildData parseChildHelper(aNode, attrs);
    parseChildHelper.setContainer(currContainer);
    ExpandDataVector<typename data_type::SubClassFamilyVector> getDataVector;
    currContainer->doDataExpansion( getDataVector );
#if XMLPARSE_DEBUG_ACTIVE
    // call debugXMLParse if applicable and check if the debugging code intends for
    // the generic parsing to continue or not
    bool stopProcessing = callDebugXMLParse<typename data_type::SubClassFamilyVector>(getDataVector, aNode);
    if(stopProcessing) {
        return;
    }
#endif //XMLPARSE_DEBUG_ACTIVE
    getDataVector.getFullDataVector(parseChildHelper);
}

// Specialization for a map container
template<typename DataType>
typename boost::enable_if<
    boost::mpl::and_<
        boost::mpl::not_<typename CheckDataFlagHelper<DataType>::is_not_parsable>,
        typename CheckDataFlagHelper<DataType>::is_container,
        has_key_type<typename DataType::value_type>
    >,
void>::type parseDataI(const rapidxml::xml_node<char>* aNode, DataType& aData) {
    using namespace std;
    using value_type = typename GetActualContainerType<DataType>::value_type;
    using data_type = typename GetActualContainerType<DataType>::data_type;
    using FactoryType = typename GetActualContainerType<DataType>::FactoryType;
    
    string nodeName(aNode->name(), aNode->name_size());
    map<string, string> attrs = XMLParseHelper::getAllAttrs(aNode);
    bool deleteFlagSet = XMLParseHelper::isAttrFlagSet( attrs, "delete" );
    bool noCreateFlagSet = XMLParseHelper::isAttrFlagSet( attrs, "nocreate" );
    
    // We need to try to find in the array if the container exists
    auto dataIter = aData.mData.end();
    bool found = false;
    for( auto currIter = aData.mData.begin(); currIter != aData.mData.end() && !found; ++currIter ) {
        if( GetFilterForContainer<data_type>::filter_type::matchesXMLAttr( (*currIter).second, attrs ) ) {
            found = true;
            dataIter = currIter;
        }
    }
    value_type currContainer = found ? (*dataIter).second : 0;
    if( !found ) {
        // The instance of the container has not been set yet.
        if( deleteFlagSet ) {
            // log delete set but container not found
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING);
            mainLog << "Could not delete node " << nodeName << " as it does not exist." << std::endl;
            return;
        } else if( noCreateFlagSet ) {
            // log nocreate
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::NOTICE );
            mainLog << "Did not create node " << nodeName << " as the nocreate input flag was set." << std::endl;
            return;
        }
        else {
            // No previous container so add a new one.
            
            // Some error checking to make sure the type of class that was created is
            // actually a subclass of the type aData was declared as.  For instance
            // LandAllocator has a base class ALandAllocatorItem however in
            // RegionMiniCAM::mLandAllocator we want to ensure only the type LandAllocator
            // is created and not for instance a LandLeaf.
            typename FactoryType::FamilyBasePtr temp = FactoryType::createType( nodeName );
            currContainer = dynamic_cast<value_type>( temp );
            if( temp && !currContainer ) {
                // log temp->getXMLName() is not a subclass of typename DataType::value_type::getXMLNameStatic()
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::ERROR );
                mainLog << "Attempted to set incompatible type " << nodeName << " as " << typeid(typename DataType::value_type).name() << std::endl;
                abort();
            }
            aData.mData[ boost::lexical_cast<typename DataType::value_type::key_type>( attrs[ GetFilterForContainer<data_type>::filter_type::getXMLAttrKey() ] ) ] = currContainer;
        }
    }
    else {
        // There is already an instance set
        if( deleteFlagSet ) {
            // when we get a delete flag we simply delete and ignore the rest.
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::DEBUG );
            mainLog << "Deleting node: " << nodeName << std::endl;
            delete currContainer;
            currContainer = 0;
            aData.mData.erase( dataIter );
            return;
        }
        else {
            // else we can just use this instance
            // TODO: we should check to make sure the XML names are the same but we would currently fail this check often
        }
    }
    
    // parse child nodes
    ParseChildData parseChildHelper(aNode, attrs);
    parseChildHelper.setContainer(currContainer);
    ExpandDataVector<typename data_type::SubClassFamilyVector> getDataVector;
    currContainer->doDataExpansion( getDataVector );
#if XMLPARSE_DEBUG_ACTIVE
    // call debugXMLParse if applicable and check if the debugging code intends for
    // the generic parsing to continue or not
    bool stopProcessing = callDebugXMLParse<typename data_type::SubClassFamilyVector>(getDataVector, aNode);
    if(stopProcessing) {
        return;
    }
#endif //XMLPARSE_DEBUG_ACTIVE
    getDataVector.getFullDataVector(parseChildHelper);
}

// Specializations for arrays of non-containers i.e. actual data but not TechVintageVector
template<typename DataType>
typename boost::enable_if<
    boost::mpl::and_<
        boost::mpl::not_<typename CheckDataFlagHelper<DataType>::is_not_parsable>,
        typename CheckDataFlagHelper<DataType>::is_array,
        boost::mpl::not_<typename std::is_same<typename DataType::value_type, objects::TechVintageVector<typename DataType::value_type::value_type> >::type>
    >,
void>::type parseDataI(const rapidxml::xml_node<char>* aNode, DataType& aData) {
    using namespace std;
    string nodeValueStr(aNode->value(), aNode->value_size());
    auto nodeValue = boost::lexical_cast<typename DataType::value_type::value_type>(nodeValueStr);
    // find the year attribute (make sure it is valid) and see if we need to fillout
    map<string, string> attrs = XMLParseHelper::getAllAttrs(aNode);
    auto yearIter = attrs.find( "year" );
    bool filloutFlagSet = XMLParseHelper::isAttrFlagSet( attrs, "fillout" );
    if( yearIter == attrs.end() ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Could not find year attribute to set simple array data" << endl;
    }
    else {
        // We are being generic about the type of vector we have here so we can't just
        // convert year to period.  We rely on GetIndexAsYear::convertIterToYear to
        // convert between iterators and years and naively loop over the entire vector
        // checking if we have hit the right year yet.
        const int currAttrYear = boost::lexical_cast<int>( (*yearIter).second );
        bool done = false;
        bool doFillout = false;
        for( auto iter = aData.mData.begin(); iter != aData.mData.end() && !done; ++iter ) {
            const int year = GetIndexAsYear::convertIterToYear( aData.mData, iter );
            if( currAttrYear == year || doFillout ) {
                (*iter) = nodeValue;
                doFillout = filloutFlagSet;
                done = !doFillout;
            }
        }
    }
}

// Specializations for arrays of non-containers i.e. actual data that is a TechVintageVector
template<typename DataType>
typename boost::enable_if<
    boost::mpl::and_<
        boost::mpl::not_<typename CheckDataFlagHelper<DataType>::is_not_parsable>,
        typename CheckDataFlagHelper<DataType>::is_array,
        typename std::is_same<typename DataType::value_type, objects::TechVintageVector<typename DataType::value_type::value_type> >::type
    >,
void>::type parseDataI(const rapidxml::xml_node<char>* aNode, DataType& aData) {
    // During XMLParse the TechVintageVector will not be initialized, therefore
    // this data will simply call parseDataI with the temporary PeriodVector
    // storage allocated for the given TechVintageVector instead.
    using T = typename DataType::value_type::value_type;
    objects::PeriodVector<T>& tvvParseArray =
        boost::fusion::at_key<T>( sTechVectorParseHelperMap )->getPeriodVector( aData.mData );
    Data<objects::PeriodVector<T>, ARRAY> tvvParseArrayData(tvvParseArray, aData.mDataName );
    parseDataI(aNode, tvvParseArrayData);
}

// Specializations for non-containers i.e. actual data that is a single value
template<typename DataType>
typename boost::enable_if<
    boost::mpl::and_<
        boost::mpl::not_<typename CheckDataFlagHelper<DataType>::is_not_parsable>,
        typename CheckDataFlagHelper<DataType>::is_simple
    >,
void>::type parseDataI(const rapidxml::xml_node<char>* aNode, DataType& aData) {
    // for SIMPLE data the only thing to do is coerce the string value into the requested
    // C++ type
    std::string nodeValueStr(aNode->value(), aNode->value_size());
    auto nodeValue = boost::lexical_cast<typename DataType::value_type>(nodeValueStr);
    aData.mData = nodeValue;
}

template<>
void XMLParseHelper::parseData<Data<std::map<std::string, std::string>, SIMPLE> >(const rapidxml::xml_node<char>* aNode, Data<std::map<std::string, std::string>, SIMPLE>& aData) {
    // TODO: kind of a hack to deal with keywords
    std::map<std::string, std::string> attrs = getAllAttrs(aNode);
    aData.mData.insert(attrs.begin(), attrs.end());
}

/*!
* \brief Check if the given XML tag corresponds to the given Data.
* \tparam DataType The Data declaration such as `Data<double, SIMPLE>` or `Data<IClimateModel*, CONTAINER>`
*                 which will drive specialized matching behavior depending on the type flags set on the Data.
* \param aNode The XML node which will be used to initialize some GCAM Data.
* \param aData The current Data member variable definition which indicated aNode corresponds to it.
*/
template<typename DataType>
void XMLParseHelper::parseData(const rapidxml::xml_node<char>* aNode, DataType& aData) {
    // defer to the parseDataI version to get the correct template specialization
    // based off of the flags set on the given Data
    parseDataI(aNode, aData);
}

//=============================================================================

/*!
 * \brief A helper template meta programming struct to help us filter a Data vector to only those that are SIMPLE and
 *        parsable.  This will be used to set Data that are specified as XML attributes.
 */
template<typename DataType>
struct IsSimpleAndParsable {
    using type = boost::mpl::and_<
        boost::integral_constant<bool, DataType::hasDataFlag(SIMPLE)>,
        boost::mpl::not_<
            boost::integral_constant<bool, DataType::hasDataFlag(NOT_PARSABLE)>
        >
    >;
};

namespace boost {
template<>
inline std::map<std::string, std::string> lexical_cast<std::map<std::string, std::string>, std::string>(const std::string& aStr) {
    // TODO: kind of a hack to deal with keywords and ensure everything compiles
    // we should never get here during runtime
    assert( false );
    return std::map<std::string, std::string>();
}
}

// ensure boost::lexical_cast knows how to parse logger levels
namespace boost {
template<>
inline ILogger::WarningLevel lexical_cast<ILogger::WarningLevel, std::string>(const std::string& aStr) {
    return static_cast<ILogger::WarningLevel>(boost::lexical_cast<int>(aStr));
}
}

//=============================================================================

/*!
 * \brief The call back from GCAM Fusion which has now expanded the full data vector for this CONTAINER subclass.
 * \details We can now loop over this Data vector and attempt to match up the child nodes of the current element to the
 *          Data member variables of the current container.  If the tags match up we will call parseData on it thus recursively
 *          processing the XML data.
 * \tparam DataVectorType The type of the full data vector which is dynamic in terms of compile time and runtime depending
 *                       on not just the fact that we are parsing a Resource object but also the fact that it could be a
 *                       RenewableResource etc.
 * \param aDataVector The boost fusion vector of Data which we can loop over to attempt to match up XML tags to Data names.
 */
template<typename DataVectorType>
void ParseChildData::processDataVector( DataVectorType aDataVector ) {
    using namespace std;
    // first attempt to parse any member variables that are set via attribute such as name
    // or year
    for(auto attr : mAttrs) {
        if(attr.first != "fillout" &&
           attr.first != "delete" &&
           attr.first != "nocreate")
        {
            // we optimize compile time / runtime here by limiting our search in the Data vector
            // to just those flagged as SIMPLE (but not also flagged as NOT_PARSABLE of course)
            boost::fusion::for_each(boost::fusion::filter_if<boost::mpl::lambda<IsSimpleAndParsable<boost::mpl::_1> >::type>(aDataVector), [attr] (auto aData) {
                if(aData.mDataName == attr.first) {
                    /*! \pre Attributes only map to SIMPLE data types. */
                    assert(aData.hasDataFlag(SIMPLE));
                    aData.mData = boost::lexical_cast<typename decltype(aData)::value_type>(attr.second);
                }
            });
        }
    }
    
    // loop over child nodes and attempt to match them to any elements in the Data vector
    for(rapidxml::xml_node<char>* child = mParentNode->first_node(); child; child = child->next_sibling()) {
        // skip whitespace for instance
        if(child->type() == rapidxml::node_element) {
            // If the current container is a subclass of AParsable the mContainer will have been
            // set and we should first ask it to attempt to parse this node.  If it returns true
            // that indicates it did in fact handle the data so we should skip any further action
            // on this node.
            bool found = mContainer ? mContainer->XMLParse(child) : false;
            // child could have changed and even moved to the end by XMLParse so double check
            if(child && !found) {
                string childNodeName(child->name(), child->name_size());
                // loop over the Data vector
                boost::fusion::for_each(aDataVector, [child, childNodeName, &found] (auto& aData) {
                    // check if the tags match (and we have not already found a match)
                    if(!found && XMLParseHelper::tagsMatch(childNodeName, aData)) {
                        // we have a match, now actually parse the XML into a C++ object
                        XMLParseHelper::parseData(child, aData);
                        found = true;
                    }
                });
                if(!found) {
                    // ideally we would send this to a logger however this method may
                    // be called before the loggers have been initialized
                    cout << "Unknown tag: " << childNodeName << " encountered while processing "
                         << string(mParentNode->name(), mParentNode->name_size()) << endl;
                }
            }
        }
    }
}

//=============================================================================

// This is kind of a wart.  Any AParsable subclasses which need to kick off recursive
// processing of its Data will have gotten a "promise" from XMLParseHelper that it would
// define how exactly to do that.  But we do not *actually* generate that code at that time
// because it would have to recursively generate ALL the GCAM CONTAINER objects contained
// below that hierarchy.  So to avoid the unresolved symbol error at the linker stage we
// manually force the generation of those parseData promises below:

template<>
void XMLParseHelper::parseData<Data<ITechnology*, CONTAINER> >(const rapidxml::xml_node<char>* aNode, Data<ITechnology*, CONTAINER>& aData) {
    parseDataI(aNode, aData);
}

template<>
void XMLParseHelper::parseData<Data<std::vector<ITechnologyContainer*>, CONTAINER> >(const rapidxml::xml_node<char>* aNode, Data<std::vector<ITechnologyContainer*>, CONTAINER>& aData) {
    parseDataI(aNode, aData);
}

template<>
void XMLParseHelper::parseData<Data<Solver*, CONTAINER> >(const rapidxml::xml_node<char>* aNode, Data<Solver*, CONTAINER>& aData) {
    parseDataI(aNode, aData);
}

template<>
void XMLParseHelper::parseData<Data<objects::PeriodVector<double>, ARRAY> >(const rapidxml::xml_node<char>* aNode, Data<objects::PeriodVector<double>, ARRAY>& aData) {
    parseDataI(aNode, aData);
}

template<>
void XMLParseHelper::parseData<Data<objects::PeriodVector<Value>, ARRAY> >(const rapidxml::xml_node<char>* aNode, Data<objects::PeriodVector<Value>, ARRAY>& aData) {
    parseDataI(aNode, aData);
}

template<>
void XMLParseHelper::parseData<Data<std::vector<int>, ARRAY> >(const rapidxml::xml_node<char>* aNode, Data<std::vector<int>, ARRAY>& aData) {
    parseDataI(aNode, aData);
}

template<>
void XMLParseHelper::parseData<Data<IScenarioRunner*, CONTAINER> >(const rapidxml::xml_node<char>* aNode, Data<IScenarioRunner*, CONTAINER>& aData) {
    parseDataI(aNode, aData);
}

template<>
void XMLParseHelper::parseData<Data<Logger*, CONTAINER> >(const rapidxml::xml_node<char>* aNode, Data<Logger*, CONTAINER>& aData) {
    parseDataI(aNode, aData);
}

//=============================================================================

/*!
 * \brief Parse the given XML file using aRootElement as the parsing context to start processing the XML.
 * \details We will load the XML file using a memory mapped file then have Rapid XML parse it.  We then
 *          kick off the actual parsing by making the first call to parseData which will recursively kick off:
 *            - At compile time the template instantiations to generate the XML parsing code for ALL of GCAM.
 *            - At runtime the handling of all of the XML nodes so that they get matched up with and initialize
 *             the GCAM data structures.
 *           Note, we assume that the instance of aRootElement already exists and is a "single" container.
 * \tparam ContainerType The type of the root CONTAINER to kick off processing.
 * \param aXMLFile A string representing the path to the XML file to parse.
 * \param aRootElement A valid instance of a GCAM class, such as Scenario, which starts the processing
 *                    of the XML nodes.
 * \return False if there was an error opening or with the XML syntax of aXMLFile.  Note, unrecognized nodes
 *         do not cause this method to return false however warnings about that will have been made.
 */
template<typename ContainerType>
bool parseXMLInternal(const string& aXMLFile, ContainerType* aRootElement) {
    try {
        // Create a Data wrapper around the given container.  Note, we are expecting
        // aRootElement to already exist and can not be changed so no need to worry
        // about setting the data name and can just leave it empty.
        Data<ContainerType*, CONTAINER> root(aRootElement, "");
        
        // open the file as a memory mapped file and make sure it was successful
        boost::iostreams::mapped_file_source xmlFile(aXMLFile.c_str());
        
        // Parse the file, note the memory for the parser will be released when doc
        // goes out of scope.
        rapidxml::xml_document<> doc;
        doc.parse<rapidxml::parse_non_destructive>(const_cast<char*>(xmlFile.data()));
        
        // Kick off the processing of the XML nodes starting with the root element.
        XMLParseHelper::parseData(doc.first_node(), root);
        
        xmlFile.close();
    }
    catch(std::ios_base::failure ioException) {
        cerr << "Could not open: " << aXMLFile << " failed with: "
             << ioException.what() << endl;
        return false;
    }
    catch(rapidxml::parse_error parseException) {
        cerr << "Failed to parse: " << aXMLFile << " with error: "
             << parseException.what() << endl;
        return false;
    }
    
    return true;
}

/*!
* \brief Parse the given XML file using the given Scenario as the parsing context to start processing the XML.
* \param aXMLFile A string representing the path to the XML file to parse.
* \param aRootElement A valid Scenario instance, which starts the processing of the XML nodes.
* \return False if there was an error opening or with the XML syntax of aXMLFile.  Note, unrecognized nodes
*         do not cause this method to return false however warnings about that will have been made.
*/
bool XMLParseHelper::parseXML(const string& aXMLFile, Scenario* aRootElement) {
    return parseXMLInternal(aXMLFile, aRootElement);
}

/*!
* \brief Parse the given XML file using the given IScenarioRunner as the parsing context to start processing the XML.
 * \details Note that aRootElement may not exist yet and will need to open the XML file to determine what the
 *          correct IScenarioRunner subclass to create.  Once we do that the parse proceeds in typical fashion.
* \param aXMLFile A string representing the path to the XML file to parse.
* \param aRootElement An IScenarioRunner instance by reference, which may bet reset to something else then will
 *     start the processing of the XML nodes.
* \return False if there was an error opening or with the XML syntax of aXMLFile.  Note, unrecognized nodes
*         do not cause this method to return false however warnings about that will have been made.
*/
bool XMLParseHelper::parseXML(const string& aXMLFile, IScenarioRunner* & aRootElement) {
    try {
        // we will first have to open and parse the XML file before we can determine the
        // correct type of IScenarioRunner to create
        boost::iostreams::mapped_file_source xmlFile(aXMLFile.c_str());
        
        rapidxml::xml_document<> doc;
        doc.parse<rapidxml::parse_non_destructive>(const_cast<char*>(xmlFile.data()));
        
        if(!aRootElement) {
            string runnerType = getNodeName(doc.first_node());
            aRootElement = ScenarioRunnerFactory::create(runnerType).release();
            if(!aRootElement) {
                // Didn't recognize runnerType, an error will have already been generated
                // so just close the file and indicate failure.
                xmlFile.close();
                return false;
            }
        }
        
        // We can now proceed with parsing as normal, we do not need to worry about the data
        // name of the root as we have already taken care of creating that.
        Data<IScenarioRunner*, CONTAINER> root(aRootElement, "");
        
        XMLParseHelper::parseData(doc.first_node(), root);
        
        xmlFile.close();
    }
    catch(std::ios_base::failure ioException) {
        cerr << "Could not open: " << aXMLFile << " failed with: "
             << ioException.what() << endl;
        return false;
    }
    catch(rapidxml::parse_error parseException) {
        cerr << "Failed to parse: " << aXMLFile << " with error: "
             << parseException.what() << endl;
        return false;
    }
    
    return true;
}

/*!
* \brief Parse the given XML file using the given LoggerFactoryWrapper as the parsing context to start processing the XML.
* \param aXMLFile A string representing the path to the XML file to parse.
* \param aRootElement A valid LoggerFactoryWrapper instance, which starts the processing of the XML nodes.
* \return False if there was an error opening or with the XML syntax of aXMLFile.  Note, unrecognized nodes
*         do not cause this method to return false however warnings about that will have been made.
*/
bool XMLParseHelper::parseXML(const string& aXMLFile, LoggerFactoryWrapper* aRootElement) {
    return parseXMLInternal(aXMLFile, aRootElement);
}

/*!
* \brief Parse the given XML file using the given Configuration as the parsing context to start processing the XML.
* \param aXMLFile A string representing the path to the XML file to parse.
* \param aRootElement A valid Configuration instance, which starts the processing of the XML nodes.
* \return False if there was an error opening or with the XML syntax of aXMLFile.  Note, unrecognized nodes
*         do not cause this method to return false however warnings about that will have been made.
*/
bool XMLParseHelper::parseXML(const string& aXMLFile, Configuration* aRootElement) {
    try {
        boost::iostreams::mapped_file_source xmlFile(aXMLFile.c_str());
        
        rapidxml::xml_document<char> doc;
        doc.parse<rapidxml::parse_non_destructive>(const_cast<char*>(xmlFile.data()));
        
        // We do not bother with the generic parseData for the Configuration object
        // as it is all custom behavior anyways and there is no recursive processing
        aRootElement->XMLParse(doc.first_node());
        
        xmlFile.close();
    }
    catch(std::ios_base::failure ioException) {
        cerr << "Could not open: " << aXMLFile << " failed with: "
             << ioException.what() << endl;
        return false;
    }
    catch(rapidxml::parse_error parseException) {
        cerr << "Failed to parse: " << aXMLFile << " with error: "
             << parseException.what() << endl;
        return false;
    }
    
    return true;
}
