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

#include "util/base/include/xml_parse_helper.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/data_definition_util.h"
#include "util/base/include/gcam_fusion.hpp"
#include "util/base/include/gcam_data_containers.h"
#include "util/base/include/factory.h"
#include "util/logger/include/logger_factory.h"
#include "util/logger/include/logger.h"
#include "util/base/include/configuration.h"

using namespace std;
using namespace rapidxml;

template<>
struct Factory<ITechnologyContainer::SubClassFamilyVector> {
    using FamilyBasePtr = ITechnologyContainer*;
    static bool canCreateType( const std::string& aXMLName ) {
        return TechnologyContainer::hasTechnologyType( aXMLName ) || StubTechnologyContainer::getXMLNameStatic() == aXMLName;
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

// Specialization for a vector (or any iteratable array that is not a map) container
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

// Specialization for non-parsable data
template<typename DataType>
typename boost::enable_if<typename CheckDataFlagHelper<DataType>::is_not_parsable,
bool>::type tagsMatchI(const std::string& aXMLTag, const DataType& aData) {
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
    using FactoryType = typename GetActualContainerType<DataType>::FactoryType;
    return FactoryType::canCreateType( aXMLTag );
}

template<typename DataType>
bool XMLParseHelper::tagsMatch(const std::string& aXMLTag, const DataType& aData) {
    return tagsMatchI(aXMLTag, aData);
}

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
        // The instance of the container has been set yet.
        if( deleteFlagSet ) {
            // log delete set but container not found
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
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
            // acutally a subclass of the type aData was declared as.  For instance
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
    getDataVector.getFullDataVector(parseChildHelper);
}

// Specialization for a vector (or any iteratable array that is not a map) container
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
            mainLog.setLevel( ILogger::ERROR );
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
            // acutally a subclass of the type aData was declared as.  For instance
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
            mainLog.setLevel( ILogger::ERROR );
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
            // acutally a subclass of the type aData was declared as.  For instance
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
    map<string, string> attrs = XMLParseHelper::getAllAttrs(aNode);
    auto yearIter = attrs.find( "year" );
    bool filloutFlagSet = XMLParseHelper::isAttrFlagSet( attrs, "fillout" );
    if( yearIter == attrs.end() ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Could not find year attribute to set simple array data" << endl;
    }
    else {
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
    std::string nodeValueStr(aNode->value(), aNode->value_size());
    auto nodeValue = boost::lexical_cast<typename DataType::value_type>(nodeValueStr);
    aData.mData = nodeValue;
}

template<>
void XMLParseHelper::parseData<Data<std::map<std::string, std::string>, SIMPLE> >(const rapidxml::xml_node<char>* aNode, Data<std::map<std::string, std::string>, SIMPLE>& aData) {
    std::map<std::string, std::string> attrs = getAllAttrs(aNode);
    aData.mData.insert(attrs.begin(), attrs.end());
}

template<typename DataType>
void XMLParseHelper::parseData(const rapidxml::xml_node<char>* aNode, DataType& aData) {
    parseDataI(aNode, aData);
}

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
    return std::map<std::string, std::string>();
}
}

namespace boost {
template<>
inline ILogger::WarningLevel lexical_cast<ILogger::WarningLevel, std::string>(const std::string& aStr) {
    return static_cast<ILogger::WarningLevel>(boost::lexical_cast<int>(aStr));
}
}
    
template<typename DataVectorType>
void ParseChildData::processDataVector( DataVectorType aDataVector ) {
    using namespace std;
    /*for(rapidxml::xml_attribute<char> *attr = mParentNode->first_attribute(); attr; attr = attr->next_attribute()) {
        const char* nameC = attr->name();
        const size_t nameCSize = attr->name_size();
        if(!(strncmp("fillout", nameC, nameCSize) == 0 ||
             strncmp("delete", nameC, nameCSize) == 0 ||
             strncmp("nocreate", nameC, nameCSize) == 0))
        {
            boost::fusion::for_each(boost::fusion::filter_if<boost::mpl::lambda<IsSimpleAndParsable<boost::mpl::_1> >::type>(aDataVector), [attr] (auto aData) {
                if(strncmp(aData.mDataName, attr->name(), attr->name_size()) == 0) {
                    /*! \pre Attributes only map to SIMPLE data types. * /
                    assert(aData.hasDataFlag(SIMPLE));
                    //XMLHelper<void>::parseSimple(attr, aData);
                    string valueStr(attr->value(), attr->value_size());
                    aData.mData = boost::lexical_cast<typename decltype(aData)::value_type>(valueStr);
                }
            });
        }
    }*/
    for(auto attr : mAttrs) {
        if(attr.first != "fillout" &&
           attr.first != "delete" &&
           attr.first != "nocreate")
        {
            boost::fusion::for_each(boost::fusion::filter_if<boost::mpl::lambda<IsSimpleAndParsable<boost::mpl::_1> >::type>(aDataVector), [attr] (auto aData) {
                if(aData.mDataName == attr.first) {
                    /*! \pre Attributes only map to SIMPLE data types. */
                    assert(aData.hasDataFlag(SIMPLE));
                    //XMLHelper<void>::parseSimple(attr, aData);
                    aData.mData = boost::lexical_cast<typename decltype(aData)::value_type>(attr.second);
                }
            });
        }
    }
    
    for(rapidxml::xml_node<char>* child = mParentNode->first_node(); child; child = child->next_sibling()) {
        if(child->type() == rapidxml::node_element) {
            bool found = mContainer ? mContainer->XMLParse(child) : false;
            // child could have changed and even moved to the end so double check
            if(child && !found) {
                string childNodeName(child->name(), child->name_size());
                boost::fusion::for_each(aDataVector, [child, childNodeName, &found] (auto& aData) {
                    if(!found && XMLParseHelper::tagsMatch(childNodeName, aData)) {
                        XMLParseHelper::parseData(child, aData);
                        found = true;
                    }
                });
                if(!found) {
                    cout << "Unknown tag: " << childNodeName << " encountered while processing "
                         << string(mParentNode->name(), mParentNode->name_size()) << endl;
                }
            }
        }
    }
}

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
void XMLParseHelper::parseData<Data<Logger*, CONTAINER> >(const rapidxml::xml_node<char>* aNode, Data<Logger*, CONTAINER>& aData) {
    parseDataI(aNode, aData);
}

template<typename ContainerType>
bool parseXMLInternal(const string& aXMLFile, ContainerType* aRootElement) {
    Data<ContainerType*, CONTAINER> root(aRootElement, "");
    
    boost::iostreams::mapped_file_source xmlFile(aXMLFile.c_str());
    
    rapidxml::xml_document<> doc;
    doc.parse<rapidxml::parse_non_destructive>(const_cast<char*>(xmlFile.data()));
    
    XMLParseHelper::parseData(doc.first_node(), root);
    
    xmlFile.close();
    
    return true;
}

bool XMLParseHelper::parseXML(const string& aXMLFile, Scenario* aRootElement) {
    return parseXMLInternal(aXMLFile, aRootElement);
}

bool XMLParseHelper::parseXML(const string& aXMLFile, LoggerFactoryWrapper* aRootElement) {
    return parseXMLInternal(aXMLFile, aRootElement);
}

bool XMLParseHelper::parseXML(const string& aXMLFile, Configuration* aRootElement) {
    boost::iostreams::mapped_file_source xmlFile(aXMLFile.c_str());
    
    rapidxml::xml_document<> doc;
    doc.parse<rapidxml::parse_non_destructive>(const_cast<char*>(xmlFile.data()));
    
    aRootElement->XMLParse(doc.first_node());
    
    xmlFile.close();
    
    return true;
}
