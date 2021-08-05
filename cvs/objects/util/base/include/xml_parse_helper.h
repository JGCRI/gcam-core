#ifndef _XML_PARSE_HELPER_H_
#define _XML_PARSE_HELPER_H_
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
 * \file xml_parse_helper.h
 * \brief A set of helper function for reading xml data.
 * \note This file contains two things.
 *       - XMLHelper, A static class that has methods for parsing XML data and
 *         static data members which cache information required by the parser.
 *       - A series of global utility functions for writing XML data.
 * \todo XMLHelper should be converted into a non-static XMLReader class. The
 *       static data members could then be regular data members. The interface
 *       to the class should not use template functions, but the class could use
 *       them as helper methods. There are several functions that are used
 *       to read XML that are not part of XMLHelper. These should be moved in.
 * \todo This file needs refactoring. The XML writing utility functions should
 *       be moved to a non-static XMLWriter class. The class should store the
 *       tabs object and output stream.
 * \warning This class is hacked b/c of poor MSVC template support. This makes
 *          it much uglier.
 * \details This library contains a set of routines for reading xml data and
 *          attribute values. It is a templated library so that it should work
 *          with any data type.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <string>
#include <vector>
#include <map>
#include <cstring>
#include <boost/lexical_cast.hpp>
#include <rapidxml.hpp>
#include "util/base/include/iparsable.h"

class Scenario;
class IScenarioRunner;
class LoggerFactoryWrapper;
class Configuration;

class ParseChildData {
public:
    ParseChildData(const rapidxml::xml_node<char>* aNode, const std::map<std::string, std::string>& aAttrs):mParentNode(aNode), mAttrs(aAttrs), mContainer(0) {}
    
    template<typename ContainerType>
    typename boost::disable_if<
        boost::is_base_of<AParsable, ContainerType>,
    void>::type setContainer(ContainerType* aContainer) {
    }
    
    template<typename ContainerType>
    typename boost::enable_if<
        boost::is_base_of<AParsable, ContainerType>,
    void>::type setContainer(ContainerType* aContainer) {
        mContainer = aContainer;
    }
    
    template<typename DataVectorType>
    void processDataVector( DataVectorType aDataVector );
    
private:
    //! The Parent XML Node
    const rapidxml::xml_node<char>* mParentNode;
    
    const std::map<std::string, std::string>& mAttrs;
    
    AParsable* mContainer;
    
};

struct XMLParseHelper {
    static void initParser();
    
    static void cleanupParser();
    
    static std::string getNodeName(const rapidxml::xml_node<char>* aNode) {
        return std::string(aNode->name(), aNode->name_size());
    }
    
    template<typename T>
    static T getValue(const rapidxml::xml_node<char>* aNode) {
        std::string nodeValueStr(aNode->value(), aNode->value_size());
        return getValue<T>(nodeValueStr);
    }
    
    template<typename T>
    static T getValue(const std::string& aValueStr) {
        try {
            T returnValue = boost::lexical_cast<T>( aValueStr );
            return returnValue;
        }
        catch( boost::bad_lexical_cast& ) {
            std::cout << "Cast of node with value " << aValueStr << " to return type " << typeid(T).name() << " failed." << std::endl;
        }
        
        return T();
    }
    
    static std::map<std::string, std::string> getAllAttrs(const rapidxml::xml_node<char>* aNode) {
        using namespace std;
        map<string, string> ret;
        for(rapidxml::xml_attribute<char> *attr = aNode->first_attribute(); attr; attr = attr->next_attribute()) {
            string key(attr->name(), attr->name_size());
            string value(attr->value(), attr->value_size());
            ret[key] = value;
        }
        return ret;
    }
    
    static bool isAttrFlagSet(const std::map<std::string, std::string>& aAttrs, const std::string& aFlag) {
        auto iter = aAttrs.find(aFlag);
        return iter != aAttrs.end() && (*iter).second == "1";
    }
    
    template<typename DataType>
    static bool tagsMatch(const std::string& aXMLTag, const DataType& aData);

    template<typename DataType>
    static void parseData(const rapidxml::xml_node<char>* aNode, DataType& aData);
    
    static bool parseXML( const std::string& aXMLFile, Scenario* aRootElement );
    
    static bool parseXML( const std::string& aXMLFile, IScenarioRunner* & aRootElement );
    
    static bool parseXML( const std::string& aXMLFile, LoggerFactoryWrapper* aRootElement );
    
    static bool parseXML( const std::string& aXMLFile, Configuration* aRootElement );
    
    static rapidxml::xml_node<char>* deepClone(rapidxml::xml_node<char>* aNode) {
        rapidxml::memory_pool<char>& memoryPool = getStoreXMLMemoryPool();
        char* nameC = memoryPool.allocate_string(aNode->name(), aNode->name_size());
        char* valueC = memoryPool.allocate_string(aNode->value(), aNode->value_size());
        rapidxml::xml_node<char>* copy = memoryPool.allocate_node(aNode->type(), nameC, valueC, aNode->name_size(), aNode->value_size());
        for(rapidxml::xml_attribute<char> *attr = aNode->first_attribute(); attr; attr = attr->next_attribute()) {
            char* nameC = memoryPool.allocate_string(attr->name(), attr->name_size());
            char* valueC = memoryPool.allocate_string(attr->value(), attr->value_size());
            rapidxml::xml_attribute<char>* attrCopy = memoryPool.allocate_attribute(nameC, valueC, attr->name_size(), attr->value_size());
            copy->append_attribute(attrCopy);
        }
        for(rapidxml::xml_node<char>* child = aNode->first_node(); child; child = child->next_sibling()) {
            if(child->type() == rapidxml::node_element) {
                rapidxml::xml_node<char>* childCopy = deepClone(child);
                copy->append_node(childCopy);
            }
        }
        return copy;
    }
    
private:
    static rapidxml::memory_pool<char>& getStoreXMLMemoryPool() {
        static rapidxml::memory_pool<char> GLOBAL_MEM_POOL;
        return GLOBAL_MEM_POOL;
    }
};


#endif // _XML_PARSE_HELPER_H
