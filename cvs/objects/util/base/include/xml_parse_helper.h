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
 * \brief The XMLParseHelper class header file.
 * \author Pralit Patel
 */

#include "util/base/include/definitions.h"
#include <string>
#include <vector>
#include <map>
#include <cstring>
#include <boost/lexical_cast.hpp>

#ifdef USE_STANDALONE_RAPIDXML
#include <rapidxml.hpp>
#else
#include <boost/property_tree/detail/rapidxml.hpp>
#endif

#include "util/base/include/aparsable.h"

// Forward declare the various classes that could represent a "root"
// for parsing XML
class Scenario;
class IScenarioRunner;
class LoggerFactoryWrapper;
class Configuration;

/*!
 * \brief A set of static helper function for parsing XML data from rapidxml into GCAM data structures.
 * \details This class includes a few utilities for dealing with rapidxml in general including
 *            - parseXML: which reads XML from a file and kicks off the GCAM parsing process.
 *            - getNodeName / getNodeValue / getAllAttrs: general purpose utilities to safely get
 *             character data out of rapidxml into standard C++ strings which may not be intuitive.
 *
 *          As well as the two main methods:
 *            - tagsMatch: A templated method to determine if an XML tag matches a GCAM Fusion
 *                     Data object which.
 *            - parseData: If the tags match then this templated method will be used to convert the
 *                     XML into the C++ data structure with the Data object wraps.
 *
 *          When CONTAINER data needs to be parsed we need one more utility class: ParseChildData
 *          which will recursively continue the process of matching tags and parsing data on the child
 *          XML nodes by having GCAM Fusion to expand the Data definitions of that given CONTAINER.
 *          Note that if the CONTAINER is a subclass of AParsable it will be given the opportunity to handle
 *          the current XML node before attempting default parsing behaviors.
 * \sa AParsable
 * \author Pralit Patel
 */
struct XMLParseHelper {
    static void initParser();
    
    static void cleanupParser();
    
    // the top level call to start the parsing of XML for the various "root" classes
    static bool parseXML( const std::string& aXMLFile, Scenario* aRootElement );
    static bool parseXML( const std::string& aXMLFile, IScenarioRunner* & aRootElement );
    static bool parseXML( const std::string& aXMLFile, LoggerFactoryWrapper* aRootElement );
    static bool parseXML( const std::string& aXMLFile, Configuration* aRootElement );
    
    /*!
     * \brief Extract the node name of the given node as a std::string.
     * \param aNode The rapidxml node to extract the data from.
     * \return The node name of the given element.
     */
    static std::string getNodeName(const rapidxml::xml_node<char>* aNode) {
        return std::string(aNode->name(), aNode->name_size());
    }
    
    /*!
     * \brief Extract the node value of the given node as the requested type T.
     * \tparam T The type to attempt to coerce to node value into.
     * \param aNode The rapidxml node to extract the data from.
     * \return The node value of the given element.
     */
    template<typename T>
    static T getValue(const rapidxml::xml_node<char>* aNode) {
        std::string nodeValueStr(aNode->value(), aNode->value_size());
        return getValue<T>(nodeValueStr);
    }
    
    /*!
     * \brief Convert the given string value to the requested type T.
     * \details This method relies on boost::lexical_cast to do the heavy work.  On failure a
     *          warning will be given and T() will be returned.
     * \tparam T The type to attempt to coerce to string value into.
     * \param aValueStr The string representation of some value to attempt to convert into
     *                 the requested C++ type.
     * \return The node value of the given string.
     */
    template<typename T>
    static T getValue(const std::string& aValueStr) {
        try {
            T returnValue = boost::lexical_cast<T>( aValueStr );
            return returnValue;
        }
        catch( boost::bad_lexical_cast& ) {
            // ideally we would send this to a logger however this method may
            // be called before the loggers have been initialized
            std::cout << "Cast of node with value " << aValueStr << " to return type "
                      << typeid(T).name() << " failed." << std::endl;
        }
        
        return T();
    }
    
    /*!
     * \brief Extract the node attributes of the given node as a map of std::string representing
     *        attribute name / value pairs.
     * \param aNode The rapidxml node to extract the attributes from.
     * \return The attributes of the given element.
     */
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
    
    /*!
     * \brief A connivence method to see if some attribute "flag" is set.
     * \details Here we consider a flag set if the attribute is found and has a value exactly equal to "1".
     * \param aAttrs A map the XML attribute name / values.
     * \param aFlag The attribute name to check.
     * \return True if aFlag is found and equal to "1".
     */
    static bool isAttrFlagSet(const std::map<std::string, std::string>& aAttrs, const std::string& aFlag) {
        auto iter = aAttrs.find(aFlag);
        return iter != aAttrs.end() && (*iter).second == "1";
    }
    
    template<typename DataType>
    static bool tagsMatch(const std::string& aXMLTag, const DataType& aData);

    template<typename DataType>
    static void parseData(const rapidxml::xml_node<char>* aNode, DataType& aData);
    
    /*!
     * \brief Make a deep copy of the given rapidxml Node into the "store xml" memory pool.
     * \details We may need to stash XML elements for later processing such as for stub technologies.  This
     *          method facilitates that by copying all the attributes, and child nodes recursively into a memory
     *          pool which will be kept alive long enough that the parsing of these elements can be deferred until
     *          completeInit.
     * \param aNode The Node which needs to be copied.
     * \return A full copy of the given node which will be kept "alive" until cleanupParser is called.
     */
    static rapidxml::xml_node<char>* deepClone(rapidxml::xml_node<char>* aNode) {
        // get the "store xml" memory pool
        rapidxml::memory_pool<char>& memoryPool = getStoreXMLMemoryPool();
        // copy node name and values
        char* nameC = memoryPool.allocate_string(aNode->name(), aNode->name_size());
        char* valueC = memoryPool.allocate_string(aNode->value(), aNode->value_size());
        rapidxml::xml_node<char>* copy = memoryPool.allocate_node(aNode->type(), nameC, valueC, aNode->name_size(), aNode->value_size());
        // copy attributes
        for(rapidxml::xml_attribute<char> *attr = aNode->first_attribute(); attr; attr = attr->next_attribute()) {
            char* nameC = memoryPool.allocate_string(attr->name(), attr->name_size());
            char* valueC = memoryPool.allocate_string(attr->value(), attr->value_size());
            rapidxml::xml_attribute<char>* attrCopy = memoryPool.allocate_attribute(nameC, valueC, attr->name_size(), attr->value_size());
            copy->append_attribute(attrCopy);
        }
        // recursively copy all element child nodes
        for(rapidxml::xml_node<char>* child = aNode->first_node(); child; child = child->next_sibling()) {
            if(child->type() == rapidxml::node_element) {
                rapidxml::xml_node<char>* childCopy = deepClone(child);
                copy->append_node(childCopy);
            }
        }
        return copy;
    }
    
private:
    /*!
     * \brief Get a reference to a memory pool which will be kept around long enough such that it
     *        can be used to copy XML node and defer their parsing until completeInit.
     * \details This memory pool will be cleared during cleanupParser releasing the memory for any
     *          copied XML nodes with it.
     * \return A reference to the "store xml" memory pool.
     */
    static rapidxml::memory_pool<char>& getStoreXMLMemoryPool() {
        static rapidxml::memory_pool<char> GLOBAL_MEM_POOL;
        return GLOBAL_MEM_POOL;
    }
};

/*!
 * \brief A helper class for GCAM Fusion call back to be able to to recursively process the
 *        child data of some CONTAINER.
 * \details In addition to providing a call back to kick of the recursive processing of child Data
 *          elements, we also stash away a reference to the parent XML node, the attributes of
 *          that node (for performance reasons), and finally determine if the CONTAINER was a
 *          subclass of AParsable in which case it should be given the opportunity to provide custom
 *          handling of child XML nodes.
 */
class ParseChildData {
public:
    /*!
     * \brief Create new helper to parse the child elements of the given node.
     * \details The parent node and its attributes are stored for convenience.
     * \param aNode The XML node who's children need to be processed.
     * \param aAttrs The attributes already converted to strings which is saved for performance reasons.
     */
    ParseChildData(const rapidxml::xml_node<char>* aNode, const std::map<std::string, std::string>& aAttrs):mParentNode(aNode), mAttrs(aAttrs), mContainer(0) {}
    

    /*!
     * \brief Sets a reference to the CONTAINER object that will be parsing the child nodes.
     * \details Only needed for CONTAINER types that are subclass of AParsable for whom
     *          the generic parsing routine will call back to so as to allow custom parsing of the
     *          XML nodes.  Otherwise this call is essentially ignored.
     * \param aContainer A pointer to the object to handle the parsing of the data.
     */
    template<typename ContainerType>
    typename boost::disable_if<
        boost::is_base_of<AParsable, ContainerType>,
    void>::type setContainer(ContainerType* aContainer) {
        // not a subclass of AParsable, ignore
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
    
    //! The attributes already converted to strings saved here for performance reasons
    const std::map<std::string, std::string>& mAttrs;
    
    //! If the current CONTAINER being processed is a subclass of AParsable we initialize
    //! mContainer to it which will then enable custom parsing behavior, otherwise this member
    //! variable will just be ignored.
    AParsable* mContainer;
};

#endif // _XML_PARSE_HELPER_H
