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
 * \file remap_data.cpp
 * \brief This file remaps and aggregates data to pre-specified region, sector combinations
 *
 * \author Pralit Patel
 */

#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "../include/remap_data.h"
#include "util/base/include/xml_helper.h"

using namespace std;
using namespace xercesc;

// Find the index of a particular item in the mapping
template<typename T>
size_t ReMapDataHelper<T>::getIndex(const T& aGCAMName) const {
    T outputName = aGCAMName;

    // If the output map isn't empty, loop over it until you find the right item.
    if(!mGCAMToOutputNameMap.empty()) {
        auto iter = mGCAMToOutputNameMap.find(aGCAMName);
        if(iter != mGCAMToOutputNameMap.end()) {
            outputName = (*iter).second;
        }
    }

    // Ensure the item exists in the ordered list of names
    auto iter = find(mInOrderOutputNames.begin(), mInOrderOutputNames.end(), outputName);
    if(iter == mInOrderOutputNames.end()) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Can't find " << outputName << " in " << mDataName << " mapping." << endl;
        abort();
    }
    
    // Return the index (i.e., the difference between where the item is in mInOrderOutputNames and
    // the beginning of mInOrderOutputNames)
    return iter - mInOrderOutputNames.begin();
}

template<typename T>
size_t ReMapDataHelper<T>::getStrideLength() const {
  return mInOrderOutputNames.size();
}

template<typename T>
string ReMapDataHelper<T>::getName() const {
    return mDataName;
}

/*! \brief Parse a column within the mapping file
 *
 * This method reads in the output-data list and any column mappings
 * \author Kate Calvin
 */
template<typename T>
bool ReMapDataHelper<T>::XMLParse( const DOMNode* aNode ) {
    
    // assume we were passed a valid node.
    assert( aNode );
    
    // get the first child the node.
    DOMNode* curr = aNode->getFirstChild();
    
    // loop through the children
    while( curr ) {
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
        if( nodeName == "#text" ) {
            curr = curr->getNextSibling();
            continue;
        }
        else if( nodeName == "output-data" ) {
            T tempName = XMLHelper<T>::getValue( curr );
            mInOrderOutputNames.push_back(tempName);
        }
        else if( nodeName == "map" ) {
            const std::string fromName = XMLHelper<std::string>::getAttr( curr, "from" );
            const std::string toName = XMLHelper<std::string>::getAttr( curr, "to" );
            mGCAMToOutputNameMap.insert( std::pair<string,string>( fromName, toName ));
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing "
            << "column information" << "." << endl;
        }
        
        // Get the next child of aNode to process.
        curr = curr->getNextSibling();
    }
    return true;
}

/*
 The ReMapData class is used to help aggregate GCAM data to user-specified levels.
 */

// Constructor
ReMapData::ReMapData():
  mIsInitialized(false),
  mData(0)
{
}

// Destructor
ReMapData::~ReMapData() {
    delete mData;
}

/*! \brief Parse the mapping files
 *
 * This method reads all of the columns in a mapping file
 * \author Kate Calvin
 */
bool ReMapData::XMLParse( const DOMNode* aNode ) {
    // assume we were passed a valid node.
    assert( aNode );
    
    // get the first child the node.
    DOMNode* curr = aNode->getFirstChild();
    
    // loop through the children
    while( curr ) {
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
        
        if( nodeName == "#text" ) {
            curr = curr->getNextSibling();
            continue;
        }
        else if( nodeName == "column" ) {
            // First determine if the node exists.
            const std::string objName = XMLHelper<std::string>::getAttr( curr, "name" );
            
            // Search the insert to vector for an item with the name.
            auto iter = mColumns.end();
            for ( auto currIter = mColumns.begin(); currIter != mColumns.end(); ++currIter ) {
                if( (*currIter).getName() == objName ) {
                    iter = currIter;
                }
            }
            
            // Check if the node already exists in the model tree.
            if( iter != mColumns.end() ){
                // Modify or delete the node based on the contents of the delete attribute.
                (*iter).XMLParse( curr );
            } else {
                ReMapDataHelper<string> newHelper;
                newHelper.mDataName = objName;
                newHelper.XMLParse( curr );
                mColumns.push_back( newHelper );
            }
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing "
            << "re-map data" << "." << endl;
        }
        
        // Get the next child of aNode to process.
        curr = curr->getNextSibling();
    }
    
    return true;
}

/*! \brief Add a column to the data map
 *
 * This method adds a column to mColumns
 * \author Pralit Patel
 */
void ReMapData::addColumn(string aDataName, vector<string> aInOrderOutputNames, map<string, string> aGCAMToOutputNameMap) {
  ReMapDataHelper<string> col;
  col.mDataName = aDataName;
  col.mInOrderOutputNames = aInOrderOutputNames;
  col.mGCAMToOutputNameMap = aGCAMToOutputNameMap;
  mColumns.push_back(col);
}

/*! \brief Add the year column
 *
 * This method overwrites mYearColumn with a new column
 *
 * \author Pralit Patel
 */
void ReMapData::addYearColumn(string aDataName, vector<int> aInOrderOutputNames, map<int, int> aGCAMToOutputNameMap) {
  ReMapDataHelper<int> col;
  col.mDataName = aDataName;
  col.mInOrderOutputNames = aInOrderOutputNames;
  col.mGCAMToOutputNameMap = aGCAMToOutputNameMap;
  mYearColumn = col;
}

/*! \brief Finalize the columns
 *
 * This method calculates the size of mYearColumn and mColumns and
 * resizes mData accordingly. It also sets a flag indicating that
 * mData has been initialized.
 *
 * \author Pralit Patel
 */
void ReMapData::finalizeColumns() {
    // TODO: Error checking (if no year column or mColumns error?)
    // Calculate total size by multiplying length of each column
    size_t size = mYearColumn.getStrideLength();
    for(auto col : mColumns) {
        size *= col.getStrideLength();
    }
    mData = new double[size];
    
    // Initalize all elements to zero and set the flag indicating data has been initialized
    fill(mData, mData+size, 0.0);
    mIsInitialized = true;
}

/*! \brief Set data in the data map
 *
 * This method calculates the size of mYearColumn and mColumns and
 * resizes mData accordingly. It also sets a flag indicating that
 * mData has been initialized.
 *
 * \param aColValues
 * \param aYearValue current year
 * \param aValue value to add to aggregated data
 *
 * \author Pralit Patel
 */
void ReMapData::setData(const vector<string>& aColValues, const int aYearValue, const double aValue) {
    // TODO: assume columns are in order?  If not we will want a vector<pair> and map column to index
    // TODO: error checking such as column lengths match
    
    // Only set data if value is non-zero
    if ( aValue != 0 ) {
        // Find the index for this particular data element
        size_t index = mYearColumn.getIndex(aYearValue);
        size_t currStride = mYearColumn.getStrideLength();
        for(size_t colIndex = aColValues.size(); colIndex-- > 0; ) {
            index += mColumns[colIndex].getIndex(aColValues[colIndex]) * currStride;
            currStride *= mColumns[colIndex].getStrideLength();
        }
        
        // If index isn't found, then abort
        if(index >= currStride) {
            abort();
        }
        
        // Add the value to the appropriate index in mData
        mData[index] += aValue;
    }
}

/*! \brief Get data
 *
 * \return mData vector with all requested data
 *
 * \author Pralit Patel
 */
double* ReMapData::getData() {
    return mData;
}
