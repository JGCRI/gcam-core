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
#include "reporting/include/xml_db_outputter.h"

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
            const T fromName = XMLHelper<T>::getAttr( curr, "from" );
            const T toName = XMLHelper<T>::getAttr( curr, "to" );
            mGCAMToOutputNameMap.insert( std::make_pair( fromName, toName ) );
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
mLandNameColumn(false),
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
        else if( nodeName == "land-name-column" ) {
            mLandNameColumn = true;
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
    // First delete the old mData so we can reinitialize it.
    delete mData;
    
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
void ReMapData::setData(vector<string>& aColValues, const int aYearValue, const double aValue) {
    // TODO: assume columns are in order?  If not we will want a vector<pair> and map column to index
    // TODO: error checking such as column lengths match
    
    // For land allocation, we need to split and recombine column names.
    if( mLandNameColumn ) {
        // First figure out which column has the region names and which has the land types
        size_t regionIndex = 0;
        size_t landTypeIndex = 1;
        for(size_t colIndex = aColValues.size(); colIndex-- > 0; ) {
            if ( mColumns[colIndex].getName() == "region" ) {
                regionIndex = colIndex;
            }
            else if ( mColumns[colIndex].getName() == "land-type" ) {
                landTypeIndex = colIndex;
            }
        }
        
        // Next, use `decomposeLandName` to determine the region and land type.
        // Update the column values to reflect these names.
        map<string, string> landNames = XMLDBOutputter::decomposeLandName(aColValues[landTypeIndex]);
        aColValues[regionIndex] = aColValues[regionIndex] + "_" + landNames["land-region"];
        aColValues[landTypeIndex] = landNames["crop"];
    }

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

/*! \brief Get length of array
 *
 * \return size size_t with number of elements in the array
 *
 * \author Pralit Patel
 */
size_t ReMapData::getArrayLength() const {
    size_t size = mYearColumn.getStrideLength();
    for(auto col : mColumns) {
        size *= col.getStrideLength();
    }
    
    return size;
}

/*!
 * \brief The print the data as a table which may be useful for
 *        diagnostics.
 * \details This method will produce column headers and seperate
 *          output with a comma.
 * \param aOut The output stream to send data to.
 * \return The given output stream for chaining
 */
ostream& ReMapData::printAsTable( ostream& aOut ) const {
    const string DELIM = ",";
    if( !mIsInitialized ) {
        aOut << "No initialized." << endl;
    }
    else {
        // print a header
        for(auto col : mColumns) {
            aOut << col.getName() << DELIM;
        }
        aOut << mYearColumn.getName() << DELIM << "value" << endl;

        // print all data
        const size_t rowLength = getArrayLength();
        vector<size_t> colIndices(mColumns.size(), 0);
        size_t yearIndex = 0;
        for(size_t row = 0; row < rowLength; ++row) {
            // update the index for each column where stride is increasing
            // fastest from right to left of the tablej
            bool incrColIndex = false;
            // avoid incrementing indicies for the very first row
            if(row > 0 && ++yearIndex == mYearColumn.mInOrderOutputNames.size()) {
                yearIndex = 0;
                incrColIndex = true;
            }
            for(size_t colIndex = mColumns.size(); colIndex-- > 0 && incrColIndex; ) {
                if(++colIndices[colIndex] == mColumns[colIndex].mInOrderOutputNames.size()) {
                    colIndices[colIndex] = 0;
                }
                else {
                    incrColIndex = false;
                }
            }

            // print values at the current indicies
            for(size_t colIndex = 0; colIndex < mColumns.size(); ++colIndex) {
                aOut << mColumns[colIndex].mInOrderOutputNames[colIndices[colIndex]] << DELIM;
            }
            aOut << mYearColumn.mInOrderOutputNames[yearIndex] << DELIM;
            aOut << mData[ row ] << endl;
        }
    }
    return aOut;
}

/*!
 * \brief Overload the output stream operator for ReMapData.
 * \details Simply calls ReMapData::printAsTable
 * \param aOut The output stream to send data to.
 * \param aData The instance of ReMapData to print.
 * \return The given output stream for chaining
 */
ostream& operator<<( ostream& aOut, const ReMapData& aData ) {
    return aData.printAsTable( aOut );
}

