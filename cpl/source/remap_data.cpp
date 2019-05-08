#include "../include/remap_data.h"

using namespace std;

template<typename T>
size_t ReMapDataHelper<T>::getIndex(const T& aGCAMName) const {
    T outputName = aGCAMName;

    if(!mGCAMToOutputNameMap.empty()) {
        auto iter = mGCAMToOutputNameMap.find(aGCAMName);
        if(iter != mGCAMToOutputNameMap.end()) {
            outputName = (*iter).second;
        }
        else {
            // error?
        }
    }

    auto iter = find(mInOrderOutputNames.begin(), mInOrderOutputNames.end(), outputName);
    if(iter == mInOrderOutputNames.end()) {
        // error handling?
        abort();
    }
    return iter - mInOrderOutputNames.begin();
}

template<typename T>
size_t ReMapDataHelper<T>::getStrideLength() const {
  return mInOrderOutputNames.size();
}

ReMapData::ReMapData():
  mIsInitialized(false),
  mData(0)
{
}

void ReMapData::addColumn(string aDataName, vector<string> aInOrderOutputNames, map<string, string> aGCAMToOutputNameMap) {
  ReMapDataHelper<string> col;
  col.mDataName = aDataName;
  col.mInOrderOutputNames = aInOrderOutputNames;
  col.mGCAMToOutputNameMap = aGCAMToOutputNameMap;
  mColumns.push_back(col);
}

void ReMapData::addYearColumn(string aDataName, vector<int> aInOrderOutputNames, map<int, int> aGCAMToOutputNameMap) {
  ReMapDataHelper<int> col;
  col.mDataName = aDataName;
  col.mInOrderOutputNames = aInOrderOutputNames;
  col.mGCAMToOutputNameMap = aGCAMToOutputNameMap;
  mYearColumn = col;
}

void ReMapData::finalizeColumns() {
    size_t size = mColumns.size() == 0 ? 0 : 1;
    for(auto col : mColumns) {
        size *= col.getStrideLength();
    }
    mData = new double[size];
    fill(mData, mData + size, 0.0);
    mIsInitialized = true;
}

void ReMapData::setData(const vector<string>& aColValues, const int aYearValue, const double aValue) {
    // TODO: assume columns are in order?  If not we will want a vector<pair> and map column to index
    // TODO: error checking such as column lengths match
    size_t index = mYearColumn.getIndex(aYearValue);
    size_t currStride = mYearColumn.getStrideLength();
    //for(size_t colIndex = aColValues.size() -1; colIndex >= 0; --colIndex) {
    for(size_t colIndex = aColValues.size(); colIndex-- > 0; ) {
        index += mColumns[colIndex].getIndex(aColValues[colIndex]) * currStride;
        currStride *= mColumns[colIndex].getStrideLength();
    }
    mData[index] += aValue;
}

double* ReMapData::getData() {
    return mData;
}
