#ifndef __REMAP_DATA__
#define __REMAP_DATA__

#include <string>
#include <vector>
#include <map>

template<typename T>
struct ReMapDataHelper {
public:
    std::string mDataName;
    std::vector<T> mInOrderOutputNames;
    std::map<T, T> mGCAMToOutputNameMap;
    size_t getIndex(const T& aGCAMName) const;
    size_t getStrideLength() const;
};

class ReMapData {
public:
    ReMapData();
    ~ReMapData();
    void addColumn(std::string aDataName, std::vector<std::string> aInOrderOutputNames, std::map<std::string, std::string> aGCAMToOutputNameMap);
    void addYearColumn(std::string aDataName, std::vector<int> aInOrderOutputNames, std::map<int, int> aGCAMToOutputNameMap);
    void finalizeColumns();
    void setData(const std::vector<std::string>& aColValues, const int aYearValue, const double aValue);
    double* getData();
private:
    bool mIsInitialized;
    std::vector<ReMapDataHelper<std::string> > mColumns;
    ReMapDataHelper<int> mYearColumn;
    double* mData;
};

#endif // __REMAP_DATA__
