#ifndef __GET_DATA_HELPER_H__
#define __GET_DATA_HELPER_H__

#include <vector>

class Scenario;
class FilterStep;
class AMatcherWrapper;
class ReMapData;

class GetDataHelper {
public:
  GetDataHelper(const std::string& aHeader)
  {
    parseFilterString(aHeader);
  }
  ~GetDataHelper();
  void run(Scenario* aScenario, ReMapData& aDataMapper);
  template<typename T>
  void processData(T& aData);
private:
  std::vector<double> mDataVector;
  std::vector<int> mYearVector;
  std::vector<std::string> mColNames;
  std::vector<AMatcherWrapper*> mPathTracker;
  std::vector<FilterStep*> mFilterSteps;

  void parseFilterString(const std::string& aFilterStr );
  FilterStep* parseFilterStepStr( const std::string& aFilterStepStr, int& aCol );
  template<typename VecType>
  void vectorDataHelper(VecType& aDataVec);
};

#endif // __GET_DATA_HELPER_H__

