/*
* \author Kate Calvin
*/

#include "util/base/include/definitions.h"

// include standard libraries
#include <iostream>
#include <fstream>
#include <string>
#include <memory>
#include <list>

// xerces xml headers
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>

// include custom headers
#include "util/base/include/xml_helper.h"
#include "util/base/include/configuration.h"
#include "containers/include/iscenario_runner.h"
#include "containers/include/scenario_runner_factory.h"
#include "containers/include/region.h"
#include "util/logger/include/ilogger.h"
#include "util/logger/include/logger_factory.h"
#include "util/base/include/timer.h"
#include "util/base/include/version.h"
#include "../include/remap_data.h"

using namespace std;
using namespace xercesc;

class GCAM_E3SM_interface {
public:
    GCAM_E3SM_interface();
    ~GCAM_E3SM_interface();
    void initGCAM(std::string aCaseName, std::string aGCAMConfig, std::string aGCAM2ELMCO2Map, std::string aGCAM2ELMLUCMap, std::string aGCAM2ELMWHMap);
    void runGCAM(int *yyyymmdd, double *gcamoluc, double *gcamoemis, std::string aBaseCO2File, int aNumLon, int aNumLat, bool aWriteCO2);
    void setDensityGCAM(int *yyyymmdd, double *aELMArea, double *aELMLandFract, double *aELMPFTFract, double *aELMNPP, double *aELMHR,
                        int aNumLon, int aNumLat, int aNumPFT, std::string aMappingFile, bool aReadScalars, bool aWriteScalars);
    void finalizeGCAM();
    int gcamStartYear;
    int gcamEndYear;
    LoggerFactoryWrapper loggerFactoryWrapper;
    
    ReMapData mCO2EmissData;
    ReMapData mLUCData;
    ReMapData mWoodHarvestData;
    
private:
    std::auto_ptr<IScenarioRunner> runner;
    typedef std::vector<Region*>::iterator RegionIterator;
};
