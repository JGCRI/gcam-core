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
    void runGCAM(int *yyyymmdd, double *gcamoluc, double *gcamoemiss, int aNumLon, int aNumLat);
    void setDensityGCAM(int *yyyymmdd, double *aELMArea, double *aELMLandFract, double *aELMPFTFract, double *aELMNPP, double *aELMHR,
                        int aNumLon, int aNumLat, int aNumPFT, std::string aMappingFile, int aFirstCoupledYear, bool aReadScalars, bool aWriteScalars);
    void downscaleEmissionsGCAM(double *gcamoemiss, double *gcamoco2sfcjan, double *gcamoco2sfcfeb, double *gcamoco2sfcmar,
                            double *gcamoco2sfcapr, double *gcamoco2sfcmay, double *gcamoco2sfcjun, double *gcamoco2sfcjul,
                            double *gcamoco2sfcaug, double *gcamoco2sfcsep, double *gcamoco2sfcoct, double *gcamoco2sfcnov,
                                double *gcamoco2sfcdec, std::string aBaseCO2File, double aBaseCO2EmissSfc,int aNumLon, int aNumLat, bool aWriteCO2);
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
