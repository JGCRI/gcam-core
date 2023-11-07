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

// include custom headers
#include "util/logger/include/logger_factory.h"
#include "containers/include/iscenario_runner.h"
#include "../include/emiss_downscale.h"
#include "../include/remap_data.h"

// forward declarations
class Region;

class GCAM_E3SM_interface {
public:
    GCAM_E3SM_interface();
    ~GCAM_E3SM_interface();
    void initGCAM(std::string aCaseName, std::string aGCAMConfig, std::string aGCAM2ELMCO2Map, std::string aGCAM2ELMLUCMap,
                  std::string aGCAM2ELMWHMap, std::string aGCAM2ELMCDENMap, int *aNumReg, int *aNumSector);
    void runGCAM(int *yyyymmdd, double *gcamoluc, double *gcamoemiss, std::string aBaseLucGcamFileName, std::string aBaseCO2GcamFileName, bool aSpinup,
                 double *aELMArea, double *aELMPFTFract, double *aELMNPP, double *aELMHR,
                 int *aNumLon, int *aNumLat, int *aNumPFT, int *aNumReg, int *aNumCty, int *aNumSector, int *aNumPeriod,
                  std::string aMappingFile, int *aFirstCoupledYear, bool aReadScalars, bool aWriteScalars,
                 bool aScaleAgYield, bool aScaleCarbon,  std::string aBaseNPPFileName, std::string aBaseHRFileName, std::string aBasePFTWtFileName, bool aRestartRun);
    void setLandProductivityScalingGCAM(int *yyyymmdd, double *aELMArea, double *aELMPFTFract, double *aELMNPP, double *aELMHR,
                        int *aNumLon, int *aNumLat, int *aNumPFT, std::string aMappingFile, int *aFirstCoupledYear, bool aReadScalars, bool aWriteScalars,
                        bool aScaleAgYield, bool aScaleCarbon, std::string aBaseNPPFileName, std::string aBaseHRFileName, std::string aBasePFTWtFileName);
    void downscaleEmissionsGCAM(double *gcamoemiss,
                                double *gcamoco2sfcjan, double *gcamoco2sfcfeb, double *gcamoco2sfcmar, double *gcamoco2sfcapr,
                                double *gcamoco2sfcmay, double *gcamoco2sfcjun, double *gcamoco2sfcjul, double *gcamoco2sfcaug,
                                double *gcamoco2sfcsep, double *gcamoco2sfcoct, double *gcamoco2sfcnov, double *gcamoco2sfcdec,
                                double *gcamoco2airlojan, double *gcamoco2airlofeb, double *gcamoco2airlomar, double *gcamoco2airloapr,
                                double *gcamoco2airlomay, double *gcamoco2airlojun, double *gcamoco2airlojul, double *gcamoco2airloaug,
                                double *gcamoco2airlosep, double *gcamoco2airlooct, double *gcamoco2airlonov, double *gcamoco2airlodec,
                                double *gcamoco2airhijan, double *gcamoco2airhifeb, double *gcamoco2airhimar, double *gcamoco2airhiapr,
                                double *gcamoco2airhimay, double *gcamoco2airhijun, double *gcamoco2airhijul, double *gcamoco2airhiaug,
                                double *gcamoco2airhisep, double *gcamoco2airhioct, double *gcamoco2airhinov, double *gcamoco2airhidec,
                                std::string aBaseCO2GcamFileName, std::string aBaseCO2SfcFile, std::string aBaseCO2ShipFile, std::string aBaseCO2AirFile,
                                std::string aRegionMappingFile,std::string aCountryMappingFile,std::string aCountry2RegionMappingFile,
                                std::string aPOPIIASAFileName, std::string aGDPIIASAFileName,
                                std::string aPOPGCAMFileName, std::string aGDPGCAMFileName, std::string aCO2GCAMFileName,
                                int *aNumReg, int *aNumCty, int *aNumSector, int *aNumPeriod, int *aNumLon, int *aNumLat, bool aWriteCO2, int *aCurrYear,
                                std::string CO2DownscalingMethod);
    void separateSurfaceMonthlyEmissions(EmissDownscale surfaceCO2, EmissDownscale shipmentCO2, double *gcamoco2sfcjan, double *gcamoco2sfcfeb, double *gcamoco2sfcmar,
                                                              double *gcamoco2sfcapr, double *gcamoco2sfcmay, double *gcamoco2sfcjun,
                                                              double *gcamoco2sfcjul, double *gcamoco2sfcaug, double *gcamoco2sfcsep,
                                                              double *gcamoco2sfcoct, double *gcamoco2sfcnov, double *gcamoco2sfcdec,
                                                              int aNumLon, int aNumLat);
    void finalizeGCAM();
    int gcamStartYear;
    int gcamEndYear;
    LoggerFactoryWrapper loggerFactoryWrapper;

    ReMapData mCO2EmissData;
    ReMapData mLUCData;
    ReMapData mWoodHarvestData;
    ReMapData mAGCDensityData;
    ReMapData mBGCDensityData;   
 
private:
    std::unique_ptr<IScenarioRunner> runner;
    typedef std::vector<Region*>::iterator RegionIterator;

    std::vector<double>  mGcamCO2EmissPreviousGCAMYear;
    std::vector<double>  mGcamCO2EmissCurrentGCAMYear;
};
