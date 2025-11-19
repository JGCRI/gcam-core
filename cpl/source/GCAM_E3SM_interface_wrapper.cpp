#include "../include/GCAM_E3SM_interface.h"

using namespace std;

GCAM_E3SM_interface *p_obj;

extern "C" {

  // Initialize a GCAM_E3SM_interface object
  void inite3sminterface_(int *aNumLon, int *aNumLat, int *aNumReg, int *aNumSector) {
    p_obj = new GCAM_E3SM_interface(aNumLon, aNumLat, aNumReg, aNumSector);
  }
  
  // Delete the GCAM_E3SM_interface object
  void deletee3sminterface_() {
    delete p_obj;
  }
    
  // Call the GCAM initialization
  void initcgcam_(int *yyyymmdd, char* aCaseName, char* aGCAMConfig, char* aGCAM2ELMCO2Map, char* aGCAM2ELMLUCMap, char* aGCAM2ELMWHMap, char* aGCAM2ELMCDENMap,
		  char *aBaseCO2GcamFileName, char *aBaseCO2SfcFile, char *aBaseCO2ShipFile, char *aBaseCO2AirFile,
                  double *aELMArea, int *aNumLon, int *aNumLat, int *aNumReg, int *aNumSector, int *aRestartRun) {

      // Convert to string - fortran doesn't handle string
      std::string CaseName(aCaseName);
      std::string GCAMConfig(aGCAMConfig);
      std::string GCAM2ELMCO2Map(aGCAM2ELMCO2Map);
      std::string GCAM2ELMLUCMap(aGCAM2ELMLUCMap);
      std::string GCAM2ELMWHMap(aGCAM2ELMWHMap);
      std::string GCAM2ELMCDENMap(aGCAM2ELMCDENMap);
      std::string BaseCO2GcamFileName(aBaseCO2GcamFileName);
      std::string BaseCO2SfcFile(aBaseCO2SfcFile);
      std::string BaseCO2ShipFile(aBaseCO2ShipFile);
      std::string BaseCO2AirFile(aBaseCO2AirFile);
      bool restartRun = *aRestartRun == 1 ? true : false;
      
    p_obj->initGCAM(yyyymmdd, CaseName, GCAMConfig, GCAM2ELMCO2Map, GCAM2ELMLUCMap, GCAM2ELMWHMap, GCAM2ELMCDENMap,
                     BaseCO2GcamFileName, BaseCO2SfcFile, BaseCO2ShipFile, BaseCO2AirFile,
                    aELMArea, aNumLon, aNumLat, aNumReg, aNumSector, restartRun);
  }

  // Run GCAM
  void runcgcam_(int *yyyymmdd, double *gcamoluc, double *gcamoemiss, char* aBaseLucGcamFileName, char* aBaseCO2GcamFileName, int *aSpinup,
                 double *aELMArea, double *aELMPFTFract, double *aELMNPP, double *aELMHR,
                 int *aNumLon, int *aNumLat, int *aNumPFT, int *aNumReg, int *aNumCty, int *aNumSector, int *aNumPeriod,
                 char* aMappingFile, int *aFirstCoupledYear, int *aReadScalars, int *aWriteScalars,
                 int *aScaleAgYield, int *aScaleCarbon, char* aBaseNPPFile, char* aBaseHRFile, char* aBasePFTwtFile, int *aRestartRun) {
  
      // convert to strings and bools where appropriate
      std::string BaseLucGcamFileName(aBaseLucGcamFileName);
      std::string BaseCO2GcamFileName(aBaseCO2GcamFileName);
      bool Spinup = *aSpinup == 1 ? true : false;
      std::string MappingFile(aMappingFile);
      std::string baseNPPFile(aBaseNPPFile);
      std::string baseHRFile(aBaseHRFile);
      std::string basePFTwtFile(aBasePFTwtFile);
      bool readScalars = *aReadScalars == 1 ? true : false;
      bool writeScalars = *aWriteScalars == 1 ? true : false;
      bool scaleAgYield = *aScaleAgYield == 1 ? true : false;
      bool scaleCarbon = *aScaleCarbon == 1 ? true : false;
      bool restartRun = *aRestartRun == 1 ? true : false;
  
      p_obj->runGCAM(yyyymmdd, gcamoluc, gcamoemiss, BaseLucGcamFileName, BaseCO2GcamFileName, Spinup,
                     aELMArea, aELMPFTFract, aELMNPP, aELMHR,
                     aNumLon, aNumLat, aNumPFT, aNumReg, aNumCty, aNumSector, aNumPeriod,
                     MappingFile, aFirstCoupledYear, readScalars, writeScalars,
                     scaleAgYield, scaleCarbon, baseNPPFile, baseHRFile, basePFTwtFile, restartRun);
  }

  // Downscale Emissions
  void downscaleemissionscgcam_(double *gcamoemiss,
                              double *gcamoco2sfcjan, double *gcamoco2sfcfeb, double *gcamoco2sfcmar, double *gcamoco2sfcapr,
                              double *gcamoco2sfcmay, double *gcamoco2sfcjun, double *gcamoco2sfcjul, double *gcamoco2sfcaug,
                              double *gcamoco2sfcsep, double *gcamoco2sfcoct, double *gcamoco2sfcnov, double *gcamoco2sfcdec,
                              double *gcamoco2airlojan, double *gcamoco2airlofeb, double *gcamoco2airlomar, double *gcamoco2airloapr,
                              double *gcamoco2airlomay, double *gcamoco2airlojun, double *gcamoco2airlojul, double *gcamoco2airloaug,
                              double *gcamoco2airlosep, double *gcamoco2airlooct, double *gcamoco2airlonov, double *gcamoco2airlodec,
                              double *gcamoco2airhijan, double *gcamoco2airhifeb, double *gcamoco2airhimar, double *gcamoco2airhiapr,
                              double *gcamoco2airhimay, double *gcamoco2airhijun, double *gcamoco2airhijul, double *gcamoco2airhiaug,
                              double *gcamoco2airhisep, double *gcamoco2airhioct, double *gcamoco2airhinov, double *gcamoco2airhidec,
                              char *aRegionMappingFile, char *aCountryMappingFile,char *aCountry2RegionMappingFile,
                              char *aPOPIIASAFileName, char *aGDPIIASAFileName,
                              char *aPOPGCAMFileName, char *aGDPGCAMFileName, char *aCO2GCAMFileName,
                              int *aNumReg, int *aNumCty, int *aNumSector, int *aNumPeriod, int *aNumLon, int *aNumLat,
                              int *aWriteCO2, int *aCurrYear, char *aCO2DownscalingMethod) {
      
      // Convert to string - fortran doesn't handle string
      //std::string BaseCO2SfcFile(aBaseCO2SfcFile);
      //std::string BaseCO2ShipFile(aBaseCO2ShipFile);
      //std::string BaseCO2AirFile(aBaseCO2AirFile);
      //std::string BaseCO2GcamFileName(aBaseCO2GcamFileName);
      std::string RegionMappingFile(aRegionMappingFile);
      std::string CountryMappingFile(aCountryMappingFile);
      std::string Country2RegionMappingFile(aCountry2RegionMappingFile);
      std::string POPIIASAFileName(aPOPIIASAFileName);
      std::string GDPIIASAFileName(aGDPIIASAFileName);
      std::string POPGCAMFileName(aPOPGCAMFileName);
      std::string GDPGCAMFileName(aGDPGCAMFileName);
      std::string CO2GCAMFileName(aCO2GCAMFileName);
      std::string CO2DownscalingMethod(aCO2DownscalingMethod);
      
      // Convert to bool - fortran doesn't have a bool
      bool writeCO2 = *aWriteCO2 == 1 ? true : false;
    
      p_obj->downscaleEmissionsGCAM(gcamoemiss,
                                  gcamoco2sfcjan, gcamoco2sfcfeb, gcamoco2sfcmar, gcamoco2sfcapr,
                                  gcamoco2sfcmay, gcamoco2sfcjun, gcamoco2sfcjul, gcamoco2sfcaug,
                                  gcamoco2sfcsep, gcamoco2sfcoct, gcamoco2sfcnov, gcamoco2sfcdec,
                                  gcamoco2airlojan, gcamoco2airlofeb, gcamoco2airlomar, gcamoco2airloapr,
                                  gcamoco2airlomay, gcamoco2airlojun, gcamoco2airlojul, gcamoco2airloaug,
                                  gcamoco2airlosep, gcamoco2airlooct, gcamoco2airlonov, gcamoco2airlodec,
                                  gcamoco2airhijan, gcamoco2airhifeb, gcamoco2airhimar, gcamoco2airhiapr,
                                  gcamoco2airhimay, gcamoco2airhijun, gcamoco2airhijul, gcamoco2airhiaug,
                                  gcamoco2airhisep, gcamoco2airhioct, gcamoco2airhinov, gcamoco2airhidec,
                                  RegionMappingFile, CountryMappingFile, Country2RegionMappingFile,
                                  POPIIASAFileName,GDPIIASAFileName,
                                  POPGCAMFileName,GDPGCAMFileName,CO2GCAMFileName,
                                  aNumReg, aNumCty, aNumSector, aNumPeriod, aNumLon, aNumLat,
                                  writeCO2, aCurrYear, CO2DownscalingMethod);
}

    
  // Finalize GCAM
  void finalizecgcam_() {
    p_obj->finalizeGCAM();
  }
}
