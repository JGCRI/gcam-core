#include "../include/GCAM_E3SM_interface.h"

GCAM_E3SM_interface *p_obj;

extern "C" {

  // Initialize a GCAM_E3SM_interface object
  void inite3sminterface_() {
    p_obj = new GCAM_E3SM_interface();
  }
  
  // Delete the GCAM_E3SM_interface object
  void deletee3sminterface_() {
    delete p_obj;
  }
    
  // Call the GCAM initialization
  void initcgcam_(char* aCaseName, char* aGCAMConfig, char* aGCAM2ELMCO2Map, char* aGCAM2ELMLUCMap, char* aGCAM2ELMWHMap, char* aGCAM2ELMCDENMap) {
      
      // Convert to string - fortran doesn't handle string
      std::string CaseName(aCaseName);
      std::string GCAMConfig(aGCAMConfig);
      std::string GCAM2ELMCO2Map(aGCAM2ELMCO2Map);
      std::string GCAM2ELMLUCMap(aGCAM2ELMLUCMap);
      std::string GCAM2ELMWHMap(aGCAM2ELMWHMap);
      std::string GCAM2ELMCDENMap(aGCAM2ELMCDENMap);
      
    p_obj->initGCAM(CaseName, GCAMConfig, GCAM2ELMCO2Map, GCAM2ELMLUCMap, GCAM2ELMWHMap, GCAM2ELMCDENMap);
  }

// todo: delete setdensitycgcam

  // Set Carbon Densities in GCAM using scalers from E3SM
//  void setdensitycgcam_(int *yyyymmdd, double *aELMArea, double *aELMPFTFract, double *aELMNPP, double *aELMHR,
//                          int *aNumLon, int *aNumLat, int *aNumPFT, char* aMappingFile, int *aFirstCoupledYear, int *aReadScalars, int *aWriteScalars,
//                          int *aScaleCarbon, char* aBaseNPPFile, char* aBaseHRFile, char* aBasePFTwtFile) {
      
      // Convert to string - fortran doesn't handle string
//      std::string MappingFile(aMappingFile);
//      std::string baseNPPFile(aBaseNPPFile);
//      std::string baseHRFile(aBaseHRFile);
//      std::string basePFTwtFile(aBasePFTwtFile);
      
      // Convert to bool - fortran doesn't have a bool
//      bool readScalars = *aReadScalars == 1 ? true : false;
//      bool writeScalars = *aWriteScalars == 1 ? true : false;
//      bool scaleCarbon = *aScaleCarbon == 1 ? true : false;     
 
//      p_obj->setDensityGCAM(yyyymmdd, aELMArea, aELMPFTFract, aELMNPP, aELMHR,
//                            aNumLon, aNumLat, aNumPFT, MappingFile, aFirstCoupledYear, readScalars, writeScalars,
//                            scaleCarbon, baseNPPFile, baseHRFile, basePFTwtFile);
//  }
/// end delete

    
  // Run GCAM
  void runcgcam_(int *yyyymmdd, double *gcamoluc, double *gcamoemiss, char* aBaseLucGcamFileName, char* aBaseCO2GcamFileName, int *aSpinup,
                 double *aELMArea, double *aELMPFTFract, double *aELMNPP, double *aELMHR,
                 int *aNumLon, int *aNumLat, int *aNumPFT, char* aMappingFile, int *aFirstCoupledYear, int *aReadScalars, int *aWriteScalars,
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
                     aNumLon, aNumLat, aNumPFT, MappingFile, aFirstCoupledYear, readScalars, writeScalars,
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
                              char *aBaseCO2GcamFileName, char *aBaseCO2SfcFile, char *aBaseCO2AirFile,
                              int *aNumReg, int *aNumSector, int *aNumLon, int *aNumLat, int* aWriteCO2, int *aCurrYear) {
      
      // Convert to string - fortran doesn't handle string
      std::string BaseCO2SfcFile(aBaseCO2SfcFile);
      std::string BaseCO2AirFile(aBaseCO2AirFile);
      std::string BaseCO2GcamFileName(aBaseCO2GcamFileName);
      
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
                                  BaseCO2GcamFileName, BaseCO2SfcFile, BaseCO2AirFile,
                                  aNumReg, aNumSector, aNumLon, aNumLat, writeCO2, aCurrYear);
}

    
  // Finalize GCAM
  void finalizecgcam_() {
    p_obj->finalizeGCAM();
  }
}
