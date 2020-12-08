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
  void initcgcam_(std::string aCaseName, std::string aGCAMConfig, std::string aGCAM2ELMCO2Map, std::string aGCAM2ELMLUCMap, std::string aGCAM2ELMWHMap) {
    p_obj->initGCAM(aCaseName, aGCAMConfig, aGCAM2ELMCO2Map, aGCAM2ELMLUCMap, aGCAM2ELMWHMap);
  }

  // Set Carbon Densities in GCAM using scalers from E3SM
  void setdensitycgcam_(int *yyyymmdd, double *aELMArea, double *aELMLandFract, double *aELMPFTFract, double *aELMNPP, double *aELMHR,
                          int aNumLon, int aNumLat, int aNumPFT, std::string aMappingFile, int aFirstCoupledYear, bool aReadScalars, bool aWriteScalars, int aCurrYear) {
      p_obj->setDensityGCAM(yyyymmdd, aELMArea, aELMLandFract, aELMPFTFract, aELMNPP, aELMHR,
                            aNumLon, aNumLat, aNumPFT, aMappingFile, aFirstCoupledYear, aReadScalars, aWriteScalars, aCurrYear);
  }
    
  // Run GCAM
  void runcgcam_(int *yyyymmdd, double *gcamoluc, double *gcamoemiss, int aNumLon, int aNumLat) {
    
    p_obj->runGCAM(yyyymmdd, gcamoluc, gcamoemiss, aNumLon, aNumLat);
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
                              std::string aBaseCO2SfcFile, double aBaseCO2EmissSfc, std::string aBaseCO2AirFile, double aBaseCO2EmissAir,
                              int aNumLon, int aNumLat, bool aWriteCO2, int aCurrYear) {
    
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
                                  aBaseCO2SfcFile, aBaseCO2EmissSfc, aBaseCO2AirFile, aBaseCO2EmissAir,
                                  aNumLon, aNumLat, aWriteCO2, aCurrYear);
}

    
  // Finalize GCAM
  void finalizecgcam_() {
    p_obj->finalizeGCAM();
  }
}
