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
                          int aNumLon, int aNumLat, int aNumPFT, std::string aMappingFile, int aFirstCoupledYear, bool aReadScalars, bool aWriteScalars) {
      p_obj->setDensityGCAM(yyyymmdd, aELMArea, aELMLandFract, aELMPFTFract, aELMNPP, aELMHR,
                            aNumLon, aNumLat, aNumPFT, aMappingFile, aFirstCoupledYear, aReadScalars, aWriteScalars);
  }
    
  // Run GCAM
  void runcgcam_(int *yyyymmdd, double *gcamoluc, double *gcamoemiss, int aNumLon, int aNumLat) {
    
    p_obj->runGCAM(yyyymmdd, gcamoluc, gcamoemiss, aNumLon, aNumLat);
  }

  // Downscale Emissions
  void downscaleemissionscgcam_(double *gcamoemiss, double *gcamoco2sfcjan, double *gcamoco2sfcfeb,
                              double *gcamoco2sfcmar, double *gcamoco2sfcapr, double *gcamoco2sfcmay, double *gcamoco2sfcjun,
                              double *gcamoco2sfcjul, double *gcamoco2sfcaug, double *gcamoco2sfcsep, double *gcamoco2sfcoct,
                              double *gcamoco2sfcnov, double *gcamoco2sfcdec, std::string aBaseCO2File, double aBaseCO2Emiss,
                              int aNumLon, int aNumLat, bool aWriteCO2) {
    
    p_obj->downscaleEmissionsGCAM(gcamoemiss, gcamoco2sfcjan, gcamoco2sfcfeb, gcamoco2sfcmar, gcamoco2sfcapr,
                   gcamoco2sfcmay, gcamoco2sfcjun, gcamoco2sfcjul, gcamoco2sfcaug, gcamoco2sfcsep,
                   gcamoco2sfcoct, gcamoco2sfcnov, gcamoco2sfcdec, aBaseCO2File, aBaseCO2Emiss,
                   aNumLon, aNumLat, aWriteCO2);
  }

    
  // Finalize GCAM
  void finalizecgcam_() {
    p_obj->finalizeGCAM();
  }
}
