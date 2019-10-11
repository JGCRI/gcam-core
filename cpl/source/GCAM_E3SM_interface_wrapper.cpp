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
  void initcgcam_(std::string aCaseName, std::string aGCAMConfig, std::string aGCAM2ELMCO2Map, std::string aGCAM2ELMLUCMap) {
    p_obj->initGCAM(aCaseName, aGCAMConfig, aGCAM2ELMCO2Map, aGCAM2ELMLUCMap);
  }

  // Set Carbon Densities in GCAM using scalers from E3SM
  void setdensitycgcam_(int *yyyymmdd, double *aELMArea, double *aELMLandFract, double *aELMPFTFract, double *aELMNPP, double *aELMHR,
                          int aNumLon, int aNumLat, int aNumPFT, std::string aMappingFile, bool aReadScalars, bool aWriteScalars) {
      p_obj->setDensityGCAM(yyyymmdd, aELMArea, aELMLandFract, aELMPFTFract, aELMNPP, aELMHR,
                            aNumLon, aNumLat, aNumPFT, aMappingFile, aReadScalars, aWriteScalars);
  }
    
  // Run GCAM
  void runcgcam_(int *yyyymmdd, double *gcamoluc, double *gcamoemis, std::string aBaseCO2File, int aNumLon, int aNumLat, bool aWriteCO2) {
    p_obj->runGCAM(yyyymmdd, gcamoluc, gcamoemis, aBaseCO2File, aNumLon, aNumLat, aWriteCO2);
  }
    
  // Finalize GCAM
  void finalizecgcam_() {
    p_obj->finalizeGCAM();
  }
}
